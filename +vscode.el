;;; +vscode.el --- run .vscode tasks/launch configs, devcontainer aware -*- lexical-binding: t; -*-

(require 'json)
(require 'compile)
(require 'subr-x)
(require 'cl-lib)

(defvar +vscode-container-override nil
  "Container name to use instead of the one derived from devcontainer.json.")

(defvar +vscode--input-history (make-hash-table :test #'equal))
(defvar +vscode--task-history nil)
(defvar +vscode--launch-history nil)

;;; JSONC

(defun +vscode--strip-jsonc ()
  "Remove comments and trailing commas in the current buffer."
  (goto-char (point-min))
  (let (in-str)
    (while (not (eobp))
      (let ((c (char-after)))
        (cond
         (in-str
          (cond ((eq c ?\\) (forward-char 2))
                ((eq c ?\") (setq in-str nil) (forward-char))
                (t (forward-char))))
         ((eq c ?\") (setq in-str t) (forward-char))
         ((looking-at "//") (delete-region (point) (line-end-position)))
         ((looking-at "/\\*")
          (delete-region (point) (if (search-forward "*/" nil t) (point) (point-max))))
         ((and (eq c ?,) (looking-at ",[ \t\n\r]*[]}]")) (delete-char 1))
         (t (forward-char)))))))

(defun +vscode--read-json (file)
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (+vscode--strip-jsonc)
      (goto-char (point-min))
      (json-parse-buffer :object-type 'plist :array-type 'array
                         :null-object nil :false-object :json-false))))

;;; Workspace / devcontainer

(defun +vscode-root ()
  (or (locate-dominating-file default-directory ".vscode")
      (user-error "No .vscode directory above %s" default-directory)))

(defun +vscode--devcontainer (root)
  "Return plist (:container :user :remote) for ROOT, or nil when running locally."
  (when-let* ((dc (+vscode--read-json (expand-file-name ".devcontainer/devcontainer.json" root)))
              (remote (plist-get dc :workspaceFolder)))
    (let ((name (or +vscode-container-override
                    (when-let* ((svc (plist-get dc :service)))
                      (string-trim
                       (shell-command-to-string
                        (format "docker ps --filter label=com.docker.compose.service=%s --format '{{.Names}}' | head -1"
                                (shell-quote-argument svc))))))))
      (when (string-empty-p (or name ""))
        (user-error "Dev container for %s is not running" root))
      (list :container name
            :user (or (plist-get dc :remoteUser) (plist-get dc :containerUser))
            :remote (file-name-as-directory remote)))))

(defun +vscode--ctx (&optional root)
  "Context for ROOT.  Over TRAMP (e.g. /docker:...) commands already run
remotely, so no docker exec wrapping (:dc nil)."
  (let* ((root (file-name-as-directory (expand-file-name (or root (+vscode-root)))))
         (dc (unless (file-remote-p root) (+vscode--devcontainer root))))
    (list :root root :dc dc)))

(defun +vscode--to-remote (ctx path)
  (let ((root (plist-get ctx :root)) (dc (plist-get ctx :dc)))
    (cond ((and path (file-remote-p root)) (file-local-name path))
          ((and dc path (string-prefix-p root path))
           (concat (plist-get dc :remote) (string-remove-prefix root path)))
          (t path))))

(defun +vscode--to-local (ctx path)
  (let ((root (plist-get ctx :root)) (dc (plist-get ctx :dc)))
    (cond ((and path (file-remote-p root) (file-name-absolute-p path)
                (not (file-remote-p path)))
           (concat (file-remote-p root) path))
          ((and dc path (string-prefix-p (plist-get dc :remote) path))
           (concat root (string-remove-prefix (plist-get dc :remote) path)))
          (t path))))

(defun +vscode--exec-args (ctx &optional tty)
  "docker exec prefix (list) for CTX, nil when local."
  (when-let* ((dc (plist-get ctx :dc)))
    (append (list "docker" "exec" (if tty "-it" "-i"))
            (when-let* ((u (plist-get dc :user))) (list "-u" u))
            (list "-w" (directory-file-name (plist-get dc :remote)) (plist-get dc :container)))))

(defun +vscode--adapter-exec-args (ctx)
  "docker exec prefix for starting a debug adapter from the host.
TRAMP's docker method always uses \"-it\", which breaks DAP's stdio
framing, so for /docker: roots run a plain \"-i\" pipe instead."
  (let ((root (plist-get ctx :root)))
    (if (equal (file-remote-p root 'method) "docker")
        (append (list "docker" "exec" "-i")
                (when-let* ((u (file-remote-p root 'user))) (list "-u" u))
                (list "-w" (directory-file-name (file-local-name root))
                      (file-remote-p root 'host)))
      (+vscode--exec-args ctx))))

;;; Variable substitution

(defun +vscode--input (ctx id)
  (let* ((inputs (plist-get ctx :inputs))
         (spec (seq-find (lambda (i) (equal (plist-get i :id) id)) inputs))
         (desc (or (plist-get spec :description) id))
         (key (concat (plist-get ctx :root) id))
         (last (gethash key +vscode--input-history))
         (default (or last (plist-get spec :default)))
         (val (pcase (plist-get spec :type)
                ("pickString"
                 (completing-read (format "%s: " desc)
                                  (mapcar (lambda (o) (if (stringp o) o (plist-get o :value)))
                                          (plist-get spec :options))
                                  nil t nil nil default))
                (_ (read-string (format "%s: " desc) nil nil default)))))
    (puthash key val +vscode--input-history)
    val))

(defun +vscode--subst (ctx s)
  (if (not (stringp s)) s
    (let* ((file (or (buffer-file-name) default-directory))
           (r (lambda (p) (+vscode--to-remote ctx p)))
           (ws (directory-file-name (funcall r (plist-get ctx :root)))))
      (replace-regexp-in-string
       "\\${\\([^}]+\\)}"
       (lambda (m)
         (let ((v (match-string 1 m)))
           (save-match-data
             (cond
              ((equal v "workspaceFolder") ws)
              ((equal v "workspaceRoot") ws)
              ((equal v "workspaceFolderBasename") (file-name-nondirectory ws))
              ((equal v "file") (funcall r file))
              ((equal v "fileBasename") (file-name-nondirectory file))
              ((equal v "fileBasenameNoExtension") (file-name-base file))
              ((equal v "fileExtname") (concat "." (file-name-extension file)))
              ((equal v "fileDirname") (directory-file-name (funcall r (file-name-directory file))))
              ((equal v "relativeFile") (file-relative-name file (plist-get ctx :root)))
              ((equal v "lineNumber") (number-to-string (line-number-at-pos)))
              ((equal v "cwd") ws)
              ((equal v "pathSeparator") "/")
              ((equal v "userHome") "~")
              ((string-prefix-p "env:" v) (or (getenv (substring v 4)) ""))
              ((string-prefix-p "input:" v) (+vscode--input ctx (substring v 6)))
              (t (read-string (format "Value for ${%s}: " v)))))))
       s t t))))

(defun +vscode--subst-tree (ctx x)
  (cond ((stringp x) (+vscode--subst ctx x))
        ((vectorp x) (vconcat (mapcar (lambda (e) (+vscode--subst-tree ctx e)) x)))
        ((and (consp x) (keywordp (car x)))
         (let (out)
           (while x
             (push (car x) out)
             (push (+vscode--subst-tree ctx (cadr x)) out)
             (setq x (cddr x)))
           (nreverse out)))
        (t x)))

;;; Tasks

(defun +vscode--tasks (ctx)
  (let ((j (+vscode--read-json (expand-file-name ".vscode/tasks.json" (plist-get ctx :root)))))
    (plist-put ctx :inputs (plist-get j :inputs))
    (append (plist-get j :tasks) nil)))

(defun +vscode--task (tasks label)
  (or (seq-find (lambda (x) (equal (plist-get x :label) label)) tasks)
      (user-error "No task named %s" label)))

(defun +vscode--task-chain (tasks label &optional seen)
  "Ordered list of tasks to run for LABEL, dependencies first."
  (let* ((task (+vscode--task tasks label))
         (deps (plist-get task :dependsOn))
         (deps (cond ((stringp deps) (list deps)) ((vectorp deps) (append deps nil)))))
    (append (mapcan (lambda (d) (unless (member d seen)
                                  (+vscode--task-chain tasks d (cons label seen))))
                    deps)
            (list task))))

(defun +vscode--task-shell (ctx task)
  (when-let* ((cmd (plist-get task :command)))
    (let* ((args (mapcar (lambda (a)
                           (let ((a (if (stringp a) a (plist-get a :value))))
                             (shell-quote-argument (+vscode--subst ctx a))))
                         (plist-get task :args)))
           (cwd (when-let* ((o (plist-get task :options)) (c (plist-get o :cwd)))
                  (+vscode--subst ctx c)))
           (line (string-join (cons (+vscode--subst ctx cmd) args) " ")))
      (if cwd (format "(cd %s && %s)" (shell-quote-argument cwd) line) line))))

(defun +vscode--wrap (ctx shell-line)
  (if-let* ((exec (+vscode--exec-args ctx t)))
      (concat (mapconcat #'shell-quote-argument exec " ")
              " bash -lc " (shell-quote-argument shell-line))
    shell-line))

(defun +vscode--compilation-filename (ctx)
  (lambda (f) (+vscode--to-local ctx f)))

(defun +vscode--run-chain (ctx label &optional on-success)
  (let* ((tasks (+vscode--tasks ctx))
         (lines (delq nil (mapcar (lambda (tk) (+vscode--task-shell ctx tk))
                                  (+vscode--task-chain tasks label))))
         (cmd (+vscode--wrap ctx (string-join lines " && ")))
         (default-directory (plist-get ctx :root))
         (compilation-buffer-name-function (lambda (_) (format "*task: %s*" label)))
         (buf (compilation-start cmd t)))
    (with-current-buffer buf
      (setq-local compilation-parse-errors-filename-function (+vscode--compilation-filename ctx))
      (setq-local +vscode--on-success on-success))
    buf))

(defvar-local +vscode--on-success nil)

(defun +vscode--compilation-finished (buf status)
  (with-current-buffer buf
    (when-let* ((fn +vscode--on-success))
      (setq +vscode--on-success nil)
      (if (string-prefix-p "finished" status)
          (funcall fn)
        (message "Task failed, not launching: %s" (string-trim status))))))

(add-hook 'compilation-finish-functions #'+vscode--compilation-finished)

(defun +vscode--read-task (ctx &optional prompt)
  (let* ((labels (mapcar (lambda (tk) (plist-get tk :label)) (+vscode--tasks ctx))))
    (completing-read (or prompt "Task: ") labels nil t nil '+vscode--task-history
                     (car +vscode--task-history))))

(defun +vscode-run-task (label)
  "Run a task from .vscode/tasks.json (with its dependsOn chain)."
  (interactive (list (+vscode--read-task (+vscode--ctx))))
  (+vscode--run-chain (+vscode--ctx) label))

(defun +vscode--default-task (group)
  (let* ((ctx (+vscode--ctx))
         (tk (seq-find (lambda (tk)
                         (let ((g (plist-get tk :group)))
                           (and (listp g) (equal (plist-get g :kind) group)
                                (eq (plist-get g :isDefault) t))))
                       (+vscode--tasks ctx))))
    (if tk (+vscode--run-chain ctx (plist-get tk :label))
      (call-interactively #'+vscode-run-task))))

(defun +vscode-build ()
  "Run the default build task."
  (interactive)
  (+vscode--default-task "build"))

(defun +vscode-test ()
  "Run the default test task."
  (interactive)
  (+vscode--default-task "test"))

;;; Launch

(defvar +vscode-cpptools-glob "~/.vscode-server/extensions/ms-vscode.cpptools-*-linux-x64"
  "Where to look for OpenDebugAD7 inside the container.")

(defun +vscode--adapter (ctx type)
  "Return (command . args) for debug adapter TYPE."
  (pcase type
    ((or "cppdbg" "cpptools")
     (let ((sh (format "d=$(ls -d %s 2>/dev/null | sort -V | tail -1); exec \"$d/debugAdapters/bin/OpenDebugAD7\""
                       +vscode-cpptools-glob)))
       (if-let* ((exec (+vscode--adapter-exec-args ctx)))
           (cons (car exec) (append (cdr exec) (list "bash" "-lc" sh)))
         (let ((local (car (last (file-expand-wildcards
                                  (concat (or (bound-and-true-p dape-adapter-dir) "~") "cpptools/extension/debugAdapters/bin/OpenDebugAD7"))))))
           (list (or local "OpenDebugAD7"))))))
    ((or "lldb" "lldb-dap")
     (append (+vscode--adapter-exec-args ctx) (list "lldb-dap")))
    ("gdb"
     (append (+vscode--adapter-exec-args ctx) (list "gdb" "-i" "dap")))
    (_ (user-error "Unsupported launch type: %s" type))))

(defun +vscode--launch-configs (ctx)
  (let ((j (+vscode--read-json (expand-file-name ".vscode/launch.json" (plist-get ctx :root)))))
    (plist-put ctx :launch-inputs (plist-get j :inputs))
    (append (plist-get j :configurations) nil)))

(defun +vscode--start-dape (ctx conf)
  (require 'dape)
  (let* ((type (plist-get conf :type))
         (adapter (+vscode--adapter ctx type))
         (conf (cl-loop for (k v) on conf by #'cddr
                        unless (memq k '(:preLaunchTask :postDebugTask :presentation))
                        append (list k v)))
         (conf (let ((ctx (plist-put (copy-sequence ctx) :inputs (plist-get ctx :launch-inputs))))
                 (+vscode--subst-tree ctx conf)))
         (dc (plist-get ctx :dc))
         (root (plist-get ctx :root))
         (tramp (file-remote-p root))
         (config (append
                  (list 'command (car adapter)
                        'command-args (vconcat (cdr adapter))
                        ;; Over TRAMP the adapter runs from the host (see
                        ;; `+vscode--adapter-exec-args'); map /docker:...: away.
                        'command-cwd (if tramp temporary-file-directory root))
                  (cond (tramp (list 'prefix-local tramp 'prefix-remote ""))
                        (dc (list 'prefix-local root
                                  'prefix-remote (plist-get dc :remote))))
                  (when (equal type "gdb") (list 'defer-launch-attach t))
                  conf)))
    (dape config)))

(defun +vscode-launch (name)
  "Start a launch.json configuration with dape, running its preLaunchTask first."
  (interactive
   (let* ((ctx (+vscode--ctx)))
     (list (completing-read "Launch: " (mapcar (lambda (c) (plist-get c :name))
                                               (+vscode--launch-configs ctx))
                            nil t nil '+vscode--launch-history (car +vscode--launch-history)))))
  (let* ((ctx (+vscode--ctx))
         (conf (seq-find (lambda (c) (equal (plist-get c :name) name))
                         (+vscode--launch-configs ctx)))
         (pre (plist-get conf :preLaunchTask))
         (type (plist-get conf :type)))
    (cond
     ((equal type "node-terminal")
      (let* ((c (+vscode--subst ctx (plist-get conf :command)))
             (run (lambda ()
                    (let ((default-directory (plist-get ctx :root))
                          (compilation-buffer-name-function (lambda (_) (format "*launch: %s*" name))))
                      (compilation-start (+vscode--wrap ctx c) t)))))
        (if pre (+vscode--run-chain ctx pre run) (funcall run))))
     (pre (+vscode--run-chain ctx pre (lambda () (+vscode--start-dape ctx conf))))
     (t (+vscode--start-dape ctx conf)))))

(defun +vscode-launch-skip-task (name)
  "Start a launch.json configuration without running its preLaunchTask."
  (interactive
   (let* ((ctx (+vscode--ctx)))
     (list (completing-read "Launch (no preLaunchTask): "
                            (mapcar (lambda (c) (plist-get c :name)) (+vscode--launch-configs ctx))
                            nil t nil '+vscode--launch-history (car +vscode--launch-history)))))
  (let* ((ctx (+vscode--ctx))
         (conf (seq-find (lambda (c) (equal (plist-get c :name) name))
                         (+vscode--launch-configs ctx))))
    (+vscode--start-dape ctx conf)))

;;; clangd inside the dev container

(defun +vscode-eglot-clangd (&rest _)
  (let* ((root (locate-dominating-file default-directory ".devcontainer"))
         (ctx (and root (ignore-errors (+vscode--ctx root))))
         (dc (plist-get ctx :dc))
         (args '("--background-index" "--clang-tidy" "--header-insertion=never")))
    (if dc
        (append (+vscode--exec-args ctx) '("clangd")
                (list (format "--path-mappings=%s=%s"
                              (directory-file-name (plist-get ctx :root))
                              (directory-file-name (plist-get dc :remote))))
                args)
      (cons "clangd" args))))

(after! eglot
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode) . +vscode-eglot-clangd)))

(provide '+vscode)
;;; +vscode.el ends here
