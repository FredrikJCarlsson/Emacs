;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
;; Specify both a dark and light theme, like so and Doom will choose which one
;; to load based on your system light/dark setting:
;;
;;   (setq doom-theme '(doom-one   . doom-one-light))   ; (DARK . LIGHT)
;;
;; If you want more pro-active theme switching based on OS light/dark mode, look
;; up the `auto-dark' package.

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(setq doom-font (font-spec :family "Hack Nerd Font" :size 15 :weight 'medium))
(setq doom-big-font (font-spec :family "Hack Nerd Font" :size 17 :weight 'medium))
(setq-default tab-width 4
              line-spacing 0.12)
;; changes certain keywords to symbols, such as lambda
(global-prettify-symbols-mode 1)

;; f.el for file operations (used below and in orgmode.el)
(use-package! f
  :demand t)

;; Projectile - Project management
(let ((dev-root (cond ((eq system-type 'darwin) "/Users/fredrikcarlsson/Development")
                      ((eq system-type 'windows-nt) "C:/GIT"))))
  (when (and dev-root (file-directory-p dev-root))
    (setq projectile-project-search-path (f-directories dev-root))))

;; `org-directory' is set per OS in orgmode.el.


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `with-eval-after-load' block, otherwise Doom's defaults may override your
;; settings. E.g.
;;
;;   (with-eval-after-load 'PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look them up).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;;; WSL: always open URLs in the Windows default browser, never a Linux/GTK
;;; one. Absolute path because the Doom env PATH has no /mnt/c entries.
(defun +wsl-browse-url-windows (url &rest _)
  "Open URL in the Windows default browser."
  (let ((default-directory "/mnt/c/"))  ; avoid UNC cwd warnings
    (start-process "windows-browser" nil "/mnt/c/Windows/explorer.exe" url)))

(when (getenv "WSL_DISTRO_NAME")
  (setq browse-url-browser-function #'+wsl-browse-url-windows
        browse-url-secondary-browser-function #'+wsl-browse-url-windows
        browse-url-handlers nil))


;;; VS Code tasks/launch + dev container debugging
(load! "+vscode")

(map! :leader
      (:prefix ("v" . "vscode")
       :desc "Run task"            "t" #'+vscode-run-task
       :desc "Build (default)"     "b" #'+vscode-build
       :desc "Test (default)"      "T" #'+vscode-test
       :desc "Launch / debug"      "d" #'+vscode-launch
       :desc "Launch, skip task"   "D" #'+vscode-launch-skip-task
       :desc "Toggle breakpoint"   "p" #'dape-breakpoint-toggle
       :desc "Debug REPL"          "r" #'dape-repl
       :desc "Quit debugger"       "q" #'dape-quit))

(defun +vscode-f5 ()
  (interactive)
  (if (and (featurep 'dape) (dape--live-connection 'last t))
      (call-interactively #'dape-continue)
    (call-interactively #'+vscode-launch)))

(map! "<f5>"    #'+vscode-f5
      "S-<f5>"  #'dape-quit
      "<f9>"    #'dape-breakpoint-toggle
      "<f10>"   #'dape-next
      "<f11>"   #'dape-step-in
      "S-<f11>" #'dape-step-out)

;;; Claude Code (agent-shell over ACP)
(use-package! agent-shell
  :commands (agent-shell agent-shell-anthropic-start-claude-code)
  :init
  (map! :leader :desc "Claude Code" "o c" #'agent-shell-anthropic-start-claude-code)
  :config
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t)
        agent-shell-path-resolver-function #'+agent-shell-tramp-resolve-path
        agent-shell-transcript-file-path-function #'+agent-shell-transcript-path))

;; Transcripts are appended to on every agent update. TRAMP can't append, so
;; for /docker: projects each append re-copied the whole file and froze the UI.
;; Keep remote projects' transcripts on the local disk instead.
(defun +agent-shell-transcript-path ()
  (let ((cwd (agent-shell-cwd)))
    (if-let* ((host (file-remote-p cwd 'host)))
        (let ((dir (expand-file-name
                    (file-name-concat host (file-name-nondirectory
                                            (directory-file-name (file-local-name cwd))))
                    (concat doom-cache-dir "agent-shell-transcripts/"))))
          (make-directory dir t)
          (expand-file-name (format-time-string "%F-%H-%M-%S.md") dir))
      (agent-shell--default-transcript-file-path))))

(defun +agent-shell-tramp-resolve-path (path)
  "Map /docker:host:/x <-> /x when the agent-shell project is on TRAMP."
  (if-let* ((remote (file-remote-p (agent-shell-cwd))))
      (if (file-remote-p path) (file-local-name path) (concat remote path))
    path))

;; claude-agent-acp is installed in the container's ~/.local/bin (persistent
;; home volume); let TRAMP's executable lookup find it there. TRAMP doesn't
;; expand "~" here, so the path is spelled out.
(after! tramp
  (add-to-list 'tramp-remote-path "/home/developer/.local/bin" t))

;; TRAMP's docker method gives processes a TTY, which corrupts ACP's JSON
;; stream. For /docker: projects, start the agent from the host through a
;; plain `docker exec -i' pipe instead.
(defadvice! +acp-docker-pipe-a (fn &rest args)
  :around #'acp--start-client
  (let ((client (plist-get args :client))
        (dir default-directory))
    (if (and client
             (equal (file-remote-p dir 'method) "docker")
             (not (equal (map-elt client :command) "docker")))
        (let ((default-directory temporary-file-directory))
          (map-put! client :command-params
                    (append (list "exec" "-i")
                            (when-let* ((u (file-remote-p dir 'user))) (list "-u" u))
                            (mapcan (lambda (e) (list "-e" e))
                                    (map-elt client :environment-variables))
                            (list "-w" (directory-file-name (file-local-name dir))
                                  (file-remote-p dir 'host)
                                  "bash" "-lc" "PATH=\"$HOME/.local/bin:$PATH\" exec \"$@\"" "sh"
                                  (map-elt client :command))
                            (map-elt client :command-params)))
          (map-put! client :command "docker")
          (apply fn args))
      (apply fn args))))

;; Project roots on TRAMP come back abbreviated as "/docker:host:~/...".
;; `docker exec -w' rejects a non-absolute cwd, so eglot/clangd (and vc)
;; die on startup. Expand the root so the remote cwd is absolute.
(defadvice! +tramp-expand-project-root-a (root)
  :filter-return #'project-root
  (if (and root (file-remote-p root)) (expand-file-name root) root))

;;; gptel via GitHub Copilot (business plan). First use prompts for a
;;; device-code login (or run M-x gptel-gh-login).
(after! gptel
  (setq gptel-model 'claude-sonnet-5.5
        gptel-backend (gptel-make-gh-copilot "Copilot"
                        :host "api.business.githubcopilot.com"
                        ;; gptel's built-in list lags behind Copilot's; add
                        ;; newer models here.
                        :models (append
                                 '((claude-sonnet-5.5
                                    :description "Latest Sonnet"
                                    :capabilities (media tool-use cache)
                                    :mime-types ("image/jpeg" "image/png" "image/gif"
                                                 "image/webp" "application/pdf")
                                    :context-window 1000))
                                 gptel--gh-models))))

;;; gptel-quick ("Explain" in the llm leader menu): explain in a popup at point instead of the
;;; echo area. Press + in the popup for a longer answer, M-w to copy.
(after! gptel-quick
  (setq gptel-quick-display 'posframe
        gptel-quick-word-count 60
        gptel-quick-timeout 30))

;;; Explai (lisp/explai.el): explain / ask about code, trace callers and find
;;; implementations in a popup next to the code, through the gptel backend above.
;;; In the popup: ESC close, M-w copy, M-m more, M-o open as buffer, M-j jump.
(add-load-path! "lisp")
(use-package! explai
  :commands (explai-explain explai-explain-detailed explai-ask explai-callers
             explai-find explai-last explai-model explai-close)
  :init
  (map! :leader
        (:prefix ("o" . "open")
         (:prefix ("l" . "llm")
          :desc "Explain"                "e" #'explai-explain
          :desc "Explain in detail"      "E" #'explai-explain-detailed
          :desc "Ask about code"         "q" #'explai-ask
          :desc "Trace callers"          "k" #'explai-callers
          :desc "Find implementation"    "i" #'explai-find
          :desc "Show last Explai popup" "p" #'explai-last))))

;;; Tree-sitter: this Emacs's libtree-sitter only loads grammar ABI 13-14,
;;; but the default sources pin commits that build ABI 15 (rejected, so Emacs
;;; re-prompts to install on every file). Pin the last ABI-14 releases instead.
(defun +treesit-pin-abi14-sources-h ()
  (when (< (treesit-library-abi-version) 15)
    (dolist (src '((c       "https://github.com/tree-sitter/tree-sitter-c"       "v0.23.6")
                   (cpp     "https://github.com/tree-sitter/tree-sitter-cpp"     "v0.23.4")
                   ;; Emacs's own pin (ABI 14). Doom's v0.20.0 is too old for
                   ;; csharp-ts-mode's font-lock rules.
                   (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp"
                            :commit "362a8a41b265056592a0c3771664a21d23a71392")))
      (setf (alist-get (car src) treesit-language-source-alist) (cdr src)))))
(after! treesit (+treesit-pin-abi14-sources-h))
(after! c-ts-mode (+treesit-pin-abi14-sources-h))

;;; Copilot inline completion (copilot.el). Run M-x copilot-install-server
;;; once, then M-x copilot-login.
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :config
  ;; The server always runs on the WSL host (also for /docker: TRAMP buffers),
  ;; but copilot.el searches the *remote* PATH when visiting a TRAMP file.
  ;; Resolve the local binary once so it's always found.
  (setq copilot-indent-offset-warning-disable t
        copilot-server-executable
        (let ((default-directory "~/"))
          (or (executable-find "copilot-language-server")
              copilot-server-executable)))
  (map! :map copilot-completion-map
        "<tab>"   #'copilot-accept-completion
        "TAB"     #'copilot-accept-completion
        "C-<tab>" #'copilot-accept-completion-by-word
        "C-TAB"   #'copilot-accept-completion-by-word
        "M-n"     #'copilot-next-completion
        "M-p"     #'copilot-previous-completion))

;;; mu4e: work mail (Microsoft 365) via mbsync (~/.mbsyncrc) + msmtp
;;; (~/.msmtprc), both authenticating with OAuth2 through
;;; ~/.local/bin/outlook-oauth2. mu/mu4e 1.12 is built from source because
;;; Ubuntu's 1.6 doesn't load in Emacs 32.
(when (file-directory-p "/usr/local/share/emacs/site-lisp/mu4e")
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e"))

(setq sendmail-program (executable-find "msmtp")
      send-mail-function #'sendmail-send-it
      message-send-mail-function #'sendmail-send-it
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from"))

(set-email-account! "unipower"
  '((user-full-name         . "Fredrik J. Carlsson")
    (user-mail-address      . "fredrik.j.carlsson@unipower.se")
    (smtpmail-smtp-user     . "fredrik.j.carlsson@unipower.se")
    (mu4e-sent-folder       . "/unipower/Sent Items")
    (mu4e-drafts-folder     . "/unipower/Drafts")
    (mu4e-trash-folder      . "/unipower/Deleted Items")
    (mu4e-refile-folder     . "/unipower/Archive")
    ;; Exchange already files sent mail in Sent Items; don't save a 2nd copy.
    (mu4e-sent-messages-behavior . delete))
  t)

;;; C#: run csharp-ls on the closest .sln, not the whole repo. PQSecure.NET's
;;; root pqsecure.All.sln has ~90 projects; sub-solutions load far faster.
(defun +csharp-nearest-sln (dir)
  "Return the first .sln/.slnx found walking up from DIR, or nil."
  (when-let* ((root (locate-dominating-file
                     dir (lambda (d) (directory-files d nil "\\.slnx?\\'" t)))))
    (car (directory-files root t "\\.slnx?\\'"))))

(defun +csharp-project-find (dir)
  "Treat the directory of the nearest solution as the project (for eglot)."
  (when-let* ((sln (+csharp-nearest-sln dir)))
    (cons 'transient (file-name-directory sln))))

;; Buffer-local, so only C# buffers see the solution dir as their project;
;; projectile (SPC p) keeps using the git root.
(add-hook! '(csharp-mode-hook csharp-ts-mode-hook)
  (add-hook 'project-find-functions #'+csharp-project-find nil t))

(after! eglot
  (add-to-list 'eglot-server-programs
               `((csharp-mode csharp-ts-mode)
                 . ,(lambda (&rest _)
                      (if-let* ((sln (+csharp-nearest-sln default-directory)))
                          (list "csharp-ls" "--solution" (file-local-name sln))
                        '("csharp-ls"))))))

;;; Org mode and personal commands
(load! "orgmode")
(load! "myCommands")
