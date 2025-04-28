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
(setq doom-theme 'doom-dracula)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
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
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(after! (evil copilot)
 ;; Define the custom function that either accepts the completion or does the default behavior
 (defun my/copilot-tab-or-default ()
   (interactive)
   (if (and (bound-and-true-p copilot-mode)
            ;; Add any other conditions to check for active copilot suggestions if necessary
            )
       (copilot-accept-completion)
     (evil-insert 1))) ; Default action to insert a tab. Adjust as needed.

 ;; Bind the custom function to <tab> in Evil's insert state
 (evil-define-key 'insert 'global (kbd "<tab>") 'my/copilot-tab-or-default))

(use-package copilot-chat
  :after (request org markdown-mode shell-maker))


(menu-bar--display-line-numbers-mode-relative)


(set-face-attribute 'default nil
  :font "Hack Nerd Font"
  :height 120
  :weight 'medium)
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

;; Needed if using emacsclient. Otherwise, your fonts will be smaller than expected.
(add-to-list 'default-frame-alist '(font . "Hack Nerd Font"))
;; changes certain keywords to symbols, such as lamda!
(setq global-prettify-symbols-mode t)

;; Languages
 
(after! dap-mode
  (require 'dap-dlv-go))

;;#########################

(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"
                                "--header-insertion-decorators=0"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

;; Only on macOS
(when (eq system-type 'darwin)
  (require 'f)
  (setq projectile-project-search-path (f-entries "/Users/fredrikcarlsson/Development")))

(after! csharp-mode (setq lsp-csharp-server-path "~/.emacs.d/.local/etc/lsp/omnisharp-roslyn/latest/omnisharp-roslyn"))

(add-to-list 'auto-mode-alist '("\\.vb\\'" . csharp-mode))

(map! :leader
      :desc "Org babel tangle" "m B" #'org-babel-tangle)
(after! org
  (setq org-directory "~/org"
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
        org-log-done 'time
        org-hide-emphasis-markers t
        ;; ex. of org-link-abbrev-alist in action
        ;; [[arch-wiki:Name_of_Page][Description]]
        org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
          '(("google" . "http://www.google.com/search?q=")
            ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
            ("ddg" . "https://duckduckgo.com/?q=")
            ("wiki" . "https://en.wikipedia.org/wiki/"))
        org-table-convert-region-max-lines 20000
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
          '((sequence
             "TODO(t)"           ; A task that is ready to be tackled
             "WAIT(w)"           ; Something is holding up this task
             "|"                 ; The pipe necessary to separate "active" states and "inactive" states
             "DONE(d)"           ; Task has been completed
             "CANCELLED(c)" )))) ; Task has been cancelled


(after! org
  (setq org-agenda-files (f-entries "~/org" (lambda (f) (f-ext? f "org"))))
  (add-to-list 'org-file-apps '("docx" . default))
  (add-to-list 'org-file-apps '("xlsx" . default))
)

(setq
   ;; org-fancy-priorities-list '("[A]" "[B]" "[C]")
   ;; org-fancy-priorities-list '("❗" "[B]" "[C]")
   org-fancy-priorities-list '("🟥" "🟧" "🟨")
   org-priority-faces
   '((?A :foreground "#ff6c6b" :weight bold)
     (?B :foreground "#98be65" :weight bold)
     (?C :foreground "#c678dd" :weight bold))
   org-agenda-block-separator 8411)

(setq org-agenda-custom-commands
      '(("v" "A better agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (tags "PRIORITY=\"B\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Medium-priority unfinished tasks:")))
          (tags "PRIORITY=\"C\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Low-priority unfinished tasks:")))
          (tags "customtag"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Tasks marked with customtag:")))

          (agenda "")
          (alltodo "")))))

(after! org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t))))


(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))





(unless (package-installed-p 'helm)
 (package-refresh-contents)
 (package-install 'helm))

(require 'helm)
(require 'helm-source)
(require 'ansi-color)

(defun my-helm-rga-search (&optional arg)
 "Search inside various file types (including .docx) using ripgrep-all (rga) and display results in Helm.
If prefix ARG is set, prompt for a directory to search from."
 (interactive "P")
 (let* ((default-directory
          (if arg
              (read-directory-name "Search directory: ")
            default-directory))
        (search-term (read-string "Search with rga: "))
        (rga-command (format "rga --smart-case --hidden --with-filename --line-number --column --color=always --colors=match:fg:red --colors=match:style:bold %s"
                             (shell-quote-argument search-term)))
        (results (split-string (shell-command-to-string rga-command) "\n" t)))

   (helm :sources (helm-build-sync-source "rga Search Results"
                    :candidates (mapcar #'ansi-color-apply results)  ;; Apply ANSI color formatting
                    :candidate-number-limit 1000
                    :action (lambda (candidate)
                              (let* ((parts (split-string candidate ":" t))
                                     (file (nth 0 parts))
                                     (line (nth 1 parts)))
                                (when file
                                  (find-file file)
                                  (when line
                                    (goto-line (string-to-number line)))))))
         :buffer "*helm-rga-search*"
         :truncate-lines t)))





(defun my/lsp-roslyn-choose-solution-file ()
  "Finds all .sln files recursively from project root and lets you pick one for Roslyn."
  (interactive)
  (let* ((root (or (lsp-workspace-root) (projectile-project-root) (vc-root-dir)))
         (solution-files (when root (directory-files-recursively root "\\.sln$"))))
    (if (not solution-files)
        (lsp--error "No solution file found in project.")
      (let ((chosen (if (= (length solution-files) 1)
                        (car solution-files)
                      (completing-read "Select solution: " solution-files nil t))))
        (lsp-notify "solution/open" (list :solution (lsp--path-to-uri chosen)))
        (message "Opened solution: %s" chosen)))))
