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

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

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
(set-face-attribute 'default nil
  :font "Hack Nerd Font"
  :height 120
  :weight 'medium)

(use-package! gptel)
;; OPTIONAL configuration
(setq gptel-model 'gpt-4o
      gptel-backend (gptel-make-gh-copilot "Copilot"))


;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

;; ;; Needed if using emacsclient. Otherwise, your fonts will be smaller than expected.
;; (add-to-list 'default-frame-alist '(font . "Hack Nerd Font"))
;; changes certain keywords to symbols, such as lamda!
(setq global-prettify-symbols-mode t)

;;; LANGUAGES

;; DAP mode - Go debugging
(after! dap-mode
  (require 'dap-dlv-go))

;; C/C++ - Clangd setup
(after! lsp-clangd
  (setq lsp-clients-clangd-args
        '("-j=3"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=never"
          "--header-insertion-decorators=0"))
  (set-lsp-priority! 'clangd 2))

(use-package! f
  :demand t)

(cond
 ((eq system-type 'darwin)
  (setq projectile-project-search-path
        (f-entries "/Users/fredrikcarlsson/Development")))
 ((eq system-type 'windows-nt)
  (setq projectile-project-search-path
        (f-entries "C:/GIT"))))

;; C# - LSP Roslyn setup"
(defun my/lsp-roslyn--find-sln-in-workspace ()
  "Find all .sln files under the LSP workspace root."
  (let ((root (lsp-workspace-root)))
    (when root
      (directory-files-recursively root "\\.sln\\'"))))

(defun my/lsp-roslyn--pick-solution-file-interactively (solution-files)
  "Prompt user to select one of the SOLUTION-FILES."
  (completing-read "Select .sln file: " solution-files nil t))

(defun my/lsp-roslyn-open-solution-file ()
  "Send a Roslyn 'solution/open' notification based on discovered .sln files."
  (interactive)
  (let ((solutions (my/lsp-roslyn--find-sln-in-workspace)))
    (cond
     ((null solutions)
      (lsp--error "No .sln file found in LSP workspace: %s" (lsp-workspace-root)))
     ((= (length solutions) 1)
      (let ((file (car solutions)))
        (lsp-notify "solution/open" (list :solution (lsp--path-to-uri file)))
        (lsp--info "Roslyn solution opened: %s" file)))
     (t
      (let* ((chosen (my/lsp-roslyn--pick-solution-file-interactively solutions)))
        (lsp-notify "solution/open" (list :solution (lsp--path-to-uri chosen)))
        (lsp--info "Roslyn solution opened: %s" chosen))))))


(defun custom-lsp-roslyn--on-initialized (workspace)
  (lsp-roslyn-open-solution-file)
  (with-lsp-workspace workspace
    (lsp--set-configuration
     #s(hash-table
         size 30
         test equal
         data (
               "csharp|background_analysis.dotnet_analyzer_diagnostics_scope" "fullSolution"
               "csharp|background_analysis.dotnet_compiler_diagnostics_scope" "fullSolution"
               )))))

(advice-add 'lsp-roslyn--on-initialized :override #'custom-lsp-roslyn--on-initialized)



(setq-default tab-width 4)

;; Copilot - GitHub Copilot integration
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))


;; Keybinding for tangling
(map! :leader
      :desc "Org babel tangle" "m B" #'org-babel-tangle)

;; Org mode configuration
(after! org
  ;; Basic Org settings
  (setq org-directory "~/org"
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-log-done 'time
        org-hide-emphasis-markers t
        org-table-convert-region-max-lines 20000

        ;; Bullets and item styles
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦))

        ;; TODO states
        org-todo-keywords
        '((sequence
           "TODO(t)" "WAIT(w)" "|"
           "DONE(d)" "CANCELLED(c)"))

        ;; Link abbreviations
        org-link-abbrev-alist
        '(("google" . "http://www.google.com/search?q=")
          ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
          ("ddg" . "https://duckduckgo.com/?q=")
          ("wiki" . "https://en.wikipedia.org/wiki/"))

        ;; Priority appearance
        org-fancy-priorities-list '("🟥" "🟧" "🟨")
        org-priority-faces
        '((?A :foreground "#ff6c6b" :weight bold)
          (?B :foreground "#98be65" :weight bold)
          (?C :foreground "#c678dd" :weight bold))

        ;; Agenda appearance
        org-agenda-block-separator 8411)

  ;; Load org babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)))

  ;; Agenda sources
  (when (require 'f nil t)  ; only if `f` is available
    (setq org-agenda-files
          (f-entries org-directory (lambda (f) (f-ext? f "org")))))

  ;; Custom file handlers
  (add-to-list 'org-file-apps '("docx" . default))
  (add-to-list 'org-file-apps '("xlsx" . default))

  ;; Custom agenda view
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
            (alltodo ""))))))

;; Org-roam UI
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))



;;; TOOLS - RIPGREP WITH HELM
(use-package! helm
  :config
  (require 'helm-source)
  (require 'ansi-color))

(defun my-helm-rga-search ()
  "Run ripgrep-all (rga) in the current directory."
  (interactive)
  (let ((default-directory (read-directory-name "Search directory: ")))
    (helm :sources (helm-build-async-source "ripgrep-all"
                     :candidates-process
                     (lambda ()
                       (let ((cmd (format "rga --color=always --line-number --no-heading %s"
                                          (shell-quote-argument helm-pattern))))
                         (start-process-shell-command "helm-rga" nil cmd)))
                     :action '(("Open file" . (lambda (candidate)
                                                (let ((parts (split-string candidate ":")))
                                                  (find-file (car parts))
                                                  (goto-line (string-to-number (cadr parts))))))
                               ("Copy line" . kill-new)))
          :buffer "*helm rga*")))
