;;; orgmode.el -*- lexical-binding: t; -*-


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
