;;; myCommands.el -*- lexical-binding: t; -*-

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
