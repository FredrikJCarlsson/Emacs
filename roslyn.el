;;; roslyn.el -*- lexical-binding: t; -*-

;; C# - LSP Roslyn setup"
(require 'project)

(defvar my/lsp-roslyn--solution-opened nil)

(defun my/lsp-roslyn--find-sln-in-workspace ()
  "Find all .sln files under the LSP workspace root."
  (let ((root (lsp-workspace-root)))
    (when root
      (directory-files-recursively root "\\.sln\\'"))))

(defun my/lsp-roslyn--pick-solution-file-interactively (solution-files)
  "Prompt user to select one of the SOLUTION-FILES."
  (completing-read "Select .sln file: " solution-files nil t))

(defun my/lsp-roslyn-open-solution-file ()
  "Send a Roslyn 'solution/open' notification only once per session."
  (interactive)
  (unless my/lsp-roslyn--solution-opened
    (let ((solutions (my/lsp-roslyn--find-sln-in-workspace)))
      (cond
       ((null solutions)
        (lsp--error "No .sln file found in LSP workspace: %s" (lsp-workspace-root)))
       ((= (length solutions) 1)
        (let ((file (car solutions)))
          (lsp-notify "solution/open" (list :solution (lsp--path-to-uri file)))
          (setq my/lsp-roslyn--solution-opened t)
          (lsp--info "Roslyn solution opened: %s" file)))
       (t
        (let* ((chosen (my/lsp-roslyn--pick-solution-file-interactively solutions)))
          (lsp-notify "solution/open" (list :solution (lsp--path-to-uri chosen)))
          (setq my/lsp-roslyn--solution-opened t)
          (lsp--info "Roslyn solution opened: %s" chosen)))))))


(defun custom-lsp-roslyn--on-initialized (workspace)
  (my/lsp-roslyn-open-solution-file)
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

;; Or for project.el in Emacs 29+
(defun my/project-find-sln-root (dir)
  "Treat the nearest ancestor of DIR containing a .sln file as a project root."
  (when-let ((root (locate-dominating-file
                    dir
                    (lambda (d) (directory-files d nil "\\.sln\\'" t)))))
    (cons 'vc root)))

(add-to-list 'project-find-functions #'my/project-find-sln-root)
