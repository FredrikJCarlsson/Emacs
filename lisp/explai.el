;;; explai.el --- Explain code, ask about it, trace callers, find implementations -*- lexical-binding: t; -*-

;; Explai for Emacs: the same tool as the Explai VS Code extension and explai.nvim.
;; Select code (or put point on a line) and get an explanation in a popup next to
;; the code; ask your own question; trace how a function is reached; or describe
;; something and jump to where it is implemented.
;;
;; Built on gptel (requests, streaming, tool calls; uses your configured backend,
;; e.g. GitHub Copilot), posframe (the popup), eglot (symbols, references, call
;; hierarchy) and ripgrep (text search).
;;
;; While the popup is shown:
;;   ESC  close           M-w  copy           M-m  more detail
;;   M-o  open as buffer   M-j  jump to a step (callers / find)
;;   M-e  explain the match (find)
;; Brought back with `explai-last'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel)
(require 'posframe)
(require 'markdown-mode)
(require 'xref)

(declare-function eglot-current-server "eglot")
(declare-function eglot-managed-p "eglot")
(declare-function eglot-server-capable "eglot")
(declare-function eglot-uri-to-path "eglot")
(declare-function eglot--request "eglot")
(declare-function eglot--TextDocumentPositionParams "eglot")
(declare-function eglot--TextDocumentIdentifier "eglot")
(declare-function eglot--lsp-position-to-point "eglot")
(declare-function eglot--maybe-activate-editing-mode "eglot")
(declare-function evil-visual-state-p "evil-states")
(declare-function evil-insert-state-p "evil-states")
(declare-function evil-visual-range "evil-states")
(declare-function evil-exit-visual-state "evil-states")
(declare-function doom-project-root "doom-lib")
(declare-function project-root "project")
(declare-function which-function "which-func")
(defvar evil-local-mode)

;;;; Options

(defgroup explai nil
  "Explain code and navigate it with an LLM, in a popup next to the code."
  :group 'tools
  :prefix "explai-")

(defcustom explai-model nil
  "gptel model symbol for Explai, or nil to use `gptel-model'."
  :type '(choice (const :tag "Same as gptel" nil) symbol))

(defcustom explai-backend nil
  "gptel backend for Explai, or nil to use `gptel-backend'."
  :type '(choice (const :tag "Same as gptel" nil) sexp))

(defcustom explai-word-count 60
  "Approximate length of the short explanation, in words."
  :type 'integer)

(defcustom explai-context-lines 20
  "Lines of surrounding code sent along as context, above and below."
  :type 'integer)

(defcustom explai-max-width 90
  "Maximum popup width in columns."
  :type 'integer)

(defcustom explai-max-height 25
  "Maximum popup height in lines."
  :type 'integer)

(defcustom explai-rg-program "rg"
  "ripgrep executable used by the search tools."
  :type 'string)

(defface explai-title '((t :inherit font-lock-keyword-face :weight bold))
  "Popup title.")
(defface explai-muted '((t :inherit shadow))
  "Muted popup text: location, key hints, model.")
(defface explai-range '((t :inherit region :extend t))
  "Highlight of the code being explained.")
(defface explai-border '((t :inherit vertical-border))
  "Popup border (foreground is used).")
(defface explai-popup '((t :inherit tooltip))
  "Popup background (background is used).")

;;;; Views and the popup

(cl-defstruct (explai-view (:constructor explai-view-create) (:copier nil))
  id kind where buffer window marker (prefer 'below) hl-beg hl-end
  (text "") loading hints meta plain document links
  more explain on-cancel)

(defconst explai--spinner ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])
(defconst explai--popup-name " *explai-popup*")

(defvar explai--view nil "The view shown in the popup.")
(defvar explai--last nil "The last finished view, for `explai-last'.")
(defvar explai--popup-active nil "Non-nil while the popup is shown (enables `explai-popup-map').")
(defvar explai--header nil "Overlay with the popup title.")
(defvar explai--hl nil "Overlay highlighting the explained code.")
(defvar explai--spinner-timer nil)
(defvar explai--spinner-frame 0)
(defvar explai--render-timer nil)
(defvar explai--placed nil "Last placement, to skip needless re-positioning.")

(defun explai--title (view)
  (concat
   (propertize (format " %s Explai"
                       (if (explai-view-loading view)
                           (aref explai--spinner explai--spinner-frame)
                         "✦"))
               'face 'explai-title)
   (when-let* ((k (explai-view-kind view)))
     (propertize (concat " · " k) 'face 'explai-title))
   (when-let* ((w (explai-view-where view)))
     (propertize (concat " · " w) 'face 'explai-muted))))

(defun explai--footer (view)
  (let ((parts (delq nil (list (explai-view-hints view) (explai-view-meta view)))))
    (when parts
      (propertize (concat " " (string-join parts "   ")) 'face 'explai-muted))))

(defun explai--popup-buffer ()
  (let ((buf (get-buffer explai--popup-name)))
    (unless buf
      (setq buf (get-buffer-create explai--popup-name))
      (with-current-buffer buf
        ;; Rendered markdown with the markup hidden; skip markdown hooks (spell
        ;; checking, linters...) that make no sense in a popup.
        (delay-mode-hooks (gfm-view-mode))
        (setq-local markdown-hide-markup t
                    markdown-fontify-code-blocks-natively t
                    word-wrap t
                    truncate-lines nil
                    cursor-type nil
                    mode-line-format nil
                    header-line-format nil)
        (visual-line-mode 1)))
    buf))

(defun explai--anchor-window (view)
  "The window showing VIEW's code: the selected one if it does, else the last one
used, else any window on the main (non-child) frame."
  (let ((buf (explai-view-buffer view))
        (last (explai-view-window view)))
    (when (buffer-live-p buf)
      (let ((win (cond
                  ((and (not (frame-parent)) (eq (window-buffer (selected-window)) buf))
                   (selected-window))
                  ((and (window-live-p last) (eq (window-buffer last) buf))
                   last)
                  (t (get-buffer-window buf (or (frame-parent) (selected-frame)))))))
        (setf (explai-view-window view) win)
        win))))

(defun explai--width (view win)
  (let ((w (max 40
                (string-width (explai--title view))
                (string-width (or (explai--footer view) "")))))
    (dolist (l (split-string (explai-view-text view) "\n"))
      (setq w (max w (string-width l))))
    (min (+ w 2) explai-max-width (max 30 (- (window-body-width win) 6)))))

(defun explai--fill (view width)
  "Put VIEW's text, title and footer in the popup buffer."
  (with-current-buffer (explai--popup-buffer)
    (let ((inhibit-read-only t)
          (rule (propertize (make-string (max 1 (- width 1)) ?─) 'face 'explai-muted))
          (footer (explai--footer view)))
      (erase-buffer)
      (remove-overlays)
      (insert (string-trim-right (explai-view-text view)))
      (font-lock-ensure)
      (setq explai--header (make-overlay (point-min) (point-min) nil t))
      (overlay-put explai--header 'explai-rule rule)
      (overlay-put explai--header 'before-string (concat (explai--title view) "\n" rule "\n"))
      (when footer
        (let ((ov (make-overlay (point-max) (point-max) nil nil t)))
          (overlay-put ov 'after-string (concat "\n" rule "\n" footer))))
      (goto-char (point-min)))))

(defun explai--place (view)
  "Show or move the popup for VIEW next to its anchor line."
  (let ((win (explai--anchor-window view))
        (pos (marker-position (explai-view-marker view))))
    (if (or (not win) (not pos) (not (pos-visible-in-window-p pos win)))
        (progn (posframe-hide explai--popup-name) (setq explai--placed nil))
      (let* ((width (explai--width view win))
             (row (cdr (posn-col-row (posn-at-point pos win))))
             (height (window-body-height win))
             (below (- height row 2))
             (above row)
             ;; Estimated popup height: wrapped text lines + title/footer.
             (est (+ 4 (cl-loop for l in (split-string (explai-view-text view) "\n")
                                sum (max 1 (ceiling (string-width l) (float width))))))
             (want (min (+ est 2) (+ explai-max-height 2)))
             (up (if (eq (explai-view-prefer view) 'above)
                     (>= above want)
                   (and (< below want) (> above below))))
             (max-h (max 4 (min explai-max-height (- (if up above below) 2))))
             (key (list (explai-view-id view) (window-start win) pos width up
                        (length (explai-view-text view)) (explai-view-loading view)
                        (window-pixel-edges win) (frame-width))))
        (unless (equal key explai--placed)
          (setq explai--placed key)
          (explai--fill view width)
          (with-selected-window win
            (posframe-show explai--popup-name
                           :position pos
                           :poshandler (if up
                                           #'posframe-poshandler-point-bottom-left-corner-upward
                                         #'posframe-poshandler-point-bottom-left-corner)
                           :width width
                           :min-width width
                           :min-height 1
                           :max-height max-h
                           :left-fringe 8
                           :right-fringe 8
                           :border-width 1
                           :border-color (face-foreground 'explai-border nil t)
                           :background-color (face-background 'explai-popup nil t)
                           :foreground-color (face-foreground 'default nil t)
                           :lines-truncate nil
                           :accept-focus nil
                           :override-parameters '((cursor-type . nil)))))))))

(defun explai--render ()
  (setq explai--render-timer nil)
  (when explai--view
    (explai--place explai--view)))

(defun explai--render-soon ()
  "Re-render shortly (coalesces streaming updates)."
  (unless explai--render-timer
    (setq explai--render-timer (run-with-timer 0.05 nil #'explai--render))))

(defun explai--spin ()
  (if (not (and explai--view (explai-view-loading explai--view)))
      (explai--stop-spinner)
    (setq explai--spinner-frame (mod (1+ explai--spinner-frame) (length explai--spinner)))
    (when (overlay-buffer explai--header)
      (overlay-put explai--header 'before-string
                   (concat (explai--title explai--view) "\n"
                           (overlay-get explai--header 'explai-rule) "\n")))))

(defun explai--stop-spinner ()
  (when explai--spinner-timer
    (cancel-timer explai--spinner-timer)
    (setq explai--spinner-timer nil)))

(defun explai--highlight (view)
  (when explai--hl
    (delete-overlay explai--hl)
    (setq explai--hl nil))
  (when-let* ((beg (explai-view-hl-beg view))
              (end (explai-view-hl-end view))
              ((buffer-live-p (marker-buffer beg))))
    (setq explai--hl (make-overlay beg end (marker-buffer beg)))
    (overlay-put explai--hl 'face 'explai-range)
    (overlay-put explai--hl 'priority 10)))

(defun explai--show (view)
  "Show VIEW in the popup, or update the open popup in place when the id matches."
  (let ((reuse (and explai--view (equal (explai-view-id explai--view) (explai-view-id view)))))
    (unless reuse
      (explai-close))
    (setq explai--view view
          explai--popup-active t)
    (unless reuse
      (explai--highlight view)
      (explai--watch))
    (if (explai-view-loading view)
        (unless explai--spinner-timer
          (setq explai--spinner-timer (run-with-timer 0.1 0.1 #'explai--spin)))
      (explai--stop-spinner)
      (setq explai--last view))
    (setq explai--placed nil)
    (explai--render)))

(defun explai-close ()
  "Close the Explai popup (cancels a request that is still running)."
  (interactive)
  (let ((view explai--view))
    (setq explai--view nil
          explai--popup-active nil
          explai--placed nil)
    (explai--stop-spinner)
    (when explai--render-timer
      (cancel-timer explai--render-timer)
      (setq explai--render-timer nil))
    (when (get-buffer explai--popup-name)
      (posframe-hide explai--popup-name))
    (when explai--hl
      (delete-overlay explai--hl)
      (setq explai--hl nil))
    (explai--unwatch)
    (when (and view (explai-view-loading view) (explai-view-on-cancel view))
      (funcall (explai-view-on-cancel view)))))

;;;;; Following the code, closing

(defun explai--follow ()
  "After each command: keep the popup next to its line, or close it."
  (when-let* ((view explai--view))
    (cond
     ((not (buffer-live-p (explai-view-buffer view))) (explai-close))
     ((or (minibufferp) (frame-parent)) nil) ; minibuffer or a child frame (posframe) selected
     ((not (explai--anchor-window view)) (explai-close))
     (t (explai--render)))))

(defun explai--on-insert ()
  (when (and explai--view (eq (current-buffer) (explai-view-buffer explai--view)))
    (explai-close)))

(defun explai--watch ()
  ;; Our keys must win over evil's, which also live in `emulation-mode-map-alists'.
  (setq emulation-mode-map-alists
        (cons 'explai--emulation-alist (delq 'explai--emulation-alist emulation-mode-map-alists)))
  (add-hook 'post-command-hook #'explai--follow)
  (add-hook 'evil-insert-state-entry-hook #'explai--on-insert))

(defun explai--unwatch ()
  (remove-hook 'post-command-hook #'explai--follow)
  (remove-hook 'evil-insert-state-entry-hook #'explai--on-insert))

;;;;; Popup keys

(defun explai--popup-key (command &optional pred)
  "COMMAND, but only while the popup is up (and PRED holds); otherwise the key
falls through to its normal binding."
  `(menu-item "" ,command
              :filter ,(lambda (cmd)
                         (and explai--view
                              (not (minibufferp))
                              (not (and (bound-and-true-p evil-local-mode) (evil-insert-state-p)))
                              (or (null pred) (funcall pred))
                              cmd))))

(defun explai-escape ()
  "Close the popup (and leave visual state)."
  (interactive)
  (explai-close)
  (when (and (bound-and-true-p evil-local-mode) (evil-visual-state-p))
    (evil-exit-visual-state)))

(defun explai-copy ()
  "Copy the text of the popup."
  (interactive)
  (when-let* ((view (or explai--view explai--last)))
    (kill-new (or (explai-view-plain view) (explai-view-text view)))
    (message "Explai: copied")))

(defun explai-more ()
  "Ask for a longer answer to what the popup shows."
  (interactive)
  (when-let* ((fn (and explai--view (explai-view-more explai--view))))
    (funcall fn)))

(defun explai-explain-match ()
  "Explain the code a Find result points at."
  (interactive)
  (when-let* ((fn (and explai--view (explai-view-explain explai--view))))
    (funcall fn)))

(defun explai--goto (path line)
  "Visit PATH at LINE in the window of the popup's code, keeping a jump back."
  (when-let* ((win (and explai--view (explai--anchor-window explai--view))))
    (select-window win))
  (explai-close)
  (xref-push-marker-stack)
  (find-file path)
  (goto-char (point-min))
  (forward-line (1- line))
  (back-to-indentation)
  (recenter))

(defun explai-jump ()
  "Jump to one of the locations listed in the popup."
  (interactive)
  (when-let* ((links (and explai--view (explai-view-links explai--view))))
    (let* ((choice (if (= (length links) 1)
                       (caar links)
                     (completing-read "Explai: jump to " (mapcar #'car links) nil t)))
           (target (cdr (assoc choice links))))
      (when target
        (explai--goto (car target) (cdr target))))))

(defun explai-open-document ()
  "Open the popup's content in a normal buffer (with clickable locations)."
  (interactive)
  (when-let* ((view (or explai--view explai--last)))
    (let ((buf (get-buffer-create (format "*explai: %s*" (or (explai-view-where view) "result"))))
          (links (explai-view-links view)))
      (explai-close)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (or (explai-view-document view) (explai-view-text view)) "\n")
          (delay-mode-hooks (gfm-view-mode))
          (setq-local markdown-hide-markup t)
          (visual-line-mode 1)
          (font-lock-ensure)
          ;; Turn every "(path:line)" that matches a link into a button.
          (dolist (link links)
            (goto-char (point-min))
            (let ((needle (format "(%s:%d)" (file-relative-name (cadr link) (explai--root)) (cddr link))))
              (while (search-forward needle nil t)
                (make-button (1+ (match-beginning 0)) (1- (match-end 0))
                             'action (let ((path (cadr link)) (line (cddr link)))
                                       (lambda (_)
                                         (other-window 1)
                                         (find-file path)
                                         (goto-char (point-min))
                                         (forward-line (1- line))
                                         (recenter)))
                             'help-echo "Open this location"))))
          (goto-char (point-min))))
      (pop-to-buffer buf '((display-buffer-in-side-window) (side . right) (window-width . 0.4))))))

(defvar explai-popup-map
  (let ((m (make-sparse-keymap)))
    (define-key m [escape] (explai--popup-key #'explai-escape))
    (define-key m (kbd "M-w") (explai--popup-key #'explai-copy (lambda () (not (use-region-p)))))
    (define-key m (kbd "M-m") (explai--popup-key #'explai-more
                                                 (lambda () (explai-view-more explai--view))))
    (define-key m (kbd "M-o") (explai--popup-key #'explai-open-document))
    (define-key m (kbd "M-j") (explai--popup-key #'explai-jump
                                                 (lambda () (explai-view-links explai--view))))
    (define-key m (kbd "M-e") (explai--popup-key #'explai-explain-match
                                                 (lambda () (explai-view-explain explai--view))))
    m)
  "Keys active while the Explai popup is shown.")

(defvar explai--emulation-alist `((explai--popup-active . ,explai-popup-map)))

;;;; gptel requests

(defun explai--root ()
  (expand-file-name
   (or (and (fboundp 'doom-project-root) (doom-project-root))
       (when-let* ((p (project-current))) (project-root p))
       default-directory)))

(defun explai--rel (path)
  (let ((root (explai--root)))
    (if (string-prefix-p (downcase root) (downcase (expand-file-name path)))
        (file-relative-name path root)
      path)))

(defun explai--model-name ()
  (format "%s" (or explai-model gptel-model)))

(defun explai--request-buffer ()
  "A hidden buffer that owns one request, with gptel settings local to it."
  (let ((buf (generate-new-buffer " *explai-request*")))
    (with-current-buffer buf
      (setq-local gptel-backend (or explai-backend gptel-backend)
                  gptel-model (or explai-model gptel-model)
                  gptel-use-tools nil
                  gptel-tools nil
                  gptel-confirm-tool-calls nil
                  gptel-include-tool-results nil)
      (when (boundp 'gptel-use-context)
        (setq-local gptel-use-context nil))
      (when (boundp 'gptel-include-reasoning)
        (setq-local gptel-include-reasoning nil)))
    buf))

(defun explai--kill-request (buf)
  "Stop the request owned by BUF now; delete BUF a bit later, once gptel's
process sentinels (which still select it) have run."
  (when (buffer-live-p buf)
    (when (explai--request-active-p buf)
      (let ((inhibit-message t)) (ignore-errors (gptel-abort buf))))
    (run-at-time 10 nil (lambda () (when (buffer-live-p buf) (kill-buffer buf))))))

(defun explai--error-text (info)
  (let ((err (plist-get info :error)))
    (cond ((stringp err) err)
          ((and (listp err) (plist-get err :message)) (plist-get err :message))
          (err (format "%S" err))
          ((plist-get info :status) (format "%s" (plist-get info :status)))
          (t "request failed"))))

(defun explai--lang ()
  (replace-regexp-in-string "\\(-ts\\)?-mode\\'" "" (symbol-name major-mode)))

;;;; Explain / Ask

(defconst explai--system
  "You are Explai, a code explanation assistant inside Emacs. The reader is an experienced developer who is new to this codebase. Be concise, concrete and accurate; never invent behaviour that is not visible in the code you are given.")

(defvar explai--cache (make-hash-table :test 'equal) "Finished answers.")
(defvar explai--cache-order nil)
(defvar explai-ask-history nil)
(defvar explai-find-history nil)

(defun explai--selection ()
  "The code to explain: the region (or evil visual selection), else the current line."
  (let (beg end region)
    (cond
     ((and (bound-and-true-p evil-local-mode) (evil-visual-state-p))
      (let ((r (evil-visual-range)))
        (setq beg (nth 0 r) end (nth 1 r) region t))
      (evil-exit-visual-state))
     ((use-region-p)
      (setq beg (region-beginning) end (region-end) region t)
      (deactivate-mark))
     (t
      (setq beg (save-excursion (back-to-indentation) (point))
            end (line-end-position))))
    (let* ((last-pos (if (and region (> end beg)
                              (= end (save-excursion (goto-char end) (line-beginning-position))))
                         (1- end)
                       end))
           (first (line-number-at-pos beg))
           (last (line-number-at-pos last-pos))
           (name (if buffer-file-name (file-name-nondirectory buffer-file-name) (buffer-name))))
      (list :buffer (current-buffer)
            :beg (copy-marker beg) :end (copy-marker end)
            :first first :last last
            :text (buffer-substring-no-properties beg end)
            :where (if (= first last) (format "%s:%d" name first) (format "%s:%d–%d" name first last))))))

(defun explai--prompt (sel detailed question)
  (with-current-buffer (plist-get sel :buffer)
    (let* ((lang (explai--lang))
           (file (if buffer-file-name (explai--rel buffer-file-name) (buffer-name)))
           (n explai-context-lines)
           (before (save-excursion
                     (goto-char (plist-get sel :beg))
                     (buffer-substring-no-properties
                      (line-beginning-position (- 1 n)) (line-beginning-position))))
           (after (save-excursion
                    (goto-char (plist-get sel :end))
                    (buffer-substring-no-properties
                     (min (point-max) (1+ (line-end-position))) (line-end-position (1+ n)))))
           (bullets (cond (detailed "3-7") (question "1-4") (t "2-3")))
           (words (cond (detailed 250) (question 100) (t explai-word-count))))
      (string-join
       (list
        (if question
            (format "Answer this question about the %s code below from %s: \"%s\". If the code shown is not enough to answer with certainty, say so and say what you would need to check." lang file question)
          (format "Explain what the %s code below from %s does and why." lang file))
        (format "Use about %d words in total. Format the answer as Markdown exactly like this:" words)
        ""
        (if question "**<the direct answer, one or two sentences>**" "**<one sentence: what the code does>**")
        ""
        (if question "**Why**" "**How it works**")
        (format "- <%s short bullets>" bullets)
        ""
        "**⚠ Watch out**"
        "- <only if there are real pitfalls: bugs, edge cases, side effects, performance; omit this whole section otherwise>"
        ""
        "Wrap identifiers in `backticks`. No other headings, no preamble, do not restate the code."
        ""
        "Code:"
        (concat "```" lang)
        (plist-get sel :text)
        "```"
        ""
        "Surrounding code, for context only:"
        (concat "```" lang)
        before
        "/* ...the code above... */"
        after
        "```")
       "\n"))))

(defun explai--cache-key (sel detailed question)
  (with-current-buffer (plist-get sel :buffer)
    (list (current-buffer) (buffer-chars-modified-tick)
          (marker-position (plist-get sel :beg)) (marker-position (plist-get sel :end))
          detailed question)))

(defun explai--remember (key view)
  (unless (gethash key explai--cache)
    (push key explai--cache-order)
    (when (> (length explai--cache-order) 30)
      (remhash (car (last explai--cache-order)) explai--cache)
      (setq explai--cache-order (butlast explai--cache-order))))
  (puthash key view explai--cache))

(defun explai--anchor-marker (buffer line)
  "Marker at the first non-blank character of LINE in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (back-to-indentation)
      (copy-marker (point)))))

(defun explai--run (sel detailed question)
  "Explain SEL (from `explai--selection'), or answer QUESTION about it."
  (let* ((key (explai--cache-key sel detailed question))
         (cached (gethash key explai--cache)))
    (if cached
        (explai--show cached)
      (let* ((model (explai--model-name))
             (reqbuf (explai--request-buffer))
             (started (float-time))
             (view (explai-view-create
                    :id key
                    :kind (cond (question (format "“%s”" (truncate-string-to-width question 50 nil nil "…")))
                                (detailed "Detailed"))
                    :where (plist-get sel :where)
                    :buffer (plist-get sel :buffer)
                    :marker (explai--anchor-marker (plist-get sel :buffer) (plist-get sel :last))
                    :hl-beg (plist-get sel :beg) :hl-end (plist-get sel :end)
                    :text (format "_Asking %s…_" model)
                    :loading t
                    :hints "ESC cancel")))
        (setf (explai-view-more view)
              (unless detailed
                (lambda ()
                  (explai-close)
                  (explai--run sel t question))))
        (setf (explai-view-on-cancel view) (lambda () (explai--kill-request reqbuf)))
        (explai--show view)
        (let ((chunks nil))
          (with-current-buffer reqbuf
            (gptel-request (explai--prompt sel detailed question)
              :system explai--system
              :stream t
              :buffer reqbuf
              :callback
              (lambda (resp info)
                (pcase resp
                  ((pred stringp)
                   (push resp chunks)
                   (setf (explai-view-text view) (apply #'concat (reverse chunks)))
                   (when (eq explai--view view)
                     (explai--render-soon)))
                  ((or 't 'nil)
                   (setf (explai-view-loading view) nil)
                   (if (and (null resp) (null chunks))
                       (setf (explai-view-text view)
                             (concat "**Request failed**\n\n" (explai--error-text info))
                             (explai-view-hints view) "ESC close")
                     (let ((text (string-trim (apply #'concat (reverse chunks)))))
                       (setf (explai-view-text view) (if (string-empty-p text) "_No response._" text)
                             (explai-view-plain view) text
                             (explai-view-hints view) (concat (unless detailed "M-m more · ")
                                                             "M-w copy · M-o open · ESC close")
                             (explai-view-meta view) (format "%s · %.1fs" model (- (float-time) started)))
                       (explai--remember key view)))
                   (run-at-time 0 nil #'explai--kill-request reqbuf)
                   (if (eq explai--view view)
                       (explai--show view)
                     (unless (null resp) (setq explai--last view))))
                  (_ nil))))))))))

;;;###autoload
(defun explai-explain (&optional detailed)
  "Explain the region (or current line) in a popup. With prefix arg, in detail."
  (interactive "P")
  (explai--run (explai--selection) (and detailed t) nil))

;;;###autoload
(defun explai-explain-detailed ()
  "Explain the region (or current line) in detail."
  (interactive)
  (explai--run (explai--selection) t nil))

(defvar explai--pending-sel nil "Selection captured before prompting for a question.")

;;;###autoload
(defun explai-ask (question)
  "Ask QUESTION about the region (or current line). M-p / M-n browse earlier questions."
  (interactive
   (let ((sel (explai--selection)))
     (setq explai--pending-sel sel)
     (list (read-string "Ask Explai: " nil 'explai-ask-history))))
  (let ((sel (or (prog1 explai--pending-sel (setq explai--pending-sel nil))
                 (explai--selection))))
    (unless (string-empty-p (string-trim question))
      (explai--run sel nil (string-trim question)))))

;;;###autoload
(defun explai-last ()
  "Show the last Explai popup again, visiting its buffer if needed."
  (interactive)
  (let ((view explai--last))
    (if (not (and view (buffer-live-p (explai-view-buffer view))))
        (message "Explai: nothing to show yet.")
      (unless (get-buffer-window (explai-view-buffer view))
        (switch-to-buffer (explai-view-buffer view)))
      (select-window (get-buffer-window (explai-view-buffer view)))
      (goto-char (explai-view-marker view))
      (explai--show view))))

;;;###autoload
(defun explai-model ()
  "Choose the model Explai uses (from the gptel backend)."
  (interactive)
  (let* ((backend (or explai-backend gptel-backend))
         (models (mapcar (lambda (m) (format "%s" m)) (gptel-backend-models backend)))
         (choice (completing-read (format "Explai model (now %s): " (explai--model-name)) models nil t)))
    (setq explai-model (intern choice))
    (customize-save-variable 'explai-model explai-model)
    (message "Explai: using %s" choice)))

;;;; Workspace tools (for find / callers)

(defvar explai--agent nil "State of the running tool loop (one at a time).")
(defconst explai--max-tool-calls 40)

(defun explai--truncate (s n)
  (if (> (length s) n) (concat (substring s 0 (1- n)) "…") s))

(defun explai--resolve (path)
  "Existing absolute file for PATH (absolute or relative to the project root)."
  (when (and (stringp path) (not (string-empty-p path)))
    (let ((p (replace-regexp-in-string ":[0-9]+\\(-[0-9]+\\)?\\'" ""
                                       (replace-regexp-in-string "\\\\" "/" path))))
      (cl-loop for cand in (list p (expand-file-name p (explai--root)))
               when (file-regular-p cand) return (expand-file-name cand)))))

(defun explai--status (text)
  (when-let* ((fn (plist-get explai--agent :status)))
    (funcall fn text)))

(defun explai--server ()
  "The eglot server of the code the agent started from."
  (when-let* ((buf (plist-get explai--agent :buffer))
              ((buffer-live-p buf))
              ((fboundp 'eglot-current-server)))
    (with-current-buffer buf (eglot-current-server))))

(defun explai--visit (path)
  "Buffer visiting PATH, managed by eglot when a server for it is running."
  (let ((buf (find-file-noselect path)))
    (with-current-buffer buf
      (when (and (fboundp 'eglot-managed-p) (not (eglot-managed-p)))
        (ignore-errors (eglot--maybe-activate-editing-mode))))
    buf))

(defun explai--line-text (path line0)
  (if-let* ((buf (find-buffer-visiting path)))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (forward-line line0)
          (string-trim (buffer-substring-no-properties (point) (line-end-position)))))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (forward-line line0)
      (string-trim (buffer-substring-no-properties (point) (line-end-position))))))

(defun explai--symbol-kind (k)
  (or (nth (1- k) '("File" "Module" "Namespace" "Package" "Class" "Method" "Property" "Field"
                    "Constructor" "Enum" "Interface" "Function" "Variable" "Constant" "String"
                    "Number" "Boolean" "Array" "Object" "Key" "Null" "EnumMember" "Struct"
                    "Event" "Operator" "TypeParameter"))
      "Symbol"))

(defun explai--at-name (path line name fn)
  "Visit PATH, put point on NAME on LINE and call FN with the buffer current."
  (let ((abs (explai--resolve path)))
    (if (not abs)
        (format "File not found: %s" path)
      (with-current-buffer (explai--visit abs)
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- (max 1 (truncate (or line 1)))))
          (if (not (re-search-forward (concat "\\_<" (regexp-quote (format "%s" name)) "\\_>")
                                      (line-end-position) t))
              (format "\"%s\" does not appear on line %s of %s: %s" name line (explai--rel abs)
                      (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (goto-char (match-beginning 0))
            (if (not (eglot-managed-p))
                "No language server for this file. Use search_text."
              (funcall fn))))))))

(defun explai--incoming (item)
  (append (eglot--request (eglot-current-server) :callHierarchy/incomingCalls `(:item ,item)) nil))

(defun explai--format-call (call)
  (let* ((from (plist-get call :from))
         (path (eglot-uri-to-path (plist-get from :uri)))
         (sites (cl-loop for r across (plist-get call :fromRanges)
                         for i from 1 to 3
                         collect (let ((l (plist-get (plist-get r :start) :line)))
                                   (format "    %d: %s" (1+ l) (explai--line-text path l))))))
    (format "%s %s%s — %s:%d\n%s"
            (explai--symbol-kind (plist-get from :kind))
            (plist-get from :name)
            (if-let* ((d (plist-get from :detail)) ((not (string-empty-p d)))) (format " (%s)" d) "")
            (explai--rel path)
            (1+ (plist-get (plist-get (plist-get from :selectionRange) :start) :line))
            (string-join sites "\n"))))

(defun explai--tool-search-symbols (query)
  (let ((server (explai--server)))
    (if (not server)
        "No language server is running for this project; use search_text."
      (let ((syms (append (eglot--request server :workspace/symbol `(:query ,(format "%s" query))) nil)))
        (if (null syms)
            "No symbols found."
          (string-join
           (cl-loop for s in syms for i from 1 to 40
                    collect (let* ((loc (plist-get s :location))
                                   (uri (plist-get loc :uri))
                                   (range (plist-get loc :range)))
                              (format "%s %s%s — %s:%s"
                                      (explai--symbol-kind (plist-get s :kind))
                                      (plist-get s :name)
                                      (if-let* ((c (plist-get s :containerName)) ((stringp c)) ((not (string-empty-p c))))
                                          (format " (in %s)" c) "")
                                      (if uri (explai--rel (eglot-uri-to-path uri)) "?")
                                      (if range (1+ (plist-get (plist-get range :start) :line)) "?"))))
           "\n"))))))

(defun explai--rg (args callback)
  "Run ripgrep with ARGS in the project root; CALLBACK gets (OUTPUT EXIT-CODE)."
  (let* ((default-directory (explai--root))
         (out (generate-new-buffer " *explai-rg*"))
         (err (generate-new-buffer " *explai-rg-err*")))
    (make-process
     :name "explai-rg" :buffer out :stderr err :noquery t :coding 'utf-8
     :connection-type 'pipe
     :command (cons explai-rg-program args)
     :sentinel (lambda (proc _)
                 (when (memq (process-status proc) '(exit signal))
                   (let ((text (with-current-buffer out (buffer-string)))
                         (etext (with-current-buffer err (buffer-string))))
                     (kill-buffer out)
                     (kill-buffer err)
                     (funcall callback (if (and (string-empty-p text) (= (process-exit-status proc) 2)) (concat "Error: " etext) text)
                              (process-exit-status proc))))))))

(defun explai--rg-lines (text)
  "ripgrep output lines with \"./\" dropped and forward slashes in the path part only
\(matched code after \"path:line:\" is left untouched)."
  (cl-loop for l in (split-string text "[\r\n]+" t)
           collect (let ((l (replace-regexp-in-string "\\`\\.[/\\\\]" "" l)))
                     (if (string-match "\\`\\([^:]*\\)\\(:[0-9]+:.*\\)\\'" l)
                         (let ((path (match-string 1 l)) (rest (match-string 2 l)))
                           (concat (replace-regexp-in-string "\\\\" "/" path) rest))
                       (replace-regexp-in-string "\\\\" "/" l)))))

(defun explai--tool-search-text (cb pattern &optional glob case-sensitive)
  (explai--rg (append (list "--line-number" "--no-heading" "--color" "never" "--max-count" "5"
                            "--max-columns" "200" "--max-columns-preview"
                            (if (eq case-sensitive t) "--case-sensitive" "--smart-case"))
                      (when (and (stringp glob) (not (string-empty-p glob))) (list "--glob" glob))
                      (list "-e" (format "%s" pattern) "."))
              (lambda (out _code)
                (let ((lines (explai--rg-lines out)))
                  (funcall cb (cond ((string-prefix-p "Error: " out) out)
                                    ((null lines) "No matches.")
                                    (t (concat (string-join (seq-take lines 80) "\n")
                                               (when (> (length lines) 80)
                                                 (format "\n… %d more matches, refine the search." (- (length lines) 80)))))))))))

(defun explai--tool-list-files (cb glob)
  (explai--rg (list "--files" "--glob" (format "%s" glob))
              (lambda (out _code)
                (let ((files (explai--rg-lines out)))
                  (funcall cb (if files (string-join (seq-take files 100) "\n") "No files matched."))))))

(defun explai--tool-read-file (path &optional start end)
  (let ((abs (explai--resolve path)))
    (if (not abs)
        (format "File not found: %s" path)
      (let* ((lines (if-let* ((buf (find-buffer-visiting abs)))
                        (with-current-buffer buf (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
                      (with-temp-buffer (insert-file-contents abs) (split-string (buffer-string) "\n"))))
             (n (length lines))
             (first (max 1 (truncate (or start 1))))
             (last (min n (truncate (or end (+ first 199))) (+ first 199))))
        (concat (format "%s (%d lines total)\n" (explai--rel abs) n)
                (string-join (cl-loop for i from first to last
                                      collect (format "%d: %s" i (nth (1- i) lines)))
                             "\n"))))))

(defun explai--tool-find-callers (path line name)
  (explai--at-name
   path line name
   (lambda ()
     (if (not (eglot-server-capable :callHierarchyProvider))
         "This language server has no call hierarchy. Use find_references or search_text."
       (let* ((items (append (eglot--request (eglot-current-server) :textDocument/prepareCallHierarchy
                                             (eglot--TextDocumentPositionParams))
                             nil))
              (calls (cl-loop for item in items append (explai--incoming item))))
         (if (null calls)
             "No callers found."
           (string-join (mapcar #'explai--format-call (seq-take calls 40)) "\n")))))))

(defun explai--tool-find-references (path line name)
  (explai--at-name
   path line name
   (lambda ()
     (let ((refs (append (eglot--request (eglot-current-server) :textDocument/references
                                         (append (eglot--TextDocumentPositionParams)
                                                 '(:context (:includeDeclaration :json-false))))
                         nil)))
       (if (null refs)
           "No references found."
         (concat
          (string-join
           (cl-loop for r in refs for i from 1 to 60
                    collect (let ((p (eglot-uri-to-path (plist-get r :uri)))
                                  (l (plist-get (plist-get (plist-get r :range) :start) :line)))
                              (format "%s:%d: %s" (explai--rel p) (1+ l) (explai--line-text p l))))
           "\n")
          (when (> (length refs) 60) (format "\n… %d more" (- (length refs) 60)))))))))

(defun explai--describe-tool (name args)
  (pcase name
    ("search_symbols" (format "Searching symbols: %s" (nth 0 args)))
    ("search_text" (format "Searching text: %s" (explai--truncate (format "%s" (nth 0 args)) 40)))
    ("list_files" (format "Listing files: %s" (nth 0 args)))
    ("read_file" (format "Reading %s" (file-name-nondirectory (format "%s" (nth 0 args)))))
    ("find_callers" (format "Finding callers of %s" (nth 2 args)))
    ("find_references" (format "Finding references to %s" (nth 2 args)))
    (_ name)))

(defun explai--wrap-tool (name fn async)
  "Async tool function for gptel: progress, budget, error handling around FN."
  (lambda (cb &rest args)
    (let ((state (or explai--agent (list :calls 0 :running 0 :activity 0))))
      (cl-incf (plist-get state :calls))
      (cl-incf (plist-get state :running))
      (plist-put state :activity (float-time))
      (explai--status (explai--describe-tool name args))
      (let ((finish (lambda (out)
                      (cl-decf (plist-get state :running))
                      (plist-put state :activity (float-time))
                      (when (>= (plist-get state :calls) (- explai--max-tool-calls 5))
                        (setq out (concat out "\n\n[Tool budget almost used up: call the report tool now with what you have.]")))
                      (when (> (plist-get state :calls) explai--max-tool-calls)
                        (run-at-time 0 nil #'gptel-abort (plist-get state :reqbuf)))
                      (run-at-time 0 nil cb (explai--truncate (format "%s" out) 6000)))))
        (condition-case err
            (if async
                (apply fn finish args)
              (funcall finish (apply fn args)))
          (error (funcall finish (format "Error: %s" (error-message-string err)))))))))

(defun explai--loc-args ()
  (list '(:name "path" :type string :description "File path as returned by the other tools")
        '(:name "line" :type number :description "1-based line where `name` appears")
        '(:name "name" :type string :description "The identifier as written on that line")))

(defun explai--make-tool (name description args fn &optional async)
  (gptel-make-tool :name name :description description :args args :category "explai"
                   :async t :include nil :confirm nil
                   :function (explai--wrap-tool name fn async)))

(defvar explai--tools nil)

(defun explai--tools ()
  (or explai--tools
      (setq explai--tools
            (list
             (explai--make-tool
              "search_symbols"
              "Search workspace symbols (classes, methods, functions, properties…) by name using the language server. Fast and precise; try this first when you can guess an identifier name. Supports partial names."
              '((:name "query" :type string :description "Symbol name or part of it"))
              #'explai--tool-search-symbols)
             (explai--make-tool
              "search_text"
              "Regex search over file contents in the project (ripgrep, respects .gitignore). Returns matching lines as path:line:text. Use for strings, table names, log messages, config keys, etc."
              '((:name "pattern" :type string :description "Regular expression (Rust regex syntax)")
                (:name "glob" :type string :optional t :description "Optional glob to restrict the search, e.g. \"*.cs\"")
                (:name "caseSensitive" :type boolean :optional t :description "Default false (smart case)"))
              #'explai--tool-search-text t)
             (explai--make-tool
              "list_files"
              "List project files matching a glob, e.g. \"**/*Import*.cs\". Returns at most 100 paths."
              '((:name "glob" :type string :description "File glob"))
              #'explai--tool-list-files t)
             (explai--make-tool
              "read_file"
              "Read lines of a project file (1-based, inclusive, max 200 lines per call), prefixed with line numbers."
              '((:name "path" :type string :description "File path")
                (:name "startLine" :type number :optional t :description "First line, default 1")
                (:name "endLine" :type number :optional t :description "Last line"))
              #'explai--tool-read-file)
             (explai--make-tool
              "find_callers"
              "List the functions/methods that call the function `name` on `line` of `path` (language-server call hierarchy), with the call-site lines. If unavailable, use find_references or search_text."
              (explai--loc-args)
              #'explai--tool-find-callers)
             (explai--make-tool
              "find_references"
              "List references to the symbol `name` on `line` of `path` (language server; precise, excludes unrelated symbols with the same name). Max 60 results with the line text."
              (explai--loc-args)
              #'explai--tool-find-references)))))

(defun explai--request-active-p (buf)
  (cl-some (lambda (entry)
             (eq (plist-get (gptel-fsm-info (cadr entry)) :buffer) buf))
           gptel--request-alist))

(defun explai--agent-run (source system prompt report-name report-args status on-done)
  "Run the tool loop from SOURCE buffer until the model calls REPORT-NAME.
STATUS is called with progress text. ON-DONE is called with (RESULT-PLIST ERROR).
Returns a cancel function."
  (let* ((reqbuf (explai--request-buffer))
         (state (list :buffer source :reqbuf reqbuf :calls 0 :running 0
                      :activity (float-time) :done nil :result nil :status nil))
         watchdog)
    (cl-labels ((finish (result err)
                  (unless (plist-get state :done)
                    (plist-put state :done t)
                    (when watchdog (cancel-timer watchdog))
                    (run-at-time 0 nil #'explai--kill-request reqbuf)
                    (when (eq explai--agent state) (setq explai--agent nil))
                    (funcall on-done result err))))
      (let ((report (gptel-make-tool
                     :name report-name :category "explai" :include nil :confirm nil
                     :description (car report-args)
                     :args (cdr report-args)
                     :function (lambda (&rest args)
                                 ;; Got the answer: keep it and stop the request.
                                 (plist-put state :result
                                            (cl-loop for spec in (cdr report-args)
                                                     for value in args
                                                     append (list (intern (concat ":" (plist-get spec :name))) value)))
                                 (run-at-time 0 nil (lambda () (finish (plist-get state :result) nil)))
                                 "Reported."))))
        (setq explai--agent state)
        (plist-put state :status status)
        (with-current-buffer reqbuf
          (setq-local gptel-tools (append (explai--tools) (list report))
                      gptel-use-tools t))
        (cl-labels ((start (text)
                      (with-current-buffer reqbuf
                        (gptel-request text
                          :system system
                          :stream nil
                          :buffer reqbuf
                          :callback
                          (lambda (resp info)
                            (plist-put state :activity (float-time))
                            (pcase resp
                              ((pred stringp) (plist-put state :text resp))
                              ('abort (finish (plist-get state :result)
                                              (unless (plist-get state :result) "stopped")))
                              ('nil (finish nil (explai--error-text info)))
                              (_ nil)))))))
          (start prompt)
          ;; gptel has no "tool loop finished" callback: if the request is gone, no
          ;; tool is running and nothing was reported, the model ended with text.
          ;; Copilot's Claude models can't be forced to call a tool, so restart once
          ;; with its notes so far and ask it to finish with the report tool.
          (setq watchdog
                (run-with-timer
                 2 1 (lambda ()
                       (when (and (not (plist-get state :done))
                                  (= 0 (plist-get state :running))
                                  (> (- (float-time) (plist-get state :activity)) 2)
                                  (not (explai--request-active-p reqbuf)))
                         (if (plist-get state :nudged)
                             (finish nil "the model stopped without an answer")
                           (plist-put state :nudged t)
                           (plist-put state :activity (float-time))
                           (start (concat prompt
                                          "\n\nNotes from your investigation so far:\n"
                                          (or (plist-get state :text) "(none)")
                                          "\n\nContinue with the tools if needed, and finish by calling "
                                          report-name " now."))))))))
        (lambda ()
          (plist-put state :done t)
          (when watchdog (cancel-timer watchdog))
          (when (eq explai--agent state) (setq explai--agent nil))
          (explai--kill-request reqbuf))))))

(defun explai--progress-view (kind where)
  (let ((line (line-number-at-pos)))
    (explai-view-create
     :id (list kind (float-time))
     :kind kind :where where
     :buffer (current-buffer)
     :marker (explai--anchor-marker (current-buffer) line)
     :text "_Searching…_" :loading t :hints "ESC cancel")))

(defun explai--progress (view)
  (lambda (text)
    (when (and (eq explai--view view) (explai-view-loading view))
      (setf (explai-view-text view) (format "_%s…_" text))
      (explai--render-soon))))

(defun explai--seq (v) (if (vectorp v) (append v nil) v))

;;;; Find implementation

(defconst explai--report-results
  (cons "Finish: report where the requested thing is implemented, best match first (1-5 results). Line numbers must cover the actual implementation (e.g. the whole method), verified with read_file."
        '((:name "results" :type array :description "Matches, best first"
           :items (:type object
                   :properties (:path (:type string)
                                :startLine (:type number)
                                :endLine (:type number)
                                :title (:type string :description "Short label, e.g. \"ImportService.InsertBlocks\"")
                                :reason (:type string :description "One or two sentences: why this is the place"))
                   :required ["path" "startLine" "endLine" "title" "reason"])))))

(defun explai--show-found (r query all)
  (let* ((abs (plist-get r :abs))
         (first (plist-get r :start))
         (last (plist-get r :end))
         (name (file-name-nondirectory abs)))
    (xref-push-marker-stack)
    (find-file abs)
    (goto-char (point-min))
    (forward-line (1- first))
    (back-to-indentation)
    (recenter 5)
    (let* ((beg (copy-marker (line-beginning-position)))
           (end (copy-marker (save-excursion (forward-line (- last first)) (line-end-position))))
           (others (1- (length all)))
           (view (explai-view-create
                  :id (list 'found abs first)
                  :kind "Found"
                  :where (if (= first last) (format "%s:%d" name first) (format "%s:%d–%d" name first last))
                  :buffer (current-buffer)
                  :marker (explai--anchor-marker (current-buffer) first)
                  :prefer 'above
                  :hl-beg beg :hl-end end
                  :text (format "**%s**\n\n%s\n\n_You asked: %s_"
                                (replace-regexp-in-string "\\*\\*" "" (plist-get r :title))
                                (plist-get r :reason)
                                (explai--truncate query 100))
                  :plain (format "%s\n%s" (plist-get r :title) (plist-get r :reason))
                  :links (when (> others 0)
                           (mapcar (lambda (x)
                                     (cons (format "%s  —  %s:%d" (plist-get x :title) (plist-get x :path) (plist-get x :start))
                                           (cons (plist-get x :abs) (plist-get x :start))))
                                   all))
                  :hints (concat (when (> others 0) (format "M-j %d other match%s · " others (if (> others 1) "es" "")))
                                 "M-e explain · ESC close")
                  :meta (explai--model-name))))
      (setf (explai-view-explain view)
            (lambda ()
              (explai-close)
              (explai--run (list :buffer (marker-buffer beg) :beg beg :end end :first first :last last
                                 :text (with-current-buffer (marker-buffer beg)
                                         (buffer-substring-no-properties beg end))
                                 :where (explai-view-where view))
                           nil nil)))
      (explai--show view))))

;;;###autoload
(defun explai-find (query)
  "Describe QUERY; Explai searches the project and jumps to where it is implemented."
  (interactive (list (read-string "Explai – find implementation of: " nil 'explai-find-history)))
  (when (string-empty-p (string-trim query))
    (user-error "Nothing to find"))
  (let* ((source (current-buffer))
         (root (explai--root))
         (view (explai--progress-view "Find" (explai--truncate query 50)))
         cancel)
    (setf (explai-view-on-cancel view) (lambda () (when cancel (funcall cancel))))
    (explai--show view)
    (setq cancel
          (explai--agent-run
           source
           (concat "You are a code navigation assistant inside Emacs. Find where things are implemented in the user's project (root: "
                   root ") using the tools. Search efficiently: start with search_symbols and search_text, try synonyms and naming conventions, then read_file to confirm before answering. Prefer the actual implementation over interfaces, tests, call sites or comments, unless asked for those. Call report_results exactly once; with an empty list if nothing relevant exists.")
           (concat (when buffer-file-name (format "I currently have %s open.\n" (explai--rel buffer-file-name)))
                   "What I'm looking for: " query)
           "report_results" explai--report-results (explai--progress view)
           (lambda (result err)
             (when (eq explai--view view)
               (setf (explai-view-loading view) nil)
               (explai-close))
             (cond
              ((not result)
               (unless (equal err "stopped") (message "Explai: %s" err)))
              (t
               (let ((results
                      (cl-loop for r in (explai--seq (plist-get result :results))
                               for abs = (and (listp r) (explai--resolve (plist-get r :path)))
                               for start = (max 1 (truncate (or (plist-get r :startLine) 1)))
                               when abs
                               collect (list :abs abs :path (explai--rel abs)
                                             :start start
                                             :end (max start (truncate (or (plist-get r :endLine) start)))
                                             :title (format "%s" (or (plist-get r :title) (file-name-nondirectory abs)))
                                             :reason (format "%s" (or (plist-get r :reason) ""))))))
                 (setq results (seq-take results 5))
                 (cond
                  ((null results) (message "Explai: couldn't find \"%s\" in the project." query))
                  ((= (length results) 1) (explai--show-found (car results) query results))
                  (t
                   (let* ((names (mapcar (lambda (r) (cons (format "%s  —  %s:%d" (plist-get r :title) (plist-get r :path) (plist-get r :start)) r))
                                         results))
                          (choice (completing-read (format "Explai: %s " (explai--truncate query 50)) (mapcar #'car names) nil t)))
                     (explai--show-found (cdr (assoc choice names)) query results))))))))))))

;;;; Trace callers

(defconst explai--report-flow
  (cons "Finish: report how the target is reached. Give 1-3 distinct call paths, each ordered from the entry point (UI event, Main, API endpoint, timer, thread start, test…) down to the target itself as the last step. Paths and line numbers must be verified with the tools."
        '((:name "summary" :type string :description "One or two sentences: who triggers the target and why")
          (:name "paths" :type array :description "1-3 call paths"
           :items (:type object
                   :properties (:title (:type string :description "Short name, e.g. \"Manual import from the UI\"")
                                :steps (:type array
                                        :items (:type object
                                                :properties (:name (:type string :description "Function/method, e.g. \"ImportService.Run\"")
                                                             :path (:type string)
                                                             :line (:type number :description "1-based line of the call (declaration for the entry point)")
                                                             :description (:type string :description "What happens at this step, max ~15 words"))
                                                :required ["name" "path" "line" "description"])))
                   :required ["title" "steps"]))
          (:name "notes" :type array :optional t :items (:type string)
           :description "0-3 notable things: conditions that gate the call, threading, error handling, dead paths"))))

(defconst explai--function-kinds '(6 7 9 12 25)) ; Method Property Constructor Function Operator

(defun explai--range-contains (range line col)
  (let ((s (plist-get range :start)) (e (plist-get range :end)))
    (and (or (> line (plist-get s :line))
             (and (= line (plist-get s :line)) (>= col (plist-get s :character))))
         (or (< line (plist-get e :line))
             (and (= line (plist-get e :line)) (<= col (plist-get e :character)))))))

(defun explai--innermost-function (symbols line col)
  (let (best)
    (cl-labels ((visit (list)
                  (dolist (s (explai--seq list))
                    (let ((range (or (plist-get s :range) (plist-get (plist-get s :location) :range))))
                      (when (and range (explai--range-contains range line col))
                        (when (memq (plist-get s :kind) explai--function-kinds)
                          (setq best s))
                        (visit (plist-get s :children)))))))
      (visit symbols))
    best))

(defun explai--callers-target ()
  "The function at/around point: plist (:name :line :item :first :last), lines 1-based."
  (let ((managed (and (fboundp 'eglot-managed-p) (eglot-managed-p)))
        (uri nil))
    (or
     (when managed
       (setq uri (plist-get (eglot--TextDocumentIdentifier) :uri))
       (let* ((prepare (lambda ()
                         (when (eglot-server-capable :callHierarchyProvider)
                           (ignore-errors
                             (car (append (eglot--request (eglot-current-server) :textDocument/prepareCallHierarchy
                                                          (eglot--TextDocumentPositionParams))
                                          nil))))))
              (at-point (funcall prepare)))
         (or
          ;; 1. a function declared in this file, under point
          (when (and at-point (equal (plist-get at-point :uri) uri))
            (let ((sel (plist-get at-point :selectionRange)) (range (plist-get at-point :range)))
              (list :name (plist-get at-point :name) :item at-point
                    :line (1+ (plist-get (plist-get sel :start) :line))
                    :first (1+ (plist-get (plist-get range :start) :line))
                    :last (1+ (plist-get (plist-get range :end) :line)))))
          ;; 2. the innermost function containing point
          (let* ((symbols (ignore-errors
                            (eglot--request (eglot-current-server) :textDocument/documentSymbol
                                            `(:textDocument ,(eglot--TextDocumentIdentifier)))))
                 (pos (eglot--TextDocumentPositionParams))
                 (p (plist-get pos :position))
                 (fn (and symbols (explai--innermost-function symbols (plist-get p :line) (plist-get p :character)))))
            (when fn
              (let* ((range (or (plist-get fn :range) (plist-get (plist-get fn :location) :range)))
                     (sel (or (plist-get fn :selectionRange) range)))
                (list :name (plist-get fn :name)
                      :item (save-excursion
                              (goto-char (eglot--lsp-position-to-point (plist-get sel :start)))
                              (funcall prepare))
                      :line (1+ (plist-get (plist-get sel :start) :line))
                      :first (1+ (plist-get (plist-get range :start) :line))
                      :last (1+ (plist-get (plist-get range :end) :line))))))
          ;; 3. a call to a function in another file
          (when at-point
            (let ((l (line-number-at-pos)))
              (list :name (plist-get at-point :name) :item at-point :line l :first l :last l))))))
     ;; 4. no language server: go by name and let the model search
     (when-let* ((name (or (ignore-errors (require 'which-func) (which-function))
                           (ignore-errors (add-log-current-defun))
                           (thing-at-point 'symbol t))))
       (let ((l (line-number-at-pos)))
         (list :name (car (last (split-string (format "%s" name) "[.:]+" t))) :line l :first l :last l))))))

(defun explai--call-tree (root-item)
  (let ((lines (list (format "%s — %s:%d" (plist-get root-item :name)
                             (explai--rel (eglot-uri-to-path (plist-get root-item :uri)))
                             (1+ (plist-get (plist-get (plist-get root-item :selectionRange) :start) :line)))))
        (seen (make-hash-table :test 'equal))
        (count 0))
    (cl-labels ((key (item) (format "%s#%s" (plist-get item :uri) (plist-get item :selectionRange)))
                (walk (item depth indent)
                  (let ((calls (ignore-errors (explai--incoming item))))
                    (when (and (null calls) (= depth 4))
                      (push (concat indent "(no callers found by the language server)") lines))
                    (cl-loop for c in calls for i from 1
                             do (cond
                                 ((> i 8)
                                  (push (format "%s… %d more callers" indent (- (length calls) 8)) lines)
                                  (cl-return))
                                 ((>= count 30)
                                  (push (concat indent "… (tree truncated)") lines)
                                  (cl-return))
                                 (t
                                  (cl-incf count)
                                  (let* ((from (plist-get c :from))
                                         (k (key from))
                                         (repeat (gethash k seen)))
                                    (push (format "%s← %s — %s:%d, call at line %s%s"
                                                  indent (plist-get from :name)
                                                  (explai--rel (eglot-uri-to-path (plist-get from :uri)))
                                                  (1+ (plist-get (plist-get (plist-get from :selectionRange) :start) :line))
                                                  (mapconcat (lambda (r) (format "%d" (1+ (plist-get (plist-get r :start) :line))))
                                                             (seq-take (explai--seq (plist-get c :fromRanges)) 3) ", ")
                                                  (if repeat " [already shown above]" ""))
                                          lines)
                                    (unless repeat
                                      (puthash k t seen)
                                      (when (> depth 1)
                                        (walk from (1- depth) (concat indent "    ")))))))))))
      (puthash (key root-item) t seen)
      (walk root-item 4 "    "))
    (string-join (nreverse lines) "\n")))

(defun explai--render-flow (result target-name focus)
  "Popup text, document text and jump links for a report_flow RESULT."
  (let ((text nil) (doc nil) (links nil) (n 0)
        (summary (string-trim (format "%s" (or (plist-get result :summary) "")))))
    (cl-flet ((add (s) (push s text))
              (dadd (s) (push s doc)))
      (dadd (format "# How `%s` is reached\n" target-name))
      (unless (string-empty-p focus)
        (add (format "_“%s”_\n" (explai--truncate focus 90)))
        (dadd (format "> %s\n" focus)))
      (unless (string-empty-p summary)
        (add (format "**%s**\n" (replace-regexp-in-string "\\*\\*" "" summary)))
        (dadd (format "**%s**\n" summary)))
      (dolist (p (seq-take (explai--seq (plist-get result :paths)) 3))
        (let ((title (format "%s" (or (plist-get p :title) "Call path")))
              (steps (explai--seq (plist-get p :steps))))
          (add (format "**▶ %s**" title))
          (dadd (format "## %s\n" title))
          (cl-loop for s in steps for i from 1
                   do (let* ((name (format "%s" (plist-get s :name)))
                             (path (format "%s" (plist-get s :path)))
                             (line (max 1 (truncate (or (plist-get s :line) 1))))
                             (desc (format "%s" (or (plist-get s :description) "")))
                             (abs (explai--resolve path))
                             (short (format "%s:%d" (file-name-nondirectory path) line)))
                        (cl-incf n)
                        (add (format "%d. %s — %s  _%s_" i
                                     (if (= i (length steps)) (format "**`%s`**" name) (format "`%s`" name))
                                     desc short))
                        (dadd (format "%d. `%s` — %s (%s:%d)" i name desc (if abs (explai--rel abs) path) line))
                        (when abs
                          (push (cons (format "%s › %d. %s  —  %s" title i name short) (cons abs line)) links))))
          (add "")
          (dadd "")))
      (let ((notes (seq-take (seq-filter #'stringp (explai--seq (plist-get result :notes))) 3)))
        (when notes
          (add "**ℹ Notes**")
          (dadd "## Notes\n")
          (dolist (note notes)
            (add (concat "- " note))
            (dadd (concat "- " note))))))
    (list :text (string-trim (string-join (nreverse text) "\n"))
          :doc (string-join (nreverse doc) "\n")
          :links (nreverse links))))

;;;###autoload
(defun explai-callers (&optional focus)
  "Trace how the function at point is reached. With prefix arg, ask what to focus on."
  (interactive
   (list (when current-prefix-arg
           (read-string "Explai – what do you want to know? (optional): "))))
  (let* ((focus (string-trim (or focus "")))
         (source (current-buffer))
         (root (explai--root))
         (view (explai--progress-view "Callers" (or (thing-at-point 'symbol t) "")))
         (started (float-time))
         cancel)
    (setf (explai-view-text view) "_Finding the function…_"
          (explai-view-on-cancel view) (lambda () (when cancel (funcall cancel))))
    (explai--show view)
    (redisplay)
    (let ((target (explai--callers-target)))
      (if (not target)
          (progn (explai-close) (message "Explai: put point on or inside a function."))
        (setf (explai-view-where view) (plist-get target :name)
              (explai-view-marker view) (explai--anchor-marker source (plist-get target :line))
              (explai-view-text view) "_Building call tree…_")
        (explai--render)
        (redisplay)
        (let* ((tree (when (plist-get target :item)
                       (ignore-errors (explai--call-tree (plist-get target :item)))))
               (code (save-excursion
                       (goto-char (point-min))
                       (forward-line (1- (plist-get target :first)))
                       (let ((beg (point)))
                         (forward-line (1+ (min 100 (- (plist-get target :last) (plist-get target :first)))))
                         (buffer-substring-no-properties beg (point)))))
               (prompt
                (string-join
                 (list
                  (format "Explain how the function `%s` (%s:%d) is reached: trace its callers up to the real entry points (UI event handlers, Main, API/RPC endpoints, timers, background threads/tasks, scheduled jobs, tests)."
                          (plist-get target :name)
                          (if buffer-file-name (explai--rel buffer-file-name) (buffer-name))
                          (plist-get target :line))
                  ""
                  (if tree
                      (concat "Incoming call tree from the language server (may be incomplete: it misses calls through interfaces, delegates/events, reflection, DI, virtual dispatch and other languages):\n" tree)
                    "No call hierarchy is available from the language server; use find_references and search_text.")
                  ""
                  "Use the tools to fill gaps (find_callers on callers that weren't expanded; find_references or search_text for interface implementations, event subscriptions (+=), delegates, registrations) and read_file to verify the call sites and the conditions around them. Skip tests unless they are the only callers. Finish by calling report_flow exactly once."
                  (if (string-empty-p focus) "" (concat "\nThe user specifically wants to know: " focus))
                  ""
                  "Target code:"
                  (concat "```" (explai--lang))
                  (explai--truncate code 6000)
                  "```")
                 "\n")))
          (setf (explai-view-text view) "_Tracing callers…_")
          (explai--render-soon)
          (setq cancel
                (explai--agent-run
                 source
                 (concat "You are a code navigation assistant inside Emacs (project root: " root "). Be precise and verify with the tools.")
                 prompt "report_flow" explai--report-flow (explai--progress view)
                 (lambda (result err)
                   (if (not result)
                       (progn
                         (when (eq explai--view view)
                           (setf (explai-view-loading view) nil)
                           (explai-close))
                         (unless (equal err "stopped")
                           (message "Explai: %s" (or err (format "couldn't work out how %s is reached" (plist-get target :name))))))
                     (let ((out (explai--render-flow result (plist-get target :name) focus)))
                       (setf (explai-view-loading view) nil
                             (explai-view-text view) (plist-get out :text)
                             (explai-view-document view) (plist-get out :doc)
                             (explai-view-plain view) (plist-get out :doc)
                             (explai-view-links view) (plist-get out :links)
                             (explai-view-hints view) "M-j jump · M-o open · M-w copy · ESC close"
                             (explai-view-meta view) (format "%s · %.1fs" (explai--model-name) (- (float-time) started)))
                       (if (eq explai--view view)
                           (explai--show view)
                         (setq explai--last view)
                         (message "Explai: call trace ready, show it with `explai-last'."))))))))))))

(provide 'explai)
;;; explai.el ends here
