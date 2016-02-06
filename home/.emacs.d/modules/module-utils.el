;;; module-utils.el --- Utility functions

;; This file is NOT part of GNU Emacs.

;;; License:

;;; Commentary:

(require 'core-vars)
;; (require 'core-funcs)
;; (require 'core-keybindings)
;; (require 'core-display-init)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(defconst dotemacs-filepath (expand-file-name "." user-emacs-directory)
  "Filepath to the installed dotfile.")

(defun dotemacs-home ()
  "Go to home dotemacs buffer."
  (interactive)
  (switch-to-buffer "*dotemacs*"))

(defun dotemacs-home-delete-other-windows ()
  "Open home Dotemacs buffer and delete other windows.
Useful for making the home buffer the only visible buffer in the frame."
  (interactive)
  (dotemacs-home)
  (delete-other-windows))

(defun dotemacs-alternate-buffer ()
  "Switch back and forth between current and last buffer in the current window."
  (interactive)
  (if (evil-alternate-buffer)
      (switch-to-buffer (car (evil-alternate-buffer)))
    (switch-to-buffer (other-buffer (current-buffer) t))))

(defun dotemacs-next-error (&optional n reset)
  "Dispatch to flycheck or standard Emacs error."
  (interactive "P")
  (if (and (boundp 'flycheck-mode)
           (symbol-value flycheck-mode))
      (call-interactively 'flycheck-next-error)
    (call-interactively 'next-error)))

(defun dotemacs-previous-error (&optional n reset)
  "Dispatch to flycheck or standard Emacs error."
  (interactive "P")
  (if (and (boundp 'flycheck-mode)
           (symbol-value flycheck-mode))
      (call-interactively 'flycheck-previous-error)
    (call-interactively 'previous-error)))

(defun dotemacs/echo (msg &rest args)
  "Display MSG in echo-area without logging it in *Messages* buffer."
  (interactive)
  (let ((message-log-max nil))
    (apply 'message msg args)))

(defun dotemacs/system-is-mac ()
  (string-equal system-type "darwin"))
(defun dotemacs/system-is-linux ()
  (string-equal system-type "gnu/linux"))
(defun dotemacs/system-is-mswindows ()
  (string-equal system-type "windows-nt"))

(defun dotemacs-useless-buffer-p (buffer)
  "Determines if a buffer is useful."
  (let ((buf-paren-major-mode (get (with-current-buffer buffer
                                     major-mode)
                                   'derived-mode-parent))
        (buf-name (buffer-name buffer)))
    ;; first find if useful buffer exists, if so returns nil and don't check for
    ;; useless buffers. If no useful buffer is found, check for useless buffers.
    (unless (cl-loop for regexp in dotemacs-useful-buffers-regexp do
                     (when (or (eq buf-paren-major-mode 'comint-mode)
                               (string-match regexp buf-name))
                       (return t)))
      (cl-loop for regexp in dotemacs-useless-buffers-regexp do
               (when (string-match regexp buf-name)
                 (return t))))))

(defun dotemacs-next-useful-buffer ()
  "Switch to the next buffer and avoid special buffers."
  (interactive)
  (let ((start-buffer (current-buffer)))
    (next-buffer)
    (while (and (dotemacs-useless-buffer-p (current-buffer))
                (not (eq (current-buffer) start-buffer)))
      (next-buffer))))

(defun dotemacs-previous-useful-buffer ()
  "Switch to the previous buffer and avoid special buffers."
  (interactive)
  (let ((start-buffer (current-buffer)))
    (previous-buffer)
    (while (and (dotemacs-useless-buffer-p (current-buffer))
                (not (eq (current-buffer) start-buffer)))
      (previous-buffer))))

(defun dotemacs-location ()
  "Return the absolute path to the dotfiles dotfile."
  dotemacs-filepath)

(defun dotemacs/find-dotfile ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (find-file-existing (dotemacs-location)))

(defun dotemacs//unset-scroll-margin ()
  "Set `scroll-margin` to zero."
  (setq-local scroll-margin 0))

(defun dotemacs-expand-private-file (file-name)
  "Get the absolute path for a private file with FILE-NAME."
  (expand-file-name file-name dotemacs-private-dir))

(defun dotemacs-load-private-file (file-name &optional noerror nomessage)
  "Load a private file with FILE-NAME.

NOERROR and NOMESSAGE are passed to `load'."
  (load (dotemacs-expand-private-file file-name)
        noerror nomessage))

(defun current-line ()
  "Return the line at point as a string."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defmacro dotemacs|call-func (func &optional msg)
  "Call the function from the dotfile only if it is bound.
If MSG is not nil then display a message in `*Messages'. Errors
are caught and signalled to user in spacemacs buffer."
  `(progn
     (when ,msg (dotemacs-buffer/message ,msg))
     (when (fboundp ',func)
       (condition-case-unless-debug err
           (,func)
         (error
          (dotemacs-buffer/append (format "Error in %s: %s\n"
                                          ',(symbol-name func)
                                          (error-message-string err))
                                  t))))))

(defun dotemacs/get-variable-string-list ()
  "Return a list of all the dotemacs variables as strings."
  (all-completions "" obarray
                   (lambda (x)
                     (and (boundp x)
                          (not (keywordp x))
                          (string-prefix-p "dotemacs"
                                           (symbol-name x))))))

(defun dotemacs/get-variable-list ()
  "Return a list of all dotemacs variable symbols."
  (mapcar 'intern (dotemacs/get-variable-string-list)))

(defun dotemacs//ido-completing-read (prompt candidates)
  "Call `ido-completing-read' with a CANDIDATES alist where the key is
a display strng and the value is the actual value to return."
  (let ((ido-max-window-height (1+ (length candidates))))
    (cadr (assoc (ido-completing-read prompt (mapcar 'car candidates))
                 candidates))))

(defun dotemacs//describe-last-keys-string ()
  "Gathers info about your Emacs last keys and returns it as a string."
  (view-lossage)
  (let* ((lossage-buffer "*Help*")
         (last-keys (format "#### Emacs last keys\n```text\n%s```\n"
                            (with-current-buffer lossage-buffer
                              (buffer-string)))))
    (kill-buffer lossage-buffer)
    last-keys))

(defun spacemacs/describe-last-keys ()
  "Gathers info about your Emacs last keys and copies to clipboard."
  (interactive)
  (let ((lossage (dotemacs//describe-last-keys-string)))
    (kill-new lossage)
    (message lossage)
    (message (concat "Information has been copied to clipboard.\n"
                     (propertize
                      "PLEASE REVIEW THE DATA."
                      'face 'font-lock-warning-face)
                     "Check the *Messages* buffer if you need to review it"))))


;;; Unused functions

;; (defun dotemacs/set-attributes-from-alist (face attr)
;;   "Apply an alist of attributes in the form ((:PROP . VALUE)) to face."
;;   (while (car attr)
;;     (set-face-attribute face nil (caar attr) (cdar attr))
;;     (setq attr (cdr attr))))

;; (defun dotemacs/new-empty-buffer ()
;;   "Create a new buffer called untitled(<n>)"
;;   (interactive)
;;   (let ((newbuf (generate-new-buffer-name "untitled")))
;;     (switch-to-buffer newbuf)))

;; (defun dotemacs/toggle-fullscreen ()
;;   "Toggle full screen on X11 and Carbon"
;;   (interactive)
;;   (cond
;;    ((eq window-system 'x)
;;     (set-frame-parameter nil 'fullscreen
;;                          (when (not (frame-parameter nil 'fullscreen))
;;                            'fullboth)))
;;    ((eq window-system 'mac)
;;     (set-frame-parameter
;;      nil 'fullscreen
;;      (when (not (frame-parameter nil 'fullscreen)) 'fullscreen)))))

;; (defun dotemacs-imagep (object)
;;   "Tests whether the given object is an image (a list whose
;; first element is the symbol `image')."
;;   (and (listp object)
;;        object
;;        (eq 'image (car object))))

;; (defun dotemacs-intersperse (seq separator)
;;   "Returns a list with `SEPARATOR' added between each element
;; of the list `SEQ'."
;;   (cond
;;    ((not seq) nil)
;;    ((not (cdr seq)) seq)
;;    (t (append (list (car seq) separator)
;;               (dotemacs-intersperse (cdr seq) separator)))))

;; (defun dotemacs-mode-line-nonempty (seg)
;;   "Checks whether a modeline segment (classical Emacs style)
;; is nonempty."
;;   (let ((val (format-mode-line seg)))
;;     (cond ((listp val) val)
;;           ((stringp val) (< 0 (length val)))
;;           (t))))

;; (defun dotemacs/fill-char-to-column (char column)
;;   " Fill the line with CHAR up to the given COLUMN"
;;   (interactive "cFill with char: \nnUp to column: "
;;                char column))

;; ;; add emacs binary helper functions
;; (defun dotemacs/emacsbin-path()
;;   (interactive)
;;   (concat exec-directory (if (dotemacs/system-is-mswindows) "bin/") "emacs"))

;; (defun dotemacs/emacs()
;;   (interactive)
;;   (call-process (dotemacs/emacsbin-path) nil 0 nil)
;;   (message "Started 'emacs' - it will be ready soon ..."))

;; (defun dotemacs/emacs-debug-init()
;;   (interactive)
;;   (call-process (dotemacs/emacsbin-path) nil 0 nil "--debug-init")
;;   (message "Started 'emacs --debug-init' - it will be ready soon ..."))

;; (defun dotemacs/emacs-reload()
;;   (interactive)
;;   (load-file user-init-file)
;;   (message ".emacs reloaded successfully"))

;; (defun dotemacs/emacs-Q() (interactive)
;;        (call-process (dotemacs/emacsbin-path) nil 0 nil "-Q")
;;        (message "Started 'emacs -Q' - it will be ready soon ..."))

;; ;; from magnars
;; (defun dotemacs/eval-and-replace ()
;;   "Replace the preceding sexp with its value."
;;   (interactive)
;;   (backward-kill-sexp)
;;   (condition-case nil
;;       (prin1 (eval (read (current-kill 0)))
;;              (current-buffer))
;;     (error (message "Invalid expression")
;;            (insert (current-kill 0)))))

;; (defun dotemacs/toggle-triple-double-column-mode ()
;;   " Toggle between triple columns and double columns mode quickly. "
;;   (interactive)
;;   (if (= 3 (length (window-list)))
;;       (progn (delete-window (window-next-sibling))
;;              (golden-ratio-mode 1))
;;     (let ((num-windows (length (window-list))))
;;       (progn
;;         (golden-ratio-mode 0)
;;         (dotimes (i (max 0 (- num-windows 3)))
;;           (delete-window (window-next-sibling)))
;;         (dotimes (i (- 3 (length (window-list))))
;;           (progn (split-window-right)
;;                  (balance-windows)))))))

;; ;; idea from http://www.reddit.com/r/emacs/comments/312ge1/i_created_this_function_because_i_was_tired_of/
;; (defun dotemacs-eval-current-form ()
;;   "Looks for the current def* or set* command then evaluates, unlike `eval-defun', does not go to topmost function"
;;   (interactive)
;;   (save-excursion
;;     (search-backward-regexp "(def\\|(set")
;;     (forward-list)
;;     (call-interactively 'eval-last-sexp)))

;; ;; from magnars
;; (defun dotemacs/file-name-at-point ()
;;   (save-excursion
;;     (let* ((file-name-regexp "[./a-zA-Z0-9\-_~]")
;;            (start (progn
;;                     (while (looking-back file-name-regexp)
;;                       (forward-char -1))
;;                     (point)))
;;            (end (progn
;;                   (while (looking-at file-name-regexp)
;;                     (forward-char 1))
;;                   (point))))
;;       (buffer-substring start end))))

;; ;; from magnars
;; (defun dotemacs/find-or-create-file-at-point ()
;;   "Guesses what parts of the buffer under point is a file name and opens it."
;;   (interactive)
;;   (find-file (dotemacs/file-name-at-point)))

;; ;; from magnars
;; (defun dotemacs/find-or-create-file-at-point-other-window ()
;;   "Guesses what parts of the buffer under point is a file name and opens it."
;;   (interactive)
;;   (find-file-other-window (dotemacs/file-name-at-point)))

;; ;; from magnars
;; (defun dotemacs/touch-buffer-file ()
;;   (interactive)
;;   (insert " ")
;;   (backward-delete-char 1)
;;   (save-buffer))

;; ;; evenly split windows horizontally
;; (defun dotemacs/evenly-split-window-right ()
;;   "Evenly split frame horizontally."
;;   (interactive)
;;   (split-window-right)
;;   (balance-windows))

;; ;; evenly split windows vertically
;; (defun dotemacs/evenly-split-window-below ()
;;   "Evenly split frame vertically."
;;   (interactive)

;;   (split-window-below)
;;   (balance-windows))

(provide 'module-utils)
;;; module-utils.el ends here
