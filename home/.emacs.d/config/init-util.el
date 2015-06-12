; "After" macro definition
;
; http://www.lunaryorn.com/2013/06/25/introducing-with-eval-after-load.html
;
; At June, 13th Emacs trunk introduced a new macro `with-eval-after-load`. It
; behaves like `eval-after-load`, except that it takes multiple unquoted forms
; and wraps them into a lambda to enable byte compilation:
;
; This supersedes much of my last post about byte compilation in
; `eval-after-load`. However, the new macro does not load the corresponding
; features during byte compilation, so I’ll wrap my old `after` macro
; around it to avoid bogus warnings:
(defmacro after (feature &rest forms)
  "After FEATURE is loaded, evaluate FORMS.

FORMS is byte compiled.

FEATURE may be a named feature or a file name, see
`with-eval-after-load' for details."
  (declare (indent 1) (debug t))
  `(,(if (or (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       (message "after: cannot find %s" feature)
       'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

; To ensure compatibility with releases and older snapshot builds, I define
; with-eval-after-load if it is absent:
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))

(defmacro lazy-major-mode (pattern mode)
  "Defines a new major-mode matched by PATTERN and activates it."
  `(add-to-list 'auto-mode-alist
                '(,pattern . (lambda ()
                               (,mode)))))

(defmacro delayed-init (&rest body)
  "Runs BODY after idle for a predetermined amount of time."
  `(run-with-idle-timer
    1.5
    nil
    (lambda () ,@body)))

(defun my-recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory (concat user-emacs-directory "config") 0))

(defun my-window-killer ()
  "closes the window, and deletes the buffer if it's the last window open."
  (interactive)
  (if (> buffer-display-count 1)
      (if (= (length (window-list)) 1)
          (kill-buffer)
        (delete-window))
    (kill-buffer-and-window)))

(defun my-minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun my-set-transparency (alpha)
  "Sets the transparency of the current frame."
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha alpha))

(defun my-google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search Google: "))))))

(defun my-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun my-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (backware-kill-sexp)
    (insert (format "%s" value))))

(defun my-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun my-delete-current-buffer-file ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun my-goto-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (emacs-lisp-mode))

(defun my-insert-last-kbd-macro ()
  (interactive)
  (name-last-kbd-macro 'my-last-macro)
  (insert-kbd-macro 'my-last-macro))

(defun my-buffer-to-unix-format ()
  "Converts the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun my-buffer-to-dos-format ()
  "Converts the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

 (defun require-package (package)
  "Ensures that PACKAGE is installed."
  (unless (or (package-installed-p package)
              (require package nil 'noerror))
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(provide 'init-util)
