(require 'rx)

(defun dotemacs-elisp-find-cask-file (other-window)
    "Find the Cask file for this buffer.

When OTHER-WINDOW is non-nil, find the Cask file in another
window."
    (interactive "P")
    (unless (buffer-file-name)
      (user-error "The buffer has no file"))
    (let ((directory (locate-dominating-file (buffer-file-name) "Cask")))
      (unless directory
        (user-error "No Cask file found for this file"))
      (funcall (if other-window #'find-file-other-window #'find-file)
               (expand-file-name "Cask" directory))))

(defun dotemacs-emacs-elisp-current-feature ()
  "Return the feature provided by the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp (rx line-start "(provide '"))
      (symbol-name (symbol-at-point)))))

(defconst dotemacs-use-package-imenu-expression
  `("Use Package" ,(rx "(use-package" (optional "-with-elapsed-timer")
                       symbol-end (1+ (syntax whitespace)) symbol-start
                       (group-n 1 (1+ (or (syntax word) (syntax symbol))))
                       symbol-end) 1)
  "IMenu expression for `use-package' declarations.")

(defun dotemacs-add-use-package-to-imenu ()
  "Add `use-package' declarations to `imenu'."
  (add-to-list 'imenu-generic-expression
               dotemacs-use-package-imenu-expression))

(provide 'init-lisp)
