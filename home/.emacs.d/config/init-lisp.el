; TODO Go through the following commented code
; (require 'init-programming)
;
; (require-package 'elisp-slime-nav)
; (after "elisp-slime-nav-autoloads"
;   (defadvice elisp-slime-nav-find-elisp-thing-at-point (after advice-for-elisp-slime-nav-find-elisp-thing-at-point activate)
;     (recenter)))
;
; (defun my-lisp-hook ()
;   (progn
;     (elisp-slime-nav-mode)
;     (eldoc-mode)))
;
; (defun my-lisp-after-save-hook ()
;   (when (or (string-prefix-p (file-truename (concat user-emacs-directory "/config"))
;                              (file-truename buffer-file-name))
;             (equal (file-truename buffer-file-name)
;                    (file-truename custom-file)))
;     (emacs-lisp-byte-compile)))
;
; (add-hook 'emacs-lisp-mode-hook #'my-lisp-hook)
; (add-hook 'lisp-interaction-mode-hook #'my-lisp-hook)
; (add-hook 'ielm-mode-hook #'my-lisp-hook)
; (add-hook 'after-save-hook #'my-lisp-after-save-hook)
;
; ;; Lisp configuration
; (define-key read-expression-map (kbd "TAB") 'completion-at-point)
;
; ;; wrap keybindings
; (define-key lisp-mode-shared-map (kbd "M-(") (my-wrap-with "("))
; (define-key lisp-mode-shared-map (kbd "M-[") (my-wrap-with "["))
; (define-key lisp-mode-shared-map (kbd "M-\"") (my-wrap-with "\""))
;
;
; ;; A great lisp coding hook
; (defun my-lisp-coding-defaults ()
;   (smartparens-strict-mode +1))
;
; (setq my-lisp-coding-hook 'my-lisp-coding-defaults)
;
; ;; interactive modes don't need whitespace checks
; (defun my-interactive-lisp-coding-defaults ()
;   (smartparens-strict-mode +1)
;   (whitespace-mode -1))
;
; (setq my-interactive-lisp-coding-hook 'my-interactive-lisp-coding-defaults)


(require 'rx)

(defun dotemacs-find-cask-file (other-window)
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

(defun dotemacs-emacs-lisp-current-feature ()
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
