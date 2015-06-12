(require 'init-programming)
(require-package 'rainbow-delimiters)
(require 'rainbow-delimiters)

(require-package 'elisp-slime-nav)
(after "elisp-slime-nav-autoloads"
  (defadvice elisp-slime-nav-find-elisp-thing-at-point (after advice-for-elisp-slime-nav-find-elisp-thing-at-point activate)
    (recenter)))

(defun my-lisp-hook ()
  (progn
    (elisp-slime-nav-mode)
    (eldoc-mode)))

(defun my-lisp-after-save-hook ()
  (when (or (string-prefix-p (file-truename (concat user-emacs-directory "/config"))
                             (file-truename buffer-file-name))
            (equal (file-truename buffer-file-name)
                   (file-truename custom-file)))
    (emacs-lisp-byte-compile)))

(add-hook 'emacs-lisp-mode-hook #'my-lisp-hook)
(add-hook 'lisp-interaction-mode-hook #'my-lisp-hook)
(add-hook 'ielm-mode-hook #'my-lisp-hook)
(add-hook 'after-save-hook #'my-lisp-after-save-hook)

;; Lisp configuration
(define-key read-expression-map (kbd "TAB") 'completion-at-point)

;; wrap keybindings
(define-key lisp-mode-shared-map (kbd "M-(") (my-wrap-with "("))
(define-key lisp-mode-shared-map (kbd "M-[") (my-wrap-with "["))
(define-key lisp-mode-shared-map (kbd "M-\"") (my-wrap-with "\""))


;; A great lisp coding hook
(defun my-lisp-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

(setq my-lisp-coding-hook 'my-lisp-coding-defaults)

;; interactive modes don't need whitespace checks
(defun my-interactive-lisp-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (whitespace-mode -1))

(setq my-interactive-lisp-coding-hook 'my-interactive-lisp-coding-defaults)

(provide 'init-lisp)
