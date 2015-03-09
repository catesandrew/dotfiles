(require 'init-programming)
(require 'rainbow-delimiters)

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
