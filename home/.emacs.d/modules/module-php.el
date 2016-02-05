;;; module-php.el --- PHP Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-vars)
;; (require 'core-funcs)
;; (require 'core-keybindings)
;; (require 'core-display-init)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(dotemacs-defvar-company-backends php-mode)

(use-package drupal-mode
  :defer t)

; todo
; (defun php/post-init-eldoc ()
;   (add-hook 'php-mode-hook 'eldoc-mode)
;     (dotemacs-ggtags-enable-eldoc 'php-mode))
;
; (defun php/post-init-ggtags ()
;   (add-hook 'php-mode-hook 'ggtags-mode))
;
; (defun php/post-init-helm-gtags ()
;   (dotemacs-helm-gtags-define-keys-for-mode 'php-mode))

(use-package php-auto-yasnippets
  :defer t
  :ensure t)

(use-package php-extras
  :defer t
  :quelpa (php-extras :fetcher github :repo "arnested/php-extras"))

(use-package php-mode                   ; Because sometimes you have to
  :defer t
  :mode ("\\.php\\'" . php-mode))

(use-package phpcbf
  :defer t
  :ensure t)

(use-package phpunit
  :defer t
  :ensure t)

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'php-mode))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook php-mode))))

(provide 'module-php)
;;; module-php.el ends here
