;;; module-php.el --- PHP Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'use-package)
;; (require 'core-vars)
;; (require 'core-funcs)
;; (require 'core-keybindings)
;; (require 'core-display-init)
(require 'core-use-package-ext)
(require 'core-auto-completion)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(dotemacs-defvar-company-backends php-mode)

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (dotemacs-add-company-hook php-mode)))

(use-package drupal-mode
  :defer t)

(dotemacs-use-package-add-hook eldoc
  :post-init
  (add-hook 'php-mode-hook 'eldoc-mode)
  (dotemacs-ggtags-enable-eldoc 'php-mode))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (add-hook 'php-mode-hook 'flycheck-mode))

(dotemacs-use-package-add-hook ggtags
  :post-init
  (add-hook 'php-mode-hook 'ggtags-mode))

(dotemacs-use-package-add-hook helm-gtags
  :post-init
  (dotemacs-helm-gtags-define-keys-for-mode 'php-mode))

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

(provide 'module-php)
;;; module-php.el ends here
