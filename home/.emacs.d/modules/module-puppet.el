;;; module-puppet.el --- Puppet Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-vars)
;; (require 'core-funcs)
(require 'core-keybindings)
;; (require 'core-display-init)
(require 'core-use-package-ext)
(require 'core-auto-completion)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(dotemacs-defvar-company-backends puppet-mode)

(use-package puppet-mode                ; Puppet manifests
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'puppet-mode
      "{" 'beginning-of-defun
      "}" 'end-of-defun
      "$" 'puppet-interpolate
      "a" 'puppet-align-block
      "'" 'puppet-toggle-string-quotes
      ";" 'puppet-clear-string
      "j" 'imenu
      "c" 'puppet-apply
      "v" 'puppet-validate
      "l" 'puppet-lint
      )))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook puppet-mode))))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'puppet-mode))

(provide 'module-puppet)
;;; module-puppet.el ends here
