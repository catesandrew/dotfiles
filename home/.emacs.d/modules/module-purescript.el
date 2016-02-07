;;; module-purescript.el --- PureScript Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'core-auto-completion)
;; (require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
;; (require 'core-display-init)
(require 'core-use-package-ext)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(dotemacs-defvar-company-backends purescript-mode)

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (dotemacs-add-company-hook purescript-mode)))

(use-package purescript-mode
  :defer t
  :ensure t
  :init
  (progn
    (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)
    (dotemacs-set-leader-keys-for-major-mode 'purescript-mode
      "i="  'purescript-mode-format-imports
      "i`"  'purescript-navigate-imports-return
      "ia"  'purescript-align-imports
      "in"  'purescript-navigate-imports)))

(use-package psci
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-register-repl 'psci 'psci "purescript")
    (add-hook 'purescript-mode-hook 'inferior-psci-mode)
    (dotemacs-set-leader-keys-for-major-mode 'purescript-mode
      "'"  'psci
      "sb" 'psci/load-current-file!
      "si" 'psci
      "sm" 'psci/load-module!
      "sp" 'psci/load-project-modules!)))

(use-package psc-ide
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'purescript-mode-hook 'psc-ide-mode)
    (push 'company-psc-ide-backend company-backends-purescript-mode)
    (dotemacs-set-leader-keys-for-major-mode 'purescript-mode
      "ms" 'psc-ide-server-start
      "ml" 'psc-ide-load-module
      "ht" 'psc-ide-show-type)))

(provide 'module-purescript)
;;; module-purescript.el ends here
