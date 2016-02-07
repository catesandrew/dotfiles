;;; module-virtual-machine.el --- Virtual Machine Module
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
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

;; Docker
(use-package dockerfile-mode
  :defer t
  :ensure t
  :config
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'dockerfile-mode
      "cb" 'dockerfile-build-buffer)))

;; Terraform
(use-package terraform-mode
  :defer t
  :ensure t)

(provide 'module-virtual-machine)
;;; module-virtual-machine.el ends here
