;;; Virtual Machine
(require 'module-global)

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
