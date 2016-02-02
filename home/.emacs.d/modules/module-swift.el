;;; Swift
(require 'module-global)

(use-package swift-mode                 ; Swift sources
  :ensure t
  :defer t
  :config (with-eval-after-load 'flycheck
            (add-to-list 'flycheck-checkers 'swift)))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dolist (mode '(swift-mode sh-mode))
    (dotemacs/add-flycheck-hook mode)))

(provide 'module-swift)
;;; module-swift.el ends here
