;;; module-swift.el --- Swift Module
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
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

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
