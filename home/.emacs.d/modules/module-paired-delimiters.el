;;; module-paired-delimiters.el --- Paired Delimiters Module
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

(use-package elec-pair                  ; Electric pairs
  :disabled t
  :init (electric-pair-mode))

(use-package paren                      ; Highlight paired delimiters
  :ensure t
  :init
  (progn
    (setq show-paren-delay 0)
    (dotemacs-add-toggle show-paren-mode
      :status show-paren-mode
      :on (show-paren-mode)
      :off (show-paren-mode -1)
      :documentation "Highlight matching pairs of parentheses."
      :evil-leader "tCP")
    (if (eq dotemacs-highlight-delimiters 'all)
        (show-paren-mode)))
  :config (setq show-paren-when-point-inside-paren t
                show-paren-when-point-in-periphery t))

(provide 'module-paired-delimiters)
;;; module-paired-delimiters.el ends here
