;;; module-fringe.el --- Fringe Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-funcs)
;; (require 'core-keybindings)
(require 'core-display-init)
(require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)
;; (require 'linum)
;; (require 'linum-relative)

;;; Code:

(dotemacs|do-after-display-system-init
 (when (display-graphic-p)
   (custom-set-variables
    '(fringe-mode (quote (4 . 4)) nil (fringe)))
   (setq-default fringe-indicator-alist
                 '((truncation . nil) (continuation . nil)))))

(when dotemacs-line-numbers
  (add-hook 'text-mode-hook 'linum-mode))

(setq linum-format "%4d") ;; line number

(custom-set-faces
 '(linum ((t (:height 0.9 :family "Bebas Neue")))))

(use-package linum-relative
  :ensure t
  :commands (linum-relative-toggle linum-relative-on)
  :init
  (progn
    (when (eq dotemacs-line-numbers 'relative)
      (linum-relative-on))
    (dotemacs-set-leader-keys "tr" 'linum-relative-toggle))
  :config
  (progn
    (setq linum-relative-current-symbol "→")))

(provide 'module-fringe)
;;; module-fringe.el ends here
