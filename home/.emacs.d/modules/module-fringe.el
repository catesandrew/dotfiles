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
