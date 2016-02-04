;;; module-systemd.el --- systemd Module
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

(use-package systemd
  :defer t
  :ensure t
  :init
  (setq systemd-use-company-p t)
  :config
  (dotemacs-set-leader-keys-for-major-mode 'systemd-mode
    "hd" 'systemd-doc-directives
    "ho" 'systemd-doc-open))

(provide 'module-systemd)
;;; module-systemd.el ends here
