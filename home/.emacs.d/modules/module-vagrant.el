;;; module-agrant.el --- Vagrant Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'use-package)
(require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
(require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(use-package vagrant
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-declare-prefix "V" "vagrant")
    (dotemacs-set-leader-keys
      "VD" 'vagrant-destroy
      "Ve" 'vagrant-edit
      "VH" 'vagrant-halt
      "Vp" 'vagrant-provision
      "Vr" 'vagrant-resume
      "Vs" 'vagrant-status
      "VS" 'vagrant-suspend
      "VV" 'vagrant-up)))

(use-package vagrant-tramp
  :defer t
  :ensure t
  :init
  (progn
    (defvar dotemacs--vagrant-tramp-loaded nil)
    (defadvice vagrant-tramp-term (before dotemacs-load-vagrant activate)
      "Lazy load vagrant-tramp."
      (unless dotemacs--vagrant-tramp-loaded
        (vagrant-tramp-add-method)
        (setq dotemacs--vagrant-tramp-loaded t)))
    (dotemacs-set-leader-keys "Vt" 'vagrant-tramp-term)))

(provide 'module-vagrant)
;;; module-vagrant.el ends here
