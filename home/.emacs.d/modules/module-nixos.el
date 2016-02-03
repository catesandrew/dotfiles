;;; module-nixos.el --- NixOS Module
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

(dotemacs-defvar-company-backends nix-mode)

(use-package nix-mode   ; This layer adds tools for
  :ensure t             ; better integration of emacs in NixOS.
  :defer t)

(use-package nixos-options
  :ensure t
  :defer t)

(use-package helm-nixos-options
  :ensure t
  :defer t
  :config
  (dotemacs-set-leader-keys
    "h>" 'helm-nixos-options))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :pre-init
    (progn
      (push 'company-capf company-backends-nix-mode)
      (dotemacs-add-company-hook nix-mode))))

(use-package company-nixos-options
  :if (eq dotemacs-completion-engine 'company)
  :defer t
  :init
  (progn
    (push 'company-nixos-options company-backends-nix-mode)))

(provide 'module-nixos)
;;; module-nixos.el ends here
