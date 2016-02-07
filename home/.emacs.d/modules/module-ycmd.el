;;; module-ycmd.el --- You Complete Me Module
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
(require 'core-fonts-support)
(require 'core-keybindings)
(require 'core-auto-completion)
(require 'core-use-package-ext)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

(unless (boundp 'ycmd-server-command)
  (message (concat "YCMD won't work unless you set the ycmd-server-command "
                   "variable to the path to a ycmd install.")))

(when (eq dotemacs-completion-engine 'company)
  (use-package company-ycmd
    :ensure t
    :defer t
    :commands company-ycmd)

  (use-package flycheck-ycmd
    :ensure t
    :defer t
    :init (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup))

  (dotemacs-use-package-add-hook company
    :post-init
    (add-hook 'yaml-mode-hook 'company-mode)))

(use-package ycmd
  :ensure t
  :defer t
  :init
  (unless (boundp 'ycmd-global-config)
    (let ((dir (concat user-emacs-directory "etc/")))
      (setq-default ycmd-global-config (concat dir "global_conf.py")))))

(provide 'module-ycmd)
;;; module-ycmd.el ends here
