;;; module-semantic.el --- Semantic Module
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

;;; Code:


;; funcs
(defun semantic/enable-semantic-mode (mode)
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook (lambda ()
                     (require 'semantic)
                     (add-to-list 'semantic-default-submodes
                                  'global-semantic-stickyfunc-mode)
                     (add-to-list 'semantic-default-submodes
                                  'global-semantic-idle-summary-mode)
                     (semantic-mode 1)))))


;; packages
(use-package semantic
  :ensure t
  :defer t
  :init
  (progn
    (setq srecode-map-save-file (concat dotemacs-cache-directory
                                        "srecode-map.el"))
    (setq semanticdb-default-save-directory (concat dotemacs-cache-directory
                                                    "semanticdb/"))
    (unless (file-exists-p semanticdb-default-save-directory)
      (make-directory semanticdb-default-save-directory))))

(use-package srefactor
  :ensure t
  :defer t
  :init
  (progn
    (defun dotemacs/lazy-load-srefactor ()
      "Lazy load the package."
      (require 'srefactor)
      ;; currently, evil-mode overrides key mapping of srefactor menu must
      ;; expplicity enable evil-emacs-state. This is ok since srefactor supports
      ;; j,k,/ and ? commands when Evil is available
      (add-hook 'srefactor-ui-menu-mode-hook 'evil-emacs-state))))

(use-package stickyfunc-enhance
  :ensure t
  :defer t
  :init
  (defun dotemacs/lazy-load-stickyfunc-enhance ()
    "Lazy load the package."
    (require 'stickyfunc-enhance)))

(provide 'module-semantic)
;;; module-semantic.el ends here
