;;; module-pandoc.el --- Pandoc Module
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
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(use-package pandoc-mode ; http://joostkremers.github.io/pandoc-mode/
  :defer t
  :ensure t
  :commands dotemacs/run-pandoc
  :config
  (progn
    (defun dotemacs/run-pandoc ()
      "Start pandoc for the buffer and open the menu"
      (interactive)
      (pandoc-mode)
      (pandoc-main-hydra/body))
    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))
  :init
  (progn
    (dotemacs-set-leader-keys "P/" 'dotemacs/run-pandoc)))

(use-package ox-pandoc
  :defer t
  :ensure t
  :init
  (with-eval-after-load 'org (require 'ox-pandoc)))

(provide 'module-pandoc)
;;; module-pandoc.el ends here
