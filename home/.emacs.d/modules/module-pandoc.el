;;; module-pandoc.el --- Pandoc Module
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

(use-package pandoc-mode ; http://joostkremers.github.io/pandoc-mode/
  :defer t
  :commands dotemacs-run-pandoc
  :config
  (progn
    (defun dotemacs-run-pandoc ()
      "Start pandoc for the buffer and open the menu"
      (interactive)
      (pandoc-mode)
      (pandoc-main-hydra/body))
    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))
  :init
  (progn
    (dotemacs-set-leader-keys "P/" 'dotemacs-run-pandoc)))

                                        ; As there’s not, yet, an EPUB reader for Emacs, you can still set up Emacs to
                                        ; be able to open .epub files to see what’s inside them, since they are, after
                                        ; all, just ZIP files
(setq auto-mode-alist
      (append (list '("\\.epub$" . archive-mode)) auto-mode-alist))

(setq auto-coding-alist
      (append (list '("\\.epub$" . no-conversion)) auto-coding-alist))

(provide 'module-pandoc)
;;; module-pandoc.el ends here
