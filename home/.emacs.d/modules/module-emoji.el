;;; module-emoji.el --- Emoji Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'use-package)
(require 'evil-evilified-state)
;; (require 'core-vars)
;; (require 'core-funcs)
(require 'core-keybindings)
;; (require 'core-display-init)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(use-package emoji-cheat-sheet-plus
  :ensure t
  :commands (emoji-cheat-sheet-plus-insert
             emoji-cheat-sheet-plus-buffer
             emoji-cheat-sheet-plus-display-mode)
  :init
  (progn
    (dotemacs-set-leader-keys "aE" 'emoji-cheat-sheet-plus-buffer)
    (dotemacs-set-leader-keys "ie" 'emoji-cheat-sheet-plus-insert)
    (evilified-state-evilify emoji-cheat-sheet-plus-buffer-mode
      emoji-cheat-sheet-plus-buffer-mode-map
      "<RET>" 'emoji-cheat-sheet-plus-echo-and-copy)
    (defun dotemacs-delay-emoji-cheat-sheet-hook ()
      "Work-around for org buffers."
      ;; we need to wait for org buffer to be fully loaded before
      ;; calling the emoji mode.
      ;; If we directly call the emoji mode at hook runtime then some
      ;; text properties are not applied correctly.
      (run-at-time 0.1 nil 'emoji-cheat-sheet-plus-display-mode))))

(use-package company-emoji
  :if (eq dotemacs-completion-engine 'company)
  :defer t
  :init
  (setq company-emoji-insert-unicode nil))

(provide 'module-emoji)
;;; module-emoji.el ends here
