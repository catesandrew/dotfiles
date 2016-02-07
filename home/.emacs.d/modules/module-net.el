;;; module-net.el --- Net Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;

(require 'use-package)
;; (require 'core-vars)
;; (require 'core-funcs)
(require 'core-transient-state)
(require 'core-keybindings)
(require 'core-fonts-support)
(require 'core-toggle)
;; (require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(use-package browse-url                 ; Browse URLs
  :bind (("C-c w u" . browse-url)))

(use-package eww                        ; Emacs' built-in web browser
  :bind (("C-c w b" . eww-list-bookmarks)
         ("C-c w w" . eww)))

(use-package sendmail                   ; Send mails from Emacs
  :defer t
  :config (setq send-mail-function 'smtpmail-send-it))

(use-package message                    ; Compose mails from Emacs
  :defer t
  :config (setq message-send-mail-function 'smtpmail-send-it
                ;; Don't keep message buffers around
                message-kill-buffer-on-exit t))

(use-package link-hint
  :ensure t
  :defer t
  :init
  (dotemacs-set-leader-keys
    "xo" 'link-hint-open-link
    "xO" 'link-hint-open-multiple-links))

(provide 'module-net)
;;; module-net.el ends here
