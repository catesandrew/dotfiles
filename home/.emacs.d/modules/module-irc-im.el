;;; module-irc-im.el --- IRC and IM Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'core-vars)
;; (require 'core-funcs)
;; (require 'core-keybindings)
;; (require 'core-display-init)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(defvar dotemacs/erc-nick 'catesandrew
  "The erc nickname to use.")

(use-package erc                        ; Powerful IRC client
  :defer t
  :config
  (progn
    ;; Default server and nick
    (setq erc-server "chat.freenode.net"
          erc-log-channels-directory (concat dotemacs-cache-directory "erc/logs")
          erc-port 7000
          erc-nick 'dotemacs/erc-nick
          erc-nick-uniquifier "_"
          ;; Never open unencrypted ERC connections
          erc-server-connect-function 'erc-open-tls-stream)

    ;; Spell-check ERC buffers
    (add-to-list 'erc-modules 'spelling)
    (erc-update-modules)))

(use-package erc-gitter
  :quelpa (erc-gitter :fetcher github :repo "jleechpe/erc-gitter")
  :disabled t
  :defer t
  :ensure t
  :config
  (add-to-list 'erc-modules 'gitter))

(use-package erc-join                   ; Automatically join channels with ERC
  :defer t
  :config
  ;; Standard channels on Freenode
  (setq erc-autojoin-channels-alist '(("\\.freenode\\.net" . ("#emacs")))))

(use-package erc-track                  ; Track status of ERC in mode line
  :defer t
  :config
  ;; Switch to newest buffer by default, and don't ask before rebinding the keys
  (setq erc-track-switch-direction 'newest
        erc-track-enable-keybindings t))

(use-package jabber        ;; Jabber (XMPP) client for Emacs
  :defer t
  :ensure t
  :init (dotemacs-set-leader-keys "aj" 'jabber-connect-all)
  :config (dotemacs-set-leader-keys-for-major-mode 'jabber-roster-mode
            "a" 'jabber-send-presence
            "b" 'jabber-get-browse
            "d" 'jabber-disconnect
            "e" 'jabber-roster-edit-action-at-point
            "g" 'jabber-display-roster
            "i" 'jabber-get-disco-items
            "j" 'jabber-muc-join
            "o" 'jabber-roster-toggle-offline-display
            "q" 'bury-buffer
            "s" 'jabber-send-subscription-request
            "v" 'jabber-get-version
            "RET" 'jabber-roster-ret-action-at-point))

(provide 'module-irc-im)
;;; module-irc-im.el ends here
