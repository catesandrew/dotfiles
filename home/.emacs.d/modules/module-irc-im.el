;;; module-irc-im.el --- IRC and IM Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'use-package)
(require 'core-vars)
;; (require 'core-funcs)
(require 'core-keybindings)
(require 'core-transient-state)
(require 'core-auto-completion)
;; (require 'core-display-init)
;; (require 'module-vars)
(require 'module-common)
;; (require 'module-core)
(require 'module-utils)

;;; Code:

(dotemacs-defvar-company-backends erc-mode)

(defvar dotemacs/erc-nick 'catesandrew
  "The erc nickname to use.")

(defvar erc-enable-sasl-auth t
  "If non nil then use SASL authenthication with ERC.")

(use-package erc                        ; Powerful IRC client
  :defer t
  :init
  (progn
    (setq erc-server "chat.freenode.net"
          erc-log-channels-directory (concat dotemacs-cache-directory "erc/logs")
          erc-port 7000
          erc-nick 'dotemacs/erc-nick
          erc-nick-uniquifier "_"
          ;; Never open unencrypted ERC connections
          erc-server-connect-function 'erc-open-tls-stream)

    (dotemacs-set-leader-keys
      "aie" 'erc
      "aiE" 'erc-tls
      "aii" 'erc-track-switch-buffer)

    ;; utf-8 always and forever
    (setq erc-server-coding-system '(utf-8 . utf-8))
    ;; disable linum mode in erc check if this will not be efficient
    (defun no-linum (&rest ignore)
      (when (or 'linum-mode global-linum-mode)
        (linum-mode 0)))
    (dotemacs/add-to-hooks 'no-linum '(erc-hook
                                       erc-mode-hook
                                       erc-insert-pre-hook)))
  :config
  (progn
    (use-package erc-autoaway
      :config
      (setq erc-auto-discard-away t
            erc-autoaway-idle-seconds 600
            erc-autoaway-use-emacs-idle t))
      (erc-services-mode 1)
      (defun erc-list-command ()
        "execute the list command"
        (interactive)
        (insert "/list")
        (erc-send-current-line))
      (setq erc-kill-buffer-on-part t
            erc-kill-queries-on-quit t
            erc-kill-server-buffer-on-quit t)
      (add-hook 'erc-connect-pre-hook (lambda (x) (erc-update-modules)))
      (erc-track-mode t)
      (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE")
            erc-server-coding-system '(utf-8 . utf-8))
      (setq erc-prompt (lambda () (concat "[" (buffer-name) "]")))

      (require 'notifications)
      (defun erc-global-notify (match-type nick message)
        "Notify when a message is recieved."
        (notifications-notify
         :title nick
         :body message
         :urgency 'low))

      ;; osx doesn't have dbus support
      (when (boundp 'dbus-compiled-version)
        (add-hook 'erc-text-matched-hook 'erc-global-notify))

      ;; keybindings
      (dotemacs-set-leader-keys-for-major-mode 'erc-mode
        "d" 'erc-input-action
        "j" 'erc-join-channel
        "n" 'erc-channel-names
        "l" 'erc-list-command
        "p" 'erc-part-from-channel
        "q" 'erc-quit-server)))

(use-package erc-gitter
  :quelpa (erc-gitter :fetcher github :repo "jleechpe/erc-gitter")
  :config
  (add-to-list 'erc-modules 'gitter))

(use-package erc-hl-nicks
  :ensure t
  :defer t)

(use-package erc-sasl
  :if erc-enable-sasl-auth
  :disabled t
  ;; Following http://www.emacswiki.org/emacs/ErcSASL
  ;; Maybe an advice would be better?
  :config
  (progn
    ;; Add any server like this
    ;; (add-to-list 'erc-sasl-server-regexp-list "host\\.server\\.com")
    (add-to-list 'erc-sasl-server-regexp-list "irc\\.freenode\\.net")
    (defun erc-login ()
      "Perform user authentication at the IRC server."
      (erc-log (format "login: nick: %s, user: %s %s %s :%s"
                       (erc-current-nick)
                       (user-login-name)
                       (or erc-system-name (system-name))
                       erc-session-server
                       erc-session-user-full-name))
      (if erc-session-password
          (erc-server-send (format "PASS %s" erc-session-password))
        (message "Logging in without password"))
      (when (and (featurep 'erc-sasl) (erc-sasl-use-sasl-p))
        (erc-server-send "CAP REQ :sasl"))
      (erc-server-send (format "NICK %s" (erc-current-nick)))
      (erc-server-send
       (format "USER %s %s %s :%s"
               ;; hacked - S.B.
               (if erc-anonymous-login erc-email-userid (user-login-name))
               "0" "*"
               erc-session-user-full-name))
      (erc-update-mode-line))))

(when (dotemacs/system-is-mac)
  (use-package erc-terminal-notifier
    :defer t
    :if (executable-find "terminal-notifier")
    :ensure t))

(use-package erc-social-graph
  :ensure t
  :init
  (progn
    ;; does not exist ?
    ;; (erc-social-graph-enable)
    (setq erc-social-graph-dynamic-graph t)
    (dotemacs-set-leader-keys-for-major-mode 'erc-mode
                                              "D" 'erc-social-graph-draw)))
(use-package erc-yt
  :ensure t
  :init (with-eval-after-load 'erc (add-to-list 'erc-modules 'youtube)))

(use-package erc-view-log
  :ensure t
  :init
  (progn
    (with-eval-after-load 'erc (add-to-list 'erc-modules 'log))
    (setq erc-log-channels-directory
          (expand-file-name
           (concat dotemacs-cache-directory
                   "erc-logs")))
    (unless (file-exists-p erc-log-channels-directory)
      (make-directory erc-log-channels-directory)))

  :config
  ;; ERC Logging
  (progn
    (add-to-list 'auto-mode-alist
                 `(,(format "%s/.*\\.[log|txt]"
                            (regexp-quote
                             (expand-file-name
                              erc-log-channels-directory))) . erc-view-log-mode))
    ;; Following https://raw.githubusercontent.com/Niluge-KiWi/erc-view-log/master/erc-view-log.el
    ;; installation instructions
    (add-hook 'erc-view-log-mode-hook 'turn-on-auto-revert-tail-mode)

    (dotemacs-define-transient-state erc-log
      :title "ERC Log Transient State"
      :doc "\n[_r_] reload the log file  [_>_/_<_] go to the next/prev mention"
      :bindings
      ("r" erc-view-log-reload-file)
      (">" erc-view-log-next-mention)
      ("<" erc-view-log-previous-mention))
    (dotemacs-set-leader-keys-for-major-mode 'erc-mode
      "." 'dotemacs/erc-log-transient-state/body)))

(use-package erc-image
  :ensure t
  :init (with-eval-after-load 'erc (add-to-list 'erc-modules 'image)))

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

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (dotemacs-add-company-hook erc-mode)
    (push 'company-capf company-backends-erc-mode))

  (dotemacs-use-package-add-hook company-emoji
    :post-init
    (push 'company-emoji company-backends-erc-mode)))

(dotemacs-use-package-add-hook emoji-cheat-sheet-plus
  :post-init
  (add-hook 'erc-mode-hook 'emoji-cheat-sheet-plus-display-mode))

(dotemacs-use-package-add-hook persp-mode
  :post-init
  (dotemacs-define-custom-layout "@ERC"
    :binding "E"
    :body
    (progn
      (add-hook 'erc-mode #'(lambda ()
                              (persp-add-buffer (current-buffer))))
      (call-interactively 'erc)))
  :post-config
  ;; do not save erc buffers
  (push (lambda (b) (with-current-buffer b (eq major-mode 'erc-mode)))
        persp-filter-save-buffers-functions))

(dotemacs-use-package-add-hook smooth-scrolling
  :post-init
  (add-hook 'erc-mode-hook 'dotemacs//unset-scroll-margin))

(provide 'module-irc-im)
;;; module-irc-im.el ends here
