;;; module-emacs.el --- Emacs Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'core-vars)
;; (require 'core-funcs)
(require 'core-keybindings)
(require 'evil-evilified-state)
(require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)
(require 'use-package)

;;; Code:

(use-package hardhat ; Protect user-writable files
  :ensure t
  :init (global-hardhat-mode)
  (setq hardhat-buffer-protected-functions '(hardhat-protected-by-ignoramus))
  :config (setq hardhat-mode-lighter "🔒"))

(use-package tramp                      ; Access remote files
  :defer t
  :init
  (progn
    (setq tramp-ssh-controlmaster-options
          (concat
           "-o ControlPath=~/.ssh/conn-%%r@%%h:%%p"))
    (setq tramp-default-method "ssh"
          vc-ignore-dir-regexp
          (format "\\(%s\\)\\|\\(%s\\)"
                  vc-ignore-dir-regexp
                  tramp-file-name-regexp)))
  :config
  ;; Store auto-save files locally
  (setq tramp-auto-save-directory (concat dotemacs-cache-directory "tramp-auto-save")))

(use-package bookmark                   ; Bookmarks for Emacs buffers
  :ensure t
  :defer t
  :init
  (progn
    (setq bookmark-save-flag 1 ;; autosave each change
          ;; Store auto-save files locally
          bookmark-default-file (concat dotemacs-cache-directory "bookmarks"))))

(use-package restart-emacs
  :defer t
  :init
  (dotemacs-set-leader-keys "qr" 'dotemacs/restart-emacs)
  (defun dotemacs/restart-emacs ()
    (interactive)
    (setq dotemacs-really-kill-emacs t)
    (restart-emacs)))

(use-package savehist                   ; Save minibuffer history
  :init
  (progn
    ;; Minibuffer history
    (setq savehist-file (concat dotemacs-cache-directory "savehist")
          enable-recursive-minibuffers t ; Allow commands in minibuffers
          history-length 2000
          savehist-additional-variables '(mark-ring
                                          global-mark-ring
                                          search-ring
                                          regexp-search-ring
                                          extended-command-history)
          savehist-autosave-interval 180)
    (savehist-mode t)))

;; move cursor to the last position upon open
(use-package saveplace                  ; Save point position in files
  :init
  (progn
    (if (fboundp 'save-place-mode)
        ;; Emacs 25 has a proper mode for `save-place'
        (save-place-mode)
      (setq save-place t))
    ;; Save point position between sessions
    (setq save-place-file (concat dotemacs-cache-directory "places"))))

;; tools for customizing Emacs and Lisp packages
(use-package cus-edit
  :defer t
  :init (load dotemacs-custom-file 'no-error 'no-message)
  :config
  (setq custom-buffer-done-kill nil            ; Kill when existing
        custom-buffer-verbose-help nil         ; Remove redundant help text
        ;; Show me the real variable name
        custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil))

(use-package paradox                    ; Better package menu
  :ensure t
  :commands paradox-list-packages
  :init
  (progn
    (setq paradox-execute-asynchronously nil)
    (defun dotemacs-paradox-list-packages ()
      "Load depdendencies for auth and open the package list."
      (interactive)
      (require 'epa-file)
      (require 'auth-source)
      (when (and (not (boundp 'paradox-github-token))
                 (file-exists-p "~/.authinfo.gpg"))
        (let ((authinfo-result (car (auth-source-search
                                     :max 1
                                     :host "github.com"
                                     :port "paradox"
                                     :user "paradox"
                                     :require '(:secret)))))
          (let ((paradox-token (plist-get authinfo-result :secret)))
            (setq paradox-github-token (if (functionp paradox-token)
                                           (funcall paradox-token)
                                         paradox-token)))))
      (paradox-list-packages nil))

    (evilified-state-evilify paradox-menu-mode paradox-menu-mode-map
      "H" 'paradox-menu-quick-help
      "J" 'paradox-next-describe
      "K" 'paradox-previous-describe
      "L" 'paradox-menu-view-commit-list
      "o" 'paradox-menu-visit-homepage)
    (dotemacs-set-leader-keys
      "ak" 'dotemacs-paradox-list-packages)))

(use-package server                     ; The server of `emacsclient'
  :defer t
  :disabled t
  :init (server-mode)
  :diminish server-buffer-clients)

;;; View PDF/PostScript/DVI files in Emacs
(use-package doc-view
  :defer t
  :init
  (evilified-state-evilify doc-view-mode doc-view-mode-map
    "/"  'dotemacs-doc-view-search-new-query
    "?"  'dotemacs-doc-view-search-new-query-backward
    "gg" 'doc-view-first-page
    "G"  'doc-view-last-page
    "gt" 'doc-view-goto-page
    "h"  'doc-view-previous-page
    "j"  'doc-view-next-line-or-next-page
    "k"  'doc-view-previous-line-or-previous-page
    "K"  'doc-view-kill-proc-and-buffer
    "l"  'doc-view-next-page
    "n"  'doc-view-search
    "N"  'doc-view-search-backward
    (kbd "C-d") 'doc-view-scroll-up-or-next-page
    (kbd "C-k") 'doc-view-kill-proc
    (kbd "C-u") 'doc-view-scroll-down-or-previous-page)
  :config
  (progn
    (defun dotemacs-doc-view-search-new-query ()
      "Initiate a new query."
      (interactive)
      (doc-view-search 'newquery))

    (defun dotemacs-doc-view-search-new-query-backward ()
      "Initiate a new query."
      (interactive)
      (doc-view-search 'newquery t))

    ;; fixed a weird issue where toggling display does not
    ;; swtich to text mode
    (defadvice doc-view-toggle-display
        (around dotemacs-doc-view-toggle-display activate)
      (if (eq major-mode 'doc-view-mode)
          (progn
            ad-do-it
            (text-mode)
            (doc-view-minor-mode))
        ad-do-it))))

(use-package request
  :ensure t
  :init
  (setq request-storage-directory (concat dotemacs-cache-directory
                                          "request/")))

(provide 'module-emacs)
;;; module-emacs.el ends here
