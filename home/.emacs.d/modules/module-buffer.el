;;; module-buffer.el --- Buffer Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'use-package)
;; (require 'core-funcs)
(require 'core-keybindings)
;; (require 'core-display-init)
(require 'core-fonts-support)
(require 'core-toggle)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)
(require 'module-helm)

;;; Code:

(defvar buffer-force-save-some-buffers t
  "Force save buffers when focus is lost.")

(defvar buffer-do-not-kill-important-buffers nil
  "Do not kill important buffer.")

;; Don't kill the important buffers
(defvar dotemacs-do-not-kill-buffer-names
  '("*scratch*"
    "*Messages*"
    "*Require Times*")
  "Names of buffers that should not be killed.")

;; Auto refresh
(use-package autorevert                 ; Auto-revert buffers of changed files
  :init
  (progn
    (setq auto-revert-check-vc-info nil
          auto-revert-mode-text " ♻"
          auto-revert-tail-mode-text " ♻~")
    (defun auto-revert-turn-on-maybe ()
      (unless (file-remote-p default-directory)
        (auto-revert-mode)))
    (add-hook 'find-file-hook 'auto-revert-turn-on-maybe)))

(use-package page-break-lines           ; Turn page breaks into lines
  :ensure t
  :init
  (progn
    (global-page-break-lines-mode)
    (dotemacs-hide-lighter page-break-lines-mode)))

(use-package buffer-move
  :defer t
  :init
  (dotemacs-set-leader-keys
    "bmh" 'buf-move-left
    "bmj" 'buf-move-down
    "bmk" 'buf-move-up
    "bml" 'buf-move-right))

;; Make buffer names unique
(use-package uniquify
  ;; When having windows with repeated filenames, uniquify them
  ;; by the folder they are in rather those annoying <2>,<3>,.. etc
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets
                uniquify-separator "/"
                uniquify-ignore-buffers-re "^\\*" ; leave special buffers alone
                uniquify-after-kill-buffer-p t))

(use-package helm-buffers
  :ensure helm
  :defer t
  :config (setq helm-buffers-fuzzy-matching t))

(when buffer-do-not-kill-important-buffers
  (defun dotemacs-do-not-kill-important-buffers ()
    "Inhibit killing of important buffers.
Add this to `kill-buffer-query-functions'."
    (if (not (member (buffer-name) dotemacs-do-not-kill-buffer-names))
        t
      (message "Not allowed to kill %s, burying instead" (buffer-name))
      (bury-buffer)
      nil))
  (add-hook 'kill-buffer-query-functions 'dotemacs-do-not-kill-important-buffers))

;; Autosave buffers when focus is lost, see
(when buffer-force-save-some-buffers
  (defun dotemacs-force-save-some-buffers ()
    "Save all modified buffers, without prompts."
    (save-some-buffers 'dont-ask))
  (add-hook 'focus-out-hook 'dotemacs-force-save-some-buffers))

(use-package centered-cursor-mode
  :ensure t
  :commands (centered-cursor-mode
             global-centered-cursor-mode)
  :init
  (progn
    (dotemacs-add-toggle centered-point
      :status centered-cursor-mode
      :on (centered-cursor-mode)
      :off (centered-cursor-mode -1)
      :documentation
      "Keep point at the center of the window."
      :evil-leader "t-")
    (dotemacs-add-toggle centered-point-globally
      :status centered-cursor-mode
      :on (global-centered-cursor-mode)
      :off (global-centered-cursor-mode -1)
      :documentation
      "Keep point at the center of the window globally."
      :evil-leader "t C--"))
  :config
  (progn
    (setq ccm-recenter-at-end-of-file t
          ccm-ignored-commands '(mouse-drag-region
                                 mouse-set-point
                                 widget-button-click
                                 scroll-bar-toolkit-scroll
                                 evil-mouse-drag-region))
    (dotemacs-diminish centered-cursor-mode " ⊝" " -")))

(provide 'module-buffer)
;;; module-buffer.el ends here
