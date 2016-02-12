;;; module-ido.el --- IDO Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'core-vars)
(require 'evil-evilified-state)
;; (require 'core-funcs)
;; (require 'core-keybindings)
(require 'core-transient-state)
;; (require 'core-display-init)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)
(require 'use-package)

;;; Code:

(use-package ido
  :init
  (progn
    (ido-mode t)
    (setq ido-save-directory-list-file (concat dotemacs-cache-directory "ido.last")
          ;; ido-use-faces nil       ;; disable ido faces to see flx highlights.
          ;; ido-enable-prefix nil
          ;; ido-create-new-buffer 'always
          ;; ido-use-filename-at-point 'guess
          ;; ido-default-file-method 'selected-window
          ;; enable fuzzy matching
          ido-enable-flex-matching t)))

(use-package ido-vertical-mode
  :ensure t
  :init
  (progn
    (ido-vertical-mode t)
    ;; (when dotemacs-use-ido
    ;;   (dotemacs-set-leader-keys "ff" 'ido-find-file))
    (defun dotemacs//ido-minibuffer-setup ()
      "Setup the minibuffer."
      ;; Since ido is implemented in a while loop where each
      ;; iteration setup a whole new minibuffer, we have to keep
      ;; track of any activated ido navigation micro-state and force
      ;; the reactivation at each iteration.
      (when dotemacs--ido-navigation-ms-enabled
        (dotemacs/ido-navigation-micro-state)))
    (add-hook 'ido-minibuffer-setup-hook 'dotemacs//ido-minibuffer-setup)

    (defun dotemacs//ido-setup ()
      (when dotemacs--ido-navigation-ms-face-cookie-minibuffer
        (face-remap-remove-relative
         dotemacs--ido-navigation-ms-face-cookie-minibuffer))
      ;; be sure to wipe any previous micro-state flag
      (setq dotemacs--ido-navigation-ms-enabled nil)
      ;; overwrite the key bindings for ido vertical mode only
      (define-key ido-completion-map (kbd "C-<return>") 'ido-select-text)
      ;; use M-RET in terminal
      (define-key ido-completion-map "\M-\r" 'ido-select-text)
      (define-key ido-completion-map (kbd "C-h") 'ido-delete-backward-updir)
      (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
      (define-key ido-completion-map (kbd "C-k") 'ido-prev-match)
      (define-key ido-completion-map (kbd "C-l") 'ido-exit-minibuffer)
      (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
      (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
      (define-key ido-completion-map (kbd "C-S-h") 'ido-prev-match-dir)
      (define-key ido-completion-map (kbd "C-S-j") 'next-history-element)
      (define-key ido-completion-map (kbd "C-S-k") 'previous-history-element)
      (define-key ido-completion-map (kbd "C-S-l") 'ido-next-match-dir)
      (define-key ido-completion-map (kbd "C-S-n") 'next-history-element)
      (define-key ido-completion-map (kbd "C-S-p") 'previous-history-element)
      ;; ido-other window maps
      (define-key ido-completion-map (kbd "C-o") 'dotemacs/ido-invoke-in-other-window)
      (define-key ido-completion-map (kbd "C-s") 'dotemacs/ido-invoke-in-vertical-split)
      (define-key ido-completion-map (kbd "C-t") 'dotemacs/ido-invoke-in-new-frame)
      (define-key ido-completion-map (kbd "C-v") 'dotemacs/ido-invoke-in-horizontal-split)
      ;; more natural navigation keys: up, down to change current item
      ;; left to go up dir
      ;; right to open the selected item
      (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
      (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
      (define-key ido-completion-map (kbd "<left>") 'ido-delete-backward-updir)
      (define-key ido-completion-map (kbd "<right>") 'ido-exit-minibuffer)
      ;; initiate micro-state
      (define-key ido-completion-map (kbd "M-SPC") 'dotemacs/ido-navigation-micro-state)
      (define-key ido-completion-map (kbd "s-M-SPC") 'dotemacs/ido-navigation-micro-state)
      )
    (add-hook 'ido-setup-hook 'dotemacs//ido-setup)

    (defun dotemacs/ido-invoke-in-other-window ()
      "signals ido mode to switch to (or create) another window after exiting"
      (interactive)
      (setq ido-exit-minibuffer-target-window 'other)
      (ido-exit-minibuffer))

    (defun dotemacs/ido-invoke-in-horizontal-split ()
      "signals ido mode to split horizontally and switch after exiting"
      (interactive)
      (setq ido-exit-minibuffer-target-window 'horizontal)
      (ido-exit-minibuffer))

    (defun dotemacs/ido-invoke-in-vertical-split ()
      "signals ido mode to split vertically and switch after exiting"
      (interactive)
      (setq ido-exit-minibuffer-target-window 'vertical)
      (ido-exit-minibuffer))

    (defun dotemacs/ido-invoke-in-new-frame ()
      "signals ido mode to create a new frame after exiting"
      (interactive)
      (setq ido-exit-minibuffer-target-window 'frame)
      (ido-exit-minibuffer))

    ;; from https://gist.github.com/timcharper/493269
    (defun dotemacs/split-window-vertically-and-switch ()
      (interactive)
      (split-window-vertically)
      (other-window 1))

    (defun dotemacs/split-window-horizontally-and-switch ()
      (interactive)
      (split-window-horizontally)
      (other-window 1))

    (defadvice ido-read-internal
        (around ido-read-internal-with-minibuffer-other-window activate)
      (let* (ido-exit-minibuffer-target-window
             (this-buffer (current-buffer))
             (result ad-do-it))
        (cond
         ((equal ido-exit-minibuffer-target-window 'other)
          (if (= 1 (count-windows))
              (dotemacs/split-window-horizontally-and-switch)
            (other-window 1)))
         ((equal ido-exit-minibuffer-target-window 'horizontal)
          (dotemacs/split-window-horizontally-and-switch))

         ((equal ido-exit-minibuffer-target-window 'vertical)
          (dotemacs/split-window-vertically-and-switch))
         ((equal ido-exit-minibuffer-target-window 'frame)
          (make-frame)))
        ;; why? Some ido commands, such as textmate.el's
        ;; textmate-goto-symbol don't switch the current buffer
        (switch-to-buffer this-buffer)
        result))

    (defvar dotemacs--ido-navigation-ms-enabled nil
      "Flag which is non nil when ido navigation micro-state is enabled.")

    (defvar dotemacs--ido-navigation-ms-face-cookie-minibuffer nil
      "Cookie pointing to the local face remapping.")

    (defface dotemacs-ido-navigation-ms-face
      `((t :background ,(face-attribute 'error :foreground)
           :foreground "black"
           :weight bold))
      "Face for ido minibuffer prompt when ido micro-state is activated."
      :group 'dotemacs)

    (defun dotemacs//ido-navigation-ms-set-face ()
      "Set faces for ido navigation micro-state."
      (setq dotemacs--ido-navigation-ms-face-cookie-minibuffer
            (face-remap-add-relative
             'minibuffer-prompt
             'dotemacs-ido-navigation-ms-face)))

    (defun dotemacs//ido-navigation-ms-on-enter ()
      "Initialization of ido micro-state."
      (setq dotemacs--ido-navigation-ms-enabled t)
      (dotemacs//ido-navigation-ms-set-face))

    (defun dotemacs//ido-navigation-ms-on-exit ()
      "Action to perform when exiting ido micro-state."
      (face-remap-remove-relative
       dotemacs--ido-navigation-ms-face-cookie-minibuffer))

    (defun dotemacs//ido-navigation-ms-full-doc ()
      "Full documentation for ido navigation micro-state."
      "
[?]          display this help
[e]          enter dired
[j] [k]      next/previous match
[J] [K]      sub/parent directory
[h]          delete backward or parent directory
[l]          select match
[n] [p]      next/previous directory in history
[o]          open in other window
[s]          open in a new horizontal split
[t]          open in other frame
[v]          open in a new vertical split
[q]          quit")

    (dotemacs-define-transient-state ido-navigation
      :title "ido Transient State"
      :foreign-keys run
      :on-enter (dotemacs//ido-navigation-ms-on-enter)
      :on-exit  (dotemacs//ido-navigation-ms-on-exit)
      :bindings
      ;;("?" nil (dotemacs//ido-navigation-ms-full-doc))
      ("<RET>" ido-exit-minibuffer :exit t)
      ("<escape>" nil :exit t)
      ("e" ido-select-text :exit t)
      ("h" ido-delete-backward-updir)
      ("j" ido-next-match)
      ("J" ido-next-match-dir)
      ("k" ido-prev-match)
      ("K" ido-prev-match-dir)
      ("l" ido-exit-minibuffer :exit t)
      ("n" ido-next-match-dir)
      ("o" dotemacs/ido-invoke-in-other-window :exit t)
      ("p" ido-prev-match-dir)
      ("q" nil :exit t)
      ("s" dotemacs/ido-invoke-in-vertical-split :exit t)
      ("t" dotemacs/ido-invoke-in-new-frame :exit t)
      ("v" dotemacs/ido-invoke-in-horizontal-split :exit t))))

(use-package flx-ido
  :ensure t
  :init (flx-ido-mode 1))

(provide 'module-ido)
;;; module-ido.el ends here
