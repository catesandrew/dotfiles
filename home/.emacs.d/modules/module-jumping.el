;;; module-jumping.el --- Jumping Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-vars)
;; (require 'core-funcs)
(require 'core-buffers)
(require 'core-keybindings)
;; (require 'core-display-init)
(require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(use-package simple
  :init
  (progn
    ;; Handy way of getting back to previous places.
    (dotemacs-set-leader-keys
      ;; "mn" 'dotemacs-delete-file-and-buffer
      ;; "m}" 'dotemacs-delete-file-and-buffer
      "m{" 'pop-to-mark-command
      "mp" 'pop-to-mark-command))
  :config
  (progn
    ;; ensures we can quickly pop the mark several times by typing
    ;; `C-u C-SPC C-SPC`, instead of having to type `C-u C-SPC C-u C-SPC`.
    (setq set-mark-command-repeat-pop t)

    ;; When popping the mark, continue popping until the cursor actually moves
    ;; Also, if the last command was a copy - skip past all the expand-region
    ;; cruft. Testing the new `advicd-add` interface.
    (defun dotemacs/multi-pop-to-mark (orig-fun &rest args)
      "Call ORIG-FUN until the cursor moves.
Try the repeated popping up to 10 times."
      (let ((p (point)))
        (when (eq last-command 'save-region-or-current-line)
          (apply orig-fun args)
          (apply orig-fun args)
          (apply orig-fun args))
        (dotimes (i 10)
          (when (= p (point)) (apply orig-fun args)))))
    (advice-add 'pop-to-mark-command :around 'dotemacs/multi-pop-to-mark)

    ;; When popping the mark, continue popping until the cursor actually moves
    ;; Also, if the last command was a copy - skip past all the expand-region cruft.
    ;; (defadvice pop-to-mark-command (around ensure-new-position activate)
    ;;   (let ((p (point)))
    ;;     (when (eq last-command 'save-region-or-current-line)
    ;;       ad-do-it
    ;;       ad-do-it
    ;;       ad-do-it)
    ;;     (dotimes (i 10)
    ;;       (when (= p (point)) ad-do-it))))
    ))

(use-package avy                   ; Jump to characters in buffers
  :ensure t
  :commands (dotemacs-avy-open-url)
  :init
  (progn
    (setq avy-all-windows 'all-frames)
    (setq avy-background t)
    (dotemacs-set-leader-keys
      "SPC" 'avy-goto-word-or-subword-1 ; 'avy-goto-word-1
      "y" 'avy-goto-line ; 'avy-goto-char-2
      "xo" 'dotemacs-avy-open-url))
  :config
  (progn
    (defun dotemacs-avy-goto-url()
      "Use avy to go to an URL in the buffer."
      (interactive)
      (avy--generic-jump "https?://" nil 'pre))
    (defun dotemacs-avy-open-url ()
      "Use avy to select an URL in the buffer and open it."
      (interactive)
      (save-excursion
        (dotemacs-avy-goto-url)
        (browse-url-at-point)))
    (dotemacs-set-leader-keys "`" 'avy-pop-mark)))

(use-package ace-jump-helm-line
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-q") 'ace-jump-helm-line)))

(use-package ace-link                   ; Fast link jumping
  :commands dotemacs-ace-buffer-links
  :init
  (progn
    (define-key dotemacs-buffer-mode-map "o" 'dotemacs-ace-buffer-links)
    (with-eval-after-load 'info
      (define-key Info-mode-map "o" 'ace-link-info))
    (with-eval-after-load 'help-mode
      (defvar help-mode-map)  ; Silence the byte compiler
      (define-key help-mode-map "o" 'ace-link-help))
    (with-eval-after-load 'eww
      (define-key eww-link-keymap "o" 'ace-link-eww)
      (define-key eww-mode-map "o" 'ace-link-eww)))
  :config
  (progn
    (defvar dotemacs--link-pattern "~?/.+\\|\s\\[")
    (defun dotemacs-collect-buffer-links ()
      (let ((end (window-end))
            points)
        (save-excursion
          (goto-char (window-start))
          (while (re-search-forward dotemacs--link-pattern end t)
            (push (+ (match-beginning 0) 1) points))
          (nreverse points))))
    (defun dotemacs-ace-buffer-links ()
      "Ace jump to links in `emacs' buffer."
      (interactive)
      (let ((res (avy--with-avy-keys dotemacs-ace-buffer-links
                    (avy--process
                        (dotemacs-collect-buffer-links)
                        #'avy--overlay-pre))))
            (when res
              (goto-char (1+ res))
              (widget-button-press (point)))))))

(use-package ace-window                 ; Fast window switching
  :ensure t
  :defer t
  :init
  (progn
    (dotemacs-set-leader-keys
      "bM"  'ace-swap-window
      "wC"  'ace-delete-window
      "w <SPC>"  'ace-window)
    ;; set ace-window keys to home-row
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

(provide 'module-jumping)
;;; module-jumping.el ends here
