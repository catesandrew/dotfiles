;;; module-unimpaired.el --- Unimpaired Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; From tpope's unimpaired
;;
;; This layer ports some of the functionality of tpope's vim-unimpaired
;; [unimpaired][https://github.com/tpope/vim-unimpaired]
;;
;; This plugin provides several pairs of bracket maps using ~[~ to denote
;; previous, and ~]~ as next.
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

(defun unimpaired/paste-above ()
  (interactive)
  (evil-insert-newline-above)
  (evil-paste-after 1))

(defun unimpaired/paste-below ()
  (interactive)
  (evil-insert-newline-below)
  (evil-paste-after 1))

(define-key evil-normal-state-map (kbd "[ SPC") (lambda() (interactive)(evil-insert-newline-above) (forward-line)))
(define-key evil-normal-state-map (kbd "] SPC") (lambda() (interactive)(evil-insert-newline-below) (forward-line -1)))

(define-key evil-normal-state-map (kbd "[ e") 'move-text-up)
(define-key evil-normal-state-map (kbd "] e") 'move-text-down)

(define-key evil-visual-state-map (kbd "[ e") ":move'<--1")
(define-key evil-visual-state-map (kbd "] e") ":move'>+1")

;; (define-key evil-visual-state-map (kbd "[ e") 'move-text-up)
;; (define-key evil-visual-state-map (kbd "] e") 'move-text-down)

(define-key evil-normal-state-map (kbd "[ b") 'dotemacs-previous-useful-buffer)
(define-key evil-normal-state-map (kbd "] b") 'dotemacs-next-useful-buffer)

(define-key evil-normal-state-map (kbd "] l") 'dotemacs-next-error)
(define-key evil-normal-state-map (kbd "[ l") 'dotemacs-previous-error)

(define-key evil-normal-state-map (kbd "[ h") 'diff-hl-previous-hunk)
(define-key evil-normal-state-map (kbd "] h") 'diff-hl-next-hunk)

(define-key evil-normal-state-map (kbd "[ t") (lambda () (interactive)(raise-frame (previous-frame))))
(define-key evil-normal-state-map (kbd "] t") (lambda () (interactive)(raise-frame (next-frame))))

(define-key evil-normal-state-map (kbd "[ w") 'previous-multiframe-window)
(define-key evil-normal-state-map (kbd "] w") 'next-multiframe-window)

;; select pasted text
(define-key evil-normal-state-map (kbd "g p") (kbd "` [ v ` ]"))

;; paste above or below with newline
(define-key evil-normal-state-map (kbd "[ p") 'unimpaired/paste-above)
(define-key evil-normal-state-map (kbd "] p") 'unimpaired/paste-below)

(provide 'module-unimpaired)
;;; module-unimpaired.el ends here
