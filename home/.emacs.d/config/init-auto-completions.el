;;; funcs.el --- Auto-completion functions File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; auto-completion key bindings functions

(defun dotemacs-auto-completion-set-RET-key-behavior (package)
  "Bind RET key appropriately for the given PACKAGE and value of
`auto-completion-return-key-behavior'."
  (cond
   ((eq 'company package)
    (let ((map company-active-map))
      (cond
       ((eq 'complete auto-completion-return-key-behavior)
        (define-key map [return] 'company-complete-selection)
        (define-key map (kbd "RET") 'company-complete-selection))
       (t
        (define-key map [return] 'nil)
        (define-key map (kbd "RET") 'nil)))))
   (t (message "Not yet implemented for package %S" package))))

(defun dotemacs-auto-completion-set-TAB-key-behavior (package)
  "Bind TAB key appropriately for the given PACKAGE and value of
`auto-completion-tab-key-behavior'."
  (cond
   ((eq 'company package)
    (let ((map company-active-map))
      (cond
       ((eq 'complete auto-completion-tab-key-behavior)
        (define-key map (kbd "TAB") 'company-complete-selection)
        (define-key map (kbd "<tab>") 'company-complete-selection))
       ((eq 'cycle auto-completion-tab-key-behavior)
        (define-key map (kbd "TAB") 'company-complete-common-or-cycle)
        (define-key map (kbd "<tab>") 'company-complete-common-or-cycle)
        (define-key map (kbd "<S-tab>")
          'dotemacs-company-complete-common-or-cycle-backward)
        (define-key map (kbd "<backtab>")
          'dotemacs-company-complete-common-or-cycle-backward))
       (t
        (define-key map (kbd "TAB") nil)
        (define-key map (kbd "<tab>") nil)))))
   (t (message "Not yet implemented for package %S" package))))

(defun dotemacs-auto-completion-setup-key-sequence (package)
  "Setup the key sequence to complete current selection."
  (when auto-completion-complete-with-key-sequence
    (let ((first-key (elt auto-completion-complete-with-key-sequence 0)))
      (cond ((eq 'company package)
             (define-key company-active-map (kbd (char-to-string first-key))
               'dotemacs-auto-completion-key-sequence-start))
            (t (message "Not yet implemented for package %S" package))))))

;; key sequence to complete selection

(defvar dotemacs--auto-completion-time nil)
(defvar dotemacs--auto-completion-complete-last-candidate nil)
(defvar dotemacs--auto-completion-shadowed-insert-binding nil)
(defvar dotemacs--auto-completion-shadowed-emacs-binding nil)

(defun dotemacs-auto-completion-key-sequence-start ()
  "Initiate auto-completion sequence."
  (interactive)
  (self-insert-command 1)
  (setq dotemacs--auto-completion-complete-last-candidate
        (cond
         ((bound-and-true-p company-mode)
          (nth company-selection company-candidates))))
  ;; enable second key of the sequence
  (let ((second-key (kbd (char-to-string
                          (elt auto-completion-complete-with-key-sequence 1)))))
    (setq dotemacs--auto-completion-shadowed-insert-binding
          (lookup-key evil-insert-state-map second-key))
    (setq dotemacs--auto-completion-shadowed-emacs-binding
          (lookup-key evil-emacs-state-map second-key))
    (define-key
      evil-insert-state-map
      second-key
      'dotemacs-auto-completion-key-sequence-end)
    (define-key
      evil-emacs-state-map
      second-key
      'dotemacs-auto-completion-key-sequence-end))
  ;; set a timer to restore the old bindings
  (run-at-time 0.1 nil 'dotemacs-auto-completion-key-sequence-restore)
  (when dotemacs--auto-completion-complete-last-candidate
    (setq dotemacs--auto-completion-time (current-time))))

(defun dotemacs-auto-completion-key-sequence-end ()
  "Check if the auto-completion key sequence has been entered."
  (interactive)
  (if (or (null dotemacs--auto-completion-time)
          (< 0.1 (float-time (time-since dotemacs--auto-completion-time))))
      (self-insert-command 1)
    (cond
     ((bound-and-true-p company-mode)
      (unless company-candidates
        ;; if the auto-completion menu is still active then we don't need to
        ;; delete the last inserted first key of the sequence
        (delete-char -1))
      (let ((company-idle-delay))
        (company-auto-begin)
        (company-finish dotemacs--auto-completion-complete-last-candidate)))))
  (dotemacs-auto-completion-key-sequence-restore)
  (setq dotemacs--auto-completion-time nil))

(defun dotemacs-auto-completion-key-sequence-restore ()
  "Restore the shadowed key bindings used to auto-complete."
  (let ((second-key (kbd (char-to-string
                          (elt auto-completion-complete-with-key-sequence 1)))))
    (define-key
      evil-insert-state-map
      second-key
      dotemacs--auto-completion-shadowed-insert-binding)
    (define-key
      evil-emacs-state-map
      second-key
      dotemacs--auto-completion-shadowed-emacs-binding)))


(provide 'init-auto-completions)
