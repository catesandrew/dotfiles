;;; module-flyspell.el --- Flyspell Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
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

(use-package flyspell                   ; On-the-fly spell checking
  :defer t
  :init
  (progn
    (setq flyspell-use-meta-tab nil
          flyspell-issue-welcome-flag nil  ;; Make Flyspell less chatty
          flyspell-issue-message-flag nil)
    (dolist (mode '(org-mode text-mode message-mode))
      (spell-checking/add-flyspell-hook mode))

    (dotemacs-add-toggle spelling-checking
      :status flyspell-mode
      :on (if (derived-mode-p 'prog-mode)
              (flyspell-prog-mode)
            (flyspell-mode))
      :off (progn
             (flyspell-mode-off)
             ;; Also disable auto-dictionary when disabling spell-checking.
             (when (fboundp 'auto-dictionary-mode) (auto-dictionary-mode -1)))
      :documentation "Enable automatic spell checking."
      :evil-leader "tS")

    (dotemacs-set-leader-keys
      "Sb" 'flyspell-buffer
      "Sd" 'spell-checing/change-dictionary
      "Sn" 'flyspell-goto-next-error)

    ;; http://emacs.stackexchange.com/questions/14909/how-to-use-flyspell-to-efficiently-correct-previous-word/14912#14912
    (defun dotemacs/flyspell-goto-previous-error (arg)
      "Go to arg previous spelling error."
      (interactive "p")
      (while (not (= 0 arg))
        (let ((pos (point))
              (min (point-min)))
          (if (and (eq (current-buffer) flyspell-old-buffer-error)
                   (eq pos flyspell-old-pos-error))
              (progn
                (if (= flyspell-old-pos-error min)
                    ;; goto beginning of buffer
                    (progn
                      (message "Restarting from end of buffer")
                      (goto-char (point-max)))
                  (backward-word 1))
                (setq pos (point))))
          ;; seek the next error
          (while (and (> pos min)
                      (let ((ovs (overlays-at pos))
                            (r '()))
                        (while (and (not r) (consp ovs))
                          (if (flyspell-overlay-p (car ovs))
                              (setq r t)
                            (setq ovs (cdr ovs))))
                        (not r)))
            (backward-word 1)
            (setq pos (point)))
          ;; save the current location for next invocation
          (setq arg (1- arg))
          (setq flyspell-old-pos-error pos)
          (setq flyspell-old-buffer-error (current-buffer))
          (goto-char pos)
          (call-interactively 'helm-flyspell-correct)
          (if (= pos min)
              (progn
                (message "No more miss-spelled word!")
                (setq arg 0))))))

    ;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
    (defun dotemacs/ispell-word-then-abbrev (p)
      "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will be global.

If there's nothing wrong with the word at point, keep looking for
a typo until the beginning of buffer. You can skip typos you
don't want to fix with `SPC', and you can abort completely with
`C-g'."
      (interactive "P")
      (let (bef aft)
        (save-excursion
          (while (if (setq bef (thing-at-point 'word))
                     ;; Word was corrected or used quit.
                     (if (ispell-word nil 'quiet)
                         nil ; End the loop.
                       ;; Also end if we reach `bob'.
                       (not (bobp)))
                   ;; If there's no word at point, keep looking
                   ;; until `bob'.
                   (not (bobp)))
            (backward-word))
          (setq aft (thing-at-point 'word)))
        (if (and aft bef (not (equal aft bef)))
            (let ((aft (downcase aft))
                  (bef (downcase bef)))
              (define-abbrev
                (if p local-abbrev-table global-abbrev-table)
                bef aft)
              (message "\"%s\" now expands to \"%s\" %sally"
                       bef aft (if p "loc" "glob")))
          (user-error "No typo at or before point"))))

    (dotemacs-set-leader-keys
      "Si" 'dotemacs/ispell-word-then-abbrev
      "Sp" 'dotemacs/flyspell-goto-previous-error
      "Sn" 'flyspell-goto-next-error)

    (setq save-abbrevs 'silently)
    (setq-default abbrev-mode t))
  :config
  (progn
    ;; Undefine mouse buttons which get in the way
    (define-key flyspell-mouse-map [down-mouse-2] nil)
    (define-key flyspell-mouse-map [mouse-2] nil)
    (dotemacs-diminish flyspell-mode " Ⓢ" " S")))

(use-package helm-flyspell
  :ensure t
  :commands helm-flyspell-correct
  :init (dotemacs-set-leader-keys "Sc" 'helm-flyspell-correct))

(provide 'module-flyspell)
;;; module-flyspell.el ends here
