;;; Window
(require 'module-global)

(defun dotemacs-quit-bottom-side-windows ()
  "Quit side windows of the current frame."
  (interactive)
  (dolist (window (window-at-side-list))
    (quit-window nil window)))

(use-package windmove                   ; Move between windows with Shift+Arrow
  :bind (("S-<left>"  . windmove-left)
         ("S-<right>" . windmove-right)
         ("S-<up>"    . windmove-up)
         ("S-<down>"  . windmove-down)))

(use-package winner                     ; Undo and redo window configurations
  :ensure t
  :init
  (progn
    ;; activate winner mode use to undo and redo windows layout
    (winner-mode t))
  :config
  (progn
    (setq dotemacs-winner-boring-buffers '("*Completions*"
                                           "*Compile-Log*"
                                           "*inferior-lisp*"
                                           "*Fuzzy Completions*"
                                           "*Apropos*"
                                           "*Help*"
                                           "*cvs*"
                                           "*Buffer List*"
                                           "*Ibuffer*"
                                           "*esh command on file*"
                                            ))
    (setq winner-boring-buffers
          (append winner-boring-buffers dotemacs-winner-boring-buffers))))

(use-package window-numbering
  :ensure t
  :config
  (progn
    (setq window-numbering-auto-assign-0-to-minibuffer nil)
    (dotemacs-set-leader-keys
      "0" 'select-window-0
      "1" 'select-window-1
      "2" 'select-window-2
      "3" 'select-window-3
      "4" 'select-window-4
      "5" 'select-window-5
      "6" 'select-window-6
      "7" 'select-window-7
      "8" 'select-window-8
      "9" 'select-window-9)
    (window-numbering-mode 1))

  (defun dotemacs-window-numbering-assign (windows)
    "Custom number assignment for special buffers."
    (mapc (lambda (w)
            (when (and (boundp 'neo-global--window)
                       (eq w neo-global--window))
              (window-numbering-assign w 0)))
          windows))
  (add-hook 'window-numbering-before-hook 'dotemacs-window-numbering-assign)
  (add-hook 'neo-after-create-hook '(lambda (w) (window-numbering-update))))

(use-package ediff-wind
  :defer t
  :config
  (progn
    ;;revert windows on exit - needs winner mode
    (when (fboundp 'winner-undo)
      (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

    ;; Prevent Ediff from spamming the frame
    (setq ediff-diff-options "-w"
            ediff-window-setup-function #'ediff-setup-windows-plain
            ediff-split-window-function #'split-window-horizontally)))

(provide 'module-window)
;;; module-window.el ends here
