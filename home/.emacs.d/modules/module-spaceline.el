;;; Spaceline
(require 'module-global)

(use-package spaceline-config
  :ensure spaceline
  :init
  (progn
    (dotemacs|do-after-display-system-init
     (setq-default powerline-default-separator
                   (if (display-graphic-p) 'wave 'utf-8)))

    (defun dotemacs-set-powerline-for-startup-buffers ()
      "Set the powerline for buffers created when Emacs starts."
      (dolist (buffer '("*Messages*" "*dotemacs*" "*scratch" "*Compile-Log*" "*Require Times*"))
        (when (get-buffer buffer)
          (dotemacs-restore-powerline buffer))))
    (add-hook 'emacs-startup-hook
              'dotemacs-set-powerline-for-startup-buffers))
  :config
  (progn
    (spaceline-toggle-battery-off)
    (spaceline-toggle-version-control-off)
    (spaceline-toggle-minor-modes-off)
    (spaceline-toggle-major-mode-off)
    (spaceline-toggle-org-clock-off)

    (defun dotemacs/customize-powerline-faces ()
      "Alter powerline face to make them work with more themes."
      (set-face-attribute 'powerline-inactive2 nil
                          :inherit 'font-lock-comment-face))
    (dotemacs/customize-powerline-faces)

    (dolist (spec '((minor-modes "tmm")
                    (major-mode "tmM")
                    (version-control "tmv")
                    (point-position "tmp")
                    (org-clock "tmc")))
      (let* ((segment (car spec))
             (status-var (intern (format "spaceline-%S-p" segment))))
        (eval `(dotemacs-add-toggle ,(intern (format "mode-line-%S" segment))
                 :status ,status-var
                 :on (setq ,status-var t)
                 :off (setq ,status-var nil)
                 :documentation ,(format "Show %s in the mode-line."
                                         (replace-regexp-in-string
                                          "-" " " (format "%S" segment)))
                 :evil-leader ,(cadr spec)))))
    (setq spaceline-org-clock-p nil)

    (defun dotemacs//evil-state-face ()
      (if (bound-and-true-p evil-state)
          (let ((state (if (eq 'operator evil-state) evil-previous-state evil-state)))
            (intern (format "dotemacs-%S-face" state)))
        'face-of-god))
    (setq spaceline-highlight-face-func 'dotemacs//evil-state-face)

    (let ((unicodep (dotemacs-symbol-value
                     dotemacs-mode-line-unicode-symbols)))
      (setq spaceline-window-numbers-unicode unicodep)
      (setq spaceline-workspace-numbers-unicode unicodep))

    (spaceline-spacemacs-theme)
    (spaceline-helm-mode t)
    ; (when (configuration-layer/package-usedp 'info+)
    ;   (spaceline-info-mode t))

    (defun dotemacs-restore-powerline (buffer)
      "Restore the powerline in buffer"
      (with-current-buffer buffer
        (setq-local mode-line-format (default-value 'mode-line-format))
        (powerline-set-selected-window)
        (powerline-reset)))

    (defun dotemacs//prepare-diminish ()
      (when spaceline-minor-modes-p
        (let ((unicodep (dotemacs-symbol-value
                         dotemacs-mode-line-unicode-symbols)))
          (dotemacs|do-after-display-system-init
           (setq spaceline-minor-modes-separator
                 (if unicodep (if (display-graphic-p) "" " ") "|")))
          (dolist (mm dotemacs--diminished-minor-modes)
            (let ((mode (car mm)))
              (when (and (boundp mode) (symbol-value mode))
                (let* ((unicode (cadr mm))
                       (ascii (caddr mm))
                       (dim (if unicodep
                                unicode
                              (if ascii ascii unicode))))
                  (diminish mode dim))))))))
    (add-hook 'spaceline-pre-hook 'dotemacs//prepare-diminish)))

(provide 'module-spaceline)
;;; module-spaceline.el ends here
