(defun dotemacs-init-haskell-mode ()
  ;; use only internal indentation system from haskell
  (if (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1))
  (when dotemacs-haskell-enable-shm-support
    ;; in structured-haskell-mode line highlighting creates noise
    (setq-local global-hl-line-mode nil)))

(defun dotemacs-haskell-process-do-type-on-prev-line ()
  (interactive)
  (if dotemacs-haskell-enable-ghci-ng-support
      (haskell-mode-show-type-at 1)
    (haskell-process-do-type 1)))

(defun dotemacs/haskell-interactive-bring ()
  "Bring up the interactive mode for this session without
         switching to it."
  (interactive)
  (let* ((session (haskell-session))
         (buffer (haskell-session-interactive-buffer session)))
    (display-buffer buffer)))

(provide 'init-haskell)
;;; init-haskell.el ends here
