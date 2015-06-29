(defun dotemacs-init-haskell-mode ()
  ;; use only internal indentation system from haskell
  (electric-indent-local-mode -1))

(defun dotemacs-haskell-process-do-type-on-prev-line ()
  (interactive)
  (if dotemacs-haskell-enable-ghci-ng-support
      (haskell-mode-show-type-at 1)
    (haskell-process-do-type 1)))

(provide 'init-haskell)
