;; TODO to be deleted
(setq vc-make-backup-files t)

(with-eval-after-load 'vc-git
  (with-eval-after-load 'evil
    (with-eval-after-load 'magit-blame
      (defadvice magit-blame-file-on (after advice-for-magit-blame-file-on activate)
        (evil-emacs-state))
      (defadvice magit-blame-file-off (after advice-for-magit-blame-file-off activate)
        (evil-exit-emacs-state))))

  (if (display-graphic-p)
      (progn
        (require 'git-gutter-fringe+))
    (require 'git-gutter+))

  (global-git-gutter+-mode))

(require 'diff-hl)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(unless (display-graphic-p)
  (diff-hl-margin-mode))

(provide 'init-vcs)
