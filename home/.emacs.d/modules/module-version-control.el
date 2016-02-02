;;; Version Control Head
(require 'module-global)

(dotemacs-declare-prefix "gd" "diff")
(use-package diff-mode
  :defer t
  :ensure t
  :config
  (evilified-state-evilify diff-mode diff-mode-map
    "j" 'diff-hunk-next
    "k" 'diff-hunk-prev))

(use-package diff-hl                    ; Highlight hunks in fringe
  :ensure t
  :defer t
  :init
  (progn
    (setq diff-hl-side 'right)
    ;; Highlight changes to the current file in the fringe
    (global-diff-hl-mode)
    ;; Highlight changed files in the fringe of Dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

    ;; Fall back to the display margin, if the fringe is unavailable
    (dotemacs|do-after-display-system-init
      (unless (display-graphic-p)
        (setq diff-hl-side 'left)
        (diff-hl-margin-mode)))

    (dotemacs-set-leader-keys
      "gdg" 'diff-hl-diff-goto-hunk
      "gdn" 'diff-hl-next-hunk
      "gdN" 'diff-hl-previous-hunk
      "gdr" 'diff-hl-revert-hunk)))

(use-package vc-hooks                   ; Simple version control
  :defer t
  :config
  ;; Always follow symlinks to files in VCS repos
  (setq vc-follow-symlinks t))

(provide 'module-version-control)
;;; module-version-control.el ends here
