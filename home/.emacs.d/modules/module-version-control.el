;;; module-version-control.el --- Version Control Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'evil-evilified-state)
;; (require 'core-vars)
;; (require 'core-funcs)
(require 'core-keybindings)
(require 'core-display-init)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

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

(use-package smeargle
  :ensure t
  :defer t
  :init
  (progn
    (dotemacs-declare-prefix "gH" "highlight")

    ;; TODO abstract this to a function
    (let ((descr
           '(("smeargle" . "highlight by last update time")
             ("smeargle-commits" . "highlight by age of changes")
             ("smeargle-clear" . "clear"))))
      (dolist (nd descr)
        ;; ensure the target matches the whole string
        (push (cons (concat "\\`" (car nd) "\\'") (cdr nd))
              which-key-description-replacement-alist)))

    (dotemacs-set-leader-keys
     "gHc" 'smeargle-clear
     "gHh" 'smeargle-commits
     "gHt" 'smeargle)))

(provide 'module-version-control)
;;; module-version-control.el ends here
