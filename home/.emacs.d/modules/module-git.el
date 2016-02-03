;;; module-git.el --- Git Module
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

(use-package git-commit                 ; Git commit message mode
  :ensure t
  :defer t)

(use-package git-messenger
  :ensure t
  :defer t
  :init
  (dotemacs-set-leader-keys
    "gm" 'git-messenger:popup-message)
  :config
  (define-key git-messenger-map [escape] 'git-messenger:popup-close))

(use-package gitconfig-mode             ; Git configuration mode
  :ensure t
  :defer t)

(use-package gitignore-mode             ; .gitignore mode
  :ensure t
  :defer t)

(use-package gitattributes-mode         ; Git attributes mode
  :ensure t
  :defer t)

(use-package git-rebase-mode            ; Mode for git rebase -i
  :disabled t ; not compatible with magit 2.1
  :defer t
  :config
  (progn
    (evilified-state-evilify git-rebase-mode git-rebase-mode-map
      "J" 'git-rebase-move-line-down
      "K" 'git-rebase-move-line-up
      "u" 'git-rebase-undo
      "y" 'git-rebase-insert)

    (dotemacs-set-leader-keys-for-major-mode 'git-rebase-mode
      "mcc" 'git-rebase-server-edit
      "mk" 'git-rebase-abort)))

(use-package git-timemachine            ; Go back in Git time
  :ensure t
  :defer t
  :commands dotemacs-time-machine-micro-state
  :init
  (dotemacs-set-leader-keys
    "gt" 'dotemacs-time-machine-micro-state)
  :config
  (progn
    (dotemacs-define-micro-state time-machine
      :doc "[p] [N] previous [n] next [c] current [Y] copy hash [q] quit"
      :on-enter (let (golden-ratio-mode)
                  (unless (bound-and-true-p git-timemachine-mode)
                    (call-interactively 'git-timemachine)))
      :on-exit (when (bound-and-true-p git-timemachine-mode)
                 (git-timemachine-quit))
      :persistent t
      :bindings
      ("c" git-timemachine-show-current-revision)
      ("p" git-timemachine-show-previous-revision)
      ("n" git-timemachine-show-next-revision)
      ("N" git-timemachine-show-previous-revision)
      ("Y" git-timemachine-kill-revision)
      ("q" nil :exit t))))

(provide 'module-git)
;;; module-git.el ends here
