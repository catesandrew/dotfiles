;;; module-git.el --- Git Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'use-package)
(require 'core-transient-state)
(require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(use-package helm-gitignore
  :ensure t
  :defer t
  :init (dotemacs-set-leader-keys "gI" 'helm-gitignore))

(use-package git-commit
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

(use-package git-timemachine            ; Go back in Git time
  :ensure t
  :defer t
  :commands dotemacs/time-machine-transient-state/body
  :init
  (dotemacs-set-leader-keys
    "gt" 'dotemacs/time-machine-transient-state/body)
  :config
  (progn
    (dotemacs-define-transient-state time-machine
      :title "Git Timemachine Transient State"
      :doc "
[_p_/_N_] previous [_n_] next [_c_] current [_Y_] copy hash [_q_] quit"
        :on-enter (let (golden-ratio-mode)
                    (unless (bound-and-true-p git-timemachine-mode)
                      (call-interactively 'git-timemachine)))
        :on-exit (when (bound-and-true-p git-timemachine-mode)
                   (git-timemachine-quit))
        :foreign-keys run
        :bindings
        ("c" git-timemachine-show-current-revision)
        ("p" git-timemachine-show-previous-revision)
        ("n" git-timemachine-show-next-revision)
        ("N" git-timemachine-show-previous-revision)
        ("Y" git-timemachine-kill-revision)
        ("q" nil :exit t))))

(use-package gitattributes-mode
  :ensure t
  :defer t)

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package smeargle
  :ensure t
  :defer t
  :init
  (progn
    (dotemacs-declare-prefix "gH" "highlight")

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

(provide 'module-git)
;;; module-git.el ends here
