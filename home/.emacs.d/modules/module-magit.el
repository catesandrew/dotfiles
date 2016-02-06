;;; module-magit.el --- Magit Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;

(require 'evil-evilified-state)
(require 'core-vars)
;; (require 'core-funcs)
(require 'core-micro-state)
(require 'core-keybindings)
(require 'core-transient-state)
(require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
(require 'module-utils)

;;; Code:

;; gravatars from magit use this to store their cache
(setq url-configuration-directory (concat dotemacs-cache-directory "url/"))

(defun dotemacs-load-gh-pulls-mode ()
  "Start `magit-gh-pulls-mode' only after a manual request."
  (interactive)
  (magit-gh-pulls-mode)
  (magit-gh-pulls-popup))

(defun dotemacs-git-link-copy-url-only ()
  "Only copy the generated link to the kill ring."
  (interactive)
  (let (git-link-open-in-browser)
    (call-interactively 'git-link)))

(defun dotemacs-git-link-commit-copy-url-only ()
  "Only copy the generated link to the kill ring."
  (interactive)
  (let (git-link-open-in-browser)
    (call-interactively 'git-link-commit)))

;; Set Magit's repo dirs for `magit-status' from Projectile's known
;; projects.  Initialize the `magit-repo-dirs' immediately after Projectile
;; was loaded, and update it every time we switched projects, because the
;; new project might have been unknown before
(defun dotemacs-magit-set-repo-dirs-from-projectile ()
  "Set `magit-repo-dirs' from known Projectile projects."
  (let ((project-dirs (bound-and-true-p projectile-known-projects)))
    ;; Remove trailing slashes from project directories, because Magit adds
    ;; trailing slashes again, which breaks the presentation in the Magit
    ;; prompt.
    (setq magit-repo-dirs (mapcar 'directory-file-name project-dirs))))

(use-package magit                      ; The one and only Git frontend
  :ensure t
  :commands (magit-blame-mode
             magit-cherry-pick-popup
             magit-commit-popup
             magit-diff-popup
             magit-fetch-popup
             magit-log-popup
             magit-pull-popup
             magit-push-popup
             magit-rebase-popup
             magit-status)
  :init
  (progn
    (with-eval-after-load 'projectile
      (add-hook 'projectile-after-switch-project-hook
                'dotemacs-magit-set-repo-dirs-from-projectile)
      (dotemacs-magit-set-repo-dirs-from-projectile))

    (setq magit-save-some-buffers 'dontask
          magit-stage-all-confirm nil
          magit-unstage-all-confirm nil
          magit-show-child-count t
          magit-completing-read-function 'magit-builtin-completing-read
          ;; Except when you ask something useful…
          magit-set-upstream-on-push t)
    (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
    (add-hook 'git-commit-mode-hook 'fci-mode)

    ;; On Windows, we must use Git GUI to enter username and password
    ;; See: https://github.com/magit/magit/wiki/FAQ#windows-cannot-push-via-https
    (when (eq window-system 'w32)
      (setenv "GIT_ASKPASS" "git-gui--askpass"))

    (defun dotemacs-magit-diff-head ()
      "Execute `magit-diff' against current HEAD."
      (interactive)
      (magit-diff "HEAD"))

    (dotmacs-declare-prefix "gd" "diff")
    (dotmacs-set-leader-keys
     "gA" 'magit-cherry-pick-popup
     "gb" 'dotemacs/git-blame-micro-state
     "gc" 'magit-commit-popup
     "gC" 'magit-checkout
     "gd" 'magit-diff-popup
     "gD" 'dotemacs/magit-diff-head
     "ge" 'magit-ediff-compare
     "gE" 'magit-ediff-show-working-tree
     "gf" 'magit-fetch-popup
     "gF" 'magit-pull-popup
     "gi" 'magit-init
     "gl" 'magit-log-popup
     "gL" 'magit-log-buffer-file
     "gr" 'magit-rebase-popup
     "gP" 'magit-push-popup
     "gs" 'magit-status
     "gS" 'magit-stage-file
     "gU" 'magit-unstage-file)

    (dotemacs-define-micro-state git-blame
      :title "Git Blame Transient State"
      :doc "
Press [_b_] again to blame further in the history, [_q_] to go up or quit."
      :on-enter (let (golden-ratio-mode)
                  (unless (bound-and-true-p magit-blame-mode)
                    (call-interactively 'magit-blame)))
      :foreign-keys run
      :bindings
      ("b" magit-blame)
      ;; here we use the :exit keyword because we should exit the
      ;; micro-state only if the magit-blame-quit effectively disable
      ;; the magit-blame mode.
      ("q" nil :exit (progn (when (bound-and-true-p magit-blame-mode)
                              (magit-blame-quit))
                            (not (bound-and-true-p magit-blame-mode))))))
  :config
  (progn
    ;; seems to be necessary at the time of release
    (require 'git-rebase)
    ;; bind function keys
    ;; (define-key magit-mode-map (kbd "<tab>") 'magit-section-toggle)
    (unless (boundp 'dotemacs-use-evil-magit)
      ;; use auto evilification if `evil-magit' is not used
      (evilified-state-evilify-map magit-mode-map
        :bindings
        "gr" 'magit-refresh
        "gR" 'magit-refresh-all)
      (evilified-state-evilify-map magit-status-mode-map
        :mode magit-status-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (evilified-state-evilify-map magit-refs-mode-map
        :mode magit-refs-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (evilified-state-evilify-map magit-blame-mode-map
        :mode magit-blame-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (evilified-state-evilify-map magit-hunk-section-map
        :mode magit-status-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (evilified-state-evilify-map magit-diff-mode-map
        :mode magit-diff-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (evilified-state-evilify-map magit-log-read-revs-map
        :mode magit-log-read-revs
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (evilified-state-evilify-map magit-log-mode-map
        :mode magit-log-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (evilified-state-evilify-map magit-log-select-mode-map
        :mode magit-log-select-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (evilified-state-evilify-map magit-cherry-mode-map
        :mode magit-cherry-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (evilified-state-evilify-map magit-reflog-mode-map
        :mode magit-reflog-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (evilified-state-evilify-map magit-process-mode-map
        :mode magit-process-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (evilified-state-evilify-map magit-stash-mode-map
        :mode magit-stash-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (evilified-state-evilify-map git-rebase-mode-map
        :mode git-rebase-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward
        "J" 'git-rebase-move-line-down
        "K" 'git-rebase-move-line-up
        "u" 'git-rebase-undo
        "y" 'git-rebase-insert)
      ;; default state for additional modes
      (dolist (mode '(magit-popup-mode
                      magit-popup-sequence-mode))
        (evil-set-initial-state mode 'emacs))
      (let ((refresh-key "gr")
            (refresh-all-key "gR")
            (delete-key (nth 0 (where-is-internal 'magit-delete-thing
                                                  magit-mode-map))))
        (evilified-state--configure-default-state 'magit-revision-mode)
        ;; section maps
        (eval `(evilified-state-evilify-map magit-tag-section-map
                 :pre-bindings
                 ,delete-key 'magit-tag-delete
                 :bindings
                 ,refresh-key 'magit-refresh
                 ,refresh-all-key 'magit-refresh-all))
        (eval `(evilified-state-evilify-map magit-untracked-section-map
                 :pre-bindings
                 ,delete-key 'magit-discard
                 :bindings
                 ,refresh-key 'magit-refresh
                 ,refresh-all-key 'magit-refresh-all))
        (eval `(evilified-state-evilify-map magit-branch-section-map
                 :pre-bindings
                 ,delete-key 'magit-branch-delete
                 :bindings
                 ,refresh-key 'magit-refresh
                 ,refresh-all-key 'magit-refresh-all))
        (eval `(evilified-state-evilify-map magit-remote-section-map
                 :pre-bindings
                 ,delete-key 'magit-remote-remove
                 :bindings
                 ,refresh-key 'magit-refresh
                 ,refresh-all-key 'magit-refresh-all))
        (eval `(evilified-state-evilify-map magit-file-section-map
                 :pre-bindings
                 ,delete-key 'magit-discard
                 :bindings
                 ,refresh-key 'magit-refresh
                 ,refresh-all-key 'magit-refresh-all))
        (eval `(evilified-state-evilify-map magit-hunk-section-map
                 :pre-bindings
                 ,delete-key 'magit-discard
                 :bindings
                 ,refresh-key 'magit-refresh
                 ,refresh-all-key 'magit-refresh-all))
        (eval `(evilified-state-evilify-map magit-unstaged-section-map
                 :pre-bindings
                 ,delete-key 'magit-discard
                 :bindings
                 ,refresh-key 'magit-refresh
                 ,refresh-all-key 'magit-refresh-all))
        (eval `(evilified-state-evilify-map magit-staged-section-map
                 :pre-bindings
                 ,delete-key 'magit-discard
                 :bindings
                 ,refresh-key 'magit-refresh
                 ,refresh-all-key 'magit-refresh-all))
        (eval `(evilified-state-evilify-map magit-commit-section-map
                 :pre-bindings
                 ,delete-key 'magit-discard
                 :bindings
                 ,refresh-key 'magit-refresh
                 ,refresh-all-key 'magit-refresh-all))
        (eval `(evilified-state-evilify-map magit-stashes-section-map
                 :pre-bindings
                 ,delete-key 'magit-stash-clear
                 :bindings
                 ,refresh-key 'magit-refresh
                 ,refresh-all-key 'magit-refresh-all))
        (eval `(evilified-state-evilify-map magit-stash-section-map
                 :pre-bindings
                 ,delete-key 'magit-stash-drop
                 :bindings
                 ,refresh-key 'magit-refresh
                 ,refresh-all-key 'magit-refresh-all))
        (eval `(evilified-state-evilify-map magit-module-commit-section-map
                 :bindings
                 ,refresh-key 'magit-refresh
                 ,refresh-all-key 'magit-refresh-all))
        (eval `(evilified-state-evilify-map magit-unpulled-section-map
                 :bindings
                 ,refresh-key 'magit-refresh
                 ,refresh-all-key 'magit-refresh-all))
        (eval `(evilified-state-evilify-map magit-unpushed-section-map
                 :bindings
                 ,refresh-key 'magit-refresh
                 ,refresh-all-key 'magit-refresh-all))))

    ;; full screen magit-status
    (when dotemacs-git-magit-status-fullscreen
      (setq magit-display-buffer-function
            (lambda (buffer)
              (if (or
                   ;; the original should stay alive, so we can't go fullscreen
                   magit-display-buffer-noselect
                   ;; don't go fullscreen for certain magit buffers if current
                   ;; buffer is a magit buffer (we're conforming to
                   ;; `magit-display-buffer-traditional')
                   (and (derived-mode-p 'magit-mode)
                        (not (memq (with-current-buffer buffer major-mode)
                                   '(magit-process-mode
                                     magit-revision-mode
                                     magit-diff-mode
                                     magit-stash-mode
                                     magit-status-mode)))))
                  ;; open buffer according to original magit rules
                  (magit-display-buffer-traditional buffer)
                ;; open buffer in fullscreen
                (delete-other-windows)
                ;; make sure the window isn't dedicated, otherwise
                ;; `set-window-buffer' throws an error
                (set-window-dedicated-p nil nil)
                (set-window-buffer nil buffer)
                ;; return buffer's window
                (get-buffer-window buffer)))))

    (when dotemacs-major-mode-leader-key
      (add-hook 'with-editor-mode-hook 'evil-normalize-keymaps)
      (let ((mm-key dotspacemacs-major-mode-leader-key))
        (dolist (state '(normal motion))
          (evil-define-key state with-editor-mode-map
            (concat mm-key mm-key) 'with-editor-finish
            (concat mm-key "a")    'with-editor-cancel
            (concat mm-key "c")    'with-editor-finish
            (concat mm-key "k")    'with-editor-cancel))))

    ;; whitespace
    (defun magit-toggle-whitespace ()
      (interactive)
      (if (member "-w" (if (derived-mode-p 'magit-diff-mode)
                           magit-refresh-args
                         magit-diff-section-arguments))
          (magit-dont-ignore-whitespace)
        (magit-ignore-whitespace)))

    (defun magit-ignore-whitespace ()
      (interactive)
      (add-to-list (if (derived-mode-p 'magit-diff-mode)
                       'magit-refresh-args 'magit-diff-section-arguments) "-w")
      (magit-refresh))

    (defun magit-dont-ignore-whitespace ()
      (interactive)
      (setq magit-diff-options
            (remove "-w"
                    (if (derived-mode-p 'magit-diff-mode)
                        magit-refresh-args
                      magit-diff-section-arguments))) (magit-refresh))

    (define-key magit-status-mode-map (kbd "C-S-w")
      'magit-toggle-whitespace)))

(use-package evil-magit
  :defer t
  :ensure t
  :init
  (progn
    (defconst dotemacs-use-evil-magit t
      "This variable is only defined if evil-magit is used.")

    (with-eval-after-load 'magit
      (require 'evil-magit)
      (evil-define-key 'motion magit-mode-map
        (kbd dotemacs-leader-key) dotemacs-default-map))))

(use-package magit-gitflow
  :ensure t
  :commands turn-on-magit-gitflow
  :init
  (progn
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
      (with-eval-after-load 'magit
        (define-key magit-mode-map "%" 'magit-gitflow-popup)))
  :config (dotemacs-diminish magit-gitflow-mode "Flow"))

(use-package magit-svn
  :if dotemacs-git-enable-magit-svn-plugin
  :disabled t ; not compatible with magit 2.1
  :ensure t
  :commands turn-on-magit-svn
  :init (add-hook 'magit-mode-hook 'turn-on-magit-svn)
  :config
  (progn
    (evil-define-key 'emacs magit-status-mode-map
      "N" 'magit-key-mode-popup-svn)))

(provide 'module-magit)
;;; module-magit.el ends here
