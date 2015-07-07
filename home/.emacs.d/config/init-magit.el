(defun dotemacs-load-gh-pulls-mode ()
  "Start `magit-gh-pulls-mode' only after a manual request."
  (interactive)
  (magit-gh-pulls-mode)
  (magit-gh-pulls-reload))

(defun dotemacs-fetch-gh-pulls-mode ()
  "Start `magit-gh-pulls-mode' only after a manual request."
  (interactive)
  (magit-gh-pulls-mode)
  (magit-gh-pulls-fetch-commits))

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
    (setq magit-repo-dirs (mapcar #'directory-file-name project-dirs))))

(defun dotemacs-magit-diff-head ()
  "Execute `magit-diff' against current HEAD."
  (interactive)
  (magit-diff "HEAD"))

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

(provide 'init-magit)
