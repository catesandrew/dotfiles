;; ignore whitespace
;;;###autoload
(defun dotemacs-magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

;;;###autoload
(defun dotemacs-magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

;;;###autoload
(defun dotemacs-magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

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

(provide 'init-magit)
