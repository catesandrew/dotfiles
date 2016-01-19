(defun dotemacs-desktop-session-restore-and-enable ()
  "Load the desktop and enable autosaving"
  (interactive)
  (let ((desktop-load-locked-desktop "ask"))
    (if (dotemacs-saved-session)
        (desktop-read)
      (message "No desktop found."))
    (desktop-save-mode 1)))

;; use session-save to save the desktop manually
(defun dotemacs-session-save ()
  "Save an emacs session."
  (interactive)
  (if (dotemacs-saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
          (desktop-save-in-desktop-dir)
        (message "Session not saved."))
    (desktop-save-in-desktop-dir)))

;; use session-restore to restore the desktop manually
(defun dotemacs-session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (dotemacs-saved-session)
      (desktop-read)
    (message "No desktop found.")))

(defun dotemacs-desktop-after-read ()
  "Load the desktop and enable autosaving"
  (interactive)
  ;; desktop-remove clears desktop-dirname
  (setq desktop-dirname-tmp desktop-dirname)
  (desktop-remove)
  (setq desktop-dirname desktop-dirname-tmp))

(defun dotemacs-saved-session ()
  "Save session."
  (interactive)
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

(defun dotemacs-desktop-after-init ()
  "Save an emacs session."
  (interactive)
  (if (dotemacs-saved-session)
      (if (y-or-n-p "Restore desktop? ")
          (dotemacs-session-restore))))

;; remove desktop after it's been read
(add-hook 'desktop-after-read-hook 'dotemacs-desktop-after-read)

;; ask user whether to restore desktop at start-up
(add-hook 'after-init-hook 'dotemacs-desktop-after-init)

(provide 'init-desktop)
;;; init-desktop.el ends here
