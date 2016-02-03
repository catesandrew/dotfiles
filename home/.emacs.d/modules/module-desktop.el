;;; module-desktop.el --- Desktop Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; http://stackoverflow.com/a/4485083/740527
;;
(require 'core-vars)
;; (require 'core-funcs)
;; (require 'core-keybindings)
;; (require 'core-display-init)
(require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(defvar dotemacs/desktop-dirname (concat dotemacs-cache-directory "desktop/")
  "Folder where to save desktop sessions.")

(defvar dotemacs/desktop-base-file-name (
  concat "emacs_" emacs-version-short ".desktop")
  "File names of desktop files.")

(defvar dotemacs/desktop-base-lock-name (
  concat "emacs_" emacs-version-short ".desktop.lock")
  "Desktop lock file name.")

(defun dotemacs-desktop-session-restore-and-enable ()
  "Load the desktop and enable autosaving."
  (interactive)
  (let ((desktop-load-locked-desktop "ask"))
    (if (dotemacs-saved-session)
        (desktop-read)
      (message "No desktop found."))
    (desktop-save-mode 1)))

;; use session-save to save the desktop manually
(defun dotemacs-session-save ()
  "Save an Emacs session."
  (interactive)
  (if (dotemacs-saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
          (desktop-save-in-desktop-dir)
        (message "Session not saved."))
    (desktop-save-in-desktop-dir)))

;; use session-restore to restore the desktop manually
(defun dotemacs-session-restore ()
  "Restore a saved Emacs session."
  (interactive)
  (if (dotemacs-saved-session)
      (desktop-read)
    (message "No desktop found.")))

(defun dotemacs-desktop-after-read ()
  "Load the desktop and enable autosaving."
  (interactive)
  ;; desktop-remove clears desktop-dirname
  (setq desktop-dirname-tmp dotemacs/desktop-dirname)
  (desktop-remove)
  (setq desktop-dirname desktop-dirname-tmp))

(defun dotemacs-saved-session ()
  "Save session."
  (interactive)
  (file-exists-p (
    concat dotemacs/desktop-dirname "/" dotemacs/desktop-base-file-name)))

(defun dotemacs-desktop-after-init ()
  "Save an emacs session."
  (interactive)
  (if (dotemacs-saved-session)
      (if (y-or-n-p "Restore desktop? ")
          (dotemacs-session-restore))))

;; ask user whether to restore desktop at start-up
(add-hook 'after-init-hook 'dotemacs-desktop-after-init)

(use-package desktop                    ; Save buffers, windows and frames
  :defer t
  :config
  (progn
    (desktop-save-mode 0)
    (setq desktop-dirname dotemacs/desktop-dirname
          desktop-base-file-name dotemacs/desktop-base-file-name
          desktop-base-lock-name dotemacs/desktop-base-lock-name
          desktop-path (list dotemacs/desktop-dirname)
          desktop-load-locked-desktop nil
          ;; Fix the frameset warning at startup
          desktop-restore-frames nil
          ;; Save desktops a minute after Emacs was idle.
          desktop-auto-save-timeout 60)

    ;; https://github.com/purcell/emacs.d/blob/master/lisp/init-sessions.el
    ;; Save a bunch of variables to the desktop file.
    ;; For lists, specify the length of the maximal saved data too.
    (setq desktop-globals-to-save
          (append '((comint-input-ring . 50)
                    desktop-missing-file-warning
                    (dired-regexp-history . 20)
                    (extended-command-history . 30)
                    (face-name-history . 20)
                    (file-name-history . 100)
                    (ido-buffer-history . 100)
                    (ido-last-directory-list . 100)
                    (ido-work-directory-list . 100)
                    (ido-work-file-list . 100)
                    (magit-read-rev-history . 50)
                    (minibuffer-history . 50)
                    (org-refile-history . 50)
                    (org-tags-history . 50)
                    (query-replace-history . 60)
                    (read-expression-history . 60)
                    (regexp-history . 60)
                    (regexp-search-ring . 20)
                    register-alist
                    (search-ring . 20)
                    (shell-command-history . 50)
                    tags-file-name
                    tags-table-list)))

    ;; Don't save .gpg files. Restoring those files in emacsclients causes
    ;; a problem as the password prompt appears before the frame is loaded.
    (setq desktop-files-not-to-save
          (concat "\\(^/[^/:]*:\\|(ftp)$\\)" ; original value
                  "\\|\\(\\.gpg$\\)"
                  "\\|\\(\\.plstore$\\)"
                  "\\|\\(\\.desktop$\\)"
                  "COMMIT_EDITMSG\\'"
                  ;; If backup files with names like "file.sv.20150619_1641.bkp"
                  ;; are saved to the desktop file, emacsclient crashes at launch
                  ;; Need to debug why that's the case. But for now, simply not
                  ;; saving the .bkp files to the desktop file is a workable
                  ;; solution -- Fri Jun 19 16:45:50 EDT 2015 - kmodi
                  "\\|\\(\\.bkp$\\)"
                  "\\|\\(\\TAGS$\\)"))

    ;; Don't save the eww buffers
    (setq desktop-buffers-not-to-save (concat desktop-buffers-not-to-save
                                              "\\|\\(^eww\\(<[0-9]+>\\)*$\\)"))

    (dolist (mode '(magit-mode magit-log-mode))
      (add-to-list 'desktop-modes-not-to-save mode))

    ;; remove desktop after it's been read
    (add-hook 'desktop-after-read-hook 'dotemacs-desktop-after-read)))

(dotemacs-use-package-add-hook ignoramus
  :post-config
  (with-eval-after-load 'desktop
    (setq desktop-files-not-to-save (concat desktop-files-not-to-save
                                            ignoramus-boring-file-regexp))))

(provide 'module-desktop)
;;; module-desktop.el ends here
