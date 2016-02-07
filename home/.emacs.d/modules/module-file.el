;;; module-file.el --- File Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; Additional utilities for file handling, see:
;; http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
;; http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
;; http://emacsredux.com/blog/2013/04/05/recently-visited-files/
;; https://github.com/bbatsov/prelude/blob/master/core/prelude-core.el
;;
(require 'core-vars)
(require 'core-use-package-ext)
;; (require 'core-funcs)
(require 'core-keybindings)
;; (require 'core-display-init)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)
;; (require 'package)
;; (require 'lisp-mnt)
;; (require 'find-func)
;; (require 'subr-x)

;;; Code:

;; We only access these variables if the corresponding library is loaded
(defvar recentf-list)
(defvar projectile-require-project-root)

;; Assert the byte compiler that dired functions are defined, because we never
;; call them for non-dired buffers, so we can be sure that dired is always
;; loaded first.
(declare-function dired-get-marked-files "dired")
(declare-function dired-current-directory "dired")

;;; Utilities
(defun dotemacs-current-file ()
  "Gets the \"file\" of the current buffer.

The file is the buffer's file name, or the `default-directory' in
`dired-mode'."
  (if (derived-mode-p 'dired-mode)
      default-directory
    (buffer-file-name)))

;;;###autoload
(defun dotemacs-find-user-init-file-other-window ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

;;;###autoload
(defun dotemacs-recompile-packages ()
  "Recompile all packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))


;;; Working with file names
;;;###autoload
(defun dotemacs-copy-filename-as-kill (&optional arg)
  "Copy the name of the currently visited file to kill ring.

With a zero prefix arg, copy the absolute file name.  With
\\[universal-argument], copy the file name relative to the
current Projectile project, or to the current buffer's
`default-directory', if the file is not part of any project.
Otherwise copy the non-directory part only."
  (interactive "P")
  (if-let ((file-name (dotemacs-current-file))
           (name-to-copy
            (cond
             ((zerop (prefix-numeric-value arg)) file-name)
             ((consp arg)
              (let* ((projectile-require-project-root nil)
                     (directory (and (fboundp 'projectile-project-root)
                                     (projectile-project-root))))
                (file-relative-name file-name directory)))
             (t (file-name-nondirectory file-name)))))
      (progn
        (kill-new name-to-copy)
        (message "%s" name-to-copy))
    (user-error "This buffer is not visiting a file")))

;;; Working with the current file
;;;###autoload
(defun dotemacs-rename-file-and-buffer ()
  "Rename the current file and buffer."
  (interactive)
  (let* ((filename (buffer-file-name))
         (old-name (if filename
                       (file-name-nondirectory filename)
                     (buffer-name)))
         (new-name (read-file-name "New name: " nil nil nil old-name)))
    (cond
     ((not (and filename (file-exists-p filename))) (rename-buffer new-name))
     ((vc-backend filename) (vc-rename-file filename new-name))
     (t
      (rename-file filename new-name 'force-overwrite)
      (set-visited-file-name new-name 'no-query 'along-with-file)))))

;;;###autoload
(defun dotemacs-delete-file-and-buffer ()
  "Delete the current file and kill the buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (cond
     ((not filename) (kill-buffer))
     ((vc-backend filename) (vc-delete-file filename))
     (t
      (delete-file filename)
      (kill-buffer)))))

;;;###autoload
(defun dotemacs-launch-dwim ()
  "Open the current file externally."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (let ((marked-files (dired-get-marked-files)))
        (if marked-files
            (launch-files marked-files 'confirm)
          (launch-directory (dired-current-directory))))
    (if (buffer-file-name)
        (launch-file (buffer-file-name))
      (user-error "The current buffer is not visiting a file"))))

;;; Intellij integration
(defun dotemacs-intellij-project-root-p (directory)
  "Determine whether DIRECTORY is an IntelliJ project root."
  (and (file-directory-p directory)
       (directory-files directory nil (rx "." (or "iml" "idea") string-end)
                        'nosort)))

(defun dotemacs-intellij-project-root ()
  "Get the path to the nearest IntelliJ project root.

Return the absolute file name of the project root directory, or
nil if no project root was found."
  (when-let (file-name (buffer-file-name))
    (locate-dominating-file file-name #'dotemacs-intellij-project-root-p)))

(defun dotemacs-intellij-launcher ()
  "Get the IntelliJ launcher for the current system."
  (pcase system-type
    (`darwin
     (when-let (bundle (dotemacs/path-of-bundle "com.jetbrains.intellij"))
       (expand-file-name "Contents/MacOS/idea" bundle)))
    (_ (user-error "No launcher for system %S" system-type))))

;;;###autoload
(defun dotemacs-open-in-intellij ()
  "Open the current file in IntelliJ IDEA."
  (interactive)
  (let ((project-root (dotemacs-intellij-project-root))
        (launcher (dotemacs-intellij-launcher)))
    (unless project-root
      (user-error "No IntelliJ project found for the current buffer"))
    (start-process "*intellij*" nil launcher
                   (expand-file-name project-root)
                   "--line" (number-to-string (line-number-at-pos))
                   (expand-file-name (buffer-file-name)))))

;;; URLs and browsing
(defun dotemacs-browse-feature-url (feature)
  "Browse the URL of the given FEATURE.

Interactively, use the symbol at point, or prompt, if there is
none."
  (interactive
   (let ((symbol (or (symbol-at-point)
                     (completing-read "Feature: " features nil
                                      'require-match))))
     (list symbol)))
  (let* ((library (if (symbolp feature) (symbol-name feature) feature))
         (library-file (find-library-name library)))
    (when library-file
      (with-temp-buffer
        (insert-file-contents library-file)
        (let ((url (lm-header "URL")))
          (if url
              (browse-url url)
            (user-error "Library %s has no URL header" library)))))))

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun dotemacs-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(use-package files
  ;; Revert the current buffer (re-read the contents from disk). Burying a
  ;; buffer (removing it from the current window and sending it to the bottom of
  ;; the stack) is very common for dismissing buffers.
  :bind (("C-c e u" . revert-buffer)
         ("C-c e y" . bury-buffer))
  :init
  (progn
    (setq view-read-only t) ; View read-only files
    (add-hook 'find-file-hook
              (lambda ()
                (unless (eq major-mode 'org-mode)
                  (setq show-trailing-whitespace t))))
    (add-to-list 'find-file-not-found-functions 'dotemacs-create-non-existent-directory)))

(use-package open-junk-file
  :ensure t
  :defer t
  :commands (open-junk-file)
  :init
  (setq open-junk-file-directory (concat dotemacs-cache-directory "junk/%Y/%m/%d-%H%M%S."))
  (defun dotemacs/open-junk-file (&optional arg)
    "Open junk file Open junk file using helm, with `prefix-arg'
search in junk files."
    (interactive "P")
    (let* ((fname (format-time-string open-junk-file-format (current-time)))
           (rel-fname (file-name-nondirectory fname))
           (junk-dir (file-name-directory fname))
           (default-directory junk-dir))
      (cond (arg
             (require 'helm)
             (let (helm-ff-newfile-prompt-p)
               (dotemacs-helm-files-smart-do-search)))
            (t
             (require 'helm)
             (let (helm-ff-newfile-prompt-p)
               (helm-find-files-1 fname))))))
  (dotemacs-set-leader-keys "fJ" 'dotemacs/open-junk-file))

(use-package image-file                 ; Visit images as images
  :init (auto-image-file-mode))

(use-package recentf                    ; Save recently visited files
  :defer t
  :init
  (progn
    ;; lazy load recentf
    (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                      (recentf-mode)
                                      (recentf-track-opened-file))))
    (setq recentf-save-file (concat dotemacs-cache-directory "recentf")
          recentf-max-saved-items 5000
          recentf-max-menu-items 10
          recentf-auto-save-timer (run-with-idle-timer 1800 t 'recentf-save-list)
          ;; Cleanup recent files only when Emacs is idle, but not when the mode
          ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
          ;; idles often enough to have the recent files list clean up regularly
          recentf-auto-cleanup 300))
  :config
  (progn
    (setq recentf-exclude (list "COMMIT_EDITMSG\\'"
                                (expand-file-name package-user-dir)
                                (expand-file-name dotemacs-cache-directory)))))

(dotemacs-use-package-add-hook ignoramus
  :post-config
  (with-eval-after-load 'recentf
    (setq recentf-exclude (append recentf-exclude
                                  (list ignoramus-boring-file-regexp)))))

(provide 'module-file)
;;; module-file.el ends here
