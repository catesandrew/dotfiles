;;; module-buffers.el --- Buffers Module
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

(defvar dotemacs--default-mode-line mode-line-format
  "Backup of default mode line format.")

(defvar dotemacs-startup-lists '(recents projects)
  "List of items to show in the startup buffer. If nil it is disabled.
Possible values are: `recents' `bookmarks' `projects'.")

(defconst dotemacs-buffer-name "*dotemacs*"
  "The name of the dotemacs buffer.")

(defconst dotemacs-buffer--banner-length 75
  "Width of a banner.")

(defvar dotemacs-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'widget-forward)
    (define-key map (kbd "C-i") 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    (define-key map (kbd "RET") 'widget-button-press)
    (define-key map [down-mouse-1] 'widget-button-click)
    map)
  "Keymap for dotemacs buffer mode.")

(define-derived-mode dotemacs-buffer-mode special-mode "Dotemacs buffer"
  "Dotemacs major mode for startup screen.

\\<dotemacs-buffer-mode-map>
"
  :group 'dotemacs
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t)
  (page-break-lines-mode)
  ;; needed to make tab work correctly in terminal
  (evil-define-key 'motion dotemacs-buffer-mode-map
    (kbd "C-i") 'widget-forward)
  ;; motion state since this is a special mode
  (evil-set-initial-state 'dotemacs-buffer-mode 'motion))

(defun dotemacs-buffer/set-mode-line (format)
  "Set mode-line format for dotemacs buffer."
  (with-current-buffer (get-buffer-create "*dotemacs*")
    (setq mode-line-format format)))

(defun dotemacs-buffer/message (msg &rest args)
  "Display MSG in message prepended with '(emacs)'.
The message is displayed only if `init-file-debug' is non nil."
  (when init-file-debug
    (message "(emacs) %s" (apply 'format msg args))))

(defun dotemacs-buffer/warning (msg &rest args)
  "Display MSG as a warning message but in buffer `*Messages*'.
The message is always displayed. "
  (message "(emacs) Warning: %s" (apply 'format msg args)))

(defun dotemacs-buffer/insert-page-break ()
  "Insert a page break line in dotemacs buffer."
  (dotemacs-buffer/append "\n\n\n"))

(defun dotemacs-buffer/append (msg &optional messagebuf)
  "Append MSG to dotemacs buffer. If MESSAGEBUF is not nil then MSG is
 also written in message buffer."
  (with-current-buffer (get-buffer-create "*dotemacs*")
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert msg)
      (if messagebuf (message "(emacs) %s" msg)))
    (dotemacs-buffer/set-mode-line "")))

(defun dotemacs-buffer/replace-last-line (msg &optional messagebuf)
  "Replace the last line of the dotemacs buffer with MSG. If MESSAGEBUF is
 not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create "*dotemacs*")
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (delete-region (line-beginning-position) (point-max))
      (insert msg)
      (if messagebuf (message "(emacs) %s" msg)))
    (dotemacs-buffer/set-mode-line "")))

;; logo into buffer
(defconst dotemacs-logo-file (locate-user-emacs-file "logo.png")
  "The path to my logo.")

(defconst dotemacs-logo-url "http://ibasetsolumina.wpengine.com/wp-content/uploads/2014/08/logo.jpg"
  "The URL of my logo.")

(defun dotemacs-insert-logo ()
  "Insert my logo into the current buffer."
  (interactive)
  (unless (file-exists-p dotemacs-logo-file)
    (url-copy-file dotemacs-logo-url dotemacs-logo-file
                   nil 'keep-time))
  (insert-image (create-image dotemacs-logo-file) "logo")
  (insert "\n"))

(defun dotemacs-insert-logo-into-scratch ()
  "Insert my logo into the scratch buffer."
  (goto-char (point-min))
  (dotemacs-insert-logo))

(defun dotemacs-buffer/goto-buffer ()
  "Create the special buffer for `dotemacs-buffer-mode' if it doesn't
already exist, and switch to it."
  (interactive)
  (unless (buffer-live-p (get-buffer dotemacs-buffer-name))
    (with-current-buffer (get-buffer-create dotemacs-buffer-name)
      (save-excursion
        (dotemacs-buffer/set-mode-line "")
        ;; needed in case the buffer was deleted and we are recreating it
        (setq dotemacs-buffer--note-widgets nil)
        ;; start scratch in text mode (usefull to get a faster Emacs load time
        ;; because it avoids autoloads of elisp modes)
        (setq initial-major-mode 'text-mode)
        ; (dotemacs-buffer/insert-banner-and-buttons)
        ;; non-nil if emacs is loaded
        (if after-init-time
            (progn
              ; (when dotemacs-startup-lists
              ;   (dotemacs-buffer/insert-startupify-lists))
              (dotemacs-buffer/set-mode-line dotemacs--default-mode-line)
              (force-mode-line-update)
              (dotemacs-buffer-mode))
          (add-hook 'emacs-startup-hook
                    (lambda ()
                      (with-current-buffer (get-buffer dotemacs-buffer-name)
                        (dotemacs-insert-logo-into-scratch)
                        ; (when dotemacs-startup-lists
                        ;   (dotemacs-buffer/insert-startupify-lists))
                        (force-mode-line-update)
                        (dotemacs-buffer-mode)
                        ; (dotemacs-buffer/goto-link-line)
                        )) t)))))
  ; (dotemacs-buffer/goto-link-line)
  (switch-to-buffer dotemacs-buffer-name)
  (dotemacs-redisplay))

(provide 'core-buffers)
;;; core-buffers.el ends here
