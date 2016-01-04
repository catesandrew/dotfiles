(defun dotemacs-buffer/set-mode-line (format)
  "Set mode-line format for dotemacs buffer."
  (with-current-buffer (get-buffer-create "*dotemacs*")
    (setq mode-line-format format)))

(defun dotemacs-buffer/message (msg &rest args)
  "Display MSG in message prepended with '(emacs)'.
The message is displayed only if `dotemacs-verbose-loading' is non nil."
  (when dotemacs-verbose-loading
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

(provide 'core-buffers)
