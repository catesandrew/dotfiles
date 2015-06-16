;; Don't kill the important buffers
(defconst dotemacs-do-not-kill-buffer-names '("*scratch*" "*Messages*" "*Require Times*")
  "Names of buffers that should not be killed.")

(defun dotemacs-do-not-kill-important-buffers ()
  "Inhibit killing of important buffers.

Add this to `kill-buffer-query-functions'."
  (if (not (member (buffer-name) dotemacs-do-not-kill-buffer-names))
      t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (bury-buffer)
    nil))

(defun dotemacs-force-save-some-buffers ()
  "Save all modified buffers, without prompts."
  (save-some-buffers 'dont-ask))

(provide 'init-buffers)
