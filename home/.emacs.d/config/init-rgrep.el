(defun dotemacs-rgrep-quit-window ()
  (interactive)
  (kill-buffer)
  (jump-to-register ?$))

(defun dotemacs-rgrep-goto-file-and-close-rgrep ()
  (interactive)
  (compile-goto-error)
  (kill-buffer "*grep*")
  (delete-other-windows)
  (message "Type C-x r j $ to return to pre-rgrep windows."))

;; --no-color, oddly enough, is required to allow emacs to colorize the output
(defvar git-grep-switches "--extended-regexp -I -n --ignore-case --no-color"
  "Switches to pass to `git grep'.")

(provide 'init-rgrep)
