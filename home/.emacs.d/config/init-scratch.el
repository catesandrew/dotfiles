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
  (with-current-buffer "*scratch*"
    (goto-char (point-min))
    (dotemacs-insert-logo)
    (insert "\n")))

(provide 'init-scratch)
