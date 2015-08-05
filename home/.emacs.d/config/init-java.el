(defun dotemacs-java-completing-dot ()
  "Insert a period and show company completions."
  (interactive "*")
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t))
  (dotemacs-java-delete-horizontal-space)
  (insert ".")
  (company-emacs-eclim 'interactive))

(defun dotemacs-java-completing-double-colon ()
  "Insert double colon and show company completions."
  (interactive "*")
  (dotemacs-java-delete-horizontal-space)
  (insert ":")
  (let ((curr (point)))
    (when (s-matches? (buffer-substring (- curr 2) (- curr 1)) ":")
      (company-emacs-eclim 'interactive))))

(defun dotemacs-java-delete-horizontal-space ()
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t)))

(defun dotemacs-java-maven-test ()
  (interactive)
  (eclim-maven-run "test"))

(defun dotemacs-java-maven-clean-install ()
  (interactive)
  (eclim-maven-run "clean install"))

(defun dotemacs-java-maven-install ()
  (interactive)
  (eclim-maven-run "install"))

(provide 'init-java)
