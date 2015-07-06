(defun dotemacs-java-completing-dot ()
  "Insert a period and show company completions."
  (interactive "*")
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t))
  (insert ".")
  (company-emacs-eclim 'interactive))

(defun dotemacs-java-maven-test ()
  (interactive)
  (eclim-maven-run "test"))

(defun dotemacs-java-maven-clean-install ()
  (interactive)
  (eclim-maven-run "clean install"))

(defun dotemacs-java-maven-install ()
  (interactive)
  (eclim-maven-run "install"))
