(defun dotemacs-preview-md-file ()
  "use Marked 2 to preview the current file"
  (interactive)
  (shell-command
   (format "open -a 'Marked 2.app' %s"
       (shell-quote-argument (buffer-file-name)))))

(provide 'init-markdown)
