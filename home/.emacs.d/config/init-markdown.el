(require 'skeleton)
(require 'rx)
(require 'subr-x)

(defun dotemacs-default-title ()
  "Get the default post title for the current buffer."
  (when (buffer-file-name)
    (let* ((fn (file-name-base (buffer-file-name)))
           (words (split-string fn "-" 'omit-nulls (rx (1+ space)))))
      (string-join (mapcar #'capitalize words) " "))))

(defun dotemacs-preview-md-file ()
  "use Marked 2 to preview the current file"
  (interactive)
  (shell-command
   (format "open -a 'Marked 2.app' %s"
       (shell-quote-argument (buffer-file-name)))))

(define-skeleton dotemacs-markdown-post-header
  "Insert a header for blog posts."
  (read-from-minibuffer "Title: " (dotemacs-default-title))
  "---\n"
  "title: " str "\n"
  "tags: " ("Tag: " str ",") & -1 "\n"
  "published: " (format-time-string "%F") "\n"
  "---\n\n"
  -)

(provide 'init-markdown)
