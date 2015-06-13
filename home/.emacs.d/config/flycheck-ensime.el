(require 'flycheck)
(require 'ensime)

(defun flycheck-verify-ensime ()
  "Verify the Ensime syntax checker."
  (list
   (flycheck-verification-result-new
    :label "Ensime Mode"
    :message (if ensime-mode "Enabled" "Disabled")
    :face (if ensime-mode 'success '(bold warning)))
   (flycheck-verification-result-new
    :label "Ensime connection"
    :message (if (ensime-connected-p) "open" "closed")
    :face (if (ensime-connected-p) 'success '(bold warning)))))

(defun flycheck-ensime-parse-note (note checker)
  "Parse a single Ensime NOTE for CHECKER into an error."
  (let ((severity (plist-get note :severity)))
    (unless (symbolp severity)
      (setq severity (intern severity)))
    (flycheck-error-new-at
     (plist-get note :line)
     (plist-get note :col)
     severity (plist-get note :msg)
     :checker checker
     :filename (plist-get note :file)
     :buffer (current-buffer))))

(defun flycheck-ensime-parse-notes (notes checker)
  "Parse Ensime NOTES for CHECKER into Flycheck errors."
  (mapcar (lambda (n) (flycheck-ensime-parse-note n checker)) notes))

(defun flycheck-ensime-start (checker callback)
  "Start a syntax CHECKER with Ensime."
  (condition-case err
      (let* ((notes (ensime-scala-compiler-notes (ensime-connection)))
             (errors (flycheck-ensime-parse-notes notes checker)))
        (funcall callback 'finished errors))
    (error (funcall callback 'errored (error-message-string err)))))

(flycheck-define-generic-checker 'scala-ensime
  "A Scala syntax checker using Ensime.

See URL `https://github.com/ensime/ensime-emacs'."
  :start #'flycheck-ensime-start
  :verify #'flycheck-verify-ensime
  :modes '(scala-mode)
  :predicate (lambda () (and ensime-mode (ensime-connection-or-nil)))
  :next-checkers '((warning . scala-scalastyle)))

(defun flycheck-ensime-setup ()
  "Setup Flycheck for Ensime."
  (interactive)
  (add-to-list 'flycheck-checkers 'scala-ensime)
  (advice-add 'ensime-make-note-overlays :override #'ignore
              '((name . flycheck-ensime-disable-ensime-overlays))))

(provide 'flycheck-ensime)
