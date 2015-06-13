(require 'flycheck)

(defcustom flycheck-auto-scalastyle-version '("0.6.0" . "2.10")
  "Version of scala style to use for Flycheck.

A pair of `(VERSION . SCALA-VERSION)'."
  :type '(cons (string :tag "Scalastyle version")
               (string :tag "Scala language version"))
  :group 'flycheck
  :safe '(lambda (value)
           (and (consp value) (stringp (car value)) (stringp (cdr value)))))

(defcustom flycheck-auto-scalastyle-jar-dir (locate-user-emacs-file "scalastyle")
  "Directory for installed Scalastyle JARs."
  :type 'directory
  :group 'flycheck)

(defsubst flycheck-auto-scalastyle-jar-name ()
  "Get the file name of the Scalastyle JAR."
  (pcase-let ((`(,version . ,scala-version) flycheck-auto-scalastyle-version))
    (format "scalastyle_%s-%s-batch.jar" scala-version version)))

(defsubst flycheck-auto-scalastyle-url ()
  "Get the URL to download Scalastyle."
  (pcase-let ((`(,version . ,scala-version) flycheck-auto-scalastyle-version))
    (format "https://oss.sonatype.org/content/repositories/releases/org/scalastyle/scalastyle_%s/%s/%s"
            scala-version version (flycheck-auto-scalastyle-jar-name))))

(defun flycheck-auto-scalastyle-ensure ()
  "Ensure and return the scalastyle JAR for this buffer."
  (let ((file-name (expand-file-name (flycheck-auto-scalastyle-jar-name)
                                    flycheck-auto-scalastyle-jar-dir)))
    (unless (file-exists-p file-name)
      (make-directory flycheck-auto-scalastyle-jar-dir 'parents)
      (message "Downloading scalastyle JAR")
      (url-copy-file (flycheck-auto-scalastyle-url) file-name))

    file-name))

;;;###autoload
(defun flycheck-auto-scalastyle-configure ()
  "Configure Scalastyle for this buffer."
  (interactive)
  (setq flycheck-scalastyle-jar (flycheck-auto-scalastyle-ensure)))

;;;###autoload
(defun flycheck-auto-scalastyle-setup ()
  "Setup Flycheck Scalastyle for this buffer.

For use in `flycheck-mode-hook'."
  (add-hook 'hack-local-variables-hook #'flycheck-auto-scalastyle-configure))

(provide 'flycheck-auto-scalastyle)
