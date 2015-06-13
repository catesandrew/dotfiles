(defun dotemacs-id-of-bundle (bundle)
  "Get the ID of a BUNDLE.

BUNDLE is the user-visible name of the bundle as string.  Return
the id of the bundle as string.

These bundle IDs are normally constant.  Thus you may use this
function to determine the ID once, and then hard-code it in your
code."
  (let ((script (format "id of app \"%s\"" bundle)))
    (car (process-lines "osascript" "-e" script))))

(defun dotemacs-path-of-bundle (id)
  "Get the path of a bundle with ID.

ID is the bundle ID (see `my-id-of-bundle' as string.  Return
the directory path of the bundle as string."
  (let ((query (format "kMDItemCFBundleIdentifier == '%s'" id)))
    (car (process-lines "mdfind" query))))

(defun dotemacs-homebrew-prefix (&optional formula)
  "Get the homebrew prefix for FORMULA.

Without FORMULA, get the homebrew prefix itself.

Return nil, if homebrew is not available, or if the prefix
directory does not exist."
  (let ((prefix (ignore-errors (car (apply #'process-lines "brew" "--prefix"
                                           (when formula (list formula)))))))
    (when (and prefix (file-directory-p prefix))
      prefix)))

(defun dotemacs-homebrew-installed-p (&optional formula)
  "Determine whether a homebrew FORMULA is installed.

Without FORMULA determine whether Homebrew itself is available."
  (if formula
      (my-homebrew-prefix formula)
    (executable-find "brew")))

;; Open files
(defun dotemacs-open-current-file ()
  "Open current file using shell `open` command"
  (interactive)
  (shell-command (concat "open " (buffer-file-name))))

(provide 'init-macosx)