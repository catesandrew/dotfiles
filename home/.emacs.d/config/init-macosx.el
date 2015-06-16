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

(defun dotemacs-copy-from-osx ()
  "Copies the current clipboard content using the `pbcopy` command"
  (shell-command-to-string "pbpaste"))

(defun dotemacs-paste-to-osx (text &optional push)
  "Copies the top of the kill ring stack to the OSX clipboard"
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

;; Get keychain password

;; If I'm on OS X, I can fetch passwords etc. from my Keychain. This
;; is much more secure than storing them in configuration on disk:

(defun dotemacs-chomp (str)
  "Chomp leading and tailing whitespace from `str'."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str))) str)

(defun dotemacs-get-keychain-password (account-name)
  "Get `account-name' keychain password from OS X Keychain"
  (interactive "sAccount name: ")
  (when (executable-find "security")
    (dotemacs-chomp
     (shell-command-to-string
      (concat
       "security find-generic-password -wa "
       account-name)))))

(provide 'init-macosx)
