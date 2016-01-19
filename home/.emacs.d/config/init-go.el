(defun load-gopath-file(gopath name)
  "Search for NAME file in all paths referenced in GOPATH."
  (let* ((sep (if (dotemacs/system-is-mswindows) ";" ":"))
         (paths (split-string gopath sep))
         found)
    (loop for p in paths
          for file = (concat p name) when (file-exists-p file)
          do
          (load-file file)
          (setq found t)
          finally return found)))

(defun go/init-go-oracle()
  (let ((go-path (getenv "GOPATH")))
    (if (not go-path)
        (dotemacs-buffer/warning
         "GOPATH variable not found, go-oracle configuration skipped.")
      (when (load-gopath-file
             go-path "/src/golang.org/x/tools/cmd/oracle/oracle.el")
        (evil-leader/set-key-for-mode 'go-mode
          "mro" 'go-oracle-set-scope
          "mr<" 'go-oracle-callers
          "mr>" 'go-oracle-callees
          "mrc" 'go-oracle-peers
          "mrd" 'go-oracle-definition
          "mrf" 'go-oracle-freevars
          "mrg" 'go-oracle-callgraph
          "mri" 'go-oracle-implements
          "mrp" 'go-oracle-pointsto
          "mrr" 'go-oracle-referrers
          "mrs" 'go-oracle-callstack
          "mrt" 'go-oracle-describe)))))

(defun dotemacs/go-run-main ()
  (interactive)
  (shell-command
   (format "go run %s"
           (shell-quote-argument (buffer-file-name)))))

(defun dotemacs/go-run-tests (args)
  (interactive)
  (save-selected-window
    (async-shell-command (concat "go test " args))))

(defun dotemacs/go-run-package-tests ()
  (interactive)
  (dotemacs/go-run-tests ""))

(defun dotemacs/go-run-package-tests-nested ()
  (interactive)
  (dotemacs/go-run-tests "./..."))

(defun dotemacs/go-run-test-current-function ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (let ((test-method (if go-use-gocheck-for-testing
                             "-check.f"
                           "-run")))
        (save-excursion
          (re-search-backward "^func[ ]+([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\(Test[[:alnum:]]+\\)(.*)")
          (dotemacs/go-run-tests (concat test-method "='" (match-string-no-properties 2) "'"))))
    (message "Must be in a _test.go file to run go-run-test-current-function")))

(defun dotemacs/go-run-test-current-suite ()
  (interactive)
  (if (string-match "_test\.go" buffer-file-name)
      (if go-use-gocheck-for-testing
          (save-excursion
            (re-search-backward "^func[ ]+([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\(Test[[:alnum:]]+\\)(.*)")
            (dotemacs/go-run-tests (concat "-check.f='" (match-string-no-properties 1) "'")))
        (message "Gocheck is needed to test the current suite"))
    (message "Must be in a _test.go file to run go-test-current-suite")))

(provide 'init-go)
;;; init-go.el ends here
