(dotemacs-defvar-company-backends go-mode)

(defun load-gopath-file(gopath name)
  "Search for NAME file in all paths referenced in GOPATH."
  (let ((paths (split-string gopath ":"))
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
          "moo" 'go-oracle-set-scope
          "mo<" 'go-oracle-callers
          "mo>" 'go-oracle-callees
          "moc" 'go-oracle-peers
          "mod" 'go-oracle-definition
          "mof" 'go-oracle-freevars
          "mog" 'go-oracle-callgraph
          "moi" 'go-oracle-implements
          "mop" 'go-oracle-pointsto
          "mor" 'go-oracle-referrers
          "mos" 'go-oracle-callstack
          "mot" 'go-oracle-describe)))))

(defun dotemacs-go-run-package-tests ()
  (interactive)
  (shell-command "go test"))

(provide 'init-go)
