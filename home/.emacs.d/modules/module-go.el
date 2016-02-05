;;; module-go.el --- Go Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-vars)
;; (require 'core-funcs)
;; (require 'core-keybindings)
;; (require 'core-display-init)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

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
        (dotemacs-set-leader-keys-for-major-mode 'go-mode
          "ro" 'go-oracle-set-scope
          "r<" 'go-oracle-callers
          "r>" 'go-oracle-callees
          "rc" 'go-oracle-peers
          "rd" 'go-oracle-definition
          "rf" 'go-oracle-freevars
          "rg" 'go-oracle-callgraph
          "ri" 'go-oracle-implements
          "rp" 'go-oracle-pointsto
          "rr" 'go-oracle-referrers
          "rs" 'go-oracle-callstack
          "rt" 'go-oracle-describe)))))

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

(dotemacs-defvar-company-backends go-mode)

(defvar go-use-gocheck-for-testing nil
  "If using gocheck for testing when running the tests -check.f
  will be used instead of -run to specify the test that will be
  ran. Gocheck is mandatory for testing suites.")

(use-package go-rename
  :disabled t
  :init
  (dotemacs-set-leader-keys-for-major-mode 'go-mode "rn" 'go-rename))

(use-package go-mode
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save)

    (dotemacs-set-leader-keys-for-major-mode 'go-mode
      "hh" 'godoc-at-point
      "ig" 'go-goto-imports
      "ia" 'go-import-add
      "ir" 'go-remove-unused-imports
      "eb" 'go-play-buffer
      "er" 'go-play-region
      "ed" 'go-download-play
      "xx" 'dotemacs/go-run-main
      "ga" 'ff-find-other-file
      "gg" 'godef-jump
      "tt" 'dotemacs/go-run-test-current-function
      "ts" 'dotemacs/go-run-test-current-suite
      "tp" 'dotemacs/go-run-package-tests
      "tP" 'dotemacs/go-run-package-tests-nested)))

(use-package go-eldoc
  :disabled t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'go-mode))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook go-mode))))

(use-package company-go
  :if (eq dotemacs-completion-engine 'company)
  :ensure t
  :defer t
  :init
  (progn
    (push 'company-go company-backends-go-mode)))

(provide 'module-go)
;;; module-go.el ends here
