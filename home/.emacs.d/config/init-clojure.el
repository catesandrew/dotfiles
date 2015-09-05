(defun dotemacs-clojure-fancify-symbols (mode)
  "Pretty symbols for Clojure's anonymous functions and sets,
   like (λ [a] (+ a 5)), ƒ(+ % 5), and ∈{2 4 6}."
  (font-lock-add-keywords mode
    `(("(\\(fn\\)[\[[:space:]]"
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1) "λ"))))
      ("(\\(partial\\)[\[[:space:]]"
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1) "Ƥ"))))
      ("(\\(comp\\)[\[[:space:]]"
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1) "∘"))))
      ("\\(#\\)("
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1) "ƒ"))))
      ("\\(#\\){"
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1) "∈")))))))

(defun dotemacs-cider-eval-in-repl-no-focus (form)
  "Insert FORM in the REPL buffer and eval it."
  (let ((start-pos (point)))
    (while (string-match "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" form)
      (setq form (replace-match "" t t form)))
    (with-current-buffer (cider-current-repl-buffer)
      (insert form)
      (indent-region start-pos (point))
      (cider-repl-return))))

(defun dotemacs-cider-send-last-sexp-to-repl ()
  "Send last sexp to REPL and evaluate it without changing the focus."
  (interactive)
  (dotemacs-cider-eval-in-repl-no-focus (cider-last-sexp)))

(defun dotemacs-cider-send-last-sexp-to-repl-focus ()
  "Send last sexp to REPL and evaluate it and switch to the REPL in `insert state'."
  (interactive)
  (cider-insert-last-sexp-in-repl t)
  (evil-insert-state))

(defun dotemacs-cider-send-region-to-repl (start end)
  "Send region to REPL and evaluate it without changing the focus."
  (interactive "r")
  (dotemacs-cider-eval-in-repl-no-focus
   (buffer-substring-no-properties start end)))

(defun dotemacs-cider-send-region-to-repl-focus (start end)
  "Send region to REPL and evaluate it and switch to the REPL in `insert state'."
  (interactive "r")
  (cider-insert-in-repl
   (buffer-substring-no-properties start end) t)
  (evil-insert-state))

(defun dotemacs-cider-send-function-to-repl ()
  "Send current function to REPL and evaluate it without changing the focus."
  (interactive)
  (dotemacs-cider-eval-in-repl-no-focus (cider-defun-at-point)))

(defun dotemacs-cider-send-function-to-repl-focus ()
  "Send current function to REPL and evaluate it and switch to the REPL in `insert state'."
  (interactive)
  (cider-insert-defun-in-repl t)
  (evil-insert-state))

(defun dotemacs-cider-send-ns-form-to-repl ()
  "Send buffer's ns form to REPL and evaluate it without changing the focus."
  (interactive)
  (dotemacs-cider-eval-in-repl-no-focus (cider-ns-form)))

(defun dotemacs-cider-send-ns-form-to-repl-focus ()
  "Send ns form to REPL and evaluate it and switch to the REPL in `insert state'."
  (interactive)
  (cider-insert-ns-form-in-repl t)
  (evil-insert-state))

(defun dotemacs-cider-send-buffer-in-repl-and-focus ()
  "Send the current buffer in the REPL and switch to the REPL in `insert state'."
  (interactive)
  (cider-load-buffer)
  (cider-switch-to-repl-buffer)
  (evil-insert-state))

(defun dotemacs-cider-test-run-focused-test ()
  (interactive)
  (cider-load-buffer)
  (dotemacs-cider-eval-in-repl-no-focus (cider-test-run-test)))

(defun dotemacs-cider-test-run-all-tests ()
  (interactive)
  (cider-load-buffer)
  (dotemacs-cider-eval-in-repl-no-focus (cider-test-run-tests nil)))

(defun dotemacs-cider-test-rerun-tests ()
  (interactive)
  (cider-load-buffer)
  (dotemacs-cider-eval-in-repl-no-focus (cider-test-rerun-tests)))

(provide 'init-clojure)
