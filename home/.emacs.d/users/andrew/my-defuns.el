;; run clojure.test at point, with fixture (hardcoded I know)

(defun run-deftest-at-point ()
  (interactive)
  (save-excursion
    (search-backward "deftest")
    (search-forward "(")
    (backward-char)
    (let* ((beg (point))
           (end (forward-list))
           (contents (buffer-substring-no-properties beg end)))
      (nrepl-find-and-clear-repl-buffer)
      (nrepl-interactive-eval
       (concat "(h/reset-state-fixture (fn [] " contents "))")))))

(provide 'my-defuns)
