(defun dotemacs-racket-test-with-coverage ()
  "Call `racket-test' with universal argument."
  (interactive)
  (racket-test t))

(defun dotemacs-racket-run-and-switch-to-repl ()
  "Call `racket-run-and-switch-to-repl' and enable
`insert state'."
  (interactive)
  (racket-run-and-switch-to-repl)
  (evil-insert-state))

(defun dotemacs-racket-send-last-sexp-focus ()
  "Call `racket-send-last-sexp' and switch to REPL buffer in
`insert state'."
  (interactive)
  (racket-send-last-sexp)
  (racket-repl)
  (evil-insert-state))

(defun dotemacs-racket-send-definition-focus ()
  "Call `racket-send-definition' and switch to REPL buffer in
`insert state'."
  (interactive)
  (racket-send-definition)
  (racket-repl)
  (evil-insert-state))

(defun dotemacs-racket-send-region-focus (start end)
  "Call `racket-send-region' and switch to REPL buffer in
`insert state'."
  (interactive "r")
  (racket-send-region start end)
  (racket-repl)
  (evil-insert-state))

(provide 'init-racket)
