(require 'clojure-mode)
(require 'cider)

(after 'clojure-mode
     (defun my-clojure-mode-defaults ()
       (subword-mode +1)
       (run-hooks 'my-lisp-coding-hook))

     (setq my-clojure-mode-hook 'my-clojure-mode-defaults)
     (add-hook 'clojure-mode-hook (lambda ()
                                    (run-hooks 'my-clojure-mode-hook))))

(after 'cider
     (setq nrepl-log-messages t)

     (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

     (defun my-cider-repl-mode-defaults ()
       (subword-mode +1)
       (run-hooks 'my-interactive-lisp-coding-hook))

     (setq my-cider-repl-mode-hook 'my-cider-repl-mode-defaults)
     (add-hook 'cider-repl-mode-hook (lambda ()
                                       (run-hooks 'my-cider-repl-mode-hook))))


(provide 'init-clojure)
