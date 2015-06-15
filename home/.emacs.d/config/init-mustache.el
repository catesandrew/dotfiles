;; TODO to be deleted
(require 'init-programming)

(add-to-list 'auto-mode-alist '("\\.mustache\\'"    . mustache-mode))

(with-eval-after-load 'mustache-mode

  (defun my-mustache-mode-defaults ()
    (run-hooks 'my-prog-mode-hook))

  (setq my-mustache-mode-hook 'my-mustache-mode-defaults)

  (add-hook 'mustache-mode-hook (lambda ()
                                  (run-hooks 'my-mustache-mode-hook))))

(provide 'init-mustache)
