;; TODO to be deleted
(require 'init-programming)

(defun my-handlebars-load ()
  (require 'handlebars-mode)
  (handlebars-mode))

(setq my-handlebars-load-hook 'my-handlebars-load)

(add-to-list 'auto-mode-alist
             '("\\.hbs$" . (lambda ()
                              (require 'handlebars-mode)
                              (handlebars-mode))))

(add-to-list 'auto-mode-alist
             '("\\.handlebars$" . (lambda ()
                              (require 'handlebars-mode)
                              (handlebars-mode))))

(with-eval-after-load 'handlebars-mode
  (defun my-handlebars-mode-defaults ()
    ; (toggle-truncate-lines 1)
    ; (setq truncate-lines 0)
    (run-hooks 'my-prog-mode-hook))

  (setq my-handlebars-mode-hook 'my-handlebars-mode-defaults)

  (add-hook 'handlebars-mode-hook (lambda ()
                                    (run-hooks 'my-handlebars-mode-hook))))

(provide 'init-hbs)
