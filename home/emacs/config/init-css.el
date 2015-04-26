(require 'rainbow-mode)

(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

(after 'css-mode

  (setq css-indent-offset 2)

  (defun my-css-mode-defaults ()
    (rainbow-mode +1)
    (run-hooks 'my-prog-mode-hook))

  (setq my-css-mode-hook 'my-css-mode-defaults)

  (add-hook 'css-mode-hook (lambda ()
                           (run-hooks 'my-css-mode-hook))))

(provide 'init-css)
