(lazy-major-mode "\\.md$" markdown-mode)
(lazy-major-mode "\\.toml$" toml-mode)
(lazy-major-mode "\\.yaml$" yaml-mode)

;; disable auto indent
(add-hook 'yaml-mode-hook (lambda () (electric-indent-local-mode -1)))
(add-hook 'toml-mode-hook (lambda () (electric-indent-local-mode -1)))
(add-hook 'markdown-mode-hook (lambda () (electric-indent-local-mode -1)))

(provide 'init-markup)

