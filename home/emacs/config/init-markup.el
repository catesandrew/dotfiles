(lazy-major-mode "\\.md$" markdown-mode)
(lazy-major-mode "\\.toml$" toml-mode)
(lazy-major-mode "\\.yaml$" yaml-mode)

;; disable auto indent
(add-hook 'yaml-mode-hook (lambda () (electric-indent-local-mode -1)))
(add-hook 'toml-mode-hook (lambda () (electric-indent-local-mode -1)))
(add-hook 'markdown-mode-hook (lambda () (electric-indent-local-mode -1)))

;; markdown
(setq markdown-imenu-generic-expression
      '(("title"  "^\\(.*\\)[\n]=+$" 1)
        ("h2-"    "^\\(.*\\)[\n]-+$" 1)
        ("h1"   "^# \\(.*\\)$" 1)
        ("h2"   "^## \\(.*\\)$" 1)
        ("h3"   "^### \\(.*\\)$" 1)
        ("h4"   "^#### \\(.*\\)$" 1)
        ("h5"   "^##### \\(.*\\)$" 1)
        ("h6"   "^###### \\(.*\\)$" 1)
        ("fn"   "^\\[\\^\\(.*\\)\\]" 1)
        ))

(add-hook 'markdown-mode-hook
          (lambda ()
            (setq imenu-generic-expression markdown-imenu-generic-expression)))


(provide 'init-markup)

