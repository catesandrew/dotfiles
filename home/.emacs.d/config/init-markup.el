(lazy-major-mode "\\.md$" markdown-mode)
(lazy-major-mode "\\.markdown$" markdown-mode)
(lazy-major-mode "\\.apib$" markdown-mode)
(lazy-major-mode "\\.toml$" toml-mode)
(lazy-major-mode "\\.yaml$" yaml-mode)
(lazy-major-mode "\\.yml$" yaml-mode)

;; disable auto indent
(add-hook 'yaml-mode-hook (lambda () (electric-indent-local-mode -1)))
(add-hook 'toml-mode-hook (lambda () (electric-indent-local-mode -1)))
(add-hook 'markdown-mode-hook (lambda () (electric-indent-local-mode -1)))

;; TODO: gfm-mode

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

(when on_darwin
  (setq markdown-open-command "mark"))

(defun markdown-preview-file ()
  "use Marked 2 to preview the current file"
  (interactive)
  (shell-command
   (format "open -a 'Marked 2.app' %s"
       (shell-quote-argument (buffer-file-name)))))

; (when on_darwin
;   (global-set-key (kbd "C-cm") 'markdown-preview-file))


(provide 'init-markup)
