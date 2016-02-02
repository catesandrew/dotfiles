;;; Hippie Exp
(require 'module-global)

(use-package hippie-exp                 ; Powerful expansion and completion
  :bind (([remap dabbrev-expand] . hippie-expand))
  :init
  :config
  (progn
    ;; replace dabbrev-expand
    (global-set-key (kbd "M-/") 'hippie-expand)
    (define-key evil-insert-state-map (kbd "C-p") 'hippie-expand)
    (setq hippie-expand-try-functions-list '(
          ;; Try to expand yasnippet snippets based on prefix
          yas-hippie-try-expand
          ;; Try to expand word "dynamically", searching the current buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-list
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-line
          ;; Try to complete as an Emacs Lisp symbol, as many characters as
          ;; unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol))))

(provide 'module-hippie-exp)
;;; module-hippie-exp.el ends here
