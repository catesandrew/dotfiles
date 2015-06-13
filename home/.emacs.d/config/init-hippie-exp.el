(require 'init-lisp)

(defun dotemacs-try-complete-lisp-symbol-without-namespace (old)
  "Hippie expand \"try\" function which expands \"-foo\" to \"modname-foo\" in elisp."
  (unless old
    (he-init-string (he-lisp-symbol-beg) (point))
    (when (string-prefix-p "-" he-search-string)
      (let ((mod-name (dotemacs-emacs-lisp-current-feature)))
        (when mod-name
          (setq he-expand-list (list (concat mod-name he-search-string)))))))

  (when he-expand-list
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list nil)
    t))

(provide 'init-hippie-exp)
