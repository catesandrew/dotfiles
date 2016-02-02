;;; Programming Tails
(require 'module-global)

(dotemacs-use-package-add-hook smartparens
  :post-init
  (progn
    (if (version< emacs-version "24.4")
        (ad-disable-advice 'preceding-sexp 'around 'evil)
      (advice-remove 'elisp--preceding-sexp 'evil--preceding-sexp))

    ;; but alwayws enable for lisp mode
    (dotemacs/add-to-hooks 'smartparens-strict-mode '(lisp-mode))

    (defun dotemacs-eval-current-form-sp (&optional arg)
      "Call `eval-last-sexp' after moving out of one level of
parentheses. Will exit any strings and/or comments first.
Requires smartparens because all movement is done using
`sp-up-sexp'. An optional ARG can be used which is passed to
`sp-up-sexp' to move out of more than one sexp."
      (interactive "p")
      (require 'smartparens)
      (save-excursion
        (let ((max 10))
          (while (and (> max 0)
                      (sp-point-in-string-or-comment))
            (decf max)
            (sp-up-sexp)))
        (sp-up-sexp arg)
        (call-interactively 'eval-last-sexp)))

    (defun dotemacs-eval-current-symbol-sp ()
      "Call `eval-last-sexp' on the symbol underneath the
point. Requires smartparens because all movement is done using
`sp-forward-symbol'."
      (interactive)
      (require 'smartparens)
      (save-excursion
        (sp-forward-symbol)
        (call-interactively 'eval-last-sexp)))

    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (dotemacs-set-leader-keys-for-major-mode mode
        "ec" 'dotemacs-eval-current-form-sp
        "es" 'dotemacs-eval-current-symbol-sp))))

(provide 'module-prog-tail)
;;; module-prog-tail.el ends here
