;;; PureScript
(require 'module-global)

(use-package purescript-mode
  :defer t
  :ensure t
  :init
  (progn
    (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)
    (dotemacs-set-leader-keys-for-major-mode 'purescript-mode
      "i="  'purescript-mode-format-imports
      "i`"  'purescript-navigate-imports-return
      "ia"  'purescript-align-imports
      "in"  'purescript-navigate-imports)))

(with-eval-after-load 'flycheck
  (flycheck-define-checker purs-check
    "Use purscheck to flycheck PureScript code."
    :command ("purscheck" source source-original temporary-file-name)
    :error-patterns
    ((error line-start
            (or (and "Error at " (file-name)    " line " line ", column " column ":" (zero-or-more " "))
                (and "\""        (file-name) "\" (line " line ", column " column "):"))
            (or (message (one-or-more not-newline))
                (and "\n"
                     (message
                      (zero-or-more " ") (one-or-more not-newline)
                      (zero-or-more "\n"
                                    (zero-or-more " ")
                                    (one-or-more not-newline)))))
            line-end))
    :modes purescript-mode)
  (add-to-list 'flycheck-checkers 'purs-check))

; Waiting on purscheck to make it to melpa
; (dotemacs-use-package-add-hook flycheck
;   :post-init
;   (dotemacs/add-flycheck-hook 'purescript-mode))

(use-package psci
  :defer t
  :ensure t
  :init
  (progn
    (add-hook 'purescript-mode-hook 'inferior-psci-mode)
    (dotemacs-set-leader-keys-for-major-mode 'purescript-mode
      "sb" 'psci/load-current-file!
      "si" 'psci
      "sm" 'psci/load-module!
      "sp" 'psci/load-project-modules!)))

(provide 'module-purescript)
;;; module-purescript.el ends here
