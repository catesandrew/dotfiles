;;; Shell Script
(require 'module-global)

(use-package sh-script                  ; Shell scripts
  :defer t
  :init
  (progn
    ;; Use two spaces in shell scripts.
    (setq sh-indentation 2
          sh-basic-offset 2)

    ;; Use sh-mode when opening `.zsh' files, and when opening Prezto runcoms.
    (dolist (pattern '("\\.zsh\\'"
                       "zlogin\\'"
                       "zlogout\\'"
                       "zpreztorc\\'"
                       "zprofile\\'"
                       "zshenv\\'"
                       "zshrc\\'"))
      (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))

    (defun dotemacs//setup-shell ()
      (when (and buffer-file-name
                 (string-match-p "\\.zsh\\'" buffer-file-name))
        (sh-set-shell "zsh")))
    (add-hook 'sh-mode-hook 'dotemacs//setup-shell)))

(provide 'module-shell-script)
;;; module-shell-script.el ends here
