;;; module-elm.el --- Elm Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-vars)
;; (require 'core-funcs)
;; (require 'core-keybindings)
;; (require 'core-display-init)
(require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(dotemacs-defvar-company-backends elm-mode)

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook elm-mode)
      (add-hook 'elm-mode-hook 'elm-oracle-setup-completion))))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'elm-mode))

(use-package flycheck-elm
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook 'flycheck-elm-setup t))

(use-package elm-mode
  :mode ("\\.elm\\'" . elm-mode)
  :ensure t
  :init
  (progn
    (defun dotemacs/init-elm-mode ()
      "Disable electric-indent-mode and let indentation cycling feature work"
      (if (fboundp 'electric-indent-local-mode)
          (electric-indent-local-mode -1)))

    (add-hook 'elm-mode-hook 'dotemacs/init-elm-mode))
  :config
  (progn
    (push "\\*elm\\*" dotemacs-useful-buffers-regexp)

    (defun dotemacs/elm-compile-buffer-output ()
      (interactive)
      (let* ((fname (format "%s.js" (downcase (file-name-base (buffer-file-name))))))
        (elm-compile--file (elm--buffer-local-file-name) fname)))

    (defun dotemacs/elm-repl-push-decl-focus ()
      "Send current function to the REPL and focus it in insert state."
      (interactive)
      (elm-repl-push-decl)
      (run-elm-interactive)
      (evil-insert-state))

    (defun dotemacs/elm-repl-push-focus ()
      "Send current region to the REPL and focus it in insert state."
      (elm-repl-push)
      (run-elm-interactive)
      (evil-insert-state))

    (dotemacs-set-leader-keys-for-major-mode 'elm-mode
      ;; make
      "cb" 'elm-compile-buffer
      "cB" 'dotemacs/elm-compile-buffer-output
      "cm" 'elm-compile-main

      ;; oracle
      "ht" 'elm-oracle-type-at-point

      ;; repl
      "si" 'elm-repl-load
      "sf" 'elm-repl-push-decl
      "sF" 'dotemacs/elm-repl-push-decl-focus
      "sr" 'elm-repl-push
      "sR" 'dotemacs/elm-repl-push-focus

      ;; reactor
      "Rn" 'elm-preview-buffer
      "Rm" 'elm-preview-main

      ;; package
      "pi" 'elm-import
      "pc" 'elm-package-catalog
      "pd" 'elm-documentation-lookup)

    (dolist (x '(("mR" . "reactor")
                 ("mc" . "compile")
                 ("mh" . "help")
                 ("mp" . "package")
                 ("ms" . "repl")))
      (dotemacs-declare-prefix-for-mode 'elm-mode (car x) (cdr x)))

    (evilified-state-evilify elm-package-mode elm-package-mode-map
      "g" 'elm-package-refresh
      "n" 'elm-package-next
      "p" 'elm-package-prev
      "v" 'elm-package-view
      "m" 'elm-package-mark
      "u" 'elm-package-unmark
      "x" 'elm-package-install
      "q" 'quit-window)))

(dotemacs-use-package-add-hook smartparens
  :post-init
   (if dotemacs-smartparens-strict-mode
      (add-hook 'elm-mode-hook #'smartparens-strict-mode)
    (add-hook 'elm-mode-hook #'smartparens-mode)))

(provide 'module-elm)
;;; module-elm.el ends here
