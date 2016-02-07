;;; module-clisp.el --- Common Lisp Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
(require 'core-transient-state)
(require 'core-auto-completion)
(require 'core-fonts-support)
;; (require 'core-display-init)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(dotemacs-use-package-add-hook auto-highlight-symbol
  :post-init
  (with-eval-after-load 'auto-highlight-symbol
    (add-to-list 'ahs-plugin-bod-modes 'lisp-mode)))

(use-package slime
  :ensure t
  :commands slime-mode
  :init
  (progn
    (when-let (clisp (executable-find "clisp"))
      (setq inferior-lisp-program clisp))

    (defun common-lisp/init-common-lisp-snippets ())

    (dotemacs-register-repl 'slime 'slime)
    (setq slime-contribs '(slime-fancy
                           slime-indentation
                           slime-sbcl-exts
                           slime-scratch)
          inferior-lisp-program "sbcl")

    ;; enable fuzzy matching in code buffer and SLIME REPL
    (setq slime-complete-symbol*-fancy t)
    (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

    (defun slime/disable-smartparens ()
      (smartparens-strict-mode -1)
      (turn-off-smartparens-mode))
    (add-hook 'slime-repl-mode-hook #'slime/disable-smartparens)
    (dotemacs/add-to-hooks 'slime-mode '(lisp-mode-hook)))
  :config
  (progn
    (slime-setup)
    (dolist (m `(,slime-mode-map ,slime-repl-mode-map))
      (define-key m [(tab)] 'slime-fuzzy-complete-symbol))

    ;; TODO: Add bindings for the SLIME debugger?
    (dotemacs-set-leader-keys-for-major-mode 'lisp-mode
      "cc" 'slime-compile-file
      "cC" 'slime-compile-and-load-file
      "cl" 'slime-load-file
      "cf" 'slime-compile-defun
      "cr" 'slime-compile-region
      "cn" 'slime-remove-notes

      "eb" 'slime-eval-buffer
      "ef" 'slime-eval-defun
      "eF" 'slime-undefine-function
      "ee" 'slime-eval-last-sexp
      "er" 'slime-eval-region

      "gg" 'slime-inspect-definition
      "gb" 'slime-pop-find-definition-stack
      "gn" 'slime-next-note
      "gN" 'slime-previous-note

      "ha" 'slime-apropos
      "hA" 'slime-apropos-all
      "hd" 'slime-disassemble-symbol
      "hh" 'slime-describe-symbol
      "hH" 'slime-hyperspec-lookup
      "hp" 'slime-apropos-package
      "ht" 'slime-toggle-trace-fdefinition
      "hT" 'slime-untrace-all
      "h<" 'slime-who-calls
      "h>" 'slime-calls-who
      ;; TODO: Add key bindings for who binds/sets globals?
      "hr" 'slime-who-references
      "hm" 'slime-who-macroexpands
      "hs" 'slime-who-specializes

      "ma" 'slime-macroexpand-all
      "mo" 'slime-macroexpand-1

      "se" 'slime-eval-last-expression-in-repl
      "si" 'slime
      "sq" 'slime-quit-lisp

      "tf" 'slime-toggle-fancy-trace)))

(provide 'module-clisp)
;;; module-clisp.el ends here
