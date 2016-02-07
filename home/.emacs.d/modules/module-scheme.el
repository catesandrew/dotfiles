;;; module-scheme.el --- Scheme Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'use-package)
(require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
(require 'core-transient-state)
(require 'core-auto-completion)
(require 'core-use-package-ext)
(require 'core-fonts-support)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(dotemacs-defvar-company-backends geiser-mode)

(use-package geiser
  :ensure t
  :commands run-geiser
  :init (dotemacs-register-repl 'geiser 'geiser-mode-switch-to-repl "geiser")
  :config
  (progn
    (dotemacs-declare-prefix-for-mode 'scheme-mode "mc" "compiling")
    (dotemacs-declare-prefix-for-mode 'scheme-mode "mg" "navigation")
    (dotemacs-declare-prefix-for-mode 'scheme-mode "mh" "documentation")
    (dotemacs-declare-prefix-for-mode 'scheme-mode "mi" "insertion")
    (dotemacs-declare-prefix-for-mode 'scheme-mode "mm" "macroexpansion")
    (dotemacs-declare-prefix-for-mode 'scheme-mode "ms" "repl")

    (dotemacs-set-leader-keys-for-major-mode 'scheme-mode
      "'"  'geiser-mode-switch-to-repl
      ","  'lisp-state-toggle-lisp-state

      "cc" 'geiser-compile-current-buffer
      "cp" 'geiser-add-to-load-path

      "eb" 'geiser-eval-buffer
      "ee" 'geiser-eval-last-sexp
      "ef" 'geiser-eval-definition
      "el" 'lisp-state-eval-sexp-end-of-line
      "er" 'geiser-eval-region

      "gg" 'geiser-edit-symbol-at-point
      "gb" 'geiser-pop-symbol-stack
      "gm" 'geiser-edit-module
      "gn" 'next-error
      "gN" 'previous-error

      "hh" 'geiser-doc-symbol-at-point
      "hd" 'geiser-doc-look-up-manual
      "hm" 'geiser-doc-module
      "h<" 'geiser-xref-callers
      "h>" 'geiser-xref-callees

      "il" 'geiser-insert-lambda

      "me" 'geiser-expand-last-sexp
      "mf" 'geiser-expand-definition
      "mx" 'geiser-expand-region

      "si" 'geiser-mode-switch-to-repl
      "sb" 'geiser-eval-buffer
      "sB" 'geiser-eval-buffer-and-go
      "sf" 'geiser-eval-definition
      "sF" 'geiser-eval-definition-and-go
      "se" 'geiser-eval-last-sexp
      "sr" 'geiser-eval-region
      "sR" 'geiser-eval-region-and-go
      "ss" 'geiser-set-scheme)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    ;; Geiser provides completion as long as company mode is loaded.
    (dotemacs-add-company-hook geiser-mode)))

(provide 'module-scheme)
;;; module-scheme.el ends here
