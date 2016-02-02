;;; ELisp
(require 'module-global)

;;; Emacs Lisp
(dotemacs-defvar-company-backends emacs-lisp-mode)
(dotemacs-defvar-company-backends ielm-mode)

(use-package helm-elisp                 ; Helm commands for Emacs Lisp
  :ensure helm
  :bind (("C-c f l" . helm-locate-library)
         ("C-c h a" . helm-apropos)))

(use-package elisp-slime-nav            ; Jump to definition of symbol at point
  ;; Elisp go-to-definition with M-. and back again with M-,
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (dotemacs-declare-prefix-for-mode mode "mg" "find-symbol")
      (dotemacs-declare-prefix-for-mode mode "mh" "help")
      (dotemacs-set-leader-keys-for-major-mode mode
        "gg" 'elisp-slime-nav-find-elisp-thing-at-point
        "hh" 'elisp-slime-nav-describe-elisp-thing-at-point)))
  :config
  (defadvice elisp-slime-nav-find-elisp-thing-at-point
      (after advice-for-elisp-slime-nav-find-elisp-thing-at-point activate)
    (recenter))
  :diminish elisp-slime-nav-mode)

(use-package flycheck-cask              ; Setup Flycheck by Cask projects
  :ensure t
  :commands (flycheck-cask-setup)
  :init (add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(use-package flycheck-package           ; Check package conventions with Flycheck
  :ensure t
  :defer t
  :init (with-eval-after-load 'flycheck (flycheck-package-setup)))

(use-package slime
  :ensure t
  :commands slime-mode
  :init
  (progn
    (setq slime-contribs '(slime-fancy
                           slime-indentation
                           slime-sbcl-exts
                           slime-scratch))

    (when-let (clisp (executable-find "clisp"))
      (setq inferior-lisp-program clisp))

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

(dotemacs-defvar-company-backends geiser-mode)

(use-package geiser
  :ensure t
  :commands run-geiser
  :config
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'scheme-mode
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
    (progn
      ;; Geiser provides completion as long as company mode is loaded.
      (dotemacs-add-company-hook geiser-mode))))

(use-package pcre2el                    ; Convert regexps to RX and back
  :ensure t
  :defer t
  :commands rxt-fontify-regexp-at-point
  :init
  (progn
    (dotemacs-declare-prefix "R" "pcre2el")
    (dotemacs-set-leader-keys
      "R/"  'rxt-explain
      "Rc"  'rxt-convert-syntax
      "Rx"  'rxt-convert-to-rx
      "R'"  'rxt-convert-to-strings
      "Rpe" 'rxt-pcre-to-elisp
      "R%"  'pcre-query-replace-regexp
      "Rpx" 'rxt-pcre-to-rx
      "Rps" 'rxt-pcre-to-sre
      "Rp'" 'rxt-pcre-to-strings
      "Rp/" 'rxt-explain-pcre
      "Re/" 'rxt-explain-elisp
      "Rep" 'rxt-elisp-to-pcre
      "Rex" 'rxt-elisp-to-rx
      "Res" 'rxt-elisp-to-sre
      "Re'" 'rxt-elisp-to-strings
      "Ret" 'rxt-toggle-elisp-rx
      "Rt"  'rxt-toggle-elisp-rx
      "Rh"  'rxt-fontify-regexp-at-point)))

(use-package visual-regexp-steroids
  :defer t)

(use-package macrostep                  ; Interactively expand macros in code
  :ensure t
  :defer t
  :mode ("\\*.el\\'" . emacs-lisp-mode)
  :init
  (progn
    (evil-define-key 'normal macrostep-keymap "q" 'macrostep-collapse-all)
    (dotemacs-define-micro-state macrostep
      :doc "[e] expand [c] collapse [n/N] next/previous [q] quit"
      :disable-evil-leader t
      :persistent t
      :evil-leader-for-mode (emacs-lisp-mode . "dm")
      :bindings
      ("e" macrostep-expand)
      ("c" macrostep-collapse)
      ("n" macrostep-next-macro)
      ("N" macrostep-prev-macro)
      ("q" macrostep-collapse-all :exit t))))

(use-package ielm                       ; Emacs Lisp REPL
  :config
  (progn
    (defun ielm-indent-line ()
      (interactive)
      (let ((current-point (point)))
        (save-restriction
          (narrow-to-region (search-backward-regexp "^ELISP>") (goto-char current-point))
          (lisp-indent-line))))
    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (dotemacs-declare-prefix-for-mode mode "ms" "ielm")
      (dotemacs-set-leader-keys-for-major-mode mode
        "si" 'ielm))))

(use-package elisp-mode                  ; Emacs Lisp editing
  :defer t
  :interpreter ("emacs" . emacs-lisp-mode)
  :mode ("/Cask\\'" . emacs-lisp-mode)
  :config
  (progn
    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (dotemacs-declare-prefix-for-mode mode "mc" "compile")
      (dotemacs-declare-prefix-for-mode mode "me" "eval")
      (dotemacs-declare-prefix-for-mode mode "mt" "tests")
      (dotemacs-set-leader-keys-for-major-mode mode
        "cc" 'emacs-lisp-byte-compile
        "e$" 'lisp-state-eval-sexp-end-of-line
        "eb" 'eval-buffer
        "ee" 'eval-last-sexp
        "er" 'eval-region
        "ef" 'eval-defun
        "el" 'lisp-state-eval-sexp-end-of-line
        ","  'lisp-state-toggle-lisp-state
        "tb" 'dotemacs-ert-run-tests-buffer
        "tq" 'ert))))

(use-package eldoc                      ; Documentation in minibuffer
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  :config
  (progn
    (setq-default eldoc-documentation-function #'describe-char-eldoc)
    ;; enable eldoc in `eval-expression'
    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
    ;; enable eldoc in IELM
    (add-hook 'ielm-mode-hook #'eldoc-mode)
    ;; don't display eldoc on modeline
    (dotemacs-hide-lighter eldoc-mode)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (push 'company-capf company-backends-emacs-lisp-mode)
      (push '(company-files company-capf) company-backends-ielm-mode)
      (dotemacs-add-company-hook ielm-mode)
      (dotemacs-add-company-hook emacs-lisp-mode))))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (progn
    (dolist (mode '(emacs-lisp-mode lisp-mode))
      (dotemacs/add-flycheck-hook mode))
    ;; Don't activate flycheck by default in elisp
    ;; because of too much false warnings
    ;; (dotemacs-add-flycheck-hook 'emacs-lisp-mode-hook)

    ;; Make flycheck recognize packages in loadpath
    ;; i.e (require 'company) will not give an error now
    (setq flycheck-emacs-lisp-load-path 'inherit)))

(with-eval-after-load 'evil
  (progn
    (dotemacs-define-text-object ";" "elisp-comment" ";; " "")))

(dotemacs-use-package-add-hook semantic
  :post-init
  (progn
    (semantic/enable-semantic-mode 'emacs-lisp-mode)
    (with-eval-after-load 'semantic
      (semantic-default-elisp-setup))))

(dotemacs-use-package-add-hook srefactor
  :post-init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'dotemacs-lazy-load-srefactor)
    (use-package srefactor-lisp
      :commands (srefactor-lisp-format-buffer
                 srefactor-lisp-format-defun
                 srefactor-lisp-format-sexp
                 srefactor-lisp-one-line)
      :init
      (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
        (dotemacs-declare-prefix-for-mode mode "=" "srefactor")
        (dotemacs-set-leader-keys-for-major-mode mode
          "=b" 'srefactor-lisp-format-buffer
          "=d" 'srefactor-lisp-format-defun
          "=o" 'srefactor-lisp-one-line
          "=s" 'srefactor-lisp-format-sexp)))))

(provide 'module-elisp)
;;; module-elisp.el ends here
