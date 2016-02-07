;;; module-elisp.el --- ELisp Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-vars)
;; (require 'core-funcs)
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

(dotemacs-defvar-company-backends emacs-lisp-mode)
(dotemacs-defvar-company-backends ielm-mode)

(use-package ielm                       ; Emacs Lisp REPL
  :defer t
  :init
  (progn
    (dotemacs-register-repl 'ielm 'ielm)
    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (dotemacs-declare-prefix-for-mode mode "ms" "ielm")
      (dotemacs-set-leader-keys-for-major-mode mode
        "'" 'ielm
        "si" 'ielm)))
  :config
  (progn
    (defun ielm-indent-line ()
      (interactive)
      (let ((current-point (point)))
        (save-restriction
          (narrow-to-region (search-backward-regexp "^ELISP>") (goto-char current-point))
          (lisp-indent-line))))))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (push '(company-files company-capf) company-backends-ielm-mode)
      (dotemacs-add-company-hook ielm-mode))))

(dotemacs-use-package-add-hook eldoc
  :post-init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package auto-compile
  :defer t
  :ensure t
  :diminish (auto-compile-mode . "")
  :init
  (progn
    (setq auto-compile-display-buffer nil
          ;; lets spaceline manage the mode-line
          auto-compile-use-mode-line nil
          auto-compile-mode-line-counter t)
    (add-hook 'emacs-lisp-mode-hook 'auto-compile-mode))
  :config
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'emacs-lisp-mode
      "cl" 'auto-compile-display-log)))

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
        "hh" 'elisp-slime-nav-describe-elisp-thing-at-point))))

(use-package elisp-mode                  ; Emacs Lisp editing
  :init
  (progn
    (defun dotemacs-ert-run-tests-buffer ()
      "Run all the tests in the current buffer."
      (interactive)
      (save-buffer)
      (load-file (buffer-file-name))
      (ert t))

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

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (push 'company-capf company-backends-emacs-lisp-mode)
      (dotemacs-add-company-hook emacs-lisp-mode))))

(use-package macrostep                  ; Interactively expand macros in code
  :ensure t
  :defer t
  :mode ("\\*.el\\'" . emacs-lisp-mode)
  :init
  (progn
    (evil-define-key 'normal macrostep-keymap "q" 'macrostep-collapse-all)
    (dotemacs-define-transient-state macrostep
      :title "MacroStep Transient State"
      :doc "\n[_e_] expand [_c_] collapse [_n_/_N_] next/previous [_q_] quit"
      :foreign-keys run
      :bindings
      ("e" macrostep-expand)
      ("c" macrostep-collapse)
      ("n" macrostep-next-macro)
      ("N" macrostep-prev-macro)
      ("q" macrostep-collapse-all :exit t))
    (dotemacs-set-leader-keys-for-major-mode 'emacs-lisp-mode
      "dm" 'dotemacs/macrostep-transient-state/body)))

(with-eval-after-load 'evil
  (progn
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (dotemacs-define-text-object ";" "elisp-comment" ";; " "")))))

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

(use-package pcre2el                    ; Convert regexps to RX and back
  :ensure t
  :defer t
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
      "Rt"  'rxt-toggle-elisp-rx)))

(use-package flycheck-cask              ; Setup Flycheck by Cask projects
  :ensure t
  :commands (flycheck-cask-setup)
  :init (add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(use-package flycheck-package           ; Check package conventions with Flycheck
  :ensure t
  :defer t
  :init (with-eval-after-load 'flycheck (flycheck-package-setup)))

(provide 'module-elisp)
;;; module-elisp.el ends here
