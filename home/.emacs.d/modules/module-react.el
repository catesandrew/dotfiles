;;; module-react.el --- React Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'core-use-package-ext)
(require 'core-auto-completion)
;; (require 'core-vars)
;; (require 'core-funcs)
;; (require 'core-keybindings)
;; (require 'core-display-init)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(dotemacs-defvar-company-backends react-mode)

; todo: update eslint to local for react-mode
(defun dotemacs-react-mode-defaults ()
  "Default react-mode coding hook."
  (unless (bound-and-true-p my-react-mh-ran)
    (set (make-local-variable 'my-react-mh-ran) t)))

(setq dotemacs-react-mode-hook #'dotemacs-react-mode-defaults)
(add-hook 'react-mode-hook (lambda () (run-hooks #'dotemacs-react-mode-hook)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook react-mode)))

  (dotemacs-use-package-add-hook company-tern
    :post-init
    (progn
      (push 'company-capf company-backends-react-mode))))

(dotemacs-use-package-add-hook flycheck
  :post-config
  (progn
    (dotemacs-flycheck-executables-search)
    (when (bound-and-true-p dotemacs//flycheck-executables-searched)
      (when dotemacs//flycheck-executable-eslint
        (flycheck-add-mode 'javascript-eslint 'react-mode)))))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (progn
    (dotemacs/add-flycheck-hook 'react-mode)
    (add-hook 'react-mode-hook 'dotemacs-flycheck-init-react)))

(dotemacs-use-package-add-hook js-doc
  :post-init
  (add-hook 'react-mode-hook 'dotemacs/js-doc-require)
  (dotemacs/js-doc-set-key-bindings 'react-mode))

(dotemacs-use-package-add-hook js2-mode
  :post-init
  (add-hook 'react-mode-hook 'js2-imenu-extras-mode)
  (add-hook 'react-mode-hook 'js2-minor-mode))

(dotemacs-use-package-add-hook js2-refactor
  :post-init
  (add-hook 'react-mode-hook 'dotemacs/js2-refactor-require)
  (dotemacs/js2-refactor-set-key-bindings 'react-mode))

(dotemacs-use-package-add-hook tern
  :post-init
  (add-hook 'react-mode-hook 'tern-mode))

(dotemacs-use-package-add-hook web-beautify
  :post-init
  (dotemacs-set-leader-keys-for-major-mode 'react-mode  "=" 'web-beautify-js))

(dotemacs-use-package-add-hook web-mode
  :post-init
  (define-derived-mode react-mode web-mode "react")
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . react-mode))
  (add-to-list 'auto-mode-alist '("\\.react.js\\'" . react-mode))
  (add-to-list 'auto-mode-alist '("\\index.android.js\\'" . react-mode))
  (add-to-list 'auto-mode-alist '("\\index.ios.js\\'" . react-mode))
  (add-to-list 'magic-mode-alist '("/** @jsx React.DOM */" . react-mode))
  (defun dotemacs//setup-react-mode ()
    "Adjust web-mode to accommodate react-mode"
    (emmet-mode 0)
    ;; See https://github.com/CestDiego/emmet-mode/commit/3f2904196e856d31b9c95794d2682c4c7365db23
    (setq-local emmet-expand-jsx-className? t)
    ;; Enable js-mode snippets
    (yas-activate-extra-mode 'js-mode)
    ;; Force jsx content type
    (web-mode-set-content-type "jsx")
    ;; Don't auto-quote attribute values
    (setq-local web-mode-enable-auto-quoting nil)
    ;; Why do we do this ?
    (defadvice web-mode-highlight-part (around tweak-jsx activate)
      (let ((web-mode-enable-part-face nil))
        ad-do-it)))
  (add-hook 'react-mode-hook 'dotemacs//setup-react-mode))

(provide 'module-react)
;;; module-react.el ends here
