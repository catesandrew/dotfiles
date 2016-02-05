;;; module-c-c++.el --- C/C++ Module
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
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(defvar c-c++-enable-clang-support nil
  "If non nil Clang related packages and configuration are enabled.")

(dotemacs-defvar-company-backends c-mode-common)
(dotemacs-defvar-company-backends cmake-mode)

(defvar c-c++-default-mode-for-headers 'c-mode
  "Default mode to open header files. Can be `c-mode' or `c++-mode'.")

(defun company-mode/find-clang-complete-file ()
  (when buffer-file-name
    (let ((dir (locate-dominating-file buffer-file-name ".clang_complete")))
      (when dir
        (concat (file-name-as-directory dir) ".clang_complete")))))

(defun company-mode/load-clang-complete-file (cc-file)
  "Load the flags from CC-FILE, one flag per line."
  (let ((invocation-dir (expand-file-name (file-name-directory cc-file)))
        (case-fold-search nil)
        compile-flags)
    (with-temp-buffer
      (insert-file-contents cc-file)
      ;; Replace relative paths with absolute paths (by @trishume)
      ;; (goto-char (point-min))
      (while (re-search-forward "\\(-I\\|-isystem\n\\)\\(\\S-\\)" nil t)
        (replace-match (format "%s%s" (match-string 1)
                               (expand-file-name (match-string 2) invocation-dir))))
      ;; Turn lines into a list
      (setq compile-flags
            ;; remove whitespaces at the end of each line, if any
            (mapcar #'(lambda (line)
                        (if (string-match "[ \t]+$" line)
                            (replace-match "" t t line)
                          line))
                    (split-string (buffer-string) "\n" t))))
    compile-flags))

(defun company-mode/more-than-prefix-guesser ()
  (unless company-clang-arguments
    (let* ((cc-file (company-mode/find-clang-complete-file))
           (flags (if cc-file (company-mode/load-clang-complete-file cc-file) '())))
      (setq-local company-clang-arguments flags)
      (setq flycheck-clang-args flags)))
  (company-clang-guess-prefix))

(defun c-c++/post-init-helm-gtags ()
  (dotemacs-helm-gtags-define-keys-for-mode 'c-mode)
  (dotemacs-helm-gtags-define-keys-for-mode 'c++-mode))

(defun c-c++/post-init-semantic ()
  (semantic/enable-semantic-mode 'c-mode)
  (semantic/enable-semantic-mode 'c++-mode))

(use-package cc-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist `("\\.h$" . ,dotemacs-c-c++-default-mode-for-headers))
  :config
  (progn
    (require 'compile)
    (c-toggle-auto-newline 1)
    (dotemacs-set-leader-keys-for-major-mode 'c-mode
      "ga" 'projectile-find-other-file
      "gA" 'projectile-find-other-file-other-window)
    (dotemacs-set-leader-keys-for-major-mode 'c++-mode
      "ga" 'projectile-find-other-file
      "gA" 'projectile-find-other-file-other-window)))

(use-package clang-format
  :ensure t
  :if dotemacs-c-c++-enable-clang-support)

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
  :init (push 'company-cmake company-backends-cmake-mode))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook c-mode-common)
      (dotemacs-add-company-hook cmake-mode)

      (when dotemacs-c-c++-enable-clang-support
        (push 'company-clang company-backends-c-mode-common)
        ;; .clang_complete file loading
        ;; Sets the arguments for company-clang based on a project-specific text file.
        (setq company-clang-prefix-guesser 'company-mode/more-than-prefix-guesser)))))

(use-package company-c-headers
  :if (eq dotemacs-completion-engine 'company)
  :ensure t
  :defer t
  :init
  (progn
    (push 'company-c-headers company-backends-c-mode-common)))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dolist (mode '(c-mode c++-mode))
    (dotemacs/add-flycheck-hook mode)))

(use-package gdb-mi
  :ensure t
  :defer t
  :init
  (setq
   ;; use gdb-many-windows by default when `M-x gdb'
   gdb-many-windows t
   ;; Non-nil means display source file containing the main routine at startup
   gdb-show-main t))

(dotemacs-use-package-add-hook helm-gtags
  :post-init
  (progn
    (dotemacs-helm-gtags-define-keys-for-mode 'c-mode)
    (dotemacs-helm-gtags-define-keys-for-mode 'c++-mode)))

(dotemacs-use-package-add-hook semantic
  :post-init
  (progn
    (semantic/enable-semantic-mode 'c-mode)
    (semantic/enable-semantic-mode 'c++-mode)))

(dotemacs-use-package-add-hook srefactor
  :post-init
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'c-mode "r" 'srefactor-refactor-at-point)
    (dotemacs-set-leader-keys-for-major-mode 'c++-mode "r" 'srefactor-refactor-at-point)
    (dotemacs/add-to-hooks 'dotemacs-lazy-load-srefactor '(c-mode-hook c++-mode-hook)) ))

(dotemacs-use-package-add-hook stickyfunc-enhance
  :post-init
  (progn
    (dotemacs/add-to-hooks 'dotemacs-lazy-load-stickyfunc-enhance '(c-mode-hook c++-mode-hook) )))

(dotemacs-use-package-add-hook ycmd
  :post-init
  (progn
    (add-hook 'c++-mode-hook 'ycmd-mode)
    (dotemacs-set-leader-keys-for-major-mode 'c++-mode
      "gg" 'ycmd-goto
      "gG" 'ycmd-goto-imprecise)))

(dotemacs-use-package-add-hook company-ycmd
  :post-init
  (progn
    (push 'company-ycmd company-backends-c-mode-common)))

(use-package srefactor
  :ensure t
  :init
  (progn
    (defun dotemacs-lazy-load-srefactor ()
      "Lazy load the package."
      (require 'srefactor)
      ;; currently, evil-mode overrides key mapping of srefactor menu
      ;; must expplicity enable evil-emacs-state. This is ok since
      ;; srefactor supports j,k,/ and ? commands when Evil is
      ;; available
      (add-hook 'srefactor-ui-menu-mode-hook 'evil-emacs-state))))

(use-package stickyfunc-enhance
  :ensure t
  :init
  (defun dotemacs-lazy-load-stickyfunc-enhance ()
    "Lazy load the package."
    (require 'stickyfunc-enhance)))

(provide 'module-c-c++)
;;; module-c-c++ ends here
