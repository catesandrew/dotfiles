;;; module-ocaml.el --- OCaml Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'use-package)
;; (require 'core-vars)
(require 'core-funcs)
(require 'core-buffers)
(require 'core-keybindings)
;; (require 'core-display-init)
(require 'core-use-package-ext)
(require 'core-auto-completion)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:
(dotemacs-defvar-company-backends merlin-mode)

(defun dotemacs//init-ocaml-opam ()
  (if (executable-find "opam")
      (let* ((output (shell-command-to-string
                      "opam config var share 2> /dev/null"))
             (share (when (< 0 (length output))
                      (substring output 0 -1))))
        (when share
          (setq opam-share share
                opam-load-path (concat share "/emacs/site-lisp")))
        (add-to-list 'load-path opam-load-path))
    (dotemacs-buffer/warning
     (concat "Cannot find \"opam\" executable. "
             "The ocaml layer won't work properly."))))

;; packages:

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (dotemacs-add-company-hook merlin-mode)))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'merlin-mode)
  (use-package flycheck-ocaml             ; Check OCaml code with Merlin
    :ensure t
    :defer t
    :init
    (progn
      (with-eval-after-load 'merlin
        (setq merlin-error-after-save nil)
        (flycheck-ocaml-setup)))))

(use-package merlin                     ; Powerful Emacs backend for OCaml
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'tuareg-mode-hook 'merlin-mode)
    ;; (set-default 'merlin-use-auto-complete-mode t)
    (set-default 'merlin-use-auto-complete-mode nil)
    (setq merlin-completion-with-doc t)
    (push 'merlin-company-backend company-backends-merlin-mode)
    (dotemacs-set-leader-keys-for-major-mode 'tuareg-mode
      "cp" 'merlin-project-check
      "cr" 'merlin-refresh
      "cv" 'merlin-goto-project-file
      "eC" 'merlin-error-check
      "en" 'merlin-error-next
      "eN" 'merlin-error-prev
      "gb" 'merlin-pop-stack
      "gg" #'(lambda ()
               (interactive)
               (let ((merlin-locate-in-new-window 'never))
                 (merlin-locate)))
      "gG" #'(lambda ()
               (interactive)
               (let ((merlin-locate-in-new-window 'always))
                 (merlin-locate)))
      "gl" 'merlin-locate-ident
      "gi" 'merlin-switch-to-ml
      "gI" 'merlin-switch-to-mli
      "hh" 'merlin-document
      "ht" 'merlin-type-enclosing
      "hT" 'merlin-type-expr
      "rd" 'merlin-destruct))
  :config
  ;; Use Merlin from current OPAM env
  (setq merlin-command 'opam
        ;; Disable Merlin's own error checking in favour of Flycheck
        merlin-error-after-save nil))

(use-package ocp-indent
  :ensure t
  :defer t
  :init
  (add-hook 'tuareg-mode-hook 'ocp-indent-caml-mode-setup))

(dotemacs-use-package-add-hook smartparens
  :post-init
  (with-eval-after-load 'smartparens
    ;; don't auto-close apostrophes (type 'a = foo) and backticks (`Foo)
    (sp-local-pair 'tuareg-mode "'" nil :actions nil)
    (sp-local-pair 'tuareg-mode "`" nil :actions nil)))

(use-package tuareg                     ; OCaml editing
  :ensure t
  :defer t
  :init
  (progn
    (dotemacs//init-ocaml-opam)
    (dotemacs-set-leader-keys-for-major-mode 'tuareg-mode
      "ga" 'tuareg-find-alternate-file
      "cc" 'compile)
    ;; Make OCaml-generated files invisible to filename completion
    (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt" ".annot"))
      (add-to-list 'completion-ignored-extensions ext))))

(use-package utop
    :defer t
    :init
    (progn
      (add-hook 'tuareg-mode-hook 'utop-minor-mode)
      (dotemacs-register-repl 'utop 'utop "ocaml"))
    :config
    (progn
      ;; Setup environment variables using opam
      (if (executable-find "opam")
          (let ((vars (car (read-from-string
                            (shell-command-to-string "opam config env --sexp")))))
            (dolist (var vars)
              (setenv (car var) (cadr var))))
        (spacemacs-buffer/warning "Cannot find \"opam\" executable."))
      ;; Update the emacs path
      (setq exec-path (append (parse-colon-path (getenv "PATH"))
                              (list exec-directory)))

      (defun dotemacs/utop-eval-phrase-and-go ()
        "Send phrase to REPL and evaluate it and switch to the REPL in
`insert state'"
        (interactive)
        (utop-eval-phrase)
        (utop)
        (evil-insert-state))

      (defun dotemacs/utop-eval-buffer-and-go ()
        "Send buffer to REPL and evaluate it and switch to the REPL in
`insert state'"
        (interactive)
        (utop-eval-buffer)
        (utop)
        (evil-insert-state))

      (defun dotemacs/utop-eval-region-and-go (start end)
        "Send region to REPL and evaluate it and switch to the REPL in
`insert state'"
        (interactive "r")
        (utop-eval-region start end)
        (utop)
        (evil-insert-state))

      (dotemacs-set-leader-keys-for-major-mode 'tuareg-mode
        "'"  'utop
        "sb" 'utop-eval-buffer
        "sB" 'dotemacs/utop-eval-buffer-and-go
        "si" 'utop
        "sp" 'utop-eval-phrase
        "sP" 'dotemacs/utop-eval-phrase-and-go
        "sr" 'utop-eval-region
        "sR" 'dotemacs/utop-eval-region-and-go))
    (define-key utop-mode-map (kbd "C-j") 'utop-history-goto-next)
    (define-key utop-mode-map (kbd "C-k") 'utop-history-goto-prev))

(provide 'module-ocaml)
;;; module-ocaml.el ends here
