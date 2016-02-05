;;; module-ocaml.el --- OCaml Module
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

(use-package opam                       ; Initialize Emacs with OPAM env
  :ensure t
  :init (opam-init))

(use-package tuareg                     ; OCaml editing
  :ensure t
  :defer t
  :init
  (progn
    ;; (dotemacs//init-ocaml-opam)
    (dotemacs-set-leader-keys-for-major-mode 'tuareg-mode
      "ga" 'tuareg-find-alternate-file
      "cc" 'compile)
    ;; Make OCaml-generated files invisible to filename completion
    (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt" ".annot"))
      (add-to-list 'completion-ignored-extensions ext)))
  :config
  (progn
    (when (fboundp 'sp-local-pair)
      ;; don't auto-close apostrophes (type 'a = foo) and backticks (`Foo)
      (sp-local-pair 'tuareg-mode "'" nil :actions nil)
      (sp-local-pair 'tuareg-mode "`" nil :actions nil))

    ;; Disable SMIE indentation in Tuareg.  It's just broken currently…
    (setq tuareg-use-smie nil)

    ;; Please, Tuareg, don't kill my imenu
    (define-key tuareg-mode-map [?\C-c ?i] nil)))

(use-package merlin                     ; Powerful Emacs backend for OCaml
  :ensure t
  :defer t
  :init (add-hook 'tuareg-mode-hook #'merlin-mode)
  :config
  ;; Use Merlin from current OPAM env
  (setq merlin-command 'opam
        ;; Disable Merlin's own error checking in favour of Flycheck
        merlin-error-after-save nil))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'tuareg-mode))

(use-package flycheck-ocaml             ; Check OCaml code with Merlin
  :ensure t
  :commands (flycheck-ocaml-setup)
  :init (add-hook 'flycheck-mode-hook #'flycheck-ocaml-setup))

(provide 'module-ocaml)
;;; module-ocaml.el ends here
