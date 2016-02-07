;;; module-yasnippet.el --- YASnippet Module
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
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(use-package yasnippet
  :ensure t
  :commands (yas-global-mode yas-minor-mode)
  :init
  (progn
    ;; We don't want undefined variable errors
    (defvar yas-global-mode nil)

    ;; disable yas minor mode map, use hippie-expand instead
    (setq yas-minor-mode-map (make-sparse-keymap)
          ;; stop polluting messages buffer
          yas-verbosity 0
          ;; allow nested expansions
          yas-triggers-in-field t
          ;; add key into candidate list
          helm-yas-display-key-on-candidate t)
    ;; on multiple keys, fall back to completing read typically this means helm
    (setq yas-prompt-functions '(yas-completing-prompt))
    ;; disable yas minor mode map use hippie-expand instead
    (setq yas-minor-mode-map (make-sparse-keymap))
    ;; this makes it easy to get out of a nested expansion
    (define-key yas-minor-mode-map (kbd "M-s-/") 'yas-next-field)

    ;; configure snippet directories
    (let* ((dotemacs--auto-completion-dir (configuration-layer/get-layer-property 'auto-completion :dir))
           (private-yas-dir (if auto-completion-private-snippets-directory
                                auto-completion-private-snippets-directory
                              (concat dotemacs-private-dir "/snippets/")))
           (dotemacs-directory-snippets-dir (when user-emacs-directory
                                              (expand-file-name
                                               "snippets/"
                                               user-emacs-directory))))
      (setq yas-snippet-dirs nil)
      ;; ~/.emacs.d/elpa/yasnippet-xxxxx/snippets
      (push 'yas-installed-snippets-dir yas-snippet-dirs)
      ;; ~/.emacs.d/snippets
      (when dotemacs-directory-snippets-dir
        (push dotemacs-directory-snippets-dir yas-snippet-dirs))
      ;; arbitrary directories in `auto-completion-private-snippets-directory'
      (when private-yas-dir
        (if (listp private-yas-dir)
            (setq yas-snippet-dirs (append yas-snippet-dirs private-yas-dir))
          (push private-yas-dir yas-snippet-dirs))))

    (defun dotemacs/load-yasnippet ()
      (unless yas-global-mode (yas-global-mode 1))
      (yas-minor-mode 1))
    (dotemacs/add-to-hooks 'dotemacs/load-yasnippet '(prog-mode-hook
                                                      markdown-mode-hook
                                                      org-mode-hook))
    (dotemacs-add-toggle yasnippet
      :status yas-minor-mode
      :on (yas-minor-mode)
      :off (yas-minor-mode -1)
      :documentation "Enable snippets"
      :evil-leader "ty")

    (defun dotemacs/force-yasnippet-off ()
      (yas-minor-mode -1)
      (setq yas-dont-activate t))
    (dotemacs/add-to-hooks 'dotemacs/force-yasnippet-off '(term-mode-hook
                                                           shell-mode-hook
                                                           eshell-mode-hook)))
  :config (dotemacs-diminish yas-minor-mode " ⓨ" " y"))

(use-package auto-yasnippet
  :ensure t
  :defer t
  :init
  (progn
    (setq aya-persist-snippets-dir
          (or auto-completion-private-snippets-directory
              (concat dotemacs-private-dir "snippets/")))
    (defun dotemacs/auto-yasnippet-expand ()
      "Call `yas-expand' and switch to `insert state'"
      (interactive)
      (call-interactively 'aya-expand)
      (evil-insert-state))
    (dotemacs-declare-prefix "iS" "auto-yasnippet")
    (dotemacs-set-leader-keys
      "iSc" 'aya-create
      "iSe" 'dotemacs/auto-yasnippet-expand
      "iSw" 'aya-persist-snippet)))

(dotemacs-use-package-add-hook smartparens
  :post-init
  (with-eval-after-load 'smartparens
    ;;  We need to know whether the smartparens was enabled, see
    ;; `yas-before-expand-snippet-hook' below.
    (defvar smartparens-enabled-initially t
      "Stored whether smartparens is originally enabled or not.")
    (add-hook 'yas-before-expand-snippet-hook
              (lambda ()
                ;; If enabled, smartparens will mess snippets expanded by `hippie-expand`
                (setq smartparens-enabled-initially smartparens-mode)
                (smartparens-mode -1)))
    (add-hook 'yas-after-exit-snippet-hook
              (lambda ()
                (when smartparens-enabled-initially
                  (smartparens-mode 1))))))

(provide 'module-yasnippet)
;;; module-yasnippet.el ends here
