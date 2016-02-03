;;; module-yasnippet.el --- YASnippet Module
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
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(defun dotemacs-load-yasnippet ()
  (unless yas-global-mode
    (progn
      (yas-global-mode 1)
      (let ((private-yas-dir (if dotemacs-ac-private-snippets-directory
                                 dotemacs-ac-private-snippets-directory
                               (concat dotemacs-private-dir "/snippets/")))
            (dotemacs-snippets-dir
             (expand-file-name
              (concat user-emacs-directory "snippets/"))))
        (setq yas-snippet-dirs
              (append (list private-yas-dir)
                      (when (boundp 'yas-snippet-dirs)
                        yas-snippet-dirs)
                      dotemacs-snippets-dir))
        (yas-load-directory dotemacs-snippets-dir t)
        (yas-load-directory private-yas-dir t)
        (setq yas-wrap-around-region t))))
  (yas-minor-mode 1))

(defun dotemacs-force-yasnippet-off ()
  (yas-minor-mode -1)
  (setq yas-dont-activate t))

(defun dotemacs-auto-yasnippet-expand ()
  "Call `yas-expand' and switch to `insert state'"
  (interactive)
  (call-interactively 'aya-expand)
  (evil-insert-state))

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

    ;; this makes it easy to get out of a nested expansion
    (define-key yas-minor-mode-map
      (kbd "M-s-/") 'yas-next-field)

    ;; on multiple keys, fall back to completing read typically this means helm
    (setq yas-prompt-functions '(yas-completing-prompt))

    (dotemacs/add-to-hooks 'dotemacs-load-yasnippet '(prog-mode-hook
                                                      markdown-mode-hook
                                                      org-mode-hook))

    (dotemacs-add-toggle yasnippet
      :status yas-minor-mode
      :on (yas-minor-mode)
      :off (yas-minor-mode -1)
      :documentation "Enable snippets"
      :evil-leader "ty")

    (dotemacs/add-to-hooks 'dotemacs-force-yasnippet-off '(term-mode-hook
                                                           shell-mode-hook
                                                           eshell-mode-hook)))
  :config
  (progn
    ;;  We need to know whether the smartparens was enabled, see
    ;; `yas-before-expand-snippet-hook' below.
    (defvar smartparens-enabled-initially t
      "Stored whether smartparens is originally enabled or not.")

    (add-hook 'yas-before-expand-snippet-hook (lambda ()
                                                ;; If enabled, smartparens will mess snippets expanded by `hippie-expand`
                                                (setq smartparens-enabled-initially smartparens-mode)
                                                (smartparens-mode -1)))
    (add-hook 'yas-after-exit-snippet-hook (lambda ()
                                             (when smartparens-enabled-initially
                                               (smartparens-mode 1))))
    (dotemacs-diminish yas-minor-mode " ⓨ" " y")))

(use-package auto-yasnippet
  :ensure t
  :defer t
  :init
  (progn
    (setq aya-persist-snippets-dir
          (or dotemacs-ac-private-snippets-directory
              (concat dotemacs-private-dir "snippets/")))
    (dotemacs-declare-prefix "iS" "auto-yasnippet")
    (dotemacs-set-leader-keys
      "iSc" 'aya-create
      "iSe" 'dotemacs-auto-yasnippet-expand
      "iSw" 'aya-persist-snippet)))

(use-package helm-c-yasnippet
  :ensure t
  :defer t
  :init
  (progn
    (defun dotemacs-helm-yas ()
      "Properly lazy load helm-c-yasnipper."
      (interactive)
      (dotemacs-load-yasnippet)
      (require 'helm-c-yasnippet)
      (call-interactively 'helm-yas-complete))
    (dotemacs-set-leader-keys "is" 'dotemacs-helm-yas)
    (setq helm-c-yas-space-match-any-greedy t)))

(provide 'module-yasnippet)
;;; module-yasnippet.el ends here
