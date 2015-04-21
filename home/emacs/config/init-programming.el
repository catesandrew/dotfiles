(defun my-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun my-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

;; show the name of the current function definition in the modeline
(require 'which-func)
(which-function-mode 1)

;; in Emacs 24 programming major modes generally derive from a common
;; mode named prog-mode; for others, we'll arrange for our mode
;; defaults function to run my-prog-mode-hook directly. To
;; augment and/or counteract these defaults your own function
;; to my-prog-mode-hook, using:
;;
;;     (add-hook 'my-prog-mode-hook 'my-prog-mode-defaults t)
;;
;; (the final optional t sets the *append* argument)

;; enlist a more liberal guru
(setq guru-warn-only t)

(defun my-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (when (executable-find ispell-program-name)
    (flyspell-prog-mode))
  (smartparens-mode +1)

  ;; disable line wrap
  ; (unless truncate-lines (set 'truncate-lines t))   ; don't fold line
  ; (unless truncate-lines (lambda ()
  ;                          (toggle-truncate-lines t)
  ;                          (setq truncate-lines t)))   ; don't fold line

  (set 'truncate-lines t)
  (toggle-truncate-lines t)
  (my-local-comment-auto-fill)
  (my-font-lock-comment-annotations))

(setq my-prog-mode-hook 'my-prog-mode-defaults)

(add-hook 'prog-mode-hook (lambda ()
                            (run-hooks 'my-prog-mode-hook)))

;; enable on-the-fly syntax checking
(if (fboundp 'global-flycheck-mode)
    (global-flycheck-mode +1)
  (add-hook 'prog-mode-hook 'flycheck-mode))

(provide 'init-programming)
