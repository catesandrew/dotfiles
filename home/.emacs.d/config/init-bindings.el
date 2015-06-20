;; TODO to be deleted

(defcustom dotemacs-buffer-keymap-prefix (kbd "C-c b")
  "dotemacs buffer keymap prefix."
  :group 'dotemacs
  :type 'string)

;;; Minor mode
(defvar dotemacs-buffer-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'previous-buffer)
    (define-key map (kbd "n") 'next-buffer)
    map)
  "Keymap for dotemacs buffer commands after `dotemacs-buffer-keymap-prefix'.")
(fset 'dotemacs-buffer-command-map dotemacs-buffer-command-map)

(defvar dotemacs-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map dotemacs-buffer-keymap-prefix 'dotemacs-buffer-command-map)
    map)
  "Keymap for dotemacs buffer mode.")

(define-minor-mode dotemacs-buffer-mode
  "Minor mode for dotemacs buffer mode"
  :keymap dotemacs-buffer-mode-map)

(define-globalized-minor-mode dotemacs-buffer-global-mode
  dotemacs-buffer-mode
  dotemacs-buffer-mode)




(with-eval-after-load 'evil
  ; In order to define an use a <leader> prefix for your personal shortcuts
  (with-eval-after-load 'evil-leader
    (with-eval-after-load "magit-autoloads"
      ;; Set the initial evil state that certain major modes will be in.
      (evil-set-initial-state 'magit-log-edit-mode 'emacs)
      (evil-leader/set-key
        "g s" 'magit-status
        "g b" 'magit-blame-mode
        "g c" 'magit-commit
        "g l" 'magit-log)))

  (with-eval-after-load "git-gutter+-autoloads"
    (define-key evil-normal-state-map (kbd "[ h") 'git-gutter+-previous-hunk)
    (define-key evil-normal-state-map (kbd "] h") 'git-gutter+-next-hunk)
    (define-key evil-normal-state-map (kbd ", g a") 'git-gutter+-stage-hunks)
    (define-key evil-normal-state-map (kbd ", g r") 'git-gutter+-revert-hunks)
    (evil-ex-define-cmd "Gw" (dotemacs-bind (git-gutter+-stage-whole-buffer))))

  (define-key evil-normal-state-map (kbd "SPC o") 'imenu)
  (define-key evil-normal-state-map (kbd "SPC b") 'switch-to-buffer)
  (define-key evil-normal-state-map (kbd "SPC k") 'ido-kill-buffer)
  (define-key evil-normal-state-map (kbd "SPC f") 'ido-find-file)

  (with-eval-after-load "helm-autoloads"
    (define-key evil-visual-state-map (kbd "SPC SPC") 'helm-M-x)
    (define-key evil-normal-state-map (kbd "SPC SPC") 'helm-M-x)
    (define-key evil-normal-state-map (kbd "SPC b") 'helm-mini)
    (define-key evil-normal-state-map (kbd "g b") 'helm-mini)
    (define-key evil-normal-state-map (kbd "SPC a") 'helm-apropos)
    (define-key evil-normal-state-map (kbd "SPC e") 'helm-recentf)
    (define-key evil-normal-state-map (kbd "SPC f") 'helm-find-files)
    (define-key evil-normal-state-map (kbd "SPC o") 'helm-semantic-or-imenu)
    (define-key evil-normal-state-map (kbd "SPC t") 'helm-etags-select)
    (define-key evil-normal-state-map (kbd "SPC y") 'helm-show-kill-ring)
    (define-key evil-normal-state-map (kbd "SPC m") 'helm-bookmarks)
    (define-key evil-normal-state-map (kbd "SPC r") 'helm-register)
    (with-eval-after-load "helm-swoop-autoloads"
      (define-key evil-normal-state-map (kbd "SPC l") 'helm-swoop)
      (define-key evil-normal-state-map (kbd "SPC L") 'helm-multi-swoop)))


  (define-key evil-normal-state-map (kbd "[ SPC") (dotemacs-bind (evil-insert-newline-above) (forward-line)))
  (define-key evil-normal-state-map (kbd "] SPC") (dotemacs-bind (evil-insert-newline-below) (forward-line -1)))
  (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "] b") 'next-buffer)
  (define-key evil-normal-state-map (kbd "[ q") 'previous-error)
  (define-key evil-normal-state-map (kbd "] q") 'next-error)

  (define-key evil-normal-state-map (kbd "g p") (kbd "` [ v ` ]"))

  (with-eval-after-load "etags-select-autoloads"
    (define-key evil-normal-state-map (kbd "g ]") 'etags-select-find-tag-at-point))

  ;; emacs lisp
  (evil-define-key 'normal emacs-lisp-mode-map "K" (dotemacs-bind (help-xref-interned (symbol-at-point))))
  (with-eval-after-load "elisp-slime-nav-autoloads"
    (evil-define-key 'normal emacs-lisp-mode-map (kbd "g d") 'elisp-slime-nav-find-elisp-thing-at-point))

  ;; Evil Tabs
  ; (with-eval-after-load 'evil-tabs
  ;   (evil-define-key 'normal evil-tabs-mode-map
  ;     (kbd "M-{") 'elscreen-previous
  ;     (kbd "M-}") 'elscreen-next
  ;     (kbd "M-t") 'elscreen-create
  ;     (kbd "M-w") 'elscreen-kill
  ;   )
  ; )

  (with-eval-after-load "projectile-autoloads"
    (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
    (let ((binding (kbd "SPC /")))
      (cond ((executable-find "pt")
             (define-key evil-normal-state-map binding 'projectile-pt))
            ((executable-find "ag")
             (define-key evil-normal-state-map binding
	       (dotemacs-bind
		(setq current-prefix-arg t)
		(call-interactively #'projectile-ag))))
            ((executable-find "ack")
             (define-key evil-normal-state-map binding 'projectile-ack))
            (t
             (define-key evil-normal-state-map binding 'projectile-grep))))
    (after "helm-projectile-autoloads"
      (helm-projectile-on)
      (define-key evil-normal-state-map (kbd "SPC e") 'helm-projectile-recentf)
      (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile)))
  )

(with-eval-after-load 'magit
  (global-set-key (kbd "C-x g") 'magit-status)
  (define-key magit-status-mode-map (kbd "C-n") 'magit-goto-next-sibling-section)
  (define-key magit-status-mode-map (kbd "C-p") 'magit-goto-previous-sibling-section))


(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'my-company-tab)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (with-eval-after-load "helm-company-autoloads"
    (define-key company-mode-map (kbd "<C-return>") 'helm-company)
    (define-key company-active-map (kbd "<C-return>") 'helm-company)))


(with-eval-after-load "helm-autoloads"
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-m") 'helm-M-x)
  (global-set-key (kbd "C-c C-m") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c h") #'helm-eshell-history))))

(with-eval-after-load 'help-mode
  (define-key help-mode-map (kbd "n") 'next-line)
  (define-key help-mode-map (kbd "p") 'previous-line)
  (define-key help-mode-map (kbd "j") 'next-line)
  (define-key help-mode-map (kbd "k") 'previous-line))


(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c s") 'my-goto-scratch-buffer)
(global-set-key (kbd "C-c e") 'my-eval-and-replace)
(global-set-key (kbd "C-c t") 'my-new-eshell-split)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

(global-set-key (kbd "C-x p") 'proced)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward)


(provide 'init-bindings)
