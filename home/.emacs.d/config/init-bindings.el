(defmacro bind (&rest commands)
  "Convience macro which creates a lambda interactive command."
  `(lambda ()
     (interactive)
     ,@commands))


(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)
(setq guide-key/idle-delay 0.8)

(after 'guide-key
  (add-hook 'evil-leader-mode-hook
            #'(lambda () (guide-key/add-local-guide-key-sequence evil-leader/leader))))


(require 'popwin)
(popwin-mode 1)
(global-set-key (kbd "C-c P") 'popwin:popup-last-buffer)
(when (eq system-type 'darwin)
  (global-set-key (kbd "s-P") 'popwin:popup-last-buffer))

;; As well as the defaults, I want ag, magit, flycheck and occur to
;; ‘pop’. I don’t want to auto-select the Magit process buffer as it’s
;; for information only.

(after 'popwin
  (add-to-list 'popwin:special-display-config `"*ag search*")
  (add-to-list 'popwin:special-display-config `("*magit-process*" :noselect t))
  (add-to-list 'popwin:special-display-config `"*Flycheck errors*")
  (add-to-list 'popwin:special-display-config `"*Occur*")
  (add-to-list 'popwin:special-display-config `("*Compile-Log*" :noselect t)))


(after 'evil
  (require 'key-chord)
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)

  ; In order to define an use a <leader> prefix for your personal shortcuts
  (after 'evil-leader
    (evil-leader/set-key
      "w" 'save-buffer
      "e" 'eval-last-sexp
      ", e" 'eval-defun
      "E" 'eval-defun
      "c" 'my-new-eshell-split
      "C" 'customize-group
      "b d" 'kill-this-buffer
      "b p" 'previous-buffer
      "b n" 'next-buffer
      "V" (bind (term "vim"))
      "h" help-map
      "h h" 'help-for-help-internal
      "SPC" 'lazy-highlight-cleanup
      "SPC" 'evil-search-highlight-persist-remove-all
      "f" 'end-of-defun            ; <leader>f/F end/start of function
      "F" 'beginning-of-defun
      "gof" 'open-explorer-path    ; open explorer window of buffer path
      "," 'evil-ace-jump-word-mode ; <leader>, for Ace Jump (word)
      "l" 'evil-ace-jump-line-mode ; <leader>l for Ace Jump (line)
      "x" 'evil-ace-jump-char-mode ; <leader>x for Ace Jump (char)
      "tn" 'elscreen-create        ; <leader>tn create new tab
      "tc" 'elscreen-kill          ; <leader>tc close tab
      "p b" 'projectile-switch-to-buffer
      "p D" 'projectile-dired
      "p d" 'projectile-find-dir
      "p j" 'projectile-find-tag
      "p k" 'projectile-kill-buffers
      "p R" 'projectile-regenerate-tags
      "p r" 'helm-projectile-recentf
      "p s" 'helm-projectile-switch-project
      ; "cv" 'delete-other-windows   ; <leader>cv to close other splits
      ; "im" 'helm-imenu             ; shows functions
      ; "co" 'evilnc-comment-or-uncomment-lines ; nerd commenter
      ; "m" 'emmet-expand-line ; emmet
      "=" (lambda(begin end)        ; <leader>= align selection lines by "="
                              (interactive "r")
                              (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))
      "v" (lambda () (interactive)
                              (split-window-horizontally)
                              (evil-window-right 1))
      "p" (lambda()                 ; paste on a new line below current
                              (interactive)
                              (evil-open-below 1)
                              (evil-paste-after 1)
                              (evil-normal-state))
      "P" (lambda()                 ; paste on a new line below current
                              (interactive)
                              (evil-open-above 1)
                              (evil-previous-visual-line 1)
                              (evil-paste-after 1)
                              (evil-normal-state))
      "n" (lambda() ; open project explorer
                              (interactive)
                              (project-explorer-open)
                              )
      "N" (lambda() ; toggle project explorer
                              (interactive)
                              (project-explorer-toggle)
                              )
      "o" (lambda () ; <leader>o open line below
                              (interactive)
                              (evil-open-below 1)
                              (evil-normal-state))
      "O" (lambda () ; <leader>o open line below
                              (interactive)
                              (evil-open-above 1)
                              (evil-normal-state)))


    (after "paradox-autoloads"
      (evil-leader/set-key "P" 'paradox-list-packages))

    ; (after "flycheck-autoloads"
    ;   (evil-leader/set-key "ea" 'flycheck-list-errors)
    ;   (evil-leader/set-key "en" 'flycheck-next-error))

    (after "magit-autoloads"
      (evil-leader/set-key
        "g s" 'magit-status
        "g b" 'magit-blame-mode
        "g c" 'magit-commit
        "g l" 'magit-log)))

  (after "evil-numbers-autoloads"
    (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-S-a") 'evil-numbers/dec-at-pt))

  (after "git-gutter+-autoloads"
    (define-key evil-normal-state-map (kbd "[ h") 'git-gutter+-previous-hunk)
    (define-key evil-normal-state-map (kbd "] h") 'git-gutter+-next-hunk)
    (define-key evil-normal-state-map (kbd ", g a") 'git-gutter+-stage-hunks)
    (define-key evil-normal-state-map (kbd ", g r") 'git-gutter+-revert-hunks)
    (evil-ex-define-cmd "Gw" (bind (git-gutter+-stage-whole-buffer))))

  (define-key evil-normal-state-map (kbd "SPC o") 'imenu)
  (define-key evil-normal-state-map (kbd "SPC b") 'switch-to-buffer)
  (define-key evil-normal-state-map (kbd "SPC k") 'ido-kill-buffer)
  (define-key evil-normal-state-map (kbd "SPC f") 'ido-find-file)

  (after "helm-autoloads"
    (define-key evil-visual-state-map (kbd "SPC SPC") 'helm-M-x)
    (define-key evil-normal-state-map (kbd "SPC SPC") 'helm-M-x)
    (define-key evil-normal-state-map (kbd "SPC b") 'helm-mini)
    (define-key evil-normal-state-map (kbd "g b") 'helm-mini)
    (define-key evil-normal-state-map (kbd "SPC a") 'helm-apropos)
    (define-key evil-normal-state-map (kbd "SPC f") 'helm-find-files)
    (define-key evil-normal-state-map (kbd "SPC o") 'helm-semantic-or-imenu)
    (define-key evil-normal-state-map (kbd "SPC t") 'helm-etags-select)
    (define-key evil-normal-state-map (kbd "SPC y") 'helm-show-kill-ring)
    (define-key evil-normal-state-map (kbd "SPC m") 'helm-bookmarks)
    (define-key evil-normal-state-map (kbd "SPC r") 'helm-register)
    (after "helm-swoop-autoloads"
      (define-key evil-normal-state-map (kbd "SPC l") 'helm-swoop)
      (define-key evil-normal-state-map (kbd "SPC L") 'helm-multi-swoop)))

  ; c-k/c-j for page down/up

  ; One thing that surprised me considering how complete Evil is, is the lack
  ; of Vim's Control-d/Control-u for page down/up. Probably because C-u is
  ; pretty important in Emacs (it's the shortcut to give a numeric parameter to
  ; other commands). I've in fact these mapped on my .vimrc to c-k/c-j
  ; (because I think they're more consistent with Vim's j/k movement keys) so
  ; that's how I mapped them in Emacs:

  (define-key evil-normal-state-map (kbd "C-k") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-scroll-down)

  (define-key evil-normal-state-map (kbd "[ SPC") (bind (evil-insert-newline-above) (forward-line)))
  (define-key evil-normal-state-map (kbd "] SPC") (bind (evil-insert-newline-below) (forward-line -1)))
  (define-key evil-normal-state-map (kbd "[ e") (kbd "ddkP"))
  (define-key evil-normal-state-map (kbd "] e") (kbd "ddp"))
  (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "] b") 'next-buffer)
  (define-key evil-normal-state-map (kbd "[ q") 'previous-error)
  (define-key evil-normal-state-map (kbd "] q") 'next-error)

  (define-key evil-normal-state-map (kbd "g p") (kbd "` [ v ` ]"))

  (after "etags-select-autoloads"
    (define-key evil-normal-state-map (kbd "g ]") 'etags-select-find-tag-at-point))

  (global-set-key (kbd "C-w") 'evil-window-map)
  (define-key evil-normal-state-map (kbd "C-w h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-w j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-w k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-w l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-w d") 'elscreen-kill)


  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)

  (define-key evil-normal-state-map (kbd "Q") 'my-window-killer)
  (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

  (define-key evil-visual-state-map (kbd ", e") 'eval-region)

  ;; emacs lisp
  (evil-define-key 'normal emacs-lisp-mode-map "K" (bind (help-xref-interned (symbol-at-point))))
  (after "elisp-slime-nav-autoloads"
    (evil-define-key 'normal emacs-lisp-mode-map (kbd "g d") 'elisp-slime-nav-find-elisp-thing-at-point))

  ;; Project Explorer
  (after 'project-explorer
    (evil-set-initial-state 'project-explorer-mode 'normal)
    (evil-define-key 'normal project-explorer-mode-map
        (kbd "+") 'pe/create-file
        (kbd "-") 'pe/delete-file
        (kbd "x") 'pe/fold
        (kbd "u") 'pe/up-element
        (kbd "a") 'pe/goto-top
        (kbd "TAB") 'pe/tab
        (kbd "<backtab>") 'pe/backtab
        (kbd "J") 'pe/forward-element
        (kbd "K") 'pe/backward-element
        (kbd "]") 'pe/forward-element
        (kbd "[") 'pe/backward-element
        (kbd "n") 'next-line
        (kbd "p") 'previous-line
        (kbd "j") 'next-line
        (kbd "k") 'previous-line
        (kbd "l") 'forward-char
        (kbd "h") 'backward-char
        (kbd "RET") 'pe/return
        (kbd "q") 'pe/quit
        [escape] 'pe/quit
        (kbd "s") 'pe/change-directory
        (kbd "r") 'pe/rename-file
        (kbd "c") 'pe/copy-file
        (kbd "f") 'pe/find-file
        (kbd "w") 'pe/copy-file-name-as-kill
        (kbd "M-l") 'pe/set-filter-regex
        (kbd "M-o") 'pe/toggle-omit
    )
  )

  (after 'coffee-mode
    (evil-define-key 'visual coffee-mode-map (kbd ", p") 'coffee-compile-region)
    (evil-define-key 'normal coffee-mode-map (kbd ", p") 'coffee-compile-buffer))

  (after 'stylus-mode
    (define-key stylus-mode-map [remap eval-last-sexp] 'my-stylus-compile-and-eval-buffer)
    (evil-define-key 'visual stylus-mode-map (kbd ", p") 'my-stylus-compile-and-show-region)
    (evil-define-key 'normal stylus-mode-map (kbd ", p") 'my-stylus-compile-and-show-buffer))

  (after "projectile-autoloads"
    (define-key evil-normal-state-map (kbd "SPC e") 'projectile-recentf)
    (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
    (let ((binding (kbd "SPC /")))
      (cond ((executable-find "pt")
             (define-key evil-normal-state-map binding 'projectile-pt))
            ((executable-find "ag")
             (define-key evil-normal-state-map binding
	       (bind
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

  (after "multiple-cursors-autoloads"
    (after 'js2-mode
      (evil-define-key 'normal js2-mode-map (kbd "g r") 'js2r-rename-var))
    (define-key evil-normal-state-map (kbd "g r") 'mc/mark-all-like-this-dwim))

  (after "ace-jump-mode-autoloads"
    (evil-leader/set-key "e" 'evil-ace-jump-word-mode) ; ,e for Ace Jump (word)
    (evil-leader/set-key "l" 'evil-ace-jump-line-mode) ; ,l for Ace Jump (line)
    (evil-leader/set-key "x" 'evil-ace-jump-char-mode) ; ,x for Ace Jump (char)
    (define-key evil-operator-state-map (kbd "z") 'evil-ace-jump-char-mode)
    (define-key evil-normal-state-map (kbd "s") 'evil-ace-jump-char-mode)
    (define-key evil-motion-state-map (kbd "S-SPC") 'evil-ace-jump-line-mode))

  ;; butter fingers
  (evil-ex-define-cmd "Q" 'evil-quit)
  (evil-ex-define-cmd "Qa" 'evil-quit-all)
  (evil-ex-define-cmd "QA" 'evil-quit-all))


;; escape minibuffer
(define-key minibuffer-local-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'my-minibuffer-keyboard-quit)

(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)

(after 'magit
  (global-set-key (kbd "C-x g") 'magit-status)
  (define-key magit-status-mode-map (kbd "C-n") 'magit-goto-next-sibling-section)
  (define-key magit-status-mode-map (kbd "C-p") 'magit-goto-previous-sibling-section))


;; Project Explorer
(after "project-explorer-autoloads"
  (autoload 'pe/show-file "project-explorer")
  (global-set-key [f3] 'pe/show-file)
  (after 'project-explorer
    (define-key project-explorer-mode-map (kbd "C-l") 'evil-window-right)))


(after "multiple-cursors-autoloads"
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))


(after 'comint
  (define-key comint-mode-map [up] 'comint-previous-input)
  (define-key comint-mode-map [down] 'comint-next-input))


(after 'auto-complete
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-p") 'ac-previous))


(after 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'my-company-tab)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (after "helm-company-autoloads"
    (define-key company-mode-map (kbd "<C-return>") 'helm-company)
    (define-key company-active-map (kbd "<C-return>") 'helm-company)))


(after "expand-region-autoloads"
  (global-set-key (kbd "C-=") 'er/expand-region))


(after 'web-mode
  (after "angular-snippets-autoloads"
    (define-key web-mode-map (kbd "C-c C-d") 'ng-snip-show-docs-at-point)))


;; mouse scrolling in terminal
(unless (display-graphic-p)
  (global-set-key [mouse-4] (bind (scroll-down 1)))
  (global-set-key [mouse-5] (bind (scroll-up 1))))


(after "helm-autoloads"
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-m") 'helm-M-x)
  (global-set-key (kbd "C-c C-m") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-buffers-list))


(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c h") #'my-eshell-ido-complete-command-history)))


(after 'help-mode
  (define-key help-mode-map (kbd "n") 'next-line)
  (define-key help-mode-map (kbd "p") 'previous-line)
  (define-key help-mode-map (kbd "j") 'next-line)
  (define-key help-mode-map (kbd "k") 'previous-line))


(global-set-key [prior] 'previous-buffer)
(global-set-key [next] 'next-buffer)

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c s") 'my-goto-scratch-buffer)
(global-set-key (kbd "C-c e") 'my-eval-and-replace)
(global-set-key (kbd "C-c t") 'my-new-eshell-split)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

(global-set-key (kbd "C-x p") 'proced)
(after "vkill-autoloads"
  (autoload 'vkill "vkill" nil t)
  (global-set-key (kbd "C-x p") 'vkill))

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward)


;; have no use for these default bindings
(global-unset-key (kbd "C-x m"))


;; replace with [r]eally [q]uit
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") (bind (message "Thou shall not quit!")))
(after 'evil
  (defadvice evil-quit (around advice-for-evil-quit activate)
    (message "Thou shall not quit!"))
  (defadvice evil-quit-all (around advice-for-evil-quit-all activate)
    (message "Thou shall not quit!")))

(provide 'init-bindings)
