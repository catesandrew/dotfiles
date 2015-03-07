(defgroup dotemacs-evil nil
  "Configuration options for evil-mode."
  :group 'dotemacs
  :prefix 'dotemacs-evil)

(defcustom dotemacs-evil/evil-state-modes
  '(fundamental-mode
    text-mode
    prog-mode
    sws-mode
    dired-mode
    comint-mode
    log-edit-mode
    compilation-mode)
  "List of modes that should start up in Evil state."
  :type '(repeat (symbol))
  :group 'dotemacs-evil)

(defcustom dotemacs-evil/emacs-state-modes
  '(debugger-mode
    git-commit-mode
    git-rebase-mode)
  "List of modes that should start up in Evil Emacs state."
  :type '(repeat (symbol))
  :group 'dotemacs-evil)

(defcustom dotemacs-evil/emacs-cursor
  "red"
  "The color of the cursor when in Emacs state."
  :type 'color)


(setq evil-search-module 'evil-search)
(setq evil-magic 'very-magic)

(setq evil-emacs-state-cursor `(,dotemacs-evil/emacs-cursor box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

; Don't move back the cursor one position when exiting insert mode
(setq evil-move-cursor-back nil)

(require 'evil)

(unless (display-graphic-p)
  (evil-esc-mode))


; Vim-like search highlighting

; I prefer how Vim's highlight search and left the highlighted terms until you
; make another search or clean the highlighted terms. I tough this would be
; easy to get but it turned it wasn't so easy (for me). At the end I made my
; first Emacs extension (and the first time I've programmed in Lisp since the
; university a long time ago...) so all turned well. The extension is already
; on Melpa has the very brief name of evil-search-highlight-persist. You can
; enable it with:

(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)

; To map a shortcut (leader-space) to clear the highlights I have:
;
; (evil-leader/set-key "SPC" 'evil-search-highlight-persist-remove-all)
;
; I must note that another good way to search in Emacs is to use occur or
; helm-occur. This will show the search results on a list (on a split window
; with occur) and you'll be able to jump easily to any match.


; Leader key

(require 'evil-leader)

; Later, I found that the evil-leader key didn't work on some modes (like when
; editing the .emacs file in emacs-lisp-mode), but the package FAQ solved the
; problem, you have to add this before the `global-evil-leader-mode` setting:
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode t)

; Easymotion => Evil Ace Jump

; The functionality provided by the awesome Easymotion plugin on Vim is
; actually integrated by default on Evil since it incorporates a package called
; Ace Jump that does mostly the same. It's less powerful than Easymotion (some
; jumps like backwards-only / forward-only / end-of-word and others are
; missing) and I prefer how Easymotion shows directly two chars when a jump is
; going to require them (instead of showing one and after pressing it, the
; other which is what Ace-Jump does) but the important modes (bidirectional
; jump to word and to char) that were the ones I was mostly using are provided.

; Unlike Easymotion, jump to word asks for a letter, but that can be easily
; disabled with: (setq ace-jump-word-mode-use-query-char nil). The author makes
; the case that without asking for a char you're probably entering more key
; presses most of the time. This is probably true, but when I want to jump to
; a random word inside the buffer my brain-eye connection has already
; identified the word but I've to stop and look/think for the first char, so in
; the end for me is actually faster to get jump shortcuts to all the words
; without having to provide the leading character.


(setq evilnc-hotkey-comment-operator "gc")
(require 'evil-nerd-commenter)


(require 'evil-surround)
(global-evil-surround-mode t)


; Tabs

; If you install the evil-tabs package and enable it with you'll have :tabnew,
; gt and friends with numbered tabs by default. Showing the tab number is
; a very useful feature when you can change to a tab with #gt like in Vim
; (with # being a number from 0 to 9), but unfortunately this package doesn't
; support #gt but I worked it around with my awesome Elisp skills (close to
; zero):

(require 'evil-tabs)
(global-evil-tabs-mode t)
(define-key evil-normal-state-map (kbd "C-0") (lambda() (interactive) (elscreen-goto 0)))
(define-key evil-normal-state-map (kbd "C-1") (lambda() (interactive) (elscreen-goto 1)))
(define-key evil-normal-state-map (kbd "C-2") (lambda() (interactive) (elscreen-goto 2)))
(define-key evil-normal-state-map (kbd "C-3") (lambda() (interactive) (elscreen-goto 3)))
(define-key evil-normal-state-map (kbd "C-4") (lambda() (interactive) (elscreen-goto 4)))
(define-key evil-normal-state-map (kbd "C-5") (lambda() (interactive) (elscreen-goto 5)))
(define-key evil-normal-state-map (kbd "C-6") (lambda() (interactive) (elscreen-goto 6)))
(define-key evil-normal-state-map (kbd "C-7") (lambda() (interactive) (elscreen-goto 7)))
(define-key evil-normal-state-map (kbd "C-8") (lambda() (interactive) (elscreen-goto 8)))
(define-key evil-normal-state-map (kbd "C-9") (lambda() (interactive) (elscreen-goto 9)))
(define-key evil-insert-state-map (kbd "C-0") (lambda() (interactive) (elscreen-goto 0)))
(define-key evil-insert-state-map (kbd "C-1") (lambda() (interactive) (elscreen-goto 1)))
(define-key evil-insert-state-map (kbd "C-2") (lambda() (interactive) (elscreen-goto 2)))
(define-key evil-insert-state-map (kbd "C-3") (lambda() (interactive) (elscreen-goto 3)))
(define-key evil-insert-state-map (kbd "C-4") (lambda() (interactive) (elscreen-goto 4)))
(define-key evil-insert-state-map (kbd "C-5") (lambda() (interactive) (elscreen-goto 5)))
(define-key evil-insert-state-map (kbd "C-6") (lambda() (interactive) (elscreen-goto 6)))
(define-key evil-insert-state-map (kbd "C-7") (lambda() (interactive) (elscreen-goto 7)))
(define-key evil-insert-state-map (kbd "C-8") (lambda() (interactive) (elscreen-goto 8)))
(define-key evil-insert-state-map (kbd "C-9") (lambda() (interactive) (elscreen-goto 9)))


(require 'evil-exchange)
(evil-exchange-install)


(setq evil-jumper-auto-center t)
(setq evil-jumper-file (concat dotemacs-cache-directory "evil-jumps"))
(setq evil-jumper-auto-save-interval 3600)
(require 'evil-jumper)
(global-evil-jumper-mode t)


(require 'evil-matchit)
(defun evilmi-customize-keybinding ()
  (evil-define-key 'normal evil-matchit-mode-map
    "%" 'evilmi-jump-items))
(global-evil-matchit-mode t)


(require 'evil-indent-textobject)


(require 'evil-visualstar)
(global-evil-visualstar-mode t)


(require 'evil-numbers)


(defun my-major-mode-evil-state-adjust ()
  (if (apply 'derived-mode-p dotemacs-evil/evil-state-modes)
      (turn-on-evil-mode)
    (set-cursor-color dotemacs-evil/emacs-cursor))
  (when (apply 'derived-mode-p dotemacs-evil/emacs-state-modes)
    (turn-off-evil-mode)
    (set-cursor-color dotemacs-evil/emacs-cursor)))
(add-hook 'after-change-major-mode-hook #'my-major-mode-evil-state-adjust)

(defun my-send-string-to-terminal (string)
  (unless (display-graphic-p) (send-string-to-terminal string)))

(defun my-evil-terminal-cursor-change ()
  (when (string= (getenv "TERM_PROGRAM") "iTerm.app")
    (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\e]50;CursorShape=1\x7")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\e]50;CursorShape=0\x7"))))
  (when (and (getenv "TMUX") (string= (getenv "TERM_PROGRAM") "iTerm.app"))
    (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=1\x7\e\\")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=0\x7\e\\")))))

(add-hook 'after-make-frame-functions (lambda (frame) (my-evil-terminal-cursor-change)))
(my-evil-terminal-cursor-change)

(defun my-evil-modeline-change (default-color)
  "changes the modeline color when the evil mode changes"
  (let ((color (cond ((evil-insert-state-p) '("#002233" . "#ffffff"))
                     ((evil-visual-state-p) '("#330022" . "#ffffff"))
                     ((evil-normal-state-p) default-color)
                     (t '("#440000" . "#ffffff")))))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))

(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook (lambda () (my-evil-modeline-change default-color))))

(defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
  (recenter))

(defadvice evil-ex-search-previous (after advice-for-evil-ex-search-previous activate)
  (recenter))

(after 'edebug
  (add-hook 'edebug-mode-hook (lambda ()
                                (if edebug-mode
                                    (evil-emacs-state)
                                  (evil-normal-state)))))

(after 'paren
  ;; the default behavior only highlights with the point one-after the closing paren
  ;; this changes it such it will match with the point on the closing paren
  (defadvice show-paren-function (around show-paren-closing-before activate)
    (if (and (or
              (evil-normal-state-p)
              (evil-visual-state-p))
             (eq (syntax-class (syntax-after (point))) 5))
        (save-excursion
          (forward-char)
          ad-do-it)
      ad-do-it)))

(when (>= emacs-major-version 25)
  (defadvice elisp--preceding-sexp (around evil activate)
    "In normal-state or motion-state, last sexp ends at point."
    (if (or (evil-normal-state-p) (evil-motion-state-p))
        (save-excursion
          (unless (or (eobp) (eolp)) (forward-char))
          ad-do-it)
      ad-do-it)))

(provide 'init-evil)
