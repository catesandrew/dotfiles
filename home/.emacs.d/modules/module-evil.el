;;; module-evil.el --- Evil Module

;; This file is NOT part of GNU Emacs.

;;; License:

;;; Commentary:

(require 'core-vars)
(require 'core-keybindings)
(require 'core-transient-state)
(require 'module-vars)
;; (require 'module-common)
(require 'evil)

;;; Code:

(defvar dotemacs-enable-paste-micro-state t
  "If non nil the paste micro-state is enabled.  While enabled pressing `p`
several times cycle between the kill ring content.'")

(defvar dotemacs-colorize-cursor-according-to-state t
  "If non nil the cursor color matches the state color in GUI Emacs.")

;; Thanks to `editorconfig-emacs' for many of these
(defvar dotemacs--indent-variable-alist
  '(((awk-mode c-mode c++-mode java-mode groovy-mode
      idl-mode java-mode objc-mode pike-mode) . c-basic-offset)
    (python-mode . python-indent-offset)
    (cmake-mode . cmake-tab-width)
    (coffee-mode . coffee-tab-width)
    (cperl-mode . cperl-indent-level)
    (css-mode . css-indent-offset)
    (elixir-mode . elixir-smie-indent-basic)
    ((emacs-lisp-mode lisp-mode) . lisp-indent-offset)
    (enh-ruby-mode . enh-ruby-indent-level)
    (erlang-mode . erlang-indent-level)
    ((js-mode json-mode) . js-indent-level)
    (js2-mode . js2-basic-offset)
    (js3-mode . js3-indent-level)
    (latex-mode . (LaTeX-indent-level tex-indent-basic))
    (livescript-mode . livescript-tab-width)
    (mustache-mode . mustache-basic-offset)
    (nxml-mode . nxml-child-indent)
    (perl-mode . perl-indent-level)
    (puppet-mode . puppet-indent-level)
    (ruby-mode . ruby-indent-level)
    (scala-mode . scala-indent:step)
    (sgml-mode . sgml-basic-offset)
    (sh-mode . sh-basic-offset)
    (web-mode . web-mode-markup-indent-offset)
    (yaml-mode . yaml-indent-offset))
  "An alist where each key is either a symbol corresponding
to a major mode, a list of such symbols, or the symbol t,
acting as default. The values are either integers, symbols
or lists of these.")

;; functions

;; taken from Prelude: https://github.com/bbatsov/prelude
(defmacro dotemacs-advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.
The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command
                      (,class ,(intern (format "%S-%s" command advice-name))
                              activate)
                    ,@body))
               commands)))

;; indent on paste
;; from Prelude: https://github.com/bbatsov/prelude
(defun dotemacs/yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) dotemacs-yank-indent-threshold)
      (indent-region beg end nil)))

(dotemacs-advise-commands
 "indent" (yank yank-pop evil-paste-before evil-paste-after) around
 "If current mode is not one of dotemacs-indent-sensitive-modes
 indent yanked text (with universal arg don't indent)."
 (evil-start-undo-step)
 ad-do-it
 (if (and (not (equal '(4) (ad-get-arg 0)))
          (not (member major-mode dotemacs-indent-sensitive-modes))
          (or (derived-mode-p 'prog-mode)
              (member major-mode dotemacs-indent-sensitive-modes)))
     (let ((transient-mark-mode nil)
           (save-undo buffer-undo-list))
       (dotemacs/yank-advised-indent-function (region-beginning)
                                              (region-end))))
 (evil-end-undo-step))

(defun dotemacs-set-evil-shift-width ()
  "Set the value of `evil-shift-width' based on the indentation settings of the
current major mode."
  (let ((shift-width
         (catch 'break
           (dolist (test dotemacs--indent-variable-alist)
             (let ((mode (car test))
                   (val (cdr test)))
               (when (or (and (symbolp mode) (derived-mode-p mode))
                         (and (listp mode) (apply 'derived-mode-p mode))
                         (eq 't mode))
                 (when (not (listp val))
                   (setq val (list val)))
                 (dolist (v val)
                   (cond
                    ((integerp v) (throw 'break v))
                    ((and (symbolp v) (boundp v))
                     (throw 'break (symbol-value v))))))))
           (throw 'break (default-value 'evil-shift-width)))))
    (when (and (integerp shift-width)
               (< 0 shift-width))
      (setq-local evil-shift-width shift-width))))

(use-package evil
  :ensure t
  :init
  (progn
    (defvar dotemacs-evil-cursors '(("normal" "DarkGoldenrod2" box)
                                    ("insert" "chartreuse3" (bar . 2))
                                    ("emacs" "SkyBlue2" box)
                                    ("hybrid" "SkyBlue2" (bar . 2))
                                    ("replace" "chocolate" (hbar . 2))
                                    ("evilified" "LightGoldenrod3" box)
                                    ("visual" "gray" (hbar . 2))
                                    ("motion" "plum3" box)
                                    ("lisp" "HotPink1" box)
                                    ("iedit" "firebrick1" box)
                                    ("iedit-insert" "firebrick1" (bar . 2)))
      "Colors assigned to evil states with cursor definitions.")

    (loop for (state color cursor) in dotemacs-evil-cursors
          do
          (eval `(defface ,(intern (format "dotemacs-%s-face" state))
                   `((t (:background ,color
                                     :foreground ,(face-background 'mode-line)
                                     :box ,(face-attribute 'mode-line :box)
                                     :inherit 'mode-line)))
                   (format "%s state face." state)
                   :group 'dotemacs))
          (eval `(setq ,(intern (format "evil-%s-state-cursor" state))
                       (list (when dotemacs-colorize-cursor-according-to-state color)
                             cursor))))

    ;; put back refresh of the cursor on post-command-hook see status of:
    ;; https://bitbucket.org/lyro/evil/issue/502/cursor-is-not-refreshed-in-some-cases
    ;; (add-hook 'post-command-hook 'evil-refresh-cursor)

    (defun dotemacs/state-color-face (state)
      "Return the symbol of the face for the given STATE."
      (intern (format "dotemacs-%s-face" (symbol-name state))))

    (defun dotemacs/state-color (state)
      "Return the color string associated to STATE."
      (face-background (dotemacs/state-color-face state)))

    (defun dotemacs/current-state-color ()
      "Return the color string associated to the current state."
      (face-background (dotemacs/state-color-face evil-state)))

    (defun dotemacs/state-face (state)
      "Return the face associated to the STATE."
      (dotemacs/state-color-face state))

    (defun dotemacs/current-state-face ()
      "Return the face associated to the current state."
      (let ((state (if (eq evil-state 'operator)
                       evil-previous-state
                     evil-state)))
        (dotemacs/state-color-face state)))

    (defun evil-insert-state-cursor-hide ()
      (setq evil-insert-state-cursor '((hbar . 0))))

    ;; https://bitbucket.org/lyro/evil/issues/444/evils-undo-granularity-is-too-coarse
    (setq evil-want-fine-undo t)

    ;; Don't move back the cursor one position when exiting insert mode
    (setq evil-move-cursor-back nil)
    ;; prevent certain movement commands from breaking at the end of the lines
    ;; (setq evil-move-beyond-eol t)

    ;; (setq evil-search-module 'evil-search)
    ;; (setq evil-magic 'very-magic)

    (evil-mode 1))
  :config
  (progn
    ;; c-k/c-j for page down/up
    ;;
    ;; One thing that surprised me considering how complete Evil is, is the lack
    ;; of Vim's Control-d/Control-u for page down/up. Probably because C-u is
    ;; pretty important in Emacs (it's the shortcut to give a numeric parameter
    ;; to other commands). I've in fact these mapped on my .vimrc to c-k/c-j
    ;; (because I think they're more consistent with Vim's j/k movement keys) so
    ;; that's how I mapped them in Emacs:
    (define-key evil-motion-state-map (kbd "C-k") 'evil-scroll-up)
    (define-key evil-motion-state-map (kbd "C-j") 'evil-scroll-down)

    ;; bind function keys

    ;; bind evil-jump-forward for GUI only.
    (define-key evil-motion-state-map [C-i] 'evil-jump-forward)

    ;; Make the current definition and/or comment visible.
    (define-key evil-normal-state-map "zf" 'reposition-window)
    ;; toggle maximize buffer
    (define-key evil-window-map (kbd "o") 'dotemacs-toggle-maximize-buffer)
    (define-key evil-window-map (kbd "C-o") 'dotemacs-toggle-maximize-buffer)
    ;; make cursor keys work
    (define-key evil-window-map (kbd "<left>") 'evil-window-left)
    (define-key evil-window-map (kbd "<right>") 'evil-window-right)
    (define-key evil-window-map (kbd "<up>") 'evil-window-up)
    (define-key evil-window-map (kbd "<down>") 'evil-window-down)
    (dotemacs-set-leader-keys "re" 'evil-show-registers)
    (define-key evil-visual-state-map (kbd "<escape>") 'keyboard-quit)
    ;; motions keys for help buffers
    (evil-define-key 'motion help-mode-map (kbd "ESC") 'quit-window)
    (evil-define-key 'motion help-mode-map (kbd "<tab>") 'forward-button)
    (evil-define-key 'motion help-mode-map (kbd "S-<tab>") 'backward-button)
    (evil-define-key 'motion help-mode-map (kbd "]") 'help-go-forward)
    (evil-define-key 'motion help-mode-map (kbd "gf") 'help-go-forward)
    (evil-define-key 'motion help-mode-map (kbd "[") 'help-go-back)
    (evil-define-key 'motion help-mode-map (kbd "gb") 'help-go-back)
    (evil-define-key 'motion help-mode-map (kbd "gh") 'help-follow-symbol)

    ;; replace `dired-goto-file' with `helm-find-files', since `helm-find-files'
    ;; can do the same thing and with fuzzy matching and other features.
    (with-eval-after-load 'dired
      (evil-define-key 'normal dired-mode-map "J" 'dotemacs-helm-find-files)
      (define-key dired-mode-map "j" 'dotemacs-helm-find-files)
      (evil-define-key 'normal dired-mode-map (kbd dotemacs-leader-key)
        dotemacs-default-map))

    ;; It's better that the default value is too small than too big
    (setq-default evil-shift-width 2)

    ;; After major mode has changed, reset evil-shift-width
    (add-hook 'after-change-major-mode-hook 'dotemacs-set-evil-shift-width 'append)

    (defmacro evil-map (state key seq)
      "Map for a given STATE a KEY to a sequence SEQ of keys.

Can handle recursive definition only if KEY is the first key of SEQ.
Example: (evil-map visual \"<\" \"<gv\")"
      (let ((map (intern (format "evil-%S-state-map" state))))
        `(define-key ,map ,key
           (lambda ()
             (interactive)
             ,(if (string-equal key (substring seq 0 1))
                  `(progn
                     (call-interactively ',(lookup-key evil-normal-state-map key))
                     (execute-kbd-macro ,(substring seq 1)))
                (execute-kbd-macro ,seq))))))
    ;; Keep the region active when shifting
    (evil-map visual "<" "<gv")
    (evil-map visual ">" ">gv")

    (defun dotemacs/evil-smart-doc-lookup ()
      "Version of `evil-lookup' that attempts to use
        the mode specific goto-definition binding,
        i.e. `SPC m h h`, to lookup the source of the definition,
        while falling back to `evil-lookup'"
      (interactive)
      (condition-case nil
          (execute-kbd-macro (kbd (concat dotemacs-leader-key " mhh")))
        (error (evil-lookup))))
    (define-key evil-normal-state-map (kbd "K") 'dotemacs/evil-smart-doc-lookup)

    (defun dotemacs/evil-smart-goto-definition ()
      "Version of `evil-goto-definition' that attempts to use
        the mode specific goto-definition binding,
        i.e. `SPC m g g`, to lookup the source of the definition,
        while falling back to `evil-goto-definition'"
      (interactive)
      (condition-case nil
          (execute-kbd-macro (kbd (concat dotemacs-leader-key " mgg")))
        (error (evil-goto-definition))))
    (define-key evil-normal-state-map
      (kbd "gd") 'dotemacs/evil-smart-goto-definition)

    (defun dotemacs/scroll-half-page-up ()
      "Scroll half a page up while keeping cursor in middle of page."
      (interactive)
      (evil-window-top)
      (let ((recenter-redisplay nil))
        (recenter nil)))
    (defun dotemacs/scroll-half-page-down ()
      "Scroll half a page down while keeping cursor in middle of page."
      (interactive)
      (evil-window-bottom)
      ;; required to make repeated presses idempotent
      (evil-next-visual-line)
      (let ((recenter-redisplay nil))
        (recenter nil)))

    (dotemacs-define-transient-state scroll
      :title "Scrolling Transient State"
      :bindings
      ("," evil-scroll-page-up "page up")
      ("." evil-scroll-page-down "page down")
      ;; half page
      ("<" dotemacs/scroll-half-page-up "half page up")
      (">" dotemacs/scroll-half-page-down "half page down"))

    (dotemacs-set-leader-keys
     "n," 'dotemacs/scroll-transient-state/evil-scroll-page-up
     "n." 'dotemacs/scroll-transient-state/evil-scroll-page-down
     "n<" 'dotemacs/scroll-transient-state/dotemacs/scroll-half-page-up
     "n>" 'dotemacs/scroll-transient-state/dotemacs/scroll-half-page-down)

    ;; pasting transient-state
    (dotemacs-define-transient-state paste
      :title "Pasting Transient State"
      :doc "\n[%s(length kill-ring-yank-pointer)/%s(length kill-ring)] \
[_J_/_K_] cycles through yanked text, [_p_/_P_] pastes the same text above or \
below. Anything else exits."
        :bindings
        ("J" evil-paste-pop)
        ("K" evil-paste-pop-next)
        ("p" evil-paste-after)
        ("P" evil-paste-before))
      (when dotemacs-enable-paste-transient-state
        (define-key evil-normal-state-map "p" 'dotemacs/paste-transient-state/evil-paste-after)
        (define-key evil-normal-state-map "P" 'dotemacs/paste-transient-state/evil-paste-before))

    ;; define text objects
    (defmacro dotemacs-define-text-object (key name start end)
      (let ((inner-name (make-symbol (concat "evil-inner-" name)))
            (outer-name (make-symbol (concat "evil-outer-" name)))
            (start-regex (regexp-opt (list start)))
            (end-regex (regexp-opt (list end))))
        `(progn
           (evil-define-text-object ,inner-name (count &optional beg end type)
             (evil-select-paren ,start-regex ,end-regex beg end type count nil))
           (evil-define-text-object ,outer-name (count &optional beg end type)
             (evil-select-paren ,start-regex ,end-regex beg end type count t))
           (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
           (define-key evil-outer-text-objects-map ,key (quote ,outer-name))
           (with-eval-after-load 'evil-surround
             (push (cons (string-to-char ,key)
                         (if ,end
                             (cons ,start ,end)
                           ,start))
                   evil-surround-pairs-alist)))))

      (dotemacs-define-text-object "$" "dollar" "$" "$")
      (dotemacs-define-text-object "*" "star" "*" "*")
      (dotemacs-define-text-object "8" "block-star" "/*" "*/")
      (dotemacs-define-text-object "|" "bar" "|" "|")
      (dotemacs-define-text-object "%" "percent" "%" "%")
      (dotemacs-define-text-object "/" "slash" "/" "/")
      (dotemacs-define-text-object "_" "underscore" "_" "_")
      (dotemacs-define-text-object "-" "hyphen" "-" "-")
      (dotemacs-define-text-object "~" "tilde" "~" "~")
      (dotemacs-define-text-object "=" "equal" "=" "=")

      (evil-define-text-object evil-pasted (count &rest args)
        (list (save-excursion (evil-goto-mark ?\[) (point))
              (save-excursion (evil-goto-mark ?\]) (point))))
      (define-key evil-inner-text-objects-map "P" 'evil-pasted)

      ;; define text-object for entire buffer
      (evil-define-text-object evil-inner-buffer (count &optional beg end type)
        (evil-select-paren "\\`" "\\'" beg end type count nil))
      (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer)

      ;; support smart 1parens-strict-mode
      (with-eval-after-load 'smartparens
        (defadvice evil-delete-backward-char-and-join
            (around dotemacs/evil-delete-backward-char-and-join activate)
          (if (bound-and-true-p smartparens-strict-mode)
              (call-interactively 'sp-backward-delete-char)
            ad-do-it)))

      ;; Define history commands for comint
      (evil-define-key 'insert comint-mode-map
        (kbd "C-k") 'comint-next-input
        (kbd "C-j") 'comint-previous-input)
      (evil-define-key 'normal comint-mode-map
        (kbd "C-k") 'comint-next-input
        (kbd "C-j") 'comint-previous-input)))

(provide 'module-evil)
;;; module-evil.el ends here
