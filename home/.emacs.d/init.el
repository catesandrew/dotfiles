;;; init.el --- Emacs configuration

;;; Commentary:

;; User key prefixes:
;;
;; - C-c A: Align
;; - C-c a: Ag
;; - C-c b: Helm commands (b for "browse")
;; - C-c d: Data stuff
;; - C-c e: Edit commands, general and mode specific
;; - C-c f: Files
;; - C-c h: Help and documentation
;; - C-c j: Jumping and navigation
;; - C-c l: List things
;; - C-c m: Multiple cursors
;; - C-c s: Symbol commands
;; - C-c t: Toggle things and skeletons
;; - C-c u: Miscellaneous utilities
;; - C-c v: Version control
;; - C-c w: Web stuff
;; - C-x x: Perspective

;;; Code:

;; No splash screen please ... jeez
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)

;;; Debugging
(setq message-log-max 10000)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000)

;; Set path to dependencies
(defgroup dotemacs nil
  "Custom configuration for dotemacs."
  :group 'local)

(defcustom dotemacs-cache-directory (concat user-emacs-directory ".cache/")
  "The storage location for various persistent files."
  :group 'dotemacs)

(defcustom dotemacs-completion-engine
  'company
  "The completion engine the use."
  :type '(radio
          (const :tag "company-mode" company)
          (const :tag "auto-complete-mode" auto-complete))
  :group 'dotemacs)

(defcustom dotemacs-erc-nick
  'catesandrew
  "The erc nickname to use"
  :group 'dotemacs)

(defcustom dotemacs-elisp-dir (expand-file-name "elisp" user-emacs-directory)
  "The storage location lisp."
  :group 'dotemacs)

(defcustom dotemacs-config-dir (expand-file-name "config" user-emacs-directory)
  "The config location lisp."
  :group 'dotemacs)

(defcustom dotemacs-user-settings-dir (concat user-emacs-directory "users/" user-login-name)
  "The currently logged in user's storage location for settings."
  :group 'dotemacs)

(defcustom dotemacs-private-dir (locate-user-emacs-file "private")
  "Directory for private settings."
  :group 'dotemacs)

(defun dotemacs-expand-private-file (file-name)
  "Get the absolute path for a private file with FILE-NAME."
  (expand-file-name file-name dotemacs-private-dir))

(defun dotemacs-load-private-file (file-name &optional noerror nomessage)
  "Load a private file with FILE-NAME.

NOERROR and NOMESSAGE are passed to `load'."
  (load (dotemacs-expand-private-file file-name)
        noerror nomessage))

(defcustom dotemacs-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI."
  :group 'dotemacs)

(with-current-buffer (get-buffer-create "*Require Times*")
  (insert "| feature | elapsed | timestamp |\n")
  (insert "|---------+---------+-----------|\n"))

(defadvice require (around require-advice activate)
  (let ((elapsed)
        (loaded (memq feature features))
        (start (current-time)))
    (prog1
        ad-do-it
      (unless loaded
        (with-current-buffer (get-buffer-create "*Require Times*")
          (goto-char (point-max))
          (setq elapsed (float-time (time-subtract (current-time) start)))
          (insert (format "| %s | %s | %f |\n"
                          feature
                          (format-time-string "%Y-%m-%d %H:%M:%S.%3N" (current-time))
                          elapsed)))))))

;; Set up load path(s)
(add-to-list 'load-path dotemacs-config-dir)
(add-to-list 'load-path dotemacs-elisp-dir)
(add-to-list 'load-path dotemacs-user-settings-dir)

;; Add external projects to load path
(let ((base dotemacs-elisp-dir))
  (add-to-list 'load-path base)
  (dolist (dir (directory-files base t "^[^.]"))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))



;;; Locality
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top


;;; Package management
(setq load-prefer-newer t)

;; Please don't load outdated byte code
(require 'package)

;; http://stackoverflow.com/questions/11127109/
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; Requires

(eval-when-compile
  (require 'use-package))

(eval-when-compile
  (require 'cl))

(require 'bind-key)
(require 'diminish)

(require 'subr-x)
(require 'rx)
(require 'time-date)


;;; Initialization
(when (version< emacs-version "25")
  (warn "This configuration needs Emacs trunk, but this is %s!" emacs-version)
  (warn "brew install emacs --HEAD --use-git-head --with-cocoa --with-gnutls --with-rsvg --with-imagemagick"))

;; And disable the site default settings
(setq inhibit-default-init t)

;; Warn if the current build is more than a week old
(run-with-idle-timer
 2 nil
 (lambda ()
   (let ((time-since-build (time-subtract (current-time) emacs-build-time)))
     (when (> (time-to-number-of-days time-since-build) 7)
       (lwarn 'emacs :warning "Your Emacs build is more than a week old!")))))

; "After" macro definition
;
; http://www.lunaryorn.com/2013/06/25/introducing-with-eval-after-load.html
;
; At June, 13th Emacs trunk introduced a new macro `with-eval-after-load`. It
; behaves like `eval-after-load`, except that it takes multiple unquoted forms
; and wraps them into a lambda to enable byte compilation:
;
; This supersedes much of my last post about byte compilation in
; `eval-after-load`. However, the new macro does not load the corresponding
; features during byte compilation, so I’ll wrap my old `after` macro
; around it to avoid bogus warnings:
(defmacro after (feature &rest forms)
  "After FEATURE is loaded, evaluate FORMS.

FORMS is byte compiled.

FEATURE may be a named feature or a file name, see
`with-eval-after-load' for details."
  (declare (indent 1) (debug t))
  `(,(if (or (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       (message "after: cannot find %s" feature)
       'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

; To ensure compatibility with releases and older snapshot builds, I define
; with-eval-after-load if it is absent:
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))

(use-package init-util              ; Personal OS X tools
  :load-path "config/"
  :defer t
  :commands(my-recompile-init
            my-window-killer
            my-minibuffer-keyboard-quit
            my-set-transparency
            my-google
            my-copy-file-name-to-clipboard
            my-eval-and-replace
            my-rename-current-buffer-file
            my-delete-current-buffer-file
            my-goto-scratch-buffer
            my-insert-last-kbd-macro
            my-buffer-to-unix-format
            my-buffer-to-dos-format))

(use-package init-eshell
  :load-path "config/"
  :defer t
  :commands (dotemacs-new-eshell-split
             dotemacs-eshell-prompt
             dotemacs-current-git-branch))


;;; Setup environment variables from the user's shell.

;; Disable case insensitivity for filename autocompletion in shell-mode
(setq pcomplete-ignore-case t) ;; Controls case sensitivity for pcomplete

(use-package exec-path-from-shell
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (progn
    (when (string-match-p "/zsh$" (getenv "SHELL"))
      ;; Use a non-interactive shell. We use a login shell, even though we have
      ;; our paths setup in .zshenv. However, OS X adds global settings to the
      ;; login profile. Notably, this affects /usr/texbin from MacTeX
      (setq exec-path-from-shell-arguments '("-l")))

    (dolist (var '("EMAIL" "PYTHONPATH" "INFOPATH"))
      (add-to-list 'exec-path-from-shell-variables var))

    (exec-path-from-shell-initialize)

    (setq user-mail-address (getenv "EMAIL"))

    ;; Re-initialize the `Info-directory-list' from $INFOPATH. Since package.el
    ;; already initializes info, we need to explicitly add the $INFOPATH
    ;; directories to `Info-directory-list'.
    (after "info"
      (dolist (dir (parse-colon-path (getenv "INFOPATH")))
        (when dir
          (add-to-list 'Info-directory-list dir))))))

(use-package init-eshell
  :load-path "config/"
  :defer t
  :commands (dotemacs-new-eshell-split
             dotemacs-eshell-prompt
             dotemacs-current-git-branch))

;; TODO: Incorporate this into new `use-package eshell` below
;; original `init-eshell`
; (defgroup dotemacs-eshell nil
;   "Configuration options for eshell-mode."
;   :group 'dotemacs
;   :prefix 'dotemacs-eshell)
;
; (defcustom dotemacs-eshell/plan9
;   nil
;   "Turns on Plan9 style prompt in eshell when non-nil."
;   :group 'dotemacs-eshell)
;
; ;; eshell
; (setq eshell-directory-name (concat dotemacs-cache-directory "eshell"))
; (setq eshell-scroll-to-bottom-on-input 'all)
; (setq eshell-buffer-shorthand t)
;
; (when (executable-find "fortune")
;   (defadvice eshell (before advice-for-eshell activate)
;     (setq eshell-banner-message (concat (shell-command-to-string "fortune") "\n"))))
;
;
; ;; em-alias
; (setq eshell-aliases-file (concat user-emacs-directory ".eshell-aliases"))
;
;
; ;; em-glob
; (setq eshell-glob-case-insensitive t)
; (setq eshell-error-if-no-glob t)
;
; ;; em-hist
; (setq eshell-history-size 1024)
;
; ;; em-compl
; (setq eshell-cmpl-ignore-case t)
;
; ;; em-prompt
; (setq eshell-prompt-function #'dotemacs-eshell-prompt)
;
; (add-hook 'eshell-mode-hook
; 	  (lambda ()
; 	    ;; get rid of annoying 'terminal is not fully functional' warning
; 	    (when (executable-find "cat")
; 	      (setenv "PAGER" "cat"))))
;
;
; ;; plan 9 smart shell
; (when dotemacs-eshell/plan9
;   (with-eval-after-load 'esh-module
;     (add-to-list 'eshell-modules-list 'eshell-smart)
;     (setq eshell-where-to-jump 'begin)
;     (setq eshell-review-quick-commands nil)
;     (setq eshell-smart-space-goes-to-end t)))
;; end origianl `init-eshell`


;;; Customization, init file and package management
(use-package cus-edit
  :defer t
  :config
  (setq custom-file dotemacs-custom-file
        custom-buffer-done-kill nil            ; Kill when existing
        custom-buffer-verbose-help nil         ; Remove redundant help text
        ;; Show me the real variable name
        custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil)
  :init (load dotemacs-custom-file 'no-error 'no-message))

(use-package paradox                    ; Better package menu
  :ensure t
  :bind (("C-c l p" . paradox-list-packages)
         ("C-c l P" . package-list-packages-no-fetch))
  :config
  ;; Don't ask for a token, please, and don't bug me about asynchronous updates
  (setq paradox-github-token t
        paradox-execute-asynchronously nil))

(use-package bug-hunter                 ; Search init file for bugs
  :ensure t)

(use-package server                     ; The server of `emacsclient'
  :defer t
  :init (server-mode)
  ; :config (setenv "EDITOR" "emacsclient")
  :diminish server-buffer-clients)


;;; OS X support
(use-package ns-win                     ; OS X window support
  :defer t
  :if (eq system-type 'darwin)
  :init
  (progn
    (global-set-key (kbd "M-V") 'yank)
    (global-set-key (kbd "M-C") 'kill-ring-save)
    (global-set-key (kbd "M-X") 'kill-region)
    (global-set-key (kbd "M-W") 'kill-this-buffer)
    (global-set-key (kbd "M-Z") 'undo-tree-undo)
    (global-set-key (kbd "M-S") 'save-buffer))
  :config
  (setq ns-pop-up-frames nil            ; Don't pop up new frames from the
                                        ; workspace
        mac-option-modifier 'meta       ; Option is simply the natural Meta
        mac-command-modifier 'meta      ; But command is a lot easier to hit
        mac-right-command-modifier 'left
        mac-right-option-modifier 'none ; Keep right option for accented input
        ;; Just in case we ever need these keys
        mac-function-modifier 'hyper))

(use-package init-macosx              ; Personal OS X tools
  :if (eq system-type 'darwin)
  :load-path "config/"
  :defer t
  :config
  ;; Ignore .DS_Store files with ido mode
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  :bind ("C-c C-S-o" . dotemacs-mac-open-current-file))

(use-package osx-trash                  ; Trash support for OS X
  :if (eq system-type 'darwin)
  :ensure t
  :init (osx-trash-setup))


;;; User interface

;; Get rid of tool bar, menu bar and scroll bars.  On OS X we preserve the menu
;; bar, since the top menu bar is always visible anyway, and we'd just empty it
;; which is rather pointless.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No blinking and beeping, no startup screen, no scratch message and short
;; Yes/No questions.
(blink-cursor-mode -1)
(tooltip-mode -1)
(setq ring-bell-function #'ignore
      inhibit-startup-screen t
      initial-scratch-message "Hello there!\n")

(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))

;; Answering just 'y' or 'n' will do
(fset 'yes-or-no-p #'y-or-n-p)
;; Opt out from the startup message in the echo area by simply disabling this
;; ridiculously bizarre thing entirely.
(fset 'display-startup-echo-area-message #'ignore)

(use-package init-scratch          ; My logo in the scratch buffer
  :commands (dotemacs-insert-logo
             dotemacs-insert-logo-into-scratch)
  :init (add-hook 'after-init-hook #'dotemacs-insert-logo-into-scratch))


(defvar pcache-directory
  (let ((dir (concat dotemacs-cache-directory "pcache")))
    (make-directory dir t)
    dir))

(use-package dynamic-fonts              ; Select best available font
  :ensure t
  :config
  (progn
    (setq dynamic-fonts-preferred-monospace-fonts
          '(
            ;; Best fonts for Powerline
            "Source Code Pro for Powerline"   ; https://github.com/adobe-fonts/source-code-pro
            "Anonymous Pro for Powerline" ; http://www.marksimonson.com/fonts/view/anonymous-pro
            ;; Consolas and its free alternative.  Ok, but not my preference
            "Inconsolata for Powerline"
            "Consolas for Powerline"
            ;; Also still kind of ok
            "Fira Mono for Powerline"
            ;; Best fonts
            "Source Code Pro"   ; https://github.com/adobe-fonts/source-code-pro
            "Anonymous Pro" ; http://www.marksimonson.com/fonts/view/anonymous-pro
            ;; Consolas and its free alternative.  Ok, but not my preference
            "Inconsolata"
            "Consolas"
            ;; Also still kind of ok
            "Fira Mono for Powerline"
            ;; System fonts, as last resort
            "Menlo"
            "DejaVu Sans Mono"
            "Bitstream Vera Mono"
            "Courier New")
          dynamic-fonts-preferred-monospace-point-size (pcase system-type
                                                         (`darwin 13)
                                                         (_ 10))
          dynamic-fonts-preferred-proportional-fonts
          '(
            ;; Best, from
            ;; https://www.mozilla.org/en-US/styleguide/products/firefox-os/typeface/
            "Fira Sans"
            ;; System fonts, as last resort
            "Helvetica"
            "Segoe UI"
            "DejaVu Sans"
            "Bitstream Vera"
            "Tahoma"
            "Verdana"
            "Arial Unicode MS"
            "Arial")
          dynamic-fonts-preferred-proportional-point-size (pcase system-type
                                                            (`darwin 13)
                                                            (_ 10)))

    (dynamic-fonts-setup)))

(use-package unicode-fonts              ; Map Unicode blocks to fonts
  :ensure t
  ;; Enable emoticon mappings
  :config (progn (setq unicode-fonts-skip-font-groups '(low-quality-glyphs)
                       unicode-fonts-use-prepend t)
                 (unicode-fonts-setup)))

(use-package solarized                  ; My colour theme
  :disabled t
  :ensure solarized-theme
  :defer t
  :init (load-theme 'solarized-light 'no-confirm)
  :config
  ;; Disable variable pitch fonts in Solarized theme
  (setq solarized-use-variable-pitch nil
        ;; Don't add too much colours to the fringe
        solarized-emphasize-indicators nil
        ;; I find different font sizes irritating.
        solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0))

(use-package zenburn
  :ensure zenburn-theme
  :defer t
  :init (load-theme 'zenburn 'no-confirm))

(bind-key "C-c t v" #'variable-pitch-mode)


;;; The mode line
(setq-default header-line-format
              '(which-func-mode ("" which-func-format " "))
              mode-line-format
              '("%e" mode-line-front-space
                "🐮"                   ; My branding :)
                ;; Standard info about the current buffer
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position
                ;; Some specific information about the current buffer:
                ;; - Paredit
                ;; - Dired Omit Mode
                (paredit-mode (:propertize " ()" face bold))
                (smartparens-strict-mode (:propertize " ()" face bold))
                (dired-omit-mode " 👻")
                (server-buffer-clients " 💻")
                (projectile-mode projectile-mode-line)
                (vc-mode vc-mode)
                " "
                (flycheck-mode flycheck-mode-line) ; Flycheck status
                (firestarter-mode firestarter-lighter)
                (isearch-mode " 🔍")
                (anzu-mode (:eval                  ; isearch pos/matches
                            (when (> anzu--total-matched 0)
                              (anzu--update-mode-line))))
                (multiple-cursors-mode mc/mode-line) ; Number of cursors
                ;; And the modes, which we don't really care for anyway
                " " mode-line-misc-info mode-line-modes mode-line-end-spaces)
              mode-line-remote
              '(:eval
                (when-let (host (file-remote-p default-directory 'host))
                  (propertize (concat "@" host) 'face
                              '(italic warning))))
              ;; Remove which func from the mode line, since we have it in the
              ;; header line
              mode-line-misc-info
              (assq-delete-all 'which-func-mode mode-line-misc-info))

(use-package smart-mode-line   ; smart-mode-line-powerline-theme
  :ensure t
  :defer t
  :config
  (progn
    (setq sml/no-confirm-load-theme t
          sml/theme 'dark
           sml/shorten-directory t
           sml/shorten-modes t
           sml/name-width 20
           sml/mode-width 'full)
    ; (powerline-default-theme)
    (sml/setup)
    (add-to-list 'sml/replacer-regexp-list '("^/usr/local/src" ":🐘src:") t)
    (add-to-list 'sml/replacer-regexp-list '(":🐘src:/ibaset/\\(.*\\)" ":🌰ibaset/\\1:") t)))


;; Standard stuff
(line-number-mode)
(column-number-mode)

(use-package fancy-battery              ; Fancy battery info for mode line
  :ensure t
  :defer t
  :init (fancy-battery-mode))

(use-package anzu                       ; Position/matches count for isearch
  :ensure t
  :init (global-anzu-mode)
  :config (setq anzu-cons-mode-line-p nil)
  :diminish anzu-mode)

(use-package which-func                 ; Current function name in header line
  :init (which-function-mode)
  :config
  (setq which-func-unknown "⊥" ; The default is really boring…
        which-func-format
        `((:propertize (" ➤ " which-func-current)
                       local-map ,which-func-keymap
                       face which-func
                       mouse-face mode-line-highlight
                       help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end"))))


;;; Minibuffer and Helm
(setq history-length 1000)              ; Store more history

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Show the modifier combinations I just typed almost immediately:
(setq echo-keystrokes 0.1)

(use-package savehist                   ; Save minibuffer history
  :init (savehist-mode t)
  :config (setq savehist-save-minibuffer-history t
                savehist-file (concat dotemacs-cache-directory "savehist")
                savehist-additional-variables '(search ring regexp-search-ring)
                savehist-autosave-interval 180))

; Helm: Unite/CtrlP style fuzzy file/buffer/anything searcher on steroids

; Helm does the same thing as Unite/CtrlP on Vim and does it really well. You
; can also enable Helm to manage the command buffer, which is pretty awesome
; with: (helm-mode 1)
(use-package helm
  :ensure t
  :bind (("C-c b b" . helm-resume))
  :init (progn (helm-mode 1)

               (after "helm-config"
                 (warn "`helm-config' loaded! Get rid of it ASAP!")))
  :config
    (progn
      (helm-autoresize-mode t)
      (setq helm-split-window-in-side-p t
            helm-command-prefix-key "C-c b"
            helm-quick-update t
            helm-bookmark-show-location t
            helm-M-x-fuzzy-match t
            helm-apropos-fuzzy-match t
            helm-locate-fuzzy-match t
            helm-file-cache-fuzzy-match t
            helm-semantic-fuzzy-match t
            helm-imenu-fuzzy-match t
            helm-lisp-fuzzy-completion t)

      (use-package helm-swoop
        :ensure t
        :defer t
        :config (setq helm-swoop-pre-input-function #'ignore
                      helm-swoop-use-line-number-face t
                      helm-swoop-split-with-multiple-windows t))

      (use-package helm-descbinds
                   :ensure t
                   :defer t))

  :diminish helm-mode)

(use-package helm-misc                  ; Misc helm commands
  :ensure helm
  :bind (([remap switch-to-buffer] . helm-mini)))

(use-package helm-command               ; M-x in Helm
  :ensure helm
  :bind (([remap execute-extended-command] . helm-M-x)))

(use-package helm-eval                  ; Evaluate expressions with Helm
  :ensure helm
  :bind (("C-c b M-:" . helm-eval-expression-with-eldoc)
         ("C-c b *"   . helm-calcul-expression)))

(use-package helm-color                 ; Input colors with Helm
  :ensure helm
  :bind (("C-c b c" . helm-colors)))

(use-package helm-unicode               ; Unicode input with Helm
  :ensure t
  :bind ("C-c b 8" . helm-unicode))


;;; Buffer, Windows and Frames
(setq truncate-partial-width-windows nil ; Make side by side buffers function
                                         ; the same as the main window.
      frame-resize-pixelwise t           ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

(use-package frame
  :bind (("C-c t F" . toggle-frame-fullscreen))
  :init (progn
          ;; Kill `suspend-frame'
          (global-set-key (kbd "C-z") nil)
          (global-set-key (kbd "C-x C-z") nil))
  :config (add-to-list 'initial-frame-alist '(height . 72) '(width . 140))) ; '(fullscreen . maximized)

(use-package init-buffers          ; Personal buffer tools
  :load-path "config/"
  :commands (dotemacs-force-save-some-buffers
             dotemacs-do-not-kill-important-buffers)
  :init (progn
          (add-hook 'kill-buffer-query-functions
                    #'dotemacs-do-not-kill-important-buffers)

          ;; Autosave buffers when focus is lost, see
          ;; http://emacsredux.com/blog/2014/03/22/a-peek-at-emacs-24-dot-4-focus-hooks/
          (add-hook 'focus-out-hook #'dotemacs-force-save-some-buffers)))

(use-package uniquify                   ; Make buffer names unique
  :config (setq uniquify-buffer-name-style 'forward
                uniquify-separator "/"
                uniquify-ignore-buffers-re "^\\*" ; leave special buffers alone
                uniquify-after-kill-buffer-p t))

(use-package helm-buffers
  :ensure helm
  :defer t
  :config (setq helm-buffers-fuzzy-matching t))

;; http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html
(use-package ibuffer                    ; Better buffer list
  :bind (([remap list-buffers] . ibuffer))
  ;; Show VC Status in ibuffer
  :init (progn
          (add-hook 'ibuffer-hook
                    (lambda ()
                      ; (ibuffer-switch-to-saved-filter-groups "home")
                      (ibuffer-auto-mode 1)))

          (setq ibuffer-expert t
                ibuffer-show-empty-filter-groups nil))

  :config (setq ibuffer-formats
                '((mark modified read-only vc-status-mini " "
                        (name 18 18 :left :elide)
                        " "
                        (size 9 -1 :right)
                        " "
                        (mode 16 16 :left :elide)
                        " "
                        (vc-status 16 16 :left)
                        " "
                        filename-and-process)
                  (mark modified read-only " "
                        (name 18 18 :left :elide)
                        " "
                        (size 9 -1 :right)
                        " "
                        (mode 16 16 :left :elide)
                        " " filename-and-process)
                  (mark " "
                        (name 16 -1)
                        " " filename)))
    (setq ibuffer-saved-filter-groups
          '(("home"
             ("emacs-config" (or (filename . ".emacs.d")
                                 (filename . "emacs-config")))
             ("Org" (or (mode . org-mode)
                        (filename . "OrgMode")))
             ("code" (filename . "code"))
             ("Dev" (or (mode . html-mode)
                        (mode . css-mode)))
             ("Subversion" (name . "\*svn"))
             ("Magit" (name . "\*magit"))
             ("ERC" (mode . erc-mode))
             ("Help" (or (name . "\*Help\*")
                         (name . "\*Apropos\*")
                         (name . "\*info\*")))))))

(use-package ibuffer-vc                 ; Group buffers by VC project and status
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-projectile         ; Group buffers by Projectile project
  :ensure t
  :defer t)

(use-package init-window
  :load-path "config/"
  :defer t
  :bind ("C-c q" . dotemacs-quit-bottom-side-windows))

(use-package windmove                   ; Move between windows with Shift+Arrow
  :bind (("S-<left>"  . windmove-left)
         ("S-<right>" . windmove-right)
         ("S-<up>"    . windmove-up)
         ("S-<down>"  . windmove-down)))

(use-package winner                     ; Undo and redo window configurations
  :init (winner-mode))

(use-package ediff-wind
  :defer t
  :config
  ;; Prevent Ediff from spamming the frame
  (setq ediff-diff-options "-w"
        ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally))

;; http://stackoverflow.com/a/4485083/740527
(use-package init-desktop
  :load-path "config/"
  :defer t
  :bind ("C-c d d" . dotemacs-enable-desktop))

(use-package desktop                    ; Save buffers, windows and frames
  :init
  (setq desktop-dirname (concat dotemacs-cache-directory "desktop/")
        desktop-path                (list desktop-dirname)
        ; desktop-save                t
        desktop-files-not-to-save   "^$" ;reload tramp paths
        desktop-load-locked-desktop nil)
  (desktop-save-mode 0)
  :config (progn ;; Save desktops a minute after Emacs was idle.
            (setq desktop-auto-save-timeout 60)

            (dolist (mode '(magit-mode git-commit-mode))
              (add-to-list 'desktop-modes-not-to-save mode))))

(use-package writeroom-mode             ; Distraction-free editing
  :ensure t
  :bind (("C-c t R" . writeroom-mode)))



;;; File handling
; (setq make-backup-files nil) ; Stop creating backup~ files
; (setq auto-save-default nil) ; Stop creating #autosave# files

;; Keep backup and auto save files out of the way
(setq backup-directory-alist `((".*" . ,(concat dotemacs-cache-directory "backups")))
      auto-save-file-name-transforms `((".*" ,(concat dotemacs-cache-directory "backups") t))
      auto-save-list-file-prefix (concat dotemacs-cache-directory "auto-save-list/saves-"))

;; Transparently open compressed files
(auto-compression-mode t)

;; Delete files to trash
(setq delete-by-moving-to-trash t)

(use-package files
  ;; Revert the current buffer (re-read the contents from disk). Burying
  ;; a buffer (removing it from the current window and sending it to the bottom
  ;; of the stack) is very common for dismissing buffers.
  :bind (("C-c e u" . revert-buffer)
         ("C-c e y" . bury-buffer))
  :config
  ;; Use GNU ls for Emacs
  (when-let (gnu-ls (and (eq system-type 'darwin) (executable-find "gls")))
    (setq insert-directory-program gnu-ls)))

(use-package tramp                      ; Access remote files
  :defer t
  :config
  ;; Store auto-save files locally
  (setq tramp-auto-save-directory (concat dotemacs-cache-directory "tramp-auto-save")))

(use-package dired                      ; Edit directories
  :defer t
  :config
  (progn
    (require 'dired-x)

    (setq dired-auto-revert-buffer t    ; Revert on re-visiting
          ;; Move files between split panes
          dired-dwim-target t
          ;; Better dired flags: `-l' is mandatory, `-a' shows all files, `-h'
          ;; uses human-readable sizes, and `-F' appends file-type classifiers
          ;; to file names (for better highlighting)
          dired-listing-switches "-alhF"
          dired-ls-F-marks-symlinks t   ; -F marks links with @
          ;; Inhibit prompts for simple recursive operations
          dired-recursive-copies 'always)

    (when (or (memq system-type '(gnu gnu/linux))
              (string= (file-name-nondirectory insert-directory-program) "gls"))
      ;; If we are on a GNU system or have GNU ls, add some more `ls' switches:
      ;; `--group-directories-first' lists directories before files, and `-v'
      ;; sorts numbers in file names naturally, i.e. "image1" goes before
      ;; "image02"
      (setq dired-listing-switches
            (concat dired-listing-switches " --group-directories-first -v")))))

(use-package dired-x                    ; Additional tools for Dired
  :bind (("C-x C-j" . dired-jump))
  :init (add-hook 'dired-mode-hook #'dired-omit-mode)
  :config
  (progn
    (setq dired-omit-verbose nil)        ; Shut up, dired

    (when (eq system-type 'darwin)
      ;; OS X bsdtar is mostly compatible with GNU Tar
      (setq dired-guess-shell-gnutar "tar"))

    ;; Diminish dired-omit-mode. We need this hack, because Dired Omit Mode has
    ;; a very peculiar way of registering its lighter explicitly in
    ;; `dired-omit-startup'.  We can't just use `:diminish' because the lighter
    ;; isn't there yet after dired-omit-mode is loaded.
    (add-function :after (symbol-function 'dired-omit-startup)
                  (lambda () (diminish 'dired-omit-mode))
                  '((name . dired-omit-mode-diminish)))))

(use-package helm-files
  :ensure helm
  :defer t
  :bind (([remap find-file] . helm-find-files)
         ("C-c f r"         . helm-recentf))
  :config (setq helm-recentf-fuzzy-match t
                ;; Use recentf to find recent files
                helm-ff-file-name-history-use-recentf t
                ;; Find library from `require', `declare-function' and friends
                helm-ff-search-library-in-sexp t))

(use-package ignoramus                  ; Ignore uninteresting files everywhere
  :ensure t
  :config (progn (dolist (name '(".cask" ".vagrant"))
                   ;; Ignore some additional directories
                   (add-to-list 'ignoramus-file-basename-exact-names name))
                 (ignoramus-setup)))

(use-package hardhat ; Protect user-writable files
  :ensure t
  :init (global-hardhat-mode)
  (setq hardhat-buffer-protected-functions '(hardhat-protected-by-ignoramus))
  :config (setq hardhat-mode-lighter "🔒"))


(use-package tramp                      ; Access remote files
  :defer t
  :config
  ;; Store auto-save files locally
  (setq tramp-auto-save-directory (concat dotemacs-cache-directory "tramp-auto-save")))

(use-package bookmark                   ; Bookmarks for Emacs buffers
  :bind (("C-c l b" . list-bookmarks))
  ;; Save bookmarks immediately after a bookmark was added
  :config
  (setq bookmark-save-flag 1)
  ;; Store auto-save files locally
  (setq bookmark-default-file (concat dotemacs-cache-directory "bookmarks")))


;; original
(run-with-timer 1800 1800 'recentf-save-list)
;; original

(use-package recentf                    ; Save recently visited files
  :init
  (setq recentf-save-file (concat dotemacs-cache-directory "recentf"))
  (recentf-mode)

  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 50
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        recentf-auto-cleanup 300
        recentf-exclude (list "COMMIT_EDITMSG\\'"
                              "/\\.git/.*\\'" ; Git contents
                              "/elpa/.*\\'" ; Package files
                              "/itsalltext/" ; It's all text temp files
                              ;; And all other kinds of boring files
                              #'ignoramus-boring-p)))

;; move cursor to the last position upon open
(use-package saveplace                  ; Save point position in files
  :config (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

(setq view-read-only t)                 ; View read-only files

(use-package autorevert                 ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode)
  :config (setq auto-revert-verbose nil ; Shut up, please!
                ;; Revert Dired buffers, too
                global-auto-revert-non-file-buffers t))

(use-package image-file                 ; Visit images as images
  :init (auto-image-file-mode))

(use-package launch                     ; Open files in external programs
  :ensure t
  :defer t)

(use-package reveal-in-finder           ; Reveal current buffer in finder
  :ensure t
  :bind (("C-c f f" . reveal-in-finder)))

(use-package init-files            ; Personal file tools
  :load-path "config/"
  :defer t
  :commands (dotemacs-create-non-existent-directory)
  :init
  (add-to-list 'find-file-not-found-functions 'dotemacs-create-non-existent-directory)
  :bind (("C-c f D" . dotemacs-delete-file-and-buffer)
         ("C-c f i" . dotemacs-open-in-intellij)
         ("C-c f o" . dotemacs-launch-dwim)
         ("C-c f R" . dotemacs-rename-file-and-buffer)
         ("C-c f w" . dotemacs-copy-filename-as-kill)
         ("C-c f u" . dotemacs-find-user-init-file-other-window)
         ("C-c w ." . dotemacs-browse-feature-url)))

(use-package find-file-hook
  :defer t
  :init (after "init-files"
  (progn
    (add-hook 'find-file-hook
              (lambda ()
                (unless (eq major-mode 'org-mode)
                  (setq show-trailing-whitespace t))))
    (add-hook 'find-file-hook #'visual-line-mode)
    (add-hook 'find-file-hook #'dotemacs-find-file-check-large-file))))

;; Additional bindings for built-ins
(bind-key "C-c f v d" #'add-dir-local-variable)
(bind-key "C-c f v l" #'add-file-local-variable)
(bind-key "C-c f v p" #'add-file-local-variable-prop-line)


;;; Navigation and scrolling
(setq scroll-margin 3                   ; 0 to drag the point along while scrolling
      scroll-conservatively 1000        ; Never recenter the screen while scrolling
      scroll-error-top-bottom t         ; Move to beg/end of buffer before
                                        ; signalling an error
      ;; scroll-preserve-screen-position t
      ;; These settings make trackpad scrolling on OS X much more predictable
      ;; and smooth
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1))

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(use-package avy-jump                   ; Jump to characters in buffers
  :ensure avy
  :bind (("C-c j s" . avy-isearch)
         ("C-c j j" . avy-goto-char-2)
         ("C-c j w" . avy-goto-word-1)))

(use-package ace-link                   ; Fast link jumping
  :ensure t
  :defer t
  :init (progn (after "info"
                 (bind-key "C-c j l" #'ace-link-info Info-mode-map))

               (after "help-mode"
                 (defvar help-mode-map)  ; Silence the byte compiler
                 (bind-key "C-c j l" #'ace-link-help help-mode-map))))

(use-package ace-window                 ; Fast window switching
  :ensure t
  :bind (("C-x o" . ace-window)
         ("C-c o" . ace-window)))

(use-package page-break-lines           ; Turn page breaks into lines
  :ensure t
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package outline                    ; Navigate outlines in buffers
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'outline-minor-mode))
  :diminish (outline-minor-mode . "📑"))

(use-package nlinum                     ; Line numbers in display margin
  :ensure t
  :bind (("C-c t l" . nlinum-mode)))

(use-package helm-imenu
  :ensure helm
  :bind (("C-c i" . helm-imenu-in-all-buffers)))


;;; Basic editing

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; Don't highlight matches with jump-char - it's distracting
(setq jump-char-lazy-highlight-face nil)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; enable electric indent
(setq electric-indent-mode t)

;; Disable tabs, but given them proper width
(setq-default indent-tabs-mode nil
              highlight-tabs t
              tab-width 8)
;; Make Tab complete if the line is indented
(setq tab-always-indent 'complete)

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(setq indicate-empty-lines t
      require-final-newline t)

;; Globally on't break lines for me, please
; (setq-default truncate-lines t)

(setq kill-ring-max 200                 ; More killed items
      ;; Save the contents of the clipboard to kill ring before killing
      save-interprogram-paste-before-kill t)

(setq mark-ring-max 64
      global-mark-ring-max 128
      create-lockfiles nil)

;; Configure a reasonable fill column, indicate it in the buffer and enable
;; automatic filling
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)
(diminish 'auto-fill-function "↵")

(use-package init-simple           ; Personal editing helpers
  :load-path "config/"
  :bind (([remap kill-whole-line]        . dotemacs-smart-kill-whole-line)
         ([remap move-beginning-of-line] . dotemacs-back-to-indentation-or-beginning-of-line)
         ("C-<backspace>"                . dotemacs-smart-backward-kill-line)
         ("C-S-j"                        . dotemacs-smart-open-line)
         ;; Additional utilities
         ("C-c e d"                      . dotemacs-insert-current-date))
  :commands (dotemacs-auto-fill-comments-mode)
  ;; Auto-fill comments in programming modes
  :init (add-hook 'prog-mode-hook #'dotemacs-auto-fill-comments-mode))

(use-package helm-ring                  ; Helm commands for rings
  :ensure helm
  :bind (([remap yank-pop]        . helm-show-kill-ring)
         ([remap insert-register] . helm-register)))

(use-package delsel                     ; Delete the selection instead of insert
  :defer t
  :init (delete-selection-mode))

(use-package whitespace-cleanup-mode    ; Cleanup whitespace in buffers
  :ensure t
  :bind (("C-c t c" . whitespace-cleanup-mode)
         ("C-c e w" . whitespace-cleanup))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode))
  :diminish (whitespace-cleanup-mode . "⌫"))

(use-package subword                    ; Subword/superword editing
  :defer t
  :diminish subword-mode)

(use-package adaptive-wrap              ; Choose wrap prefix automatically
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package visual-fill-column
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

;;https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
(setq reb-re-syntax 'string) ;; fix backslash madness

(use-package visual-regexp              ; Regexp replace with in-buffer display
  :ensure t
  :bind (("C-c r" . vr/query-replace)
         ("C-c R" . vr/replace)))

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

(use-package easy-kill                  ; Easy killing and marking on C-w
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp]      . easy-mark)))

(use-package align                      ; Align text in buffers
  :bind (("C-c e a" . align)
         ("C-c e c" . align-current)
         ("C-c e r" . align-regexp)))

(use-package multiple-cursors           ; Edit text with multiple cursors
  :ensure t
  :bind (("C-c m <SPC>" . mc/vertical-align-with-space)
         ("C-c m a"     . mc/vertical-align)
         ("C-c m e"     . mc/mark-more-like-this-extended)
         ("C-c m h"     . mc/mark-all-like-this-dwim)
         ("C-c m l"     . mc/edit-lines)
         ("C-c m n"     . mc/mark-next-like-this)
         ("C-c m p"     . mc/mark-previous-like-this)
         ("C-c m r"     . vr/mc-mark)
         ("C-c m C-a"   . mc/edit-beginnings-of-lines)
         ("C-c m C-e"   . mc/edit-ends-of-lines)
         ("C-c m C-s"   . mc/mark-all-in-region))
  :config
  (setq mc/mode-line
        ;; Simplify the MC mode line indicator
        '(:propertize (:eval (concat " " (number-to-string (mc/num-cursors))))
                      face font-lock-warning-face)))

(use-package expand-region              ; Expand region by semantic units
  :ensure t
  :bind (("C-=" . er/expand-region)))

;; TODO: Incorporate this into `use-package undo-tree` below
;; Represent undo-history as an actual tree (visualize with C-x u)
; (setq undo-tree-mode-lighter "")
; (setq undo-tree-auto-save-history t)
; (setq undo-tree-history-directory-alist
;       `(("." . ,(concat dotemacs-cache-directory "undo"))))
;     (unless (file-exists-p (concat dotemacs-cache-directory "undo"))
;         (make-directory (concat dotemacs-cache-directory "undo")))
; (setq undo-tree-visualizer-timestamps t)
; (setq undo-tree-visualizer-diff t)

(use-package undo-tree                  ; Branching undo
  :ensure t
  :init (global-undo-tree-mode)
  :diminish (undo-tree-mode . "↺"))

;; Give us narrowing back!
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Same for region casing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package auto-insert                ; Automatic insertion into new files
  :defer t
  :bind (("C-c e i" . auto-insert)))

(use-package copyright                  ; Deal with copyright notices
  :defer t
  :bind (("C-c e C" . copyright-update))
  ;; Update copyright when visiting files
  :init (add-hook 'find-file-hook #'copyright-update)
  ;; Use ranges to denote consecutive years
  :config (setq copyright-year-ranges t
                copyright-names-regexp (regexp-quote user-full-name)))

;; Additional keybindings
(bind-key [remap just-one-space] #'cycle-spacing)


;;; Paired delimiters
(use-package elec-pair                  ; Electric pairs
  :disabled t
  :init (electric-pair-mode))

(use-package paren                      ; Highlight paired delimiters
  :disabled t
  :init
  (show-paren-mode)
  (setq show-paren-delay 0)
  :config (setq show-paren-when-point-inside-paren t
                show-paren-when-point-in-periphery t))

(use-package paredit                    ; Balanced sexp editing
  :disabled t
  :ensure t
  :defer t
  :init (dolist (hook '(eval-expression-minibuffer-setup-hook
                        emacs-lisp-mode-hook
                        inferior-emacs-lisp-mode-hook
                        clojure-mode-hook))
          (add-hook hook #'paredit-mode))
  :config
  (progn
    ;; Free M-s.  There are some useful bindings in that prefix map.
    (define-key paredit-mode-map (kbd "M-s") nil)
    (define-key paredit-mode-map (kbd "M-S-<up>") #'paredit-splice-sexp))
  :diminish paredit-mode)

;; TODO: Incorporate this into `use-package smartparens` below
;; original `init-smartparens`
; (defgroup dotemacs-smartparens nil
;   "Configuration options for smartparens."
;   :group 'dotemacs
;   :prefix 'dotemacs-smartparens)
;
; (defcustom dotemacs-smartparens/autoinsert nil
;   "When non-nil, turn on smartparens auto pairing instead of the default Emacs electric-pair-mode."
;   :group 'dotemacs-smartparens)
;
; (defcustom dotemacs-smartparens/show-paren nil
;   "When non-nil, turn on smartparens paren matching instead of the default Emacs show-paren-mode."
;   :group 'dotemacs-smartparens)
;
; (setq sp-autoescape-string-quote nil)
; (setq sp-autoinsert-quote-if-followed-by-closing-pair nil)
;
; (if dotemacs-smartparens/autoinsert
;     (progn
;       (setq sp-autoinsert-pair t)
;       (electric-pair-mode -1))
;   (setq sp-autoinsert-pair nil))
;
; (sp-use-smartparens-bindings)
;
; (when dotemacs-smartparens/show-paren
;   (setq sp-show-pair-delay 0)
;   (setq sp-show-pair-from-inside t)
;   (show-paren-mode -1)
;   (show-smartparens-global-mode t))
;
; (defun my-open-block-c-mode (id action context)
;   (when (eq action 'insert)
;     (newline)
;     (indent-according-to-mode)
;     (forward-line -1)
;     (indent-according-to-mode)))
;
; (sp-pair "{" nil :post-handlers '(:add (my-open-block-c-mode "RET")))
; (sp-pair "[" nil :post-handlers '(:add (my-open-block-c-mode "RET")))
;
; ;; fix conflict where smartparens clobbers yas' key bindings
; (with-eval-after-load 'yasnippet
;   (defadvice yas-expand (before advice-for-yas-expand activate)
;     (sp-remove-active-pair-overlay)))
;; end origianl `init-smartparens`

;; Use SmartParens instead of Paredit and Electric Pair
(use-package smartparens                ; Parenthesis editing and balancing
  :ensure t
  :init (progn (smartparens-global-mode)
               (show-smartparens-global-mode)

               (dolist (hook '(inferior-emacs-lisp-mode-hook
                               emacs-lisp-mode-hook))
                 (add-hook hook #'smartparens-strict-mode)))
  :config (setq sp-autoskip-closing-pair 'always
                ;; Don't kill entire symbol on C-k
                sp-hybrid-kill-entire-symbol nil)
  :diminish smartparens-mode)

(use-package init-smartparens      ; Personal Smartparens extensions
  :load-path "config/")


;;; Highlights and fontification
(defun dotemacs-whitespace-style-no-long-lines ()
  "Configure `whitespace-mode' for Org.

Disable the highlighting of overlong lines."
  (setq-local whitespace-style (-difference whitespace-style
                                            '(lines lines-tail))))

(defun dotemacs-whitespace-mode-local ()
  "Enable `whitespace-mode' after local variables where set up."
  (add-hook 'hack-local-variables-hook #'whitespace-mode nil 'local))

;; Trailing whitespace
;; I don’t want to leave trailing whitespace in files I touch, so set
;; up a hook that automatically deletes trailing whitespace after
;; every line when saving a file:
; (add-hook 'write-file-hooks 'delete-trailing-whitespace)

(use-package whitespace                 ; Highlight bad whitespace
  :bind (("C-c t w" . whitespace-mode))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'dotemacs-whitespace-mode-local))
  :config
  ;; Highlight tabs, empty lines at beg/end, trailing whitespaces and overlong
  ;; portions of lines via faces.  Also indicate tabs via characters
  (setq whitespace-style '(face indentation space-after-tab space-before-tab
                                tab-mark empty trailing lines-tail)
        whitespace-line-column nil)     ; Use `fill-column' for overlong lines
  :diminish (whitespace-mode . "▢"))

(use-package hl-line                    ; Highlight the current line
  :init (global-hl-line-mode 1))

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'rainbow-delimiters-mode)))

(use-package hi-lock                    ; Custom regexp highlights
  :init (global-hi-lock-mode))

(use-package highlight-numbers          ; Fontify number literals
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package highlight-quoted
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-quoted-mode))

(use-package rainbow-mode               ; Fontify color values in code
  :ensure t
  :bind (("C-c t r" . rainbow-mode))
  :init (dolist (hook '(js-mode-hook js2-mode-hook html-mode-hook web-mode-hook css-mode-hook stylus-mode-hook stylus-mode-hook handlebars-mode-hook))
          (add-hook hook #'rainbow-mode)))

(use-package highlight-symbol           ; Highlighting and commands for symbols
  :ensure t
  :defer t
  :bind
  (("C-c s %" . highlight-symbol-query-replace)
   ("C-c s n" . highlight-symbol-next-in-defun)
   ("C-c s o" . highlight-symbol-occur)
   ("C-c s p" . highlight-symbol-prev-in-defun))
  ;; Navigate occurrences of the symbol under point with M-n and M-p, and
  ;; highlight symbol occurrences
  :init (progn (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
               (add-hook 'prog-mode-hook #'highlight-symbol-mode))
  :config
  (setq highlight-symbol-idle-delay 0.3     ; Highlight almost immediately
        highlight-symbol-on-navigation-p t) ; Highlight immediately after
                                        ; navigation
  :diminish highlight-symbol-mode)


;;; Skeletons, completion and expansion

;; In `completion-at-point', do not pop up silly completion buffers for less
;; than five candidates.  Cycle instead.
(setq completion-cycle-threshold 5)

;; tell emacs where to read abbrev
(setq abbrev-file-name (concat dotemacs-cache-directory "abbrev_defs"))

(use-package hippie-exp                 ; Powerful expansion and completion
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          dotemacs-try-complete-lisp-symbol-without-namespace)))

(use-package init-hippie-exp       ; Custom expansion functions
  :load-path "config/"
  :commands (dotemacs-try-complete-lisp-symbol-without-namespace))

;; TODO: Incorporate this into `use-package company` below
;; original `init-company`
; (defgroup dotemacs-company nil
;   "Configuration options for company-mode."
;   :group 'dotemacs
;   :prefix 'dotemacs-company)
;
; (defcustom dotemacs-company/ycmd-server-command nil
;   "The path to the ycmd package."
;   :group 'dotemacs-company)
;
; (require 'company)
;
; (setq company-idle-delay 0.5)
; (setq company-minimum-prefix-length 2)
; (setq company-show-numbers t)
; (setq company-tooltip-limit 10)
; ;; invert the navigation direction if the the completion popup-isearch-match
; ;; is displayed on top (happens near the bottom of windows)
; (setq company-tooltip-flip-when-above t)
;
;
; (setq company-dabbrev-downcase nil)
; (setq company-dabbrev-ignore-case t)
;
; (setq company-dabbrev-code-ignore-case t)
; (setq company-dabbrev-code-everywhere t)
;
; (setq company-etags-ignore-case t)
;
; (unless (face-attribute 'company-tooltip :background)
;   (set-face-attribute 'company-tooltip nil :background "black" :foreground "gray40")
;   (set-face-attribute 'company-tooltip-selection nil :inherit 'company-tooltip :background "gray15")
;   (set-face-attribute 'company-preview nil :background "black")
;   (set-face-attribute 'company-preview-common nil :inherit 'company-preview :foreground "gray40")
;   (set-face-attribute 'company-scrollbar-bg nil :inherit 'company-tooltip :background "gray20")
;   (set-face-attribute 'company-scrollbar-fg nil :background "gray40"))
;
; (when (executable-find "tern")
;   (with-eval-after-load "company-tern-autoloads"
;     (add-to-list 'company-backends 'company-tern)))
;
; (setq company-global-modes
;       '(not
;         eshell-mode comint-mode org-mode erc-mode))
;
; (defadvice company-complete-common (around advice-for-company-complete-common activate)
;   (when (null (yas-expand))
;     ad-do-it))
;
; (defun my-company-tab ()
;   (interactive)
;   (when (null (yas-expand))
;     (company-select-next)))
;
; (when dotemacs-company/ycmd-server-command
;   (setq ycmd-server-command `("python" ,dotemacs-company/ycmd-server-command))
;   (require 'ycmd)
;   (ycmd-setup)
;
;   (require 'company-ycmd)
;   (company-ycmd-setup))
;
; (global-company-mode)
;
; (when (display-graphic-p)
;   (require 'company-quickhelp)
;   (setq company-quickhelp-delay 0.2)
;   (company-quickhelp-mode t))
;; end origianl `init-company`

(use-package company                    ; Graphical (auto-)completion
  :ensure t
  :if (eq dotemacs-completion-engine 'company)
  :init (global-company-mode)
  :config
  (progn
    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t))
  :diminish company-mode)

(use-package company-statistics
  :ensure t
  :if (eq dotemacs-completion-engine 'company)
  :defer t
  :init
  (setq company-statistics-file (concat dotemacs-cache-directory "company-statistics-cache.el"))
  (company-statistics-mode))

(use-package company-math               ; Completion for Math symbols
  :ensure t
  :if (eq dotemacs-completion-engine 'company)
  :defer t
  :init (after "company"
          ;; Add backends for math characters
          (add-to-list 'company-backends 'company-math-symbols-unicode)
          (add-to-list 'company-backends 'company-math-symbols-latex)))

(use-package helm-company
  :ensure t
  :if (eq dotemacs-completion-engine 'company)
  :defer t
  :init (after "company"
          ;; Use Company for completion
          (bind-key [remap completion-at-point] #'helm-company company-mode-map)
          (bind-key "C-:" #'helm-company company-mode-map)
          (bind-key "C-:" #'helm-company company-active-map)))

;; TODO: Incorporate this into `use-package auto-complete` below
;; original `init-auto-complete`
; (require 'auto-complete)
; (require 'auto-complete-config)
;
; (setq completion-ignored-extensions
;       '(".xpt" ".a" ".so" ".o" ".d" ".elc" ".class" "~" ".ckp" ".bak" ".imp" ".lpt" ".bin" ".otl" ".err" ".lib" ".x9700" ".aux" ".elf" ))
;
; (setq ac-auto-show-menu t)
; (setq ac-auto-start t)
; (setq ac-comphist-file (concat dotemacs-cache-directory "ac-comphist.dat"))
; (setq ac-quick-help-delay 0.3)
; (setq ac-quick-help-height 30)
; (setq ac-show-menu-immediately-on-auto-complete t)
;
; (dolist (mode '(vimrc-mode html-mode stylus-mode handlebars-mode mustache-mode))
;   (add-to-list 'ac-modes mode))
;
; (ac-config-default)
;
; (with-eval-after-load 'linum
;   (ac-linum-workaround))
;
; (with-eval-after-load 'yasnippet
;   (add-hook 'yas-before-expand-snippet-hook (lambda () (auto-complete-mode -1)))
;   (add-hook 'yas-after-exit-snippet-hook (lambda () (auto-complete-mode t)))
;   (defadvice ac-expand (before advice-for-ac-expand activate)
;     (when (yas-expand)
;       (ac-stop))))
;
; (require 'ac-etags)
; (setq ac-etags-requires 1)
; (after "etags"
;   (ac-etags-setup))
;; end origianl `init-auto-complete`

(use-package auto-complete
  :ensure t
  :if (eq dotemacs-completion-engine 'auto-complete)
  :diminish auto-complete-mode)

(use-package init-yasnippet
  :load-path "config/"
  :defer t
  :commands (dotemacs-load-yasnippet
             dotemacs-force-yasnippet-off))

;; TODO: Incorporate this into `use-package yasnippet` below
;; original `init-yasnippet`
; (let* ((yas-install-dir (car (file-expand-wildcards (concat package-user-dir "/yasnippet-*"))))
;        (dir (concat yas-install-dir "/snippets/js-mode")))
;   (if (file-exists-p dir)
;       (delete-directory dir t)))
;
; (setq yas-fallback-behavior 'return-nil)
; (setq yas-also-auto-indent-first-line t)
; (setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))
;
; (yas-load-directory (concat user-emacs-directory "/snippets"))
;; end origianl `init-yasnippet`

(use-package yasnippet
  :commands yas-global-mode
  :disabled t
  :ensure t
  :init
  (progn
    ;; disable yas minor mode map, use hippie-expand instead
    (setq yas-minor-mode-map (make-sparse-keymap)))
    (dolist (mode '(js2 markdown html css org))
      (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
                (lambda ()
                  (dotemacs-load-yasnippet))))
    (dolist (mode '(term shell))
      (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
                (lambda ()
                  (dotemacs-force-yasnippet-off))))
  :diminish (yas-minor-mode " ⓨ" " y"))


;;; Spelling and syntax checking
(use-package ispell                     ; Spell checking
  :defer t
  :config
  (progn
    (setq ispell-program-name (if (eq system-type 'darwin)
                                  (executable-find "aspell")
                                (executable-find "hunspell"))
          ; ispell-extra-args '("--sug-mode=ultra")
          ispell-dictionary "en_US"     ; Default dictionnary
          ispell-silently-savep t       ; Don't ask when saving the private dict
          ;; Increase the height of the choices window to take our header line
          ;; into account.
          ispell-choices-win-default-height 5)

    (unless ispell-program-name
      (warn "No spell checker available. Install Hunspell or ASpell for OS X."))))

(use-package flyspell                   ; On-the-fly spell checking
  :bind (("C-c t s" . flyspell-mode))
  :init (progn (dolist (hook '(text-mode-hook message-mode-hook org-mode-hook))
                 (add-hook hook 'turn-on-flyspell))
               (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config
  (progn
    (setq flyspell-use-meta-tab nil
          ;; Make Flyspell less chatty
          flyspell-issue-welcome-flag nil
          flyspell-issue-message-flag nil)

    ;; Free C-M-i for completion
    (define-key flyspell-mode-map "\M-\t" nil))
  :diminish (flyspell-mode . "✓"))

;; TODO: Incorporate this into `use-package flycheck` below
;; original `init-flycheck`
; (require 'flycheck)
;
; (after "flycheck"
;   ;; Remove newline checks, since they would trigger an immediate check
;   ;; when we want the idle-change-delay to be in effect while editing.
;   (setq flycheck-check-syntax-automatically '(save
;                                               idle-change
;                                               mode-enabled))
;   (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
;   (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
;   (setq flycheck-standard-error-navigation nil))
;
; (global-flycheck-mode t)
;
; ;; flycheck errors on a tooltip (doesnt work on console)
; (when (display-graphic-p (selected-frame))
;   (eval-after-load 'flycheck
;     '(custom-set-variables
;       '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))
;
; (defun my/adjust-flycheck-automatic-syntax-eagerness ()
;   "Adjust how often we check for errors based on if there are any.
;
; This lets us fix any errors as quickly as possible, but in a
; clean buffer we're an order of magnitude laxer about checking."
;   (setq flycheck-idle-change-delay
;         (if flycheck-current-errors 0.5 30.0)))
;
; ;; Each buffer gets its own idle-change-delay because of the
; ;; buffer-sensitive adjustment above.
; (make-variable-buffer-local 'flycheck-idle-change-delay)
;
; (add-hook 'flycheck-after-syntax-check-hook
;           'my/adjust-flycheck-automatic-syntax-eagerness)
;
; (defun flycheck-handle-idle-change ()
;   "Handle an expired idle time since the last change.
;
; This is an overwritten version of the original
; flycheck-handle-idle-change, which removes the forced deferred.
; Timers should only trigger inbetween commands in a single
; threaded system and the forced deferred makes errors never show
; up before you execute another command."
;   (flycheck-clear-idle-change-timer)
;   (flycheck-buffer-automatically 'idle-change))
;; end origianl `init-auto-flycheck`


(use-package flycheck                   ; On-the-fly syntax checking
  :ensure t
  :bind (("C-c l e" . list-flycheck-errors)
         ("C-c t f" . flycheck-mode))
  :init
  (global-flycheck-mode)
  ; (add-hook 'prog-mode-hook #'flycheck-mode)
  :config (progn
            (setq flycheck-display-errors-function
                  #'flycheck-display-error-messages-unless-error-list)

            ;; Use italic face for checker name
            (set-face-attribute 'flycheck-error-list-checker-name nil
                                :inherit 'italic)

            (add-to-list 'display-buffer-alist
                         `(,(rx bos "*Flycheck errors*" eos)
                           (display-buffer-reuse-window
                            display-buffer-in-side-window)
                           (side            . bottom)
                           (reusable-frames . visible)
                           (window-height   . 0.4))))
  :diminish flycheck-mode)

(use-package helm-flycheck
  :ensure t
  :bind (("C-c ! L" . helm-flycheck)))

(use-package init-flycheck         ; Personal Flycheck helpers
  :load-path "config/"
  :defer t
  :commands (dotemacs-discard-undesired-html-tidy-error
             dotemacs-flycheck-mode-line-status)
  :init (after "flycheck"
          ;; Don't highlight undesired errors from html tidy
          (add-hook 'flycheck-process-error-functions
                    #'dotemacs-discard-undesired-html-tidy-error)

          (setq flycheck-mode-line
                '(:eval (dotemacs-flycheck-mode-line-status)))))


;; Text editing
(use-package tildify
  :bind (("C-c e t" . tildify-region))
  :init (dolist (hook '(markdown-mode-hook
                        latex-mode-hook
                        rst-mode-hook))
          (add-hook hook #'tildify-mode))
  ;; Use the right space for LaTeX
  :config (add-hook 'latex-mode-hook
                    (lambda () (setq-local tildify-space-string "~"))))

(use-package typo
  :ensure t
  :bind (("C-c t t" . typo-mode))
  :init (progn
          (typo-global-mode)

          (dolist (hook '(markdown-mode-hook
                          rst-mode-hook))
            (add-hook hook 'typo-mode)))
  :diminish (typo-mode . "𝕿"))



;;; LaTeX with AUCTeX
(use-package tex-site                   ; AUCTeX initialization
  :ensure auctex)

(use-package tex                        ; TeX editing/processing
  :ensure auctex
  :defer t
  :config
  (progn
    (setq TeX-parse-self t              ; Parse documents to provide completion
                                        ; for packages, etc.
          TeX-auto-save t               ; Automatically save style information
          TeX-electric-sub-and-superscript t ; Automatically insert braces after
                                        ; sub- and superscripts in math mode
          TeX-electric-math '("\\(" "\\)")
          ;; Don't insert magic quotes right away.
          TeX-quote-after-quote t
          ;; Don't ask for confirmation when cleaning
          TeX-clean-confirm nil
          ;; Provide forward and inverse search with SyncTeX
          TeX-source-correlate-mode t
          TeX-source-correlate-method 'synctex)
    (setq-default TeX-master nil        ; Ask for the master file
                  TeX-engine 'luatex    ; Use a modern engine
                  ;; Redundant in 11.88, but keep for older AUCTeX
                  TeX-PDF-mode t)

    ;; Move to chktex
    (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s")))

(use-package tex-buf                    ; TeX buffer management
  :ensure auctex
  :defer t
  ;; Don't ask for confirmation when saving before processing
  :config (setq TeX-save-query nil))

(use-package tex-style                  ; TeX style
  :ensure auctex
  :defer t
  :config
  ;; Enable support for csquotes
  (setq LaTeX-csquotes-close-quote "}"
        LaTeX-csquotes-open-quote "\\enquote{"))

(use-package tex-fold                   ; TeX folding
  :ensure auctex
  :defer t
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(use-package tex-mode                   ; TeX mode
  :ensure auctex
  :defer t
  :config
  (font-lock-add-keywords 'latex-mode
                          `((,(rx "\\"
                                  symbol-start
                                  "fx" (1+ (or (syntax word) (syntax symbol)))
                                  symbol-end)
                             . font-lock-warning-face))))

(use-package latex                      ; LaTeX editing
  :ensure auctex
  :defer t
  :config
  (progn
    ;; Teach TeX folding about KOMA script sections
    (setq TeX-outline-extra `((,(rx (0+ space) "\\section*{") 2)
                              (,(rx (0+ space) "\\subsection*{") 3)
                              (,(rx (0+ space) "\\subsubsection*{") 4)
                              (,(rx (0+ space) "\\minisec{") 5))
          ;; No language-specific hyphens please
          LaTeX-babel-hyphen nil)

    (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)))    ; Easy math input

(use-package auctex-latexmk             ; latexmk command for AUCTeX
  :ensure t
  :defer t
  :init (after "latex"
          (auctex-latexmk-setup)))

(use-package bibtex                     ; BibTeX editing
  :defer t
  :config
  (progn
    ;; Run prog mode hooks for bibtex
    (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

    ;; Use a modern BibTeX dialect
    (bibtex-set-dialect 'biblatex)))

(defun dotemacs-reftex-find-ams-environment-caption (environment)
  "Find the caption of an AMS ENVIRONMENT."
  (let ((re (rx-to-string `(and "\\begin{" ,environment "}"))))
    ;; Go to the beginning of the label first
    (re-search-backward re)
    (goto-char (match-end 0)))
  (if (not (looking-at (rx (zero-or-more space) "[")))
      (error "Environment %s has no title" environment)
    (let ((beg (match-end 0)))
      ;; Move point onto the title start bracket and move over to the end,
      ;; skipping any other brackets in between, and eventually extract the text
      ;; between the brackets
      (goto-char (1- beg))
      (forward-list)
      (buffer-substring-no-properties beg (1- (point))))))

(use-package reftex                     ; TeX/BibTeX cross-reference management
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
  :config
  (progn
    ;; Plug into AUCTeX
    (setq reftex-plug-into-AUCTeX t
          ;; Automatically derive labels, and prompt for confirmation
          reftex-insert-label-flags '(t t)
          reftex-label-alist
          '(
            ;; Additional label definitions for RefTeX.
            ("definition" ?d "def:" "~\\ref{%s}"
             dotemacs-reftex-find-ams-environment-caption
             ("definition" "def.") -3)
            ("theorem" ?h "thm:" "~\\ref{%s}"
             dotemacs-reftex-find-ams-environment-caption
             ("theorem" "th.") -3)
            ("example" ?x "ex:" "~\\ref{%s}"
             dotemacs-reftex-find-ams-environment-caption
             ("example" "ex") -3)
            ;; Algorithms package
            ("algorithm" ?a "alg:" "~\\ref{%s}"
             "\\\\caption[[{]" ("algorithm" "alg") -3)))

    ;; Provide basic RefTeX support for biblatex
    (unless (assq 'biblatex reftex-cite-format-builtin)
      (add-to-list 'reftex-cite-format-builtin
                   '(biblatex "The biblatex package"
                              ((?\C-m . "\\cite[]{%l}")
                               (?t . "\\textcite{%l}")
                               (?a . "\\autocite[]{%l}")
                               (?p . "\\parencite{%l}")
                               (?f . "\\footcite[][]{%l}")
                               (?F . "\\fullcite[]{%l}")
                               (?x . "[]{%l}")
                               (?X . "{%l}"))))
      (setq reftex-cite-format 'biblatex)))
  :diminish reftex-mode)


;; Other markup languages
(use-package rst                        ; ReStructuredText
  :defer t
  :config
  ;; Indent with 3 spaces after all kinds of literal blocks
  (setq rst-indent-literal-minimized 3
        rst-indent-literal-normal 3)

  (bind-key "C-=" nil rst-mode-map)
  ;; For similarity with AUCTeX
  (bind-key "C-c C-j" #'rst-insert-list rst-mode-map)
  ;; …and with Markdown Mode
  (bind-key "M-RET" #'rst-insert-list rst-mode-map))

(use-package mustache-mode              ; Mustache mode
  :ensure t
  :defer t
  :mode (("\\.mustache$" . markdown-mode)))

(use-package rst                        ; ReStructuredText
  :ensure t
  :defer t
  :config
  ;; Indent with 3 spaces after all kinds of literal blocks
  (setq rst-indent-literal-minimized 3
        rst-indent-literal-normal 3)

  (bind-key "C-=" nil rst-mode-map)
  ;; For similarity with AUCTeX
  (bind-key "C-c C-j" #'rst-insert-list rst-mode-map)
  ;; …and with Markdown Mode
  (bind-key "M-RET" #'rst-insert-list rst-mode-map))

;; TODO: Incorporate this into new `use-package handlebars-mode` below
;; original `init-hbs`
; (defun my-handlebars-load ()
;   (require 'handlebars-mode)
;   (handlebars-mode))
;
; (setq my-handlebars-load-hook 'my-handlebars-load)
;
; (add-to-list 'auto-mode-alist
;              '("\\.hbs$" . (lambda ()
;                               (require 'handlebars-mode)
;                               (handlebars-mode))))
;
; (add-to-list 'auto-mode-alist
;              '("\\.handlebars$" . (lambda ()
;                               (require 'handlebars-mode)
;                               (handlebars-mode))))
;
; (with-eval-after-load 'handlebars-mode
;   (defun my-handlebars-mode-defaults ()
;     ; (toggle-truncate-lines 1)
;     ; (setq truncate-lines 0)
;     (run-hooks 'my-prog-mode-hook))
;
;   (setq my-handlebars-mode-hook 'my-handlebars-mode-defaults)
;
;   (add-hook 'handlebars-mode-hook (lambda ()
;                                     (run-hooks 'my-handlebars-mode-hook))))
;
;; end origianl `init-hbs`

(use-package markdown-mode              ; Markdown
  :ensure t
  :mode (("\\.md$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)
         ("\\.apib$" . markdown-mode))

  ;; http://www.tychoish.com/posts/imenu-for-markdown-and-writing/
  :init (setq markdown-imenu-generic-expression
               '(("title"  "^\\(.*\\)[\n]=+$" 1)
                 ("h2-"    "^\\(.*\\)[\n]-+$" 1)
                 ("h1"   "^# \\(.*\\)$" 1)
                 ("h2"   "^## \\(.*\\)$" 1)
                 ("h3"   "^### \\(.*\\)$" 1)
                 ("h4"   "^#### \\(.*\\)$" 1)
                 ("h5"   "^##### \\(.*\\)$" 1)
                 ("h6"   "^###### \\(.*\\)$" 1)
                 ("fn"   "^\\[\\^\\(.*\\)\\]" 1)))
  :config
  (progn
    ;; Process Markdown with Pandoc, using a custom stylesheet for nice output
    (let ((stylesheet (expand-file-name
                       (locate-user-emacs-file "etc/pandoc.css"))))
      (setq markdown-command
            (mapconcat #'shell-quote-argument
                       `("pandoc" "--toc" "--section-divs"
                         "--css" ,(concat "file://" stylesheet)
                         "--standalone" "-f" "markdown" "-t" "html5")
                       " ")))

    (when (eq system-type 'darwin)
      (setq markdown-open-command "mark"))

    ;; disable auto indent
    (add-hook 'markdown-mode-hook
              (lambda ()
                (electric-indent-local-mode -1)
                (setq imenu-generic-expression markdown-imenu-generic-expression)))

    ;; No filling in GFM, because line breaks are significant.
    (add-hook 'gfm-mode-hook #'turn-off-auto-fill)
    ;; Use visual lines instead
    (add-hook 'gfm-mode-hook #'visual-line-mode)
    (add-hook 'gfm-mode-hook #'init-whitespace-style-no-long-lines)

    (bind-key "C-c C-s C" #'markdown-insert-gfm-code-block markdown-mode-map)
    (bind-key "C-c C-s P" #'markdown-insert-gfm-code-block markdown-mode-map)

    ;; Fight my habit of constantly pressing M-q.  We should not fill in GFM Mode.
    (bind-key "M-q" #'ignore gfm-mode-map)))

(use-package init-markdown
  :bind (("C-c t h" . dotemacs-markdown-post-header)))

(use-package init-markdown
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :bind (("C-c m" . dotemacs-preview-md-file)))

(use-package jira-markup-mode           ; Jira markup
  :ensure t
  :defer t)

(use-package yaml-mode                  ; YAML
  :ensure t
  :defer t
  :mode (("\\.yaml$" . yaml-mode)
         ("\\.yml$" . yaml-mode))
  :config
    (add-hook 'yaml-mode-hook
              (lambda ()
                (electric-indent-local-mode -1)
                (run-hooks 'prog-mode-hook))))

(use-package json-mode                  ; JSON files
  :ensure t
  :defer t)

(use-package json-reformat              ; Reformat JSON
  :ensure t
  :defer t
  :bind (("C-c e j" . json-reformat-region)))

(use-package graphviz-dot-mode          ; Graphviz
  :ensure t
  :defer t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package systemd                    ; Mode for systemd unit files
  :ensure t
  :defer t)


;;; Programming utilities

;; Currently it supports Scala (scala-mode2), JavaScript (js-mode and js2-mode),
;; Ruby, Python, Emacs Lisp, Clojure, C, C++, and Java.
(use-package color-identifiers-mode
  :ensure t
  :defer t
  :diminish color-identifiers-mode
  :init
  (progn
    (dolist (mode '(scala js js2 ruby python emacs-lisp clojure c java))
      (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
                (lambda ()
                  (global-color-identifiers-mode))))))

(use-package init-programming
  :load-path "config/"
  :defer t
  :commands (dotemacs-local-comment-auto-fill
             dotemacs-font-lock-comment-annotations
             dotemacs-fold-overlay
             dotemacs-prog-mode-defaults)
  :init
  (progn
    (setq dotemacs-prog-mode-hook #'dotemacs-prog-mode-defaults)
    (add-hook 'prog-mode-hook
              (lambda ()
                (run-hooks #'dotemacs-prog-mode-hook)))
    ))

(use-package prog-mode                  ; Prog Mode
  :bind (("C-c t p" . prettify-symbols-mode))
  :init
  (add-hook 'js2-mode-hook
            (lambda ()
              (push '("function" . 955) prettify-symbols-alist)
              (push '("return" . 8592) prettify-symbols-alist))))

; http://stackoverflow.com/questions/1085170/how-to-achieve-code-folding-effects-in-emacs
(use-package hs-minor-mode
  :defer t
  :init (progn
    (setq hs-set-up-overlay 'dotemacs-fold-overlay)
    (add-hook 'prog-mode-hook #'hs-minor-mode)))

; This works and sets the mode correctly but the symbols do not show up
(use-package prettify-symbols-mode
  :defer t
  :init
  (progn
    ; (dolist (mode '(emacs-lisp js2 java python ruby))
    ;   (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
    ;             (lambda ()
    ;               (prettify-symbols-mode))))
    (setq prettify-symbol-categories '(lambda relational logical))))

; Instead set the mode globally
(global-prettify-symbols-mode)

(use-package compile                    ; Compile from Emacs
  :bind (("C-c c" . compile)
         ("C-c C" . recompile))
  :config (progn
            (setq compilation-ask-about-save nil
                  compilation-always-kill t
                  ;; Kill old compilation processes before starting new ones,
                  ;; and automatically scroll up to the first error.
                  compilation-scroll-output 'first-error)

            (add-to-list 'display-buffer-alist
                         `(,(rx bos "*compilation")
                           (display-buffer-reuse-window
                            display-buffer-in-side-window)
                           (side            . bottom)
                           (reusable-frames . visible)
                           (window-height   . 0.4)))))

(use-package init-compile          ; Personal helpers for compilation
  :load-path "config/"
  :commands (dotemacs-colorize-compilation-buffer)
  ;; Colorize output of Compilation Mode, see
  ;; http://stackoverflow.com/a/3072831/355252
  :init (add-hook 'compilation-filter-hook
                  #'dotemacs-colorize-compilation-buffer))

(use-package elide-head                 ; Elide lengthy GPL headers
  :bind (("C-c u h" . elide-head))
  :init (add-hook 'prog-mode-hook #'elide-head))

(use-package eldoc                      ; Documentation in minibuffer
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  :config
  (progn
    (setq-default eldoc-documentation-function #'describe-char-eldoc)
    ;; enable eldoc in `eval-expression'
    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
     ;; enable eldoc in IELM
    (add-hook 'ielm-mode-hook #'eldoc-mode))
  :diminish eldoc-mode)

(use-package restclient                ; ReST REPL for Emacs
  :ensure t
  :defer t)

(use-package company-restclient
  :ensure t
  :defer t
  :init (after "company"
          (add-to-list 'company-backends 'company-restclient)))


;;; Emacs Lisp
(bind-key "C-c t d" #'toggle-debug-on-error)

(use-package helm-elisp                 ; Helm commands for Emacs Lisp
  :ensure helm
  :bind (("C-c f l" . helm-locate-library)
         ("C-c h a" . helm-apropos)))

(use-package elisp-slime-nav            ; Jump to definition of symbol at point
  ;; Elisp go-to-definition with M-. and back again with M-,
  :ensure t
  :defer t
  :init
  (progn
    ; (evil-leader/set-key-for-mode 'emacs-lisp-mode
    ;   "mgg" 'elisp-slime-nav-find-elisp-thing-at-point
    ;   "mhh" 'elisp-slime-nav-describe-elisp-thing-at-point)
    (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode))
  :config
  (defadvice elisp-slime-nav-find-elisp-thing-at-point
             (after advice-for-elisp-slime-nav-find-elisp-thing-at-point activate)
    (recenter))
  :diminish elisp-slime-nav-mode)

(use-package flycheck-cask              ; Setup Flycheck by Cask projects
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(use-package flycheck-package           ; Check package conventions with Flycheck
  :ensure t
  :defer t
  :init (after "flycheck" (flycheck-package-setup)))

(use-package pcre2el                    ; Convert regexps to RX and back
  :disabled t
  :ensure t
  :init (rxt-global-mode))

(use-package macrostep                  ; Interactively expand macros in code
  :ensure t
  :defer t
  :init (after "lisp-mode"
          (bind-key "C-c e e" #'macrostep-expand emacs-lisp-mode-map)
          (bind-key "C-c e e" #'macrostep-expand lisp-interaction-mode-map)))

(use-package ielm                       ; Emacs Lisp REPL
  :bind (("C-c z" . ielm)))

;; TODO: Incorporate this into `use-package lisp-mode` below
;; original `init-common-lisp`
; (require 'init-programming)
;
; (require-package 'slime)
; (require 'slime)
;
; ;; the SBCL configuration file is in Common Lisp
; (add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))
;
; ;; Open files with .cl extension in lisp-mode
; (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
;
; ;; a list of alternative Common Lisp implementations that can be
; ;; used with SLIME. Note that their presence render
; ;; inferior-lisp-program useless. This variable holds a list of
; ;; programs and if you invoke SLIME with a negative prefix
; ;; argument, M-- M-x slime, you can select a program from that list.
; (setq slime-lisp-implementations
;       '((ccl ("ccl"))
;         (clisp ("clisp" "-q"))
;         (cmucl ("cmucl" "-quiet"))
;         (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))
;
; ;; select the default value from slime-lisp-implementations
; (if (eq system-type 'darwin)
;     ;; default to Clozure CL on OS X
;     (setq slime-default-lisp 'ccl)
;   ;; default to SBCL on Linux and Windows
;   (setq slime-default-lisp 'sbcl))
;
; (add-hook 'lisp-mode-hook (lambda () (run-hooks 'my-lisp-coding-hook)))
; (add-hook 'slime-repl-mode-hook (lambda () (run-hooks 'my-interactive-lisp-coding-hook)))
;
; (eval-after-load "slime"
;   '(progn
;      (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
;            slime-fuzzy-completion-in-place t
;            slime-enable-evaluate-in-emacs t
;            slime-autodoc-use-multiline-p t
;            slime-auto-start 'always)
;
;      (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
;      (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector)))
;; end origianl `init-commo-lisp`

(use-package lisp-mode                  ; Emacs Lisp editing
  :defer t
  :interpreter ("emacs" . emacs-lisp-mode)
  :mode ("/Cask\\'" . emacs-lisp-mode)
  :config (require 'ert))

;; TODO: Incorporate this into `use-package lisp` below
;; original `init-lisp`
; (require 'init-programming)
;
;
; (defun my-lisp-hook ()
;   (progn
;     (elisp-slime-nav-mode)
;     (eldoc-mode)))
;
; (defun my-lisp-after-save-hook ()
;   (when (or (string-prefix-p (file-truename (concat user-emacs-directory "/config"))
;                              (file-truename buffer-file-name))
;             (equal (file-truename buffer-file-name)
;                    (file-truename custom-file)))
;     (emacs-lisp-byte-compile)))
;
; (add-hook 'emacs-lisp-mode-hook #'my-lisp-hook)
; (add-hook 'lisp-interaction-mode-hook #'my-lisp-hook)
; (add-hook 'ielm-mode-hook #'my-lisp-hook)
; (add-hook 'after-save-hook #'my-lisp-after-save-hook)
;
; ;; Lisp configuration
; (define-key read-expression-map (kbd "TAB") 'completion-at-point)
;
; ;; wrap keybindings
; (define-key lisp-mode-shared-map (kbd "M-(") (my-wrap-with "("))
; (define-key lisp-mode-shared-map (kbd "M-[") (my-wrap-with "["))
; (define-key lisp-mode-shared-map (kbd "M-\"") (my-wrap-with "\""))
;
;
; ;; A great lisp coding hook
; (defun my-lisp-coding-defaults ()
;   (smartparens-strict-mode +1))
;
; (setq my-lisp-coding-hook 'my-lisp-coding-defaults)
;
; ;; interactive modes don't need whitespace checks
; (defun my-interactive-lisp-coding-defaults ()
;   (smartparens-strict-mode +1)
;   (whitespace-mode -1))
;
; (setq my-interactive-lisp-coding-hook 'my-interactive-lisp-coding-defaults)
;; end origianl `init-lisp`

(use-package init-lisp             ; Personal tools for Emacs Lisp
  :load-path "config/"
  :commands (dotemacs-find-cask-file
             dotemacs-add-use-package-to-imenu)
  :init (progn
          (add-hook 'emacs-lisp-mode-hook #'dotemacs-add-use-package-to-imenu)

          (after "lisp-mode"
            (bind-key "C-c f c" #'dotemacs-find-cask-file
                      emacs-lisp-mode-map))))


;;; Scala
(use-package scala-mode2                ; Scala editing
  :ensure t
  :defer t
  :config (progn (setq scala-indent:default-run-on-strategy
                       scala-indent:operator-strategy)

                 (bind-key "C-c z" #'ensime scala-mode-map)))

(use-package flycheck-auto-scalastyle   ; Scalastyle setup
  :load-path "config/"
  :defer t
  :commands (flycheck-auto-scalastyle-configure
             flycheck-auto-scalastyle-setup)
  :init (after "scala-mode2"
          (add-hook 'flycheck-mode-hook #'flycheck-auto-scalastyle-setup)))

(use-package sbt-mode                   ; Scala build tool
  :ensure t
  :defer t
  :config (progn
            (setq sbt:sbt-prompt-regexp
                  (rx bol (or (and (optional "scala") ">") ; Default prompt
                              ;; Sbt Prompt plugin
                              (and "[" (1+ (not (any "]")))"] " (1+ word) ":"))
                      (0+ " ")))

            (after "scala-mode2"
              (bind-key "C-c c" #'sbt-command scala-mode-map))

            (defun dotemacs-sbt-buffer-p (buffer-name &rest _)
              "Determine whether BUFFER-OR-NAME denotes an SBT buffer."
              (string-prefix-p sbt:buffer-name-base buffer-name))

            ;; Get SBT buffers under control: Display them below the current
            ;; window, at a third of the height of the current window, but try
            ;; to reuse any existing and visible window for the SBT buffer
            ;; first.
            (add-to-list 'display-buffer-alist
                         '(dotemacs-sbt-buffer-p
                           (display-buffer-reuse-window
                            display-buffer-in-side-window)
                           (side            . bottom)
                           (reusable-frames . visible)
                           (window-height   . 0.4)))))

(use-package ensime                     ; Scala interaction mode
  :ensure t
  :defer t
  :config (progn
            ;; Automatically open new Ensime sessions if needed
            (setq ensime-auto-connect 'always)

            ;; Enable Ensime for all Scala buffers.  We don't do this in :init,
            ;; because `ensime-mode' isn't autoloaded, and ensime-mode makes no
            ;; sense before the first session was started anyway
            (add-hook 'scala-mode-hook #'ensime-mode)

            ;; Disable Flycheck in Ensime, since Ensime features its own error
            ;; checking.  TODO: Maybe write a Flycheck checker for Ensime
            (after "flycheck"
              (add-hook 'ensime-mode-hook (lambda () (flycheck-mode -1))))

            ;; Free M-n and M-p again
            (bind-key "M-n" nil ensime-mode-map)
            (bind-key "M-p" nil ensime-mode-map)
            (bind-key "C-c M-n" #'ensime-forward-note ensime-mode-map)
            (bind-key "C-c M-p" #'ensime-backward-note ensime-mode-map)))

(use-package ensime-sbt
  :ensure ensime
  :defer t
  ;; Compile on save.
  :config (setq ensime-sbt-perform-on-save "test:compile"))

(use-package flycheck-ensime
  :disabled t
  :load-path "config/"
  :defer t)


;;; Python
(use-package python
  :defer t
  :config
  (progn
    ;; PEP 8 compliant filling rules, 79 chars maximum
    (add-hook 'python-mode-hook (lambda () (setq fill-column 79)))
    (add-hook 'python-mode-hook #'subword-mode)

    (let ((ipython (executable-find "ipython")))

      (if ipython
          (setq python-shell-interpreter ipython)
        (warn "IPython is missing, falling back to default python")))))

(use-package flycheck-virtualenv        ; Setup Flycheck by virtualenv
  :load-path "config/"
  :commands (flycheck-virtualenv-setup)
  :init (add-hook 'flycheck-mode-hook #'flycheck-virtualenv-setup))

(use-package anaconda-mode              ; Powerful Python backend for Emacs
  :ensure t
  :defer t
  :init (add-hook 'python-mode-hook #'anaconda-mode))

(use-package company-anaconda           ; Python backend for Company
  :ensure t
  :defer t
  :init (after "company"
          (add-to-list 'company-backends 'company-anaconda)))

(use-package pip-requirements           ; requirements.txt files
  :ensure t
  :defer t)


;;; Ruby
(use-package inf-ruby                   ; Ruby REPL
  :ensure t
  :defer t
  :init (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
  :config
  ;; Easily switch to Inf Ruby from compilation modes to Inf Ruby
  (inf-ruby-switch-setup))

(use-package robe                       ; Ruby backend for Emacs
  :ensure t
  :defer t
  :init (after "company"
          (add-to-list 'company-backends 'company-robe)))


;;; Rust
(use-package rust-mode                  ; Rust
  :ensure t
  :defer t)

(use-package flycheck-rust              ; Flycheck setup for Rust
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package toml-mode                  ; Toml for Cargo files
  :ensure t
  :defer t
  :mode ("\\.toml$" . toml-mode)
  :config
    (add-hook 'toml-mode-hook
              (lambda ()
                (electric-indent-local-mode -1)
                (run-hooks 'prog-mode-hook))))


;;; Haskell

;; This Haskell setup needs:
;;
;; cabal install hasktags haskell-docs hoogle hindent
;;
;; Additionally, to be installed from source:
;;
;; - https://github.com/chrisdone/ghci-ng

(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'haskell-mode-hook #'subword-mode)           ; Subword navigation
    (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode) ; Scan and navigate
                                        ; declarations
    ;; Insert module templates into new buffers
    (add-hook 'haskell-mode-hook #'haskell-auto-insert-module-template)

    ;; Automatically run hasktags
    (setq haskell-tags-on-save t
          ;; Suggest adding/removing imports as by GHC warnings and Hoggle/GHCI
          ;; loaded modules respectively
          haskell-process-suggest-remove-import-lines t
          haskell-process-auto-import-loaded-modules t
          haskell-process-use-presentation-mode t ; Don't clutter the echo area
          haskell-process-show-debug-tips nil     ; Disable tips
          haskell-process-log t                   ; Log debugging information
          ;; Suggest imports automatically with Hayoo.  Hayoo is slower because
          ;; it's networked, but it covers all of hackage, which is really an
          ;; advantage.
          haskell-process-suggest-hoogle-imports nil
          haskell-process-suggest-hayoo-imports t)

    (when-let (ghci-ng (executable-find "ghci-ng"))
      ;; Use GHCI NG from https://github.com/chrisdone/ghci-ng
      (setq haskell-process-path-ghci ghci-ng)
      (add-to-list 'haskell-process-args-cabal-repl
                   (concat "--with-ghc=" ghci-ng)))

    (bind-key "C-c h d" #'haskell-describe haskell-mode-map)
    (bind-key "C-c j i" #'haskell-navigate-imports haskell-mode-map)
    (bind-key "C-c f c" #'haskell-cabal-visit-file haskell-mode-map)))

(use-package haskell
  :ensure haskell-mode
  :defer t
  :init (dolist (hook '(haskell-mode-hook haskell-cabal-mode-hook))
          (add-hook hook #'interactive-haskell-mode))
  :config
  (progn
    (bind-key "C-c C-t" #'haskell-mode-show-type-at
              interactive-haskell-mode-map)
    (bind-key "M-." #'haskell-mode-goto-loc
              interactive-haskell-mode-map)
    (bind-key "C-c u u" #'haskell-mode-find-uses
              interactive-haskell-mode-map)))

(use-package haskell-interactive-mode
  :ensure haskell-mode
  :defer t
  :config (add-hook 'haskell-interactive-mode-hook #'subword-mode))

(use-package haskell-simple-indent      ; Primitive Haskell indentation
  :ensure haskell-mode
  :disabled t
  :defer t
  :init (add-hook 'haskell-mode-hook #'haskell-simple-indent-mode))

(use-package haskell-indentation
  :ensure haskell-mode
  :defer t
  :init (add-hook 'haskell-mode-hook #'haskell-indentation-mode))

(use-package hindent                    ; Automated Haskell indentation
  :ensure t
  :defer t
  :init (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package flycheck-haskell           ; Setup Flycheck from Cabal projects
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(use-package helm-hayoo
  :ensure t
  :defer t
  :init (after "haskell-mode"
          (bind-key "C-c h h" #'helm-hayoo haskell-mode-map)))

(use-package helm-hoogle
  :ensure t
  :defer t
  :init (after "haskell-mode"
          (bind-key "C-c h H" #'helm-hoogle haskell-mode-map)))


;;; Go

;; TODO: Incorporate this into new `use-package go-mode` below
;; original `init-go`
; (lazy-major-mode "\\.go$" go-mode)
;
; (with-eval-after-load 'go-mode
;   (require 'go-eldoc)
;   (add-hook 'go-mode-hook 'go-eldoc-setup)
;
;   (with-eval-after-load "company-autoloads"
;     (require 'company-go)
;     (require 'company-go)
;     (add-hook 'go-mode-hook (lambda ()
;                               (set (make-local-variable 'company-backends) '(company-go))))))
;; end origianl `init-go`


;;; Clojure

;; TODO: Incorporate this into `use-package clojure` below
;; original `init-clojure`
; (require 'init-lisp)
; (require 'clojure-mode)
; (require 'cider)
;
; (defadvice clojure-test-run-tests (before save-first activate)
;   (save-buffer))
;
; (defadvice nrepl-load-current-buffer (before save-first activate)
;   (save-buffer))
;
; (require 'clj-refactor)
;
; (with-eval-after-load 'clojure-mode
;   (defun my-clojure-mode-defaults ()
;     (clj-refactor-mode 1)
;     (subword-mode +1)
;     (run-hooks 'my-lisp-coding-hook))
;
;   (setq my-clojure-mode-hook 'my-clojure-mode-defaults)
;   (add-hook 'clojure-mode-hook (lambda ()
;                                (run-hooks 'my-clojure-mode-hook))))
;
; (with-eval-after-load 'cider
;   (setq nrepl-log-messages t)
;
;   (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;
;   (defun my-cider-repl-mode-defaults ()
;     (subword-mode +1)
;     (run-hooks 'my-interactive-lisp-coding-hook))
;
;    (setq my-cider-repl-mode-hook 'my-cider-repl-mode-defaults)
;    (add-hook 'cider-repl-mode-hook (lambda ()
;                                    (run-hooks 'my-cider-repl-mode-hook))))
;
; ;; Indent and highlight more commands
; (put-clojure-indent 'match 'defun)
;
; ;; Hide nrepl buffers when switching buffers (switch to by prefixing with space)
; (setq nrepl-hide-special-buffers t)
;
; ;; Enable error buffer popping also in the REPL:
; (setq cider-repl-popup-stacktraces t)
;
; ;; Specify history file
; (setq cider-history-file "~/.emacs.d/nrepl-history")
;
; ;; auto-select the error buffer when it's displayed
; (setq cider-auto-select-error-buffer t)
;
; ;; Prevent the auto-display of the REPL buffer in a separate window after connection is established
; (setq cider-repl-pop-to-buffer-on-connect nil)
;
; ;; Enable eldoc in Clojure buffers
; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;
; ;; Cycle between () {} []
;
; (defun live-delete-and-extract-sexp ()
;   "Delete the sexp and return it."
;   (interactive)
;   (let* ((begin (point)))
;     (forward-sexp)
;     (let* ((result (buffer-substring-no-properties begin (point))))
;       (delete-region begin (point))
;       result)))
;
; (defun live-cycle-clj-coll ()
;   "convert the coll at (point) from (x) -> {x} -> [x] -> (x) recur"
;   (interactive)
;   (let* ((original-point (point)))
;     (while (and (> (point) 1)
;                 (not (equal "(" (buffer-substring-no-properties (point) (+ 1 (point)))))
;                 (not (equal "{" (buffer-substring-no-properties (point) (+ 1 (point)))))
;                 (not (equal "[" (buffer-substring-no-properties (point) (+ 1 (point))))))
;       (backward-char))
;     (cond
;      ((equal "(" (buffer-substring-no-properties (point) (+ 1 (point))))
;       (insert "{" (substring (live-delete-and-extract-sexp) 1 -1) "}"))
;      ((equal "{" (buffer-substring-no-properties (point) (+ 1 (point))))
;       (insert "[" (substring (live-delete-and-extract-sexp) 1 -1) "]"))
;      ((equal "[" (buffer-substring-no-properties (point) (+ 1 (point))))
;       (insert "(" (substring (live-delete-and-extract-sexp) 1 -1) ")"))
;      ((equal 1 (point))
;       (message "beginning of file reached, this was probably a mistake.")))
;     (goto-char original-point)))
;
; (define-key clojure-mode-map (kbd "C-´") 'live-cycle-clj-coll)
;
; ;; Warn about missing nREPL instead of doing stupid things
;
; (defun nrepl-warn-when-not-connected ()
;   (interactive)
;   (message "Oops! You're not connected to an nREPL server. Please run M-x cider or M-x cider-jack-in to connect."))
;
; (define-key clojure-mode-map (kbd "C-M-x")   'nrepl-warn-when-not-connected)
; (define-key clojure-mode-map (kbd "C-x C-e") 'nrepl-warn-when-not-connected)
; (define-key clojure-mode-map (kbd "C-c C-e") 'nrepl-warn-when-not-connected)
; (define-key clojure-mode-map (kbd "C-c C-l") 'nrepl-warn-when-not-connected)
; (define-key clojure-mode-map (kbd "C-c C-r") 'nrepl-warn-when-not-connected)
; (define-key clojure-mode-map (kbd "C-c C-z") 'nrepl-warn-when-not-connected)
; (define-key clojure-mode-map (kbd "C-c C-k") 'nrepl-warn-when-not-connected)
; (define-key clojure-mode-map (kbd "C-c C-n") 'nrepl-warn-when-not-connected)
;
; (setq cljr-magic-require-namespaces
;       '(("io"   . "clojure.java.io")
;         ("set"  . "clojure.set")
;         ("str"  . "clojure.string")
;         ("walk" . "clojure.walk")
;         ("zip"  . "clojure.zip")
;         ("time" . "clj-time.core")))
;
; ;; Set up linting of clojure code with eastwood
;
; ;; Make sure to add [acyclic/squiggly-clojure "0.1.2-SNAPSHOT"]
; ;; to your :user :dependencies in .lein/profiles.clj
;
; (require 'flycheck-clojure)
; (add-hook 'cider-mode-hook (lambda () (flycheck-mode 1)))
;
; (eval-after-load 'flycheck '(add-to-list 'flycheck-checkers 'clojure-cider-eastwood))
;
; ;; Make some clj-refactor commands more snappy by populating caches in the
; ;; background:
;
; (add-hook 'nrepl-connected-hook #'cljr-update-artifact-cache)
; (add-hook 'nrepl-connected-hook #'cljr-warm-ast-cache)
;
; ;; Make q quit out of find-usages to previous window config
;
; (defadvice cljr-find-usages (before setup-grep activate)
;   (window-configuration-to-register ?$))
;; end origianl `init-clojure`


;;; OCaml
(use-package opam                       ; Initialize Emacs with OPAM env
  :ensure t
  :init (opam-init))

(use-package tuareg                     ; OCaml editing
  :ensure t
  :defer t
  :config
  (progn
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

(use-package flycheck-ocaml             ; Check OCaml code with Merlin
  :ensure t
  :defer t
  :init (after "merlin"
          (flycheck-ocaml-setup)))


;;; Web languages

;; TODO: Incorporate this into various `use-packages` below
;; original `init-web`
; (lazy-major-mode "\\.coffee\\'" coffee-mode)
; (lazy-major-mode "\\.jade$" jade-mode)
;
; (lazy-major-mode "\\.html?$" web-mode)
;
; (with-eval-after-load 'web-mode
;   (setq web-mode-markup-indent-offset 2) ; web-mode, html tag in html file
;   (setq web-mode-css-indent-offset 2) ; web-mode, css in html file
;   (setq web-mode-code-indent-offset 2) ; web-mode, js code in html file
;
;   (with-eval-after-load 'yasnippet
;     (require 'angular-snippets)
;     (angular-snippets-initialize)))
;
; ;; indent after deleting a tag
; (defadvice sgml-delete-tag (after reindent activate)
;   (indent-region (point-min) (point-max)))
;; end origianl `init-web`

(use-package emmet-mode
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'html-mode-hook 'emmet-mode)
    ; (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode))
  :config
  (progn
    ; (evil-define-key 'insert emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
    ; (evil-define-key 'insert emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
    ; (evil-define-key 'emacs emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
    ; (evil-define-key 'emacs emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
    ))

(use-package web-mode                   ; Template editing
  :ensure t
  :defer t
  :mode "/templates?/.*\\.\\(php\\|html\\)\\'"
  :config
  (setq web-mode-markup-indent-offset 2))

;; TODO: Incorporate this into `use-package js2-mode` below
;; original `init-js`
; (require 'init-programming)
;
; (add-to-list 'auto-mode-alist '("\\.js\\'"    . js2-mode))
; (add-to-list 'auto-mode-alist '("\\.pac\\'"   . js2-mode))
; (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
; (add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
; (add-to-list 'auto-mode-alist '("\\.jshintrc$" . javascript-mode))
; (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
;
; (with-eval-after-load 'javascript-mode
;   (setq javascript-indent-level 2)) ; javascript-mode
;
; (with-eval-after-load 'js-mode
;   (setq js-indent-level 2)) ; js-mode
;
; (with-eval-after-load 'js2-mode
;   (defun my-js2-mode-defaults ()
;     (js2-imenu-extras-mode +1)
;     (setq mode-name "JS2")
;     ; '(define-key js-mode-map "," 'self-insert-command)
;     ; '(define-key js-mode-map ";" 'self-insert-command)
;     ;; electric-layout-mode doesn't play nice with smartparens
;     (setq-local electric-layout-rules '((?\; . after)))
;     (run-hooks 'my-prog-mode-hook)
;     (message "My JS2 hook"))
;
;   (setq my-js2-mode-hook 'my-js2-mode-defaults)
;   (add-hook 'js2-mode-hook (lambda ()
;                              (run-hooks 'my-js2-mode-hook)))
;
;   (add-hook 'js2-mode-hook (lambda ()
;     (local-set-key (kbd "C-c C-c") #'dotemacs-js-ctrl-c-ctrl-c)))
;
;   (setq indent-tabs-mode nil
;         tab-width 2
;         js-indent-level 2)
;   (setq js2-highlight-level 3)
;   (setq js2-basic-offset 2)
;   (setq js2-concat-multiline-strings (quote eol))
;   (setq js2-include-node-externs t)
;   (setq js2-indent-switch-body t)
;
;   (setq js2-allow-rhino-new-expr-initializer nil)
;   (setq js2-auto-indent-p nil)
;   (setq js2-enter-indents-newline nil)
;   (setq js2-global-externs '("setTimeout" "clearTimeout" "setInterval" "clearInterval" "__dirname" "console" "JSON" "_" "assert" "refute" "buster" "require" "global" "exports" "module" "describe" "it" "before" "after" "beforeEach" "afterEach" "chai" "expect" "sinon" "test" "asyncTest" "ok" "equal" "notEqual" "deepEqual" "expect"))
;   (setq js2-idle-timer-delay 0.8)
;   (setq js2-indent-on-enter-key nil)
;   (setq js2-mirror-mode nil)
;   (setq js2-strict-inconsistent-return-warning nil)
;   (setq js2-include-rhino-externs nil)
;   (setq js2-include-gears-externs nil)
;   (setq js2-rebind-eol-bol-keys nil)
;
;   ;; Let flycheck handle parse errors
;   (setq js2-show-parse-errors nil)
;   (setq js2-strict-missing-semi-warning nil)
;   (setq js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason
;
;   (define-key js2-mode-map (kbd "C-c RET jt") 'jump-to-test-file)
;   (define-key js2-mode-map (kbd "C-c RET ot") 'jump-to-test-file-other-window)
;   (define-key js2-mode-map (kbd "C-c RET js") 'jump-to-source-file)
;   (define-key js2-mode-map (kbd "C-c RET os") 'jump-to-source-file-other-window)
;   (define-key js2-mode-map (kbd "C-c RET jo") 'jump-between-source-and-test-files)
;   (define-key js2-mode-map (kbd "C-c RET oo") 'jump-between-source-and-test-files-other-window)
;
;   (define-key js2-mode-map (kbd "C-c RET dp") 'js2r-duplicate-object-property-node)
;
;   (define-key js2-mode-map (kbd "C-c RET ta") 'toggle-assert-refute)
;
;   (defadvice js2r-inline-var (after reindent-buffer activate)
;     (cleanup-buffer))
;
;   (define-key js2-mode-map (kbd "C-c t") 'dotemacs-hide-test-functions)
;
;   (define-key js2-mode-map (kbd "TAB") 'dotemacs-tab-properly)
;
;   ;; When renaming/deleting js-files, check for corresponding testfile
;   (define-key js2-mode-map (kbd "C-x C-r") 'js2r-rename-current-buffer-file)
;   (define-key js2-mode-map (kbd "C-x C-k") 'js2r-delete-current-buffer-file)
;
;   (define-key js2-mode-map (kbd "C-k") 'js2r-kill)
;
;   ;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
;   ;; add any symbols to a buffer-local var of acceptable global vars
;   ;; Note that we also support the "symbol: true" way of specifying names via a hack (remove any ":true"
;   ;; to make it look like a plain decl, and any ':false' are left behind so they'll effectively be ignored as
;   ;; you can;t have a symbol called "someName:false"
;   (add-hook 'js2-post-parse-callbacks
;             (lambda ()
;               (when (> (buffer-size) 0)
;                 (let ((btext (replace-regexp-in-string
;                               ": *true" " "
;                               (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
;                   (mapc (apply-partially 'add-to-list 'js2-additional-externs)
;                         (split-string
;                          (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
;                          " *, *" t))
;                   ))))
;
;   (require 'js2-refactor)
;   (js2r-add-keybindings-with-prefix "C-c C-m")
;
;   (require 'js2-imenu-extras)
;   (js2-imenu-extras-setup)
;
;   ;; jshintrc
;   (when (executable-find "tern")
;     (require 'tern)
;     (add-hook 'js2-mode-hook 'tern-mode)
;     (with-eval-after-load 'tern
;       (with-eval-after-load 'auto-complete
;         (require 'tern-auto-complete)
;         (tern-ac-setup))
;       (with-eval-after-load 'company-mode
;         (require 'company-tern)))))
;; end origianl `init-js`
(use-package js2-mode                   ; Javascript editing
  :ensure t
  :mode "\\.js\\'"
  :config (progn (setq-default js2-basic-offset 2)
                 (setq js2-global-externs '("angular"))

                 (add-hook 'js2-mode-hook #'js2-highlight-unused-variables-mode)))

;; TODO: Incorporate this into `use-package css-mode` below
;; original `init-css`
; (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
; (add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
;
; (after "css-mode"
;
;   (setq css-indent-offset 2)
;
;   (defun my-css-mode-defaults ()
;     (run-hooks 'my-prog-mode-hook))
;
;   (setq my-css-mode-hook 'my-css-mode-defaults)
;
;   (add-hook 'css-mode-hook (lambda ()
;                            (run-hooks 'my-css-mode-hook))))
;; end origianl `init-css`
(use-package css-mode
  :defer t
  :config
  (progn
    ;; Run Prog Mode hooks, because for whatever reason CSS Mode derives from
    ;; `fundamental-mode'.
    (add-hook 'css-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

    ;; Mark css-indent-offset as safe local variable.  TODO: Report upstream
    (put 'css-indent-offset 'safe-local-variable #'integerp)))

(use-package css-eldoc                  ; Basic Eldoc for CSS
  :ensure t
  :commands (turn-on-css-eldoc)
  :init (add-hook 'css-mode-hook #'turn-on-css-eldoc))

(use-package php-mode                   ; Because sometimes you have to
  :ensure t)


;;; Stylus
(use-package init-stylus
  :load-path "config/"
  :defer t
  :commands(dotemacs-stylus-mode-defaults))

(use-package stylus-mode
  :ensure t
  :defer t
  :mode ("\\.styl$" . stylus-mode)
  :init (add-hook 'stylus-mode-hook #'dotemacs-stylus-mode-defaults))


;;; Skewer
(use-package init-skewer
  :load-path "config/"
  :defer t
  :commands(dotemacs-skewer-start
            dotemacs-skewer-demo))

(use-package skewer-mode
  :ensure t
  :defer t
  :init (after "js2-mode"
          (skewer-setup))
  :config
  (progn
    (require 'skewer-repl)
    (require 'skewer-html)
    (require 'skewer-css))
  :diminish skewer-mode)


;;; Misc programming languages
(use-package sh-script                  ; Shell scripts
  :mode ("\\.zsh\\'" . sh-mode)
  :config
  ;; Use two spaces in shell scripts.
  (setq sh-indentation 2                ; The basic indentation
        sh-basic-offset 2               ; The offset for nested indentation
        ))

(use-package puppet-mode                ; Puppet manifests
  :ensure t
  :defer t
  :config
  ;; Fontify variables in Puppet comments
  (setq puppet-fontify-variables-in-comments t))

(use-package nxml-mode                  ; XML editing
  :defer t
  ;; Complete closing tags, and insert XML declarations into empty files
  :config (setq nxml-slash-auto-complete-flag t
                nxml-auto-insert-xml-declaration-flag t))

(use-package feature-mode               ; Feature files for ecukes/cucumber
  :ensure t
  :defer t
  :config
  (progn
    ;; Add standard hooks for Feature Mode, since it is no derived mode
    (add-hook 'feature-mode-hook #'whitespace-mode)
    (add-hook 'feature-mode-hook #'whitespace-cleanup-mode)
    (add-hook 'feature-mode-hook #'flyspell-mode)))

(use-package cmake-mode                 ; CMake files
  :ensure t
  :defer t)

(use-package thrift                     ; Thrift interface files
  :ensure t
  :defer t
  :init (put 'thrift-indent-level 'safe-local-variable #'integerp))

(use-package swift-mode                 ; Swift sources
  :ensure t
  :defer t
  :config (after "flycheck"
            (add-to-list 'flycheck-checkers 'swift)))


;; Databases
(use-package sql
  :bind (("C-c d c" . sql-connect)
         ("C-c d m" . sql-mysql))
  :config (progn (dotemacs-load-private-file "sql-connections" 'noerror)

                 (add-to-list 'display-buffer-alist
                              `(,(rx bos "*SQL")
                                (display-buffer-reuse-window
                                 display-buffer-in-side-window
                                 (side            . bottom)
                                 (reusable-frames . visible)
                                 (window-height   . 0.4))))))


;;; Version control

;; TODO: Incorporate this code from `init-vcs`
;; original `init-vcs`
; (with-eval-after-load 'vc-git
;   (with-eval-after-load 'evil
;     (with-eval-after-load 'magit-blame
;       (defadvice magit-blame-file-on (after advice-for-magit-blame-file-on activate)
;         (evil-emacs-state))
;       (defadvice magit-blame-file-off (after advice-for-magit-blame-file-off activate)
;         (evil-exit-emacs-state))))
;
;   (if (display-graphic-p)
;       (progn
;         (require 'git-gutter-fringe+))
;     (require 'git-gutter+))
;
;   (global-git-gutter+-mode))
;
; (require 'diff-hl)
; (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
; (unless (display-graphic-p)
;   (diff-hl-margin-mode))
;; end origianl `init-vcs`

(use-package vc-hooks                   ; Simple version control
  :defer t
  :config
  ;; Always follow symlinks to files in VCS repos
  (setq vc-follow-symlinks t))

(use-package diff-hl                    ; Highlight hunks in fringe
  :ensure t
  :defer t
  :init (progn
          ;; Highlight changes to the current file in the fringe
          (global-diff-hl-mode)
          ;; Highlight changed files in the fringe of Dired
          (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

          ;; Fall back to the display margin, if the fringe is unavailable
          (unless (display-graphic-p)
            (diff-hl-margin-mode))))

(use-package init-magit
  :load-path "config/"
  :defer t
  :commands (dotemacs-magit-toggle-whitespace
             dotemacs-magit-ignore-whitespace
             dotemacs-magit-dont-ignore-whitespace))

;; TODO: Incorporate this into `use-package magit` below
; (setq magit-last-seen-setup-instructions "1.4.0")
;
; (require 'magit)
; (with-eval-after-load 'magit
;
;   (defun my-magit-mode-defaults ()
;     ; (if (boundp 'yas-minor-mode)
;     ;     (yas-minor-mode))
;     ; (run-hooks 'my-prog-mode-hook)
;     (message "my-magit-mode-defaults"))
;   (setq my-magit-mode-hook 'my-magit-mode-defaults)
;   (add-hook 'magit-mode-hook (lambda ()
;                              (run-hooks 'my-magit-mode-hook)))
;    (setq magit-diff-options '("--histogram"))
;   (setq magit-stage-all-confirm nil)
;   (setq magit-unstage-all-confirm nil)
;   (setq magit-status-buffer-switch-function 'switch-to-buffer)
;   (setq magit-show-child-count t))
;; original `init-magit`

(use-package magit                      ; The one and only Git frontend
  :ensure t
  :bind (("C-c g"   . magit-status)
         ("C-c v g" . magit-status)
         ("C-c v v" . magit-status)
         ("C-c v g" . magit-blame-mode)
         ("C-c v l" . magit-file-log))
  :init
  ;; Seriously, Magit?! Set this variable before Magit is loaded to silence the
  ;; most stupid warning ever
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (progn
    ;; Shut up, Magit!
    (setq magit-save-some-buffers 'dontask
          magit-stage-all-confirm nil
          magit-unstage-all-confirm nil
          ;; Except when you ask something useful…
          magit-set-upstream-on-push t)

    ;; Set Magit's repo dirs for `magit-status' from Projectile's known
    ;; projects.  Initialize the `magit-repo-dirs' immediately after Projectile
    ;; was loaded, and update it every time we switched projects, because the
    ;; new project might have been unknown before
    (defun dotemacs-magit-set-repo-dirs-from-projectile ()
      "Set `magit-repo-dirs' from known Projectile projects."
      (let ((project-dirs (bound-and-true-p projectile-known-projects)))
        ;; Remove trailing slashes from project directories, because Magit adds
        ;; trailing slashes again, which breaks the presentation in the Magit
        ;; prompt.
        (setq magit-repo-dirs (mapcar #'directory-file-name project-dirs))))

    (after "projectile"
      (dotemacs-magit-set-repo-dirs-from-projectile))

    (add-hook 'projectile-switch-project-hook
              #'dotemacs-magit-set-repo-dirs-from-projectile))

  :diminish magit-auto-revert-mode)

(use-package magit-gh-pulls
  :ensure t
  :defer t
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

(use-package git-commit-mode            ; Git commit message mode
  :ensure t
  :defer t)

(use-package gitconfig-mode             ; Git configuration mode
  :ensure t
  :defer t)

(use-package gitignore-mode             ; .gitignore mode
  :ensure t
  :defer t)

(use-package gitattributes-mode         ; Git attributes mode
  :ensure t
  :defer t)

(use-package git-rebase-mode            ; Mode for git rebase -i
  :ensure t
  :defer t)

(use-package git-timemachine            ; Go back in Git time
  :ensure t
  :bind (("C-c v t" . git-timemachine)))


;;; Search
(use-package isearch                   ; Search buffers
  :bind (("C-c s s" . isearch-forward-symbol-at-point))
  ;; `:diminish' doesn't work for isearch, because it uses eval-after-load on
  ;; the feature name, but isearch.el does not provide any feature
  :init (diminish 'isearch-mode))

(use-package helm-regex                 ; Helm regex tools
  :ensure helm
  :bind (([remap occur] . helm-occur)
         ("C-c e o"     . helm-multi-occur)))

(use-package init-rgrep
  :load-path "config/"
  :defer t
  :commands (dotemacs-rgrep-quit-window
             dotemacs-rgrep-goto-file-and-close-rgrep))

(use-package grep
  :defer t
  :config
  (progn

    ;; Don't recurse into some directories
    (add-to-list 'grep-find-ignored-directories "node_modules")
    (add-to-list 'grep-find-ignored-directories "build")
    (add-to-list 'grep-find-ignored-directories "vendor")
    (add-to-list 'grep-find-ignored-directories "elpa")
    (add-to-list 'grep-find-ignored-directories ".git")
    (add-to-list 'grep-find-ignored-directories ".hg")
    (add-to-list 'grep-find-ignored-directories ".svn")
    (add-to-list 'grep-find-ignored-directories ".idea")
    (add-to-list 'grep-find-ignored-directories ".sass-cache")

    ;; Add custom keybindings
    (define-key grep-mode-map "q" #'dotemacs-rgrep-quit-window)
    (define-key grep-mode-map (kbd "C-<return>") #'dotemacs-rgrep-goto-file-and-close-rgrep)

    (when-let (gnu-find (and (eq system-type 'darwin)
                             (executable-find "gfind")))
      (setq find-program gnu-find))

    (when-let (gnu-xargs (and (eq system-type 'darwin)
                              (executable-find "gxargs")))
      (setq xargs-program gnu-xargs))))

(use-package locate                     ; Search files on the system
  :defer t
  :config
  ;; Use mdfind as locate substitute on OS X, to utilize the Spotlight database
  (when-let (mdfind (and (eq system-type 'darwin) (executable-find "mdfind")))
    (setq locate-command mdfind)))

(use-package ag                         ; Search code in files/projects
  :ensure t
  :bind (("C-c a d" . ag-dired-regexp)
         ("C-c a D" . ag-dired)
         ("C-c a f" . ag-files)
         ("C-c a k" . ag-kill-other-buffers)
         ("C-c a K" . ag-kill-buffers))
  :config
  (setq ag-reuse-buffers t            ; Don't spam buffer list with ag buffers
        ag-highlight-search t         ; A little fanciness
        ;; Use Projectile to find the project root
        ag-project-root-function (lambda (d) (let ((default-directory d))
                                               (projectile-project-root)))))

(use-package wgrep                      ; Edit grep/occur/ag results in-place
  :ensure t
  :defer t
  :config
  (progn
    ;; Add custom keybindings
    (define-key grep-mode-map (kbd "C-x C-s") #'wgrep-save-all-buffers)
    ;; Use same keybinding as occur
    (setq wgrep-enable-key "e")))

(use-package wgrep-ag                   ; Wgrep for ag
  :ensure t
  :defer t)

(use-package helm-ag
  :ensure t
  :bind (("C-c a a" . helm-do-ag)
         ("C-c a A" . helm-ag))
  :config (setq helm-ag-fuzzy-match t
                helm-ag-insert-at-point 'symbol
                helm-ag-source-type 'file-line))


;;; Project management with Projectile
(use-package ido
  :preface
  :disabled t
  (progn
    ;; `defvar's to prevent compile warnings
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/68
    (defvar ido-cur-item nil)
    (defvar ido-default-item nil)
    (defvar predicate nil)
    (defvar inherit-input-method nil)
    (defvar ido-cur-list nil)
    (defvar ido-context-switch-command nil))
  :init
  (progn
    (setq ido-enable-flex-matching t
          ido-use-faces nil       ;; disable ido faces to see flx highlights.
          ido-enable-prefix nil
          ido-create-new-buffer 'always
          ido-use-filename-at-point 'guess
          ido-max-prospects 10
          ido-save-directory-list-file (concat dotemacs-cache-directory "ido.hist")
          ido-default-file-method 'selected-window
          ido-everywhere t
          ido-auto-merge-work-directories-length 0))
  :config
  (progn
    (ido-mode 1)
    (ido-everywhere 1)))

(use-package flx-ido
  :disabled t
  :defer t
  :init
  (progn
    (setq flx-ido-use-faces 1)
    (flx-ido-mode 1)))

(use-package ido-ubiquitous
  :disabled t
  :defer t
  :preface
  (progn
    (defvar ido-ubiquitous-debug-mode nil))
  :init (after "flx-ido"
          (ido-ubiquitous-mode 1)))

(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :config
  (progn
    ;; Remove dead projects when Emacs is idle
    (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

    (add-to-list 'projectile-globally-ignored-directories "elpa")
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (add-to-list 'projectile-globally-ignored-directories ".cache")
    (add-to-list 'projectile-globally-ignored-directories "build")
    (add-to-list 'projectile-globally-ignored-directories "vendor")
    (add-to-list 'projectile-globally-ignored-directories ".cask")
    (add-to-list 'projectile-globally-ignored-directories ".git")
    (add-to-list 'projectile-globally-ignored-directories ".hg")
    (add-to-list 'projectile-globally-ignored-directories ".svn")
    (add-to-list 'projectile-globally-ignored-directories ".idea")
    (add-to-list 'projectile-globally-ignored-directories ".sass-cache")

    (setq projectile-completion-system 'helm
          projectile-cache-file (concat dotemacs-cache-directory "projectile.cache")
          projectile-known-projects-file (concat dotemacs-cache-directory "projectile-bookmarks.eld")
          projectile-indexing-method 'alien ; force alien for Windwos
          projectile-enable-caching t       ; To enable caching unconditionally
          projectile-find-dir-includes-top-level t
          projectile-mode-line '(:propertize
                                 (:eval (concat " " (projectile-project-name)))
                                 face font-lock-constant-face)))
  :diminish projectile-mode)

(use-package helm-projectile
  :ensure t
  :defer t
  :init (after "projectile" (helm-projectile-on))
  :config
  (progn
    ; (add-to-list 'helm-projectile-sources-list 'helm-source-projectile-recentf-list)
    (setq projectile-switch-project-action #'helm-projectile)))


;;; Perspective
(use-package init-perspective
  :load-path "config/"
  :defer t
  :commands (dotemacs-persp-switch-project
             dotemacs-custom-persp-last))

(use-package perspective
  :ensure t
  :defer t
  :commands (custom-persp
             persp-add-buffer
             persp-set-buffer
             persp-kill
             persp-remove-buffer
             persp-rename
             persp-switch
             projectile-persp-bridge)
  :init
  (progn
    (persp-mode t)
    ;; muh perspectives
    ; (defun custom-persp/emacs ()
    ;   (interactive)
    ;   (custom-persp ".emacs.d"
    ;                 (find-file (locate-user-emacs-file "init.el"))))

    ; (defun custom-persp/org ()
    ;   (interactive)
    ;   (custom-persp "@org"
    ;                   (find-file (first org-agenda-files))))
  )
  :config
  (progn
    ;; loading code for our custom perspectives
    ;; taken from Magnar Sveen
    (defmacro custom-persp (name &rest body)
      `(let ((initialize (not (gethash ,name perspectives-hash)))
             (current-perspective persp-curr))
         (persp-switch ,name)
         (when initialize ,@body)
         (setq persp-last current-perspective)))

    (define-key persp-mode-map (kbd "C-x x l") 'dotemacs-custom-persp-last)
    (add-hook 'after-init-hook '(lambda ()
                                  (persp-rename "@dotfiles")))
    ))

(use-package persp-projectile
  :ensure t
  :defer t
  :config
  (progn
    (projectile-persp-bridge helm-projectile-switch-project)
    ; (evil-leader/set-key
    ;   "pp" 'dotemacs-persp-switch-project)
    ))


;;; Processes and commands
(use-package proced                     ; Edit system processes
  ;; Proced isn't available on OS X
  :if (not (eq system-type 'darwin))
  :bind ("C-x p" . proced))

(use-package firestarter                ; Run commands after save
  :ensure t
  :init (firestarter-mode)
  :config (progn (setq firestarter-default-type 'failure)
                 (dotemacs-load-private-file "firestarter-safe-values.el"
                                              'noerror))

  ;; Remove space from firestarter lighter
  :diminish firestarter-mode)

(use-package init-firestarter
  :load-path "config/"
  :commands (dotemacs-firestarter-mode-line)
  :init (after "firestarter"
          (setq firestarter-lighter
                '(:eval (dotemacs-firestarter-mode-line)))))


;;; Date and time
(use-package calendar                   ; Built-in calendar
  :bind ("C-c u c" . calendar)
  :config
  ;; In Europe we start on Monday
  (setq calendar-week-start-day 1))

(use-package time                       ; Show current time
  :bind (("C-c u i" . emacs-init-time)
         ("C-c u t" . display-time-world))
  :config
  (setq display-time-world-time-format "%H:%M %Z, %d. %b"
        display-time-world-list '(("Europe/Berlin"    "Berlin")
                                  ("Europe/London"    "London")
                                  ("Europe/Istanbul"  "Istanbul")
                                  ("America/Winnipeg" "Winnipeg (CA)")
                                  ("America/New_York" "New York (USA)")
                                  ("Asia/Tokyo"       "Tokyo (JP)"))))


;;; Terminal emulation and shells
(use-package shell                      ; Dump shell in Emacs
  :bind ("C-c u s" . shell)
  :config (add-to-list 'display-buffer-alist
                       `(,(rx bos "*shell")
                         (display-buffer-reuse-window
                          display-buffer-in-side-window
                          (side            . bottom)
                          (reusable-frames . visible)
                          (window-height   . 0.4)))))

(use-package term                       ; Terminal emulator in Emacs
  :bind ("C-c u S" . ansi-term))


;;; Net & Web
(use-package browse-url                 ; Browse URLs
  :bind (("C-c w u" . browse-url)))

(use-package bug-reference              ; Turn bug refs into browsable buttons
  :defer t
  :init (progn (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
               (add-hook 'text-mode-hook #'bug-reference-mode)))

(use-package eww                        ; Emacs' built-in web browser
  :bind (("C-c w b" . eww-list-bookmarks)
         ("C-c w w" . eww)))

(use-package sx                         ; StackExchange client for Emacs
  :ensure t
  :bind (("C-c w s" . sx-tab-frontpage)
         ("C-c w S" . sx-tab-newest)
         ("C-c w a" . sx-ask)))

(use-package sx-compose
  :ensure sx
  :defer t
  :config
  (progn
    ;; Don't fill in SX questions/answers, and use visual lines instead.  Plays
    ;; more nicely with the website.
    (add-hook 'sx-compose-mode-hook #'turn-off-auto-fill)
    (add-hook 'sx-compose-mode-hook #'visual-line-mode)
    (add-hook 'sx-compose-mode-hook
              #'dotemacs-whitespace-style-no-long-lines)

    ;; Clean up whitespace before sending questions
    (add-hook 'sx-compose-before-send-hook
              (lambda ()
                (whitespace-cleanup)
                t))

    (bind-key "M-q" #'ignore sx-compose-mode-map)))

(use-package sx-question-mode
  :ensure sx
  :defer t
  ;; Display questions in the same window
  :config (setq sx-question-mode-display-buffer-function #'switch-to-buffer))

(use-package sendmail                   ; Send mails from Emacs
  :defer t
  :config (setq send-mail-function 'smtpmail-send-it))

(use-package message                    ; Compose mails from Emacs
  :defer t
  :config (setq message-send-mail-function 'smtpmail-send-it
                ;; Don't keep message buffers around
                message-kill-buffer-on-exit t))

;; TODO: Incorporate this into `use-package erc` below
;; original `init-erc`
; (after "erc"
;   (setq erc-log-channels-directory (concat dotemacs-cache-directory "erc/logs"))
;   (setq erc-hide-list '("JOIN" "PART" "QUIT"))
;
;   (setq erc-timestamp-only-if-changed-flag nil)
;   (setq erc-timestamp-format "[%H:%M] ")
;   (setq erc-insert-timestamp-function 'erc-insert-timestamp-left)
;
;   (setq erc-truncate-mode t)
;
;   (add-hook 'window-configuration-change-hook
;             (lambda ()
;               (setq erc-fill-column (- (window-width) 2)))))
;; end origianl `init-erc`

(use-package erc                        ; Powerful IRC client
  :defer t
  :config
  (progn
    ;; Default server and nick
    (setq erc-server "chat.freenode.net"
          erc-port 7000
          erc-nick 'dotemacs-erc-nick
          erc-nick-uniquifier "_"
          ;; Never open unencrypted ERC connections
          erc-server-connect-function 'erc-open-tls-stream)

    ;; Spell-check ERC buffers
    (add-to-list 'erc-modules 'spelling)
    (erc-update-modules)))

(use-package erc-join                   ; Automatically join channels with ERC
  :defer t
  :config
  ;; Standard channels on Freenode
  (setq erc-autojoin-channels-alist '(("\\.freenode\\.net" . ("#emacs")))))

(use-package erc-track                  ; Track status of ERC in mode line
  :defer t
  :config
  ;; Switch to newest buffer by default, and don't ask before rebinding the keys
  (setq erc-track-switch-direction 'newest
        erc-track-enable-keybindings t))


;;; Org Mode
(use-package org-plus-contrib
  :ensure t
  :defer t
  :mode ("\\.org$" . org-mode)
  :init
  (progn
    (setq org-replace-disputed-keys t ;; Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
          org-src-fontify-natively t ;; Fontify org-mode code blocks
          org-log-done t
          org-startup-indented t)

    (add-hook 'org-load-hook
              (lambda ()
                (unless (file-exists-p org-directory)
                  (make-directory org-directory))

                (setq my-inbox-org-file (concat org-directory "/inbox.org")
                      org-indent-indentation-per-level 2
                      org-use-fast-todo-selection t
                      org-completion-use-ido t
                      org-treat-S-cursor-todo-selection-as-state-change nil
                      org-agenda-files `(,org-directory))

                (setq org-capture-templates
                      '(("t" "Todo" entry (file+headline my-inbox-org-file "INBOX")
                         "* TODO %?\n%U\n%a\n")
                        ("n" "Note" entry (file+headline my-inbox-org-file "NOTES")
                         "* %? :NOTE:\n%U\n%a\n")
                        ("m" "Meeting" entry (file my-inbox-org-file)
                         "* MEETING %? :MEETING:\n%U")
                        ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
                         "* %?\n%U\n")))

                ;; org-mode colors
                (setq org-todo-keyword-faces
                      '(
                        ("INPR" . (:foreground "yellow" :weight bold))
                        ("DONE" . (:foreground "green" :weight bold))
                        ("IMPEDED" . (:foreground "red" :weight bold))
                        ))

                (setq org-refile-targets '((nil :maxlevel . 9)
                              (org-agenda-files :maxlevel . 9)))

                (setq org-todo-keywords
                      '((sequence "TODO(t)" "NEXT(n@)" "|" "DONE(d)")
                        (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")))

                (setq org-todo-state-tags-triggers
                      ' (("CANCELLED" ("CANCELLED" . t))
                         ("WAITING" ("WAITING" . t))
                         ("TODO" ("WAITING") ("CANCELLED"))
                         ("NEXT" ("WAITING") ("CANCELLED"))
                         ("DONE" ("WAITING") ("CANCELLED"))))

                ))
    ; (evil-leader/set-key-for-mode 'org-mode
    ;   "mc" 'org-capture
    ;   "md" 'org-deadline
    ;   "me" 'org-export-dispatch
    ;   "mf" 'org-set-effort
    ;   "mi" 'org-clock-in
    ;   "mj" 'helm-org-in-buffer-headings
    ;   "mo" 'org-clock-out
    ;   "mm" 'org-ctrl-c-ctrl-c
    ;   "mq" 'org-clock-cancel
    ;   "mr" 'org-refile
    ;   "ms" 'org-schedule)

    (require 'ox-md)
    (require 'ox-ascii)
    (require 'ox-confluence)
    (require 'ox-html)
    (require 'org-bullets)

    ; (after "evil" (add-hook 'org-capture-mode-hook #'evil-emacs-state))

    (after "org-agenda"
      '(progn
         (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
         (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)
         ;; Since we override SPC, let's make RET do that functionality
         (define-key org-agenda-mode-map
           (kbd "RET") 'org-agenda-show-and-scroll-up)
         (define-key org-agenda-mode-map
           (kbd "SPC") evil-leader--default-map))))
  :config
  (progn
    (require 'org-indent)
    (define-key global-map "\C-cl" 'org-store-link)
    (define-key global-map "\C-ca" 'org-agenda)))

(use-package org-bullets
  :ensure t
  :defer t
  :init
  (progn
    (setq org-bullets-bullet-list '("✿" "❀" "☢" "☯" "✸" ))
    (add-hook 'org-mode-hook #'org-bullets-mode)))

(use-package org-repo-todo
  :ensure t
  :defer t
  :commands (ort/capture-todo
             ort/capture-todo-check
             ort/goto-todos)
  :init
  (progn
    ; (evil-leader/set-key
    ;   "Ct"  'ort/capture-todo
    ;   "CT"  'ort/capture-todo-check)
    ; (evil-leader/set-key-for-mode 'org-mode
    ;   "mgt" 'ort/goto-todos)
    ))

; (use-package evil-org
;   :disabled t
;   :commands evil-org-mode
;   :init
;   (add-hook 'org-mode-hook 'evil-org-mode)
;   :config
;   (progn
;     (evil-leader/set-key-for-mode 'org-mode
;          "a" nil "ma" 'org-agenda
;          "c" nil "mA" 'org-archive-subtree
;          "o" nil "mC" 'evil-org-recompute-clocks
;          "l" nil "ml" 'evil-org-open-links
;          "t" nil "mt" 'org-show-todo-tree)
;     (diminish evil-org-mode " ⓔ" " e"))


;;; Online Help
(use-package find-func                  ; Find function/variable definitions
  :bind (("C-x F"   . find-function)
         ("C-x 4 F" . find-function-other-window)
         ("C-x K"   . find-function-on-key)
         ("C-x V"   . find-variable)
         ("C-x 4 V" . find-variable-other-window)))

(use-package info                       ; Info manual viewer
  :defer t
  :config
  ;; Fix the stupid `Info-quoted' face. Courier is an abysmal face, so go back
  ;; to the default face.
  (set-face-attribute 'Info-quoted nil :family 'unspecified
                      :inherit font-lock-type-face))

(use-package helm-info                  ; Helm tools for Info
  :ensure helm
  :bind (("C-c h e" . helm-info-emacs)
         ("C-c h i" . helm-info-at-point)))

(use-package helm-man                   ; Browse manpages with Heml
  :ensure helm
  :bind (("C-c h m" . helm-man-woman)))

(use-package helm-descbinds
  :ensure t
  :init (helm-descbinds-mode))

(use-package ansible-doc                ; Documentation lookup for Ansible
  :ensure t
  :defer t
  :init (add-hook 'yaml-mode-hook #'ansible-doc-mode)
  :diminish (ansible-doc-mode . "❓"))

(use-package dash-at-point
  :ensure t
  :defer t
  :bind (("C-c h d" . dash-at-point)
         ("C-c h D" . dash-at-point-with-docset))
  :config (add-to-list 'dash-at-point-mode-alist
                       '(swift-mode . "ios,swift")))

(bind-key "C-c h b" #'describe-personal-keybindings)

;; Andrew added this as an example
; (use-package evil
;   :init
;   (progn
;     ;; if we don't have this evil overwrites the cursor color
;     (setq evil-default-cursor t)
;
;     ;; leader shortcuts
;
;     ;; This has to be before we invoke evil-mode due to:
;     ;; https://github.com/cofi/evil-leader/issues/10
;     (use-package evil-leader
;       :init (global-evil-leader-mode)
;       :config
;       (progn
;         (setq evil-leader/in-all-states t)
;         ;; keyboard shortcuts
;         (evil-leader/set-key
;           "a" 'ag-project
;           "A" 'ag
;           "b" 'ido-switch-buffer
;           "c" 'mc/mark-next-like-this
;           "C" 'mc/mark-all-like-this
;           "e" 'er/expand-region
;           "E" 'mc/edit-lines
;           "f" 'ido-find-file
;           "g" 'magit-status
;           "i" 'idomenu
;           "j" 'ace-jump-mode
;           "k" 'kill-buffer
;           "K" 'kill-this-buffer
;           "o" 'occur
;           "p" 'magit-find-file-completing-read
;           "r" 'recentf-ido-find-file
;           "s" 'ag-project
;           "t" 'bw-open-term
;           "T" 'eshell
;           "w" 'save-buffer
;           "x" 'smex
;           )))
;
;     ;; boot evil by default
;     (evil-mode 1))
;   :config
;   (progn
;     ;; use ido to open files
;     (define-key evil-ex-map "e " 'ido-find-file)
;     (define-key evil-ex-map "b " 'ido-switch-buffer)
;
;     ;; jj escapes to normal mode
;     (define-key evil-insert-state-map (kbd "j") 'bw-evil-escape-if-next-char-is-j)
;     (setq
;      ;; h/l wrap around to next lines
;      evil-cross-lines t
;      ;; Training wheels: start evil-mode in emacs mode
;      evil-default-state 'emacs)
;
;     ;; esc should always quit: http://stackoverflow.com/a/10166400/61435
;     (define-key evil-normal-state-map [escape] 'keyboard-quit)
;     (define-key evil-visual-state-map [escape] 'keyboard-quit)
;     (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
;     (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
;     (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
;     (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
;     (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)
;
;     ;; modes to map to different default states
;     (dolist (mode-map '((comint-mode . emacs)
;                         (term-mode . emacs)
;                         (eshell-mode . emacs)
;                         (help-mode . emacs)
;                         (fundamental-mode . emacs)))
;       (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))))
;





;; (cl-loop for file in (directory-files (concat user-emacs-directory "defuns/"))
;;   if (not (file-directory-p file))
;;     do (require (intern (file-name-base file)))))

;; TODO https://github.com/IvanMalison/org-projectile

; (add-hook 'c-mode-common-hook
;           (lambda ()
;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;               (ggtags-mode 1))))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
