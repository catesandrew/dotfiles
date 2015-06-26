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
;; - C-c t: Skeletons and templates
;; - C-c u: Miscellaneous utilities, including minor modes
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

(defcustom dotemacs-erc-nick
  'catesandrew
  "The erc nickname to use"
  :group 'dotemacs)

; (defcustom dotemacs-elisp-dir (expand-file-name "elisp" user-emacs-directory)
;   "The storage location lisp."
;   :group 'dotemacs)

(defcustom dotemacs-config-dir (expand-file-name "config" user-emacs-directory)
  "The config location lisp."
  :group 'dotemacs)

(defcustom dotemacs-user-settings-dir (concat user-emacs-directory "users/" user-login-name)
  "The currently logged in user's storage location for settings."
  :group 'dotemacs)

(defcustom dotemacs-leader-key ","
  "The leader key."
  :group 'dotemacs)

(defcustom dotemacs-emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'"
  :group 'dotemacs)

(defcustom dotemacs-major-mode-leader-key nil
  "Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m`. Set it to `nil` to disable it."
  :group 'dotemacs)

(defcustom dotemacs-major-mode-emacs-leader-key nil
  "Major mode leader key accessible in `emacs state' and `insert state'"
  :group 'dotemacs)

(defcustom dotemacs-command-key ":"
  "The key used for Evil commands (ex-commands) and Emacs commands (M-x).
By default the command key is `:' so ex-commands are executed like in Vim
with `:' and Emacs commands are executed with `<leader> :'."
  :group 'dotemacs)

(defcustom dotemacs-enable-paste-micro-state t
  "If non nil the paste micro-state is enabled. While enabled pressing `p`
several times cycle between the kill ring content.'"
  :group 'dotemacs)

(defcustom dotemacs-guide-key-delay 0.4
  "Guide-key delay in seconds."
  :group 'dotemacs)

(defcustom dotemacs-search-tools '("ag" "pt" "ack" "grep")
  "List of search tool executable names. Dotemacs uses the first installed
tool of the list. Supported tools are `ag', `pt', `ack' and `grep'."
  :group 'dotemacs)

;; Regexp for useful and useless buffers for smarter buffer switching
(defcustom dotemacs-useless-buffers-regexp '("*\.\+")
  "Regexp used to determine if a buffer is not useful."
  :group 'dotemacs)

(defcustom dotemacs-useful-buffers-regexp '("\\*\\(scratch\\|terminal\.\+\\|ansi-term\\|eshell\\)\\*")
  "Regexp used to define buffers that are useful despite matching
`dotemacs-useless-buffers-regexp'."
  :group 'dotemacs)

(defcustom dotemacs-active-transparency 90
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's active or selected. Transparency
can be toggled through `toggle-transparency'."
  :group 'dotemacs)

(defcustom dotemacs-inactive-transparency 90
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's inactive or deselected. Transparency
can be toggled through `toggle-transparency'."
  :group 'dotemacs)

(defcustom dotemacs-smartparens-strict-mode t
  "If non-nil smartparens-strict-mode will be enabled in programming modes."
  :group 'dotemacs)

(defconst dotemacs-filepath (expand-file-name "." user-emacs-directory)
  "Filepath to the installed dotfile.")

(defcustom dotemacs-persistent-server nil
  "If non nil advises quit functions to keep server open when quitting.")
  :group 'dotemacs

(defcustom dotemacs-mode-line-unicode-symbols t
  "If non nil unicode symbols are displayed in the mode-line (eg. for lighters)")
  :group 'dotemacs

(defcustom dotemacs-fullscreen-use-non-native nil
  "If non nil `dotemacs-toggle-fullscreen' will not use native fullscreen. Use
to disable fullscreen animations in OSX."
  :group 'dotemacs)

;; whitespace-mode
(defcustom dotemacs-show-trailing-whitespace t
  "If t, show trailing whitespace."
  :type 'boolean
  :group 'dotemacs)

(defcustom dotemacs-highlight-delimiters 'all
  "Select a scope to highlight delimiters. Possible value is `all', `current'
or `nil'. Default is `all'"
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

(defcustom dotemacs-clojure-enable-fancify-symbols nil
  "If non nil the `fancify-symbols' function is enabled."
  :group 'dotemacs)

;; git settings
(defgroup dotemacs-git nil
  "Configuration options for git."
  :group 'dotemacs
  :prefix 'dotemacs-git)

(defcustom dotemacs-git-enable-magit-svn-plugin nil
  "If non nil `magit-svn' plugin is enabled."
  :group 'dotemacs-git)

(defcustom dotemacs-git-magit-status-fullscreen t
  "If non nil magit-status buffer is displayed in fullscreen."
  :group 'dotemacs-git)

;; evil settings
(defgroup dotemacs-evil nil
  "Configuration options for evil-mode."
  :group 'dotemacs
  :prefix 'dotemacs-evil)

(defcustom dotemacs-evil-evil-state-modes
  '(fundamental-mode
    text-mode
    prog-mode
    sws-mode
    dired-mode
    comint-mode
    log-edit-mode
    messages-buffer-mode
    project-explorer-mode
    compilation-mode)
  "List of modes that should start up in Evil state."
  :type '(repeat (symbol))
  :group 'dotemacs-evil)

(defcustom dotemacs-evil-emacs-state-modes
  '(debugger-mode
    git-commit-mode
    git-rebase-mode)
  "List of modes that should start up in Evil Emacs state."
  :type '(repeat (symbol))
  :group 'dotemacs-evil)

(defcustom dotemacs-evil-cursor-colors '((normal . "DarkGoldenrod2")
                                       (insert . "chartreuse3")
                                       (emacs  . "SkyBlue2")
                                       (evilified . "LightGoldenrod3")
                                       (visual . "gray")
                                       (motion . "plum3")
                                       (lisp   . "HotPink1")
                                       (iedit  . "firebrick1")
                                       (iedit-insert  . "firebrick1"))
  "Colors assigned to evil states."
  :type '(repeat (symbol))
  :group 'dotemacs-evil)

;; auto-completion settings
(defgroup dotemacs-ac nil
  "Configuration options for auto completion."
  :group 'dotemacs
  :prefix 'dotemacs-ac)

(defcustom dotemacs-completion-engine
  'company
  "The completion engine the use."
  :type '(radio
          (const :tag "company-mode" company)
          (const :tag "auto-complete-mode" auto-complete))
  :group 'dotemacs-ac)

(defcustom auto-completion-return-key-behavior 'complete
  "What the RET key should do when auto-completion menu is active.
Possible values are `complete' or `nil'."
  :group 'dotemacs-ac)

(defcustom auto-completion-tab-key-behavior 'cycle
  "What the TAB key should do when auto-completion menu is active.
Possible values are `complete', `cycle' or `nil'."
  :group 'dotemacs-ac)

(defcustom auto-completion-complete-with-key-sequence nil
  "Provide a key sequence (string) to complete the current
selection."
  :group 'dotemacs-ac)

(defcustom auto-completion-enable-sort-by-usage nil
  "If non nil suggestions are sorted by how often they are used."
  :group 'dotemacs-ac)

(defcustom auto-completion-enable-help-tooltip t
  "If non nil the docstring appears in a tooltip."
  :group 'dotemacs-ac)

(defcustom company-mode-completion-cancel-keywords
  '("do"
    "then"
    "begin"
    "case")
  "Keywords on which to cancel completion so that you can use RET
to complet without blocking common line endings."
  :group 'dotemacs-ac)

;; spelling/syntax settings
(defgroup dotemacs-s nil
  "Configuration options for spelling/syntax."
  :group 'dotemacs
  :prefix 'dotemacs-s)

(defcustom dotemacs-s-syntax-checking-enable-tooltips nil
  "If non nil some feedback are displayed in tooltips."
  :group 'dotemacs-s)

;;helm
(defface dotemacs-helm-navigation-ms-face
      `((t :background ,(face-attribute 'error :foreground) :foreground "black"))
      "Face for helm heder when helm micro-state is activated."
      :group 'dotemacs)

;; perf measurments
(with-current-buffer (get-buffer-create "*Require Times*")
  (insert "| feature | timestamp | elapsed |\n")
  (insert "|---------+-----------+---------|\n"))

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
; (add-to-list 'load-path dotemacs-elisp-dir)
(add-to-list 'load-path dotemacs-user-settings-dir)

; ;; Add external projects to load path
; (let ((base dotemacs-elisp-dir))
;   (add-to-list 'load-path base)
;   (dolist (dir (directory-files base t "^[^.]"))
;     (when (file-directory-p dir)
;       (add-to-list 'load-path dir))))


;;; Locality
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top


;;; After and other macros

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

(defmacro dotemacs-bind (&rest commands)
  "Convience macro which creates a lambda interactive command."
  `(lambda ()
     (interactive)
     ,@commands))


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

;; inject use-package hooks for easy customization of
;; stock package configuration
(setq use-package-inject-hooks t)


;;; Requires

(eval-when-compile
  (require 'use-package))

(eval-when-compile
  (require 'cl))

(require 'bind-key)
(require 'diminish)
;; Minor modes abbrev --------------------------------------------------------
(when (display-graphic-p)
  (after "eproject"
    '(diminish 'eproject-mode " eⓅ"))
  (after "flymake"
    '(diminish 'flymake-mode " Ⓕ2")))
;; Minor Mode (hidden) ------------------------------------------------------
(after "abbrev"
  '(diminish 'abbrev-mode))

(require 'subr-x)
(require 'rx)
(require 'time-date)


;;; Key Binding Init

(after "evil-leader"
  ;; We define prefix commands only for the sake of guide-key
  (setq dotemacs-key-binding-prefixes
        '(("a" .  "applications")
          ("ai" . "applications-irc")
          ("as" . "applications-shells")
          ("b" .  "buffers")
          ("bm" . "buffers-move")
          ("c" .  "compile/comments")
          ("C" .  "capture/colors")
          ("e" .  "errors")
          ("f" .  "files")
          ("fe" . "files-emacs/spacemacs")
          ("g" .  "git/versions-control")
          ("h" .  "helm/help/highlight")
          ("hd" . "help-describe")
          ("i" .  "insertion")
          ("j" .  "join/split")
          ("k" .  "lisp")
          ("kd" .  "lisp-delete")
          ("kD" .  "lisp-delete-backward")
          ("n" .  "narrow/numbers")
          ("p" .  "projects")
          ("p$" .  "projects/shell")
          ("q" .  "quit")
          ("r" .  "registers/rings")
          ("s" .  "search/symbol")
          ("sw" .  "search-web")
          ("t" .  "toggles")
          ("tC" . "toggles-colors")
          ("th" . "toggles-highlight")
          ("tm" . "toggles-modeline")
          ("T" .  "toggles/themes")
          ("w" .  "windows")
          ("wp" . "windows-popup")
          ("wS" . "windows-size")
          ("x" .  "text")
          ("xa" . "text-align")
          ("xd" . "text-delete")
          ("xg" . "text-google-translate")
          ("xm" . "text-move")
          ("xt" . "text-transpose")
          ("xw" . "text-words")
          ("z" .  "zoom")))
  (mapc (lambda (x) (dotemacs-declare-prefix (car x) (cdr x)))
        dotemacs-key-binding-prefixes))


;;; Initialization
;; And disable the site default settings
(setq inhibit-default-init t)

;; start scratch in text mode (usefull to get a faster Emacs load time
;; because it avoids autoloads of elisp modes)
(setq initial-major-mode 'text-mode)

;; Disable case insensitivity for filename autocompletion in shell-mode
(setq pcomplete-ignore-case t) ;; Controls case sensitivity for pcomplete

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

(use-package init-funcs
  :load-path "config/")

(when (and (system-is-mac) (version< emacs-version "25"))
  (warn "This configuration needs Emacs trunk, but this is %s!" emacs-version)
  (warn "brew install emacs --HEAD --srgb --use-git-head --with-cocoa --with-gnutls --with-rsvg --with-imagemagick"))

(when (system-is-mac)
  ;; Warn if the current build is more than a week old
  (run-with-idle-timer
   2 nil
   (lambda ()
     (let ((time-since-build (time-subtract (current-time) emacs-build-time)))
       (when (> (time-to-number-of-days time-since-build) 7)
         (lwarn 'emacs :warning "Your Emacs build is more than a week old!"))))))

(use-package core-funcs
  :load-path "core/")

(use-package core-micro-state
  :load-path "core/")

(use-package core-use-package
  :load-path "core/")

(use-package core-toggle
  :load-path "core/")

(use-package core-auto-completion
  :load-path "core/")


;;; Setup environment variables from the user's shell.
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

    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))

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
  :commands paradox-list-packages
  :init
  (progn
    (setq paradox-execute-asynchronously nil)
    (defun dotemacs-paradox-list-packages ()
      "Load depdendencies for auth and open the package list."
      (interactive)
      (require 'epa-file)
      (require 'auth-source)
      (when (and (not (boundp 'paradox-github-token))
                 (file-exists-p "~/.authinfo.gpg"))
        (let ((authinfo-result (car (auth-source-search
                                     :max 1
                                     :host "github.com"
                                     :port "paradox"
                                     :user "paradox"
                                     :require '(:secret)))))
          (let ((paradox-token (plist-get authinfo-result :secret)))
            (setq paradox-github-token (if (functionp paradox-token)
                                           (funcall paradox-token)
                                         paradox-token)))))
      (paradox-list-packages nil))

    (after "evil-evilified-state"
      (evilify paradox-menu-mode paradox-menu-mode-map
               "H" 'paradox-menu-quick-help
               "J" 'paradox-next-describe
               "K" 'paradox-previous-describe
               "L" 'paradox-menu-view-commit-list
               "o" 'paradox-menu-visit-homepage))
    (after "evil-leader"
      (evil-leader/set-key
        "aP" 'dotemacs-paradox-list-packages))))

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
  :commands (dotemacs-id-of-bundle
             dotemacs-path-of-bundle
             dotemacs-homebrew-prefix
             dotemacs-homebrew-installed-p
             dotemacs-open-current-file
             dotemacs-copy-from-osx
             dotemacs-paste-to-osx
             dotemacs-chomp
             dotemacs-get-keychain-password)
  :init
  (progn
    ;; Using this configuration, Emacs runs best in iTerm2.

    ;; On the desktop, Emacs integrates with the OS X clipboard, so kill
    ;; etc. copy to the clipboard, and yank copies from the clipboard.

    ;; Obviously this doesn’t work in the terminal, so we need to use the
    ;; interprogram-(cut|paste)-function variables to copy/paste. Most of
    ;; this code gotten from this blog comment.
    (unless (display-graphic-p)
      (setq interprogram-cut-function 'dotemacs-paste-to-osx)
      (setq interprogram-paste-function 'dotemacs-copy-from-osx)))
  :config
  (progn
    ;; Ignore .DS_Store files with ido mode
    (add-to-list 'ido-ignore-files "\\.DS_Store"))
  :bind ("C-c f o" . dotemacs-open-current-file))

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

;; fringes
(setq-default fringe-indicator-alist
              '((truncation . nil) (continuation . nil)))

;; Just don’t show them. Use native Emacs controls:
(setq use-dialog-box nil)

;; Show line number in mode line
(setq line-number-mode t)
;; Show column number in mode line
(setq column-number-mode t)
;; line number
(setq linum-format "%4d")

;; No blinking and beeping, no startup screen, no scratch message and short
;; Yes/No questions.
(blink-cursor-mode 0)
(tooltip-mode 0)

;; draw underline lower
(setq x-underline-at-descent-line t)

;; no beep pleeeeeease ! (and no visual blinking too please)
(setq ring-bell-function #'ignore
      inhibit-startup-screen t
      visible-bell nil
      initial-scratch-message nil)

(toggle-transparency)

;; Answering just 'y' or 'n' will do
(fset 'yes-or-no-p #'y-or-n-p)
;; Opt out from the startup message in the echo area by simply disabling this
;; ridiculously bizarre thing entirely.
(fset 'display-startup-echo-area-message #'ignore)

;; don't let the cursor go into minibuffer prompt
;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

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
  :init (load-theme 'solarized-dark 'no-confirm)
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
  :init
  (progn
    (load-theme 'zenburn 'no-confirm)))

(use-package linum-relative
  :ensure t
  :commands linum-relative-toggle
  :init
  (after "evil-leader"
    (evil-leader/set-key "tr" 'linum-relative-toggle))
  :config
  (progn
    (setq linum-format 'linum-relative)
    (setq linum-relative-current-symbol "")
    (linum-relative-toggle)))

(bind-key "C-c u v" #'variable-pitch-mode)


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

(use-package fancy-battery              ; Fancy battery info for mode line
  :ensure t
  :defer t
  :init (fancy-battery-mode))

(use-package evil-anzu                  ; Position/matches count for isearch
  :ensure t
  :init (global-anzu-mode)
  :config
  (progn
    (setq anzu-search-threshold 1000
          anzu-cons-mode-line-p nil))
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
;; Display current keystrokes almost immediately in mini buffer
(setq echo-keystrokes 0.2)

;; escape minibuffer
(define-key minibuffer-local-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)

;; have no use for these default bindings
(global-unset-key (kbd "C-x m"))

(use-package savehist                   ; Save minibuffer history
  :init (savehist-mode t)
  :config (setq savehist-save-minibuffer-history t
                enable-recursive-minibuffers t ; Allow commands in minibuffers
                history-length 1000
                savehist-file (concat dotemacs-cache-directory "savehist")
                savehist-additional-variables '(search ring regexp-search-ring)
                savehist-autosave-interval 180))

;; Helm: Unite/CtrlP style fuzzy file/buffer/anything searcher on steroids
;;
;; Helm does the same thing as Unite/CtrlP on Vim and does it really well. You
;; can also enable Helm to manage the command buffer, which is pretty awesome
;; with: (helm-mode 1)
(use-package init-helm
  :load-path "config/")

(use-package helm
  :ensure t
  :bind (("C-c b b" . helm-resume))
  :commands dotemacs-helm-find-files
  :init
  (progn
    (after "helm-config"
           (warn "`helm-config' loaded! Get rid of it ASAP!"))

    (setq helm-prevent-escaping-from-minibuffer t
          helm-bookmark-show-location t
          helm-display-header-line nil
          helm-split-window-in-side-p t
          helm-always-two-windows t)

    ;; fuzzy matching setting
    (setq helm-M-x-fuzzy-match t
          helm-apropos-fuzzy-match t
          helm-file-cache-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-lisp-fuzzy-completion t
          helm-locate-fuzzy-match t
          helm-recentf-fuzzy-match t
          helm-semantic-fuzzy-match t
          helm-buffers-fuzzy-matching t)

    ;; NOTE: Apple OS X users also need a version of grep that accepts --exclude-dir
    ;; brew tap homebrew/dupes
    ;; brew install homebrew/dupes/grep
    (when-let (gnu-grep (and (eq system-type 'darwin)
                           (executable-find "ggrep")))
    (setq helm-grep-default gnu-grep))

    (defadvice helm-ff-delete-char-backward
        (around dotemacs-helm-find-files-navigate-back activate)
      (if (= (length helm-pattern) (length (helm-find-files-initial-input)))
          (helm-find-files-up-one-level 1)
        ad-do-it))

    ;; use helm by default for M-x
    (global-set-key (kbd "M-x") 'helm-M-x)

    (after "evil-leader"
      (evil-leader/set-key
        "<f1>" 'helm-apropos
        "bb"   'helm-mini
        "Cl"   'helm-colors
        "ff"   'dotemacs-helm-find-files
        "fF"   'helm-find-files
        "fr"   'helm-recentf
        "hb"   'helm-pp-bookmarks
        "hi"   'helm-info-at-point
        "hl"   'helm-resume
        "hm"   'helm-man-woman
        "ry"   'helm-show-kill-ring
        "rr"   'helm-register
        "rm"   'helm-all-mark-rings
        "sL"   'dotemacs-last-search-buffer
        "sl"   'dotemacs-jump-in-buffer)

      ;; search with grep
      (evil-leader/set-key
        "sgb"  'dotemacs-helm-buffers-do-grep
        "sgB"  'dotemacs-helm-buffers-do-grep-region-or-symbol
        "sgf"  'dotemacs-helm-files-do-grep
        "sgF"  'dotemacs-helm-files-do-grep-region-or-symbol
        "sgg"  'dotemacs-helm-file-do-grep
        "sgG"  'dotemacs-helm-file-do-grep-region-or-symbol)

      (evil-leader/set-key
        dotemacs-command-key 'helm-M-x))

    (add-hook 'helm-after-initialize-hook 'dotemacs-display-helm-at-bottom)
    ;;  Restore popwin-mode after a Helm session finishes.
    (add-hook 'helm-cleanup-hook 'dotemacs-restore-previous-display-config)

    ;; Add minibuffer history with `helm-minibuffer-history'
    (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

    (add-hook 'helm-cleanup-hook 'dotemacs-helm-cleanup))
  :config
  (progn
    (helm-mode +1)
    (add-hook 'helm-find-files-before-init-hook 'dotemacs-set-dotted-directory)

    (add-hook 'helm-mode-hook 'simpler-helm-bookmark-keybindings)

    (dotemacs-helm-hjkl-navigation t)

    (after "evil-leader"
      (dotemacs-define-micro-state helm-navigation
        :persistent t
        :disable-evil-leader t
        :define-key (helm-map . "M-SPC") (helm-map . "s-M-SPC")
        :on-enter (dotemacs-helm-navigation-ms-on-enter)
        :on-exit  (dotemacs-helm-navigation-ms-on-exit)
        :bindings
        ("<tab>" helm-select-action :exit t)
        ("C-i" helm-select-action :exit t)
        ("<RET>" helm-maybe-exit-minibuffer :exit t)
        ("?" nil :doc (dotemacs-helm-navigation-ms-full-doc))
        ("a" helm-select-action :post (dotemacs-helm-navigation-ms-set-face))
        ("e" dotemacs-helm-edit)
        ("h" helm-previous-source)
        ("j" helm-next-line)
        ("k" helm-previous-line)
        ("l" helm-next-source)
        ("q" nil :exit t)
        ("t" helm-toggle-visible-mark)
        ("T" helm-toggle-all-marks)
        ("v" helm-execute-persistent-action)))

    ;; Swap default TAB and C-z commands.
    ;; For GUI.
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    ;; For terminal.
    (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z") 'helm-select-action)

    (eval-after-load "helm-mode" ; required
      '(dotemacs-hide-lighter helm-mode))))

(use-package helm-swoop
  :ensure t
  :defer t
  :init
  (after "helm"
    (progn
      (setq helm-swoop-split-with-multiple-windows t
            helm-swoop-use-line-number-face t
            helm-swoop-split-direction 'split-window-vertically
            helm-swoop-speed-or-color t
            helm-swoop-split-window-function 'helm-default-display-buffer
            helm-swoop-pre-input-function (lambda () ""))

      (defun dotemacs-helm-swoop-region-or-symbol ()
        "Call `helm-swoop' with default input."
        (interactive)
        (let ((helm-swoop-pre-input-function
               (lambda ()
                 (if (region-active-p)
                     (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                   (let ((thing (thing-at-point 'symbol t)))
                     (if thing thing ""))))))
          (call-interactively 'helm-swoop)))

      (after "evil-leader"
        (evil-leader/set-key
          "ss"    'helm-swoop
          "sS"    'dotemacs-helm-swoop-region-or-symbol
          "s C-s" 'helm-multi-swoop-all))
      (defadvice helm-swoop (before add-evil-jump activate)
        (evil-set-jump)))))

(use-package helm-misc                  ; Misc helm commands
  :ensure helm
  :bind (([remap switch-to-buffer] . helm-mini)))

(use-package helm-themes
  :ensure helm
  :defer t
  :init
  (after "evil-leader"
    (evil-leader/set-key
      "Th" 'helm-themes)))

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

(use-package helm-mode-manager
  :ensure t
  :defer t
  :init
  (after "evil-leader"
    (evil-leader/set-key
      "hM"    'helm-switch-major-mode
      ;; "hm"    'helm-disable-minor-mode
      "h C-m" 'helm-enable-minor-mode)))


;;; Buffer, Windows and Frames
(setq truncate-partial-width-windows nil ; Make side by side buffers function
                                         ; the same as the main window.
      frame-resize-pixelwise t           ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

(setq-default line-spacing 0.1)         ; A bit more spacing between lines

(use-package buffer-move
  :defer t
  :init
  (after "evil-leader"
    (evil-leader/set-key
      "bmh" 'buf-move-left
      "bmj" 'buf-move-down
      "bmk" 'buf-move-up
      "bml" 'buf-move-right)))

(use-package frame
  :bind (("C-c u F" . toggle-frame-fullscreen))
  :init
  (progn
    ;; Kill `suspend-frame'
    (global-set-key (kbd "C-z") nil)
    (global-set-key (kbd "C-x C-z") nil))
  :config
  (progn
    ; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
    (add-to-list 'initial-frame-alist '(width . 120))
    (add-to-list 'initial-frame-alist '(height . 72))))

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
  ;; When having windows with repeated filenames, uniquify them
  ;; by the folder they are in rather those annoying <2>,<3>,.. etc
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets
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
  :init
  (progn
    (add-hook 'ibuffer-hook
              (lambda ()
                ; (ibuffer-switch-to-saved-filter-groups "home")
                (ibuffer-auto-mode 1)))
    (setq ibuffer-expert t
          ibuffer-show-empty-filter-groups nil))
  :config
  (progn
    (setq ibuffer-formats
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
                         (name . "\*info\*"))))))))

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
  :ensure t
  :init
  (progn
    ;; activate winner mode use to undo and redo windows layout
    (winner-mode t))
  :config
  (progn
    (setq dotemacs-winner-boring-buffers '("*Completions*"
                                           "*Compile-Log*"
                                           "*inferior-lisp*"
                                           "*Fuzzy Completions*"
                                           "*Apropos*"
                                           "*Help*"
                                           "*cvs*"
                                           "*Buffer List*"
                                           "*Ibuffer*"
                                           "*esh command on file*"
                                            ))
    (setq winner-boring-buffers
          (append winner-boring-buffers dotemacs-winner-boring-buffers))))

(use-package window-numbering
  :ensure t
  ;; not deferred on puprose
  :init (require 'window-numbering)
  :config
  (progn
    (setq window-numbering-auto-assign-0-to-minibuffer nil)
    (after "evil-leader"
      (evil-leader/set-key
        "0" 'select-window-0
        "1" 'select-window-1
        "2" 'select-window-2
        "3" 'select-window-3
        "4" 'select-window-4
        "5" 'select-window-5
        "6" 'select-window-6
        "7" 'select-window-7
        "8" 'select-window-8
        "9" 'select-window-9) )
    (window-numbering-mode 1))

  (defun dotemacs-window-number ()
    "Return the number of the window."
    (let* ((num (window-numbering-get-number))
           (str (if num (int-to-string num))))
      (cond
       ((not dotemacs-mode-line-unicode-symbols) str)
       ((equal str "1")  "➊")
       ((equal str "2")  "➋")
       ((equal str "3")  "➌")
       ((equal str "4")  "➍")
       ((equal str "5")  "➎")
       ((equal str "6")  "❻")
       ((equal str "7")  "➐")
       ((equal str "8")  "➑")
       ((equal str "9")  "➒")
       ((equal str "0")  "➓"))))

  (defun dotemacs-window-numbering-assign (windows)
    "Custom number assignment for special buffers."
    (mapc (lambda (w)
            (when (and (boundp 'neo-global--window)
                       (eq w neo-global--window))
              (window-numbering-assign w 0)))
          windows))
  (add-hook 'window-numbering-before-hook 'dotemacs-window-numbering-assign))

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
  :bind (("C-c u r" . writeroom-mode)))

(setq editorconfig-packages '(editorconfig))
(use-package editorconfig
  :defer t
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.editorconfig" . conf-unix-mode)))

(use-package popwin
  :ensure t
  :bind ("C-c P" . popwin:popup-last-buffer)
  :init
  (progn
    ;; don't use default value but manage it ourselves
    (setq popwin:special-display-config nil)

    ;; buffers that we manage
    (push '("*Help*"                 :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
    (push '("*compilation*"          :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
    (push '("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("*Async Shell Command*"  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '(" *undo-tree*"           :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
    (push '("*ert*"                  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("*grep*"                 :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("*nosetests*"            :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("^\*WoMan.+\*$" :regexp t             :position bottom                                   ) popwin:special-display-config)
    (push '("^\*Flycheck.+\*$" :regexp t
                                     :dedicated t :position bottom :stick t :noselect t              ) popwin:special-display-config)

    (defun dotemacs-remove-popwin-display-config (str)
      "Removes the popwin display configurations that matches the passed STR"
      (setq popwin:special-display-config
            (-remove (lambda (x) (if (and (listp x) (stringp (car x)))
                                     (string-match str (car x))))
                     popwin:special-display-config))))
  :config
  (progn
    (popwin-mode 1)
    (after "evil-leader"
      (evil-leader/set-key "wpm" 'popwin:messages)
      (evil-leader/set-key "wpp" 'popwin:close-popup-window))))


;;; File handling

;; don't create backup~ or #auto-save# files
(setq backup-by-copying t
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; Keep backup and auto save files out of the way
(setq backup-directory-alist `((".*" . ,(concat dotemacs-cache-directory "backups")))
      auto-save-file-name-transforms `((".*" ,(concat dotemacs-cache-directory "backups") t))
      auto-save-list-file-prefix (concat dotemacs-cache-directory "auto-save-list/saves-"))

;; Transparently open compressed files
(auto-compression-mode t)

;; Delete files to trash
(setq delete-by-moving-to-trash t)

;; auto-save
(let
    ((autosave-dir (expand-file-name (concat dotemacs-cache-directory "autosave"))))
  (unless (file-exists-p autosave-dir)
    (make-directory autosave-dir))
  (setq auto-save-list-file-prefix (concat autosave-dir "/")
        auto-save-file-name-transforms `((".*" ,autosave-dir t))))

;; Remove prompt if the file is opened in other clients
(defun server-remove-kill-buffer-hook ()
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook 'server-remove-kill-buffer-hook)

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
  :ensure t
  :defer t
  :config
  ;; Store auto-save files locally
  (setq tramp-auto-save-directory (concat dotemacs-cache-directory "tramp-auto-save")))

(use-package open-junk-file
  :ensure t
  :defer t
  :commands (open-junk-file)
  :init
  (after "evil-leader"
    (evil-leader/set-key "fJ" 'open-junk-file))
    (setq open-junk-file-directory (concat dotemacs-cache-directory "junk/")))

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
  :init
  (progn
    (setq bookmark-save-flag 1
          ;; Store auto-save files locally
          bookmark-default-file (concat dotemacs-cache-directory "bookmarks")
          url-configuration-directory (concat dotemacs-cache-directory "url")
          eshell-directory-name (concat dotemacs-cache-directory "eshell" )
          tramp-persistency-file-name (concat dotemacs-cache-directory "tramp"))))

;; original
(run-with-timer 1800 1800 'recentf-save-list)
;; original

(use-package recentf                    ; Save recently visited files
  :defer t
  :init
  ;; lazy load recentf
  (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                         (recentf-mode)
                                         (recentf-track-opened-file))))
  :config
  (setq recentf-save-file (concat dotemacs-cache-directory "recentf")
        recentf-max-saved-items 200
        recentf-max-menu-items 50
        recentf-auto-save-timer (run-with-idle-timer 600 t 'recentf-save-list)
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
  (setq save-place-file (concat dotemacs-cache-directory "places")))

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
  :commands (dotemacs-create-non-existent-directory
             dotemacs-recompile-packages)
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

;; Mouse cursor in terminal mode
(xterm-mouse-mode 1)

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
         ("C-c j w" . avy-goto-word-1))
  :init (after "evil"
    ; (define-key evil-operator-state-map (kbd "z") 'avy-goto-char-2)
    ; (define-key evil-normal-state-map (kbd "s") 'avy-goto-char-2)
    ; (define-key evil-motion-state-map (kbd "S-SPC") 'avy-goto-line)
    ))

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
  :bind (("C-c u l" . nlinum-mode)))

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
;; that you can always see what's happening. Also remove annoying ellipsis when
;; printing sexp in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; Don't highlight matches with jump-char - it's distracting
(setq jump-char-lazy-highlight-face nil)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; enable electric indent
(setq electric-indent-mode t)

;; Text
(setq longlines-show-hard-newlines t)

;; Disable tabs, but given them proper width
(setq-default indent-tabs-mode nil
              highlight-tabs t
              tab-width 8)

;; Turn on electric-indent-mode
(electric-indent-mode)

;; Make Tab complete if the line is indented
(setq tab-always-indent 'complete)

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(setq indicate-empty-lines t
      require-final-newline t)

;; Globally do not break lines for me, please
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

(use-package clean-aindent-mode ; Keeps track of the last auto-indent operation and trims down white space
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'clean-aindent-mode))

(use-package move-text
  :ensure t
  :defer t
  :init
  (after "evil-leader"
    (dotemacs-define-micro-state move-text
      :doc "[J] move down [K] move up"
      :use-minibuffer t
      :execute-binding-on-enter t
      :evil-leader "xJ" "xK"
      :bindings
      ("J" move-text-down)
      ("K" move-text-up))))

(use-package helm-ring                  ; Helm commands for rings
  :ensure helm
  :bind (([remap yank-pop]        . helm-show-kill-ring)
         ([remap insert-register] . helm-register)))

(use-package delsel                     ; Delete the selection instead of insert
  :defer t
  :init (delete-selection-mode))

(use-package whitespace-cleanup-mode    ; Cleanup whitespace in buffers
  :ensure t
  :bind (("C-c u w c" . whitespace-cleanup-mode)
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
  :bind (("C-=" . er/expand-region))
  :defer t
  :init (after "evil-leader"
          (evil-leader/set-key "v" 'er/expand-region))
  :config
  (progn
    ;; add search capability to expand-region
    (after "helm-ag"
      (defadvice er/prepare-for-more-expansions-internal
          (around helm-ag/prepare-for-more-expansions-internal activate)
        ad-do-it
        (let ((new-msg (concat (car ad-return-value)
                               ", / to search in project, "
                               "f to search in files, "
                               "b to search in opened buffers"))
              (new-bindings (cdr ad-return-value)))
          (cl-pushnew
           '("/" (lambda ()
                   (call-interactively
                    'dotemacs-helm-project-smart-do-search-region-or-symbol)))
           new-bindings)
          (cl-pushnew
           '("f" (lambda ()
                   (call-interactively
                    'dotemacs-helm-files-smart-do-search-region-or-symbol)))
           new-bindings)
          (cl-pushnew
           '("b" (lambda ()
                   (call-interactively
                    'dotemacs-helm-buffers-smart-do-search-region-or-symbol)))
           new-bindings)
            (setq ad-return-value (cons new-msg new-bindings)))))
    (custom-set-variables
     '(expand-region-contract-fast-key "V")
     '(expand-region-reset-fast-key "r"))))

(use-package undo-tree                  ; Branching undo
  :ensure t
  :init
  (global-undo-tree-mode)
  ;; (setq undo-tree-auto-save-history t
  ;;       undo-tree-history-directory-alist
  ;;       `(("." . ,(concat dotemacs-cache-directory "undo"))))
  ;; (unless (file-exists-p (concat dotemacs-cache-directory "undo"))
  ;;     (make-directory (concat dotemacs-cache-directory "undo")))
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  :config
  :diminish (undo-tree-mode . "↺"))

;; Give us narrowing back!
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

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

(use-package init-smartparens      ; Personal Smartparens extensions
  :load-path "config/")

;; Use SmartParens instead of Paredit and Electric Pair
(use-package smartparens                ; Parenthesis editing and balancing
  :ensure t
  :defer t
  :init
  (progn
    (add-to-hooks (if dotemacs-smartparens-strict-mode
                      'smartparens-strict-mode
                    'smartparens-mode)
                  '(prog-mode-hook))

    (add-hook 'minibuffer-setup-hook 'dotemacs-conditionally-enable-smartparens-mode)

    (after "evil-leader"
      (dotemacs-add-toggle smartparens
                           :status smartparens-mode
                           :on (smartparens-mode)
                           :off (smartparens-mode -1)
                           :documentation "Enable smartparens."
                           :evil-leader "tp")
      (dotemacs-add-toggle smartparens-globally
                           :status smartparens-mode
                           :on (smartparens-global-mode)
                           :off (smartparens-global-mode -1)
                           :documentation "Enable smartparens globally."
                           :evil-leader "t C-p"))

    (setq sp-show-pair-delay 0
          sp-show-pair-from-inside t ; fix paren highlighting in normal mode
          sp-cancel-autoskip-on-backward-movement nil))
  :config
  (progn
    (require 'smartparens-config)

    ;;; Additional pairs for various modes

    ;; Emacs Lisp
    (sp-local-pair '(emacs-lisp-mode lisp-interaction-mode inferior-emacs-lisp-mode)
                   "(" nil :bind "M-(")

    ;;; Key bindings

    (let ((map smartparens-mode-map))
      ;; Movement and navigation
      (define-key map (kbd "C-M-f") #'sp-forward-sexp)
      (define-key map (kbd "C-M-b") #'sp-backward-sexp)
      (define-key map (kbd "C-M-u") #'sp-backward-up-sexp)
      (define-key map (kbd "C-M-d") #'sp-down-sexp)
      (define-key map (kbd "C-M-p") #'sp-backward-down-sexp)
      (define-key map (kbd "C-M-n") #'sp-up-sexp)
      ;; Deleting and killing
      (define-key map (kbd "C-M-k") #'sp-kill-sexp)
      (define-key map (kbd "C-M-w") #'sp-copy-sexp)
      ;; Depth changing
      (define-key map (kbd "M-S-<up>") #'sp-splice-sexp)
      (define-key map (kbd "M-<up>") #'sp-splice-sexp-killing-backward)
      (define-key map (kbd "M-<down>") #'sp-splice-sexp-killing-forward)
      (define-key map (kbd "M-C-<up>") #'sp-splice-sexp-killing-around)
      (define-key map (kbd "M-?") #'sp-convolute-sexp)
      ;; Barfage & Slurpage
      (define-key map (kbd "C-)") #'sp-forward-slurp-sexp)
      (define-key map (kbd "C-<right>") #'sp-forward-slurp-sexp)
      (define-key map (kbd "C-}") #'sp-forward-barf-sexp)
      (define-key map (kbd "C-<left>") #'sp-forward-barf-sexp)
      (define-key map (kbd "C-(") #'sp-backward-slurp-sexp)
      (define-key map (kbd "C-M-<left>") #'sp-backward-slurp-sexp)
      (define-key map (kbd "C-{") #'sp-backward-barf-sexp)
      (define-key map (kbd "C-M-<right>") #'sp-backward-barf-sexp)
      ;; Miscellaneous commands
      (define-key map (kbd "M-S") #'sp-split-sexp)
      (define-key map (kbd "M-J") #'sp-join-sexp)
      (define-key map (kbd "C-M-t") #'sp-transpose-sexp))

    (let ((map smartparens-strict-mode-map))
      (define-key map (kbd "M-q") #'sp-indent-defun))

    (setq sp-autoskip-closing-pair 'always
          ;; Don't kill entire symbol on C-k
          sp-hybrid-kill-entire-symbol nil)

    (show-smartparens-global-mode +1)

    ;; don't create a pair with single quote in minibuffer
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

    (sp-pair "{" nil :post-handlers
             '(:add (dotemacs-smartparens-pair-newline-and-indent "RET")))
    (sp-pair "[" nil :post-handlers
             '(:add (dotemacs-smartparens-pair-newline-and-indent "RET"))))
  :diminish (smartparens-mode " ⓟ" " p"))


;;; Highlights and fontification
(defun dotemacs-whitespace-style-no-long-lines ()
  "Configure `whitespace-mode' for Org.

Disable the highlighting of overlong lines."
  (setq-local whitespace-style (-difference whitespace-style
                                            '(lines lines-tail))))
;; Trailing whitespace
;; I don’t want to leave trailing whitespace in files I touch, so set
;; up a hook that automatically deletes trailing whitespace after
;; every line when saving a file:
; (add-hook 'write-file-hooks 'delete-trailing-whitespace)

(use-package whitespace                 ; Highlight bad whitespace
  :ensure t
  :bind (("C-c u w w" . whitespace-mode))
  :init
  (progn
    (after "evil-leader"
      (dotemacs-add-toggle whitespace
                           :status whitespace-mode
                           :on (whitespace-mode)
                           :off (whitespace-mode -1)
                           :documentation "Display whitespace."
                           :evil-leader "tw")
      (dotemacs-add-toggle whitespace-globally
                           :status global-whitespace-mode
                           :on (global-whitespace-mode)
                           :off (global-whitespace-mode -1)
                           :documentation "Globally display whitespace."
                           :evil-leader "t C-w"))

    (defun dotemacs-set-whitespace-style-for-diff ()
      "Whitespace configuration for `diff-mode'"
      (setq-local whitespace-style '(face
                                     tabs
                                     tab-mark
                                     spaces
                                     space-mark
                                     trailing
                                     indentation::space
                                     indentation::tab
                                     newline
                                     newline-mark)))

    (defun dotemacs-set-whitespace-style-for-others ()
      "Whitespace configuration for `prog-mode, `text-mode, `conf-mode'"
      ;; Highlight tabs, empty lines at beg/end, trailing whitespaces and overlong
      ;; portions of lines via faces.  Also indicate tabs via characters
      (setq-local whitespace-style '(face
                                     indentation
                                     space-after-tab
                                     space-before-tab
                                     tab-mark
                                     empty
                                     trailing
                                     lines-tail))

      ; Use `fill-column' for overlong lines
      (setq-local whitespace-line-column nil))

    (defun dotemacs-whitespace-mode-local ()
      "Enable `whitespace-mode' after local variables where set up."
      (add-hook 'hack-local-variables-hook #'whitespace-mode nil 'local))

    (add-hook 'diff-mode-hook #'whitespace-mode)
    (add-hook 'diff-mode-hook #'dotemacs-set-whitespace-style-for-diff)

    (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
      (progn
        (add-hook hook #'dotemacs-whitespace-mode-local)
        (add-hook hook #'dotemacs-set-whitespace-style-for-others))))
  :diminish ((whitespace-mode . " ⓦ")
             (global-whitespace-mode . " Ⓦ")))

(use-package hl-line                    ; Highlight the current line
  :init (global-hl-line-mode 1))

(use-package hl-anything ;; Highlight things at point, selections, enclosing parentheses
  :ensure t
  :defer t
  :init
  (progn
    (setq-default hl-highlight-save-file (concat dotemacs-cache-directory ".hl-save"))
    (after "evil-leader"
      (evil-leader/set-key
        "hc"  'hl-unhighlight-all-local
        "hgc" 'hl-unhighlight-all-global
        "hgh" 'hl-highlight-thingatpt-global
        "hh"  'hl-highlight-thingatpt-local
        "hn"  'hl-find-next-thing
        "hN"  'hl-find-prev-thing
        "hp"  'hl-paren-mode
        "hr"  'hl-restore-highlights
        "hs"  'hl-save-highlights)))
  :config
  (progn
    (dotemacs-hide-lighter hl-highlight-mode))
  :diminish (hl-paren-mode . " (Ⓗ)"))

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :defer t
  :init
  (progn
    (after "evil-leader"
      (evil-leader/set-key "tCd" 'rainbow-delimiters-mode))

    (when (eq dotemacs-highlight-delimiters 'all)
      (dolist (hook '(text-mode-hook prog-mode-hook))
        (add-hook hook #'rainbow-delimiters-mode)))))

(use-package hi-lock                    ; Custom regexp highlights
  :init (global-hi-lock-mode)
  :diminish hi-lock-mode)

(use-package highlight-numbers          ; Fontify number literals
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook #'highlight-numbers-mode)
    (add-hook 'asm-mode-hook (lambda () (highlight-numbers-mode -1)))))

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
(use-package init-auto-completions
  :load-path "config/")

(after "evil-leader"
  (dotemacs-add-toggle auto-completion
                       :status
                        (if (boundp 'auto-completion-front-end)
                            (if (eq 'company auto-completion-front-end)
                                company-mode
                              auto-complete-mode)
                          ;; default completion hardcoded to be company for now
                          (setq auto-completion-front-end 'company)
                          nil)
                        :on
                        (progn
                          (if (eq 'company auto-completion-front-end)
                              (company-mode)
                            (auto-complete-mode))
                          (message "Enabled auto-completion (using %S)."
                                   auto-completion-front-end))
                        :off
                        (progn
                          (if (eq 'company auto-completion-front-end)
                              (company-mode -1)
                            (auto-complete-mode -1))
                          (message "Disabled auto-completion."))
                        :documentation "Activate auto-completion."
                        :evil-leader "ta"))

;; In `completion-at-point', do not pop up silly completion buffers for less
;; than five candidates.  Cycle instead.
(setq completion-cycle-threshold 5)

;; tell emacs where to read abbrev
(setq abbrev-file-name (concat dotemacs-cache-directory "abbrev_defs"))

(use-package init-hippie-exp       ; Custom expansion functions
  :load-path "config/"
  :commands (dotemacs-try-complete-lisp-symbol-without-namespace))

(use-package hippie-exp                 ; Powerful expansion and completion
  :bind (([remap dabbrev-expand] . hippie-expand))
  :init
  (after "yasnippet"
    ;; Try to expand yasnippet snippets based on prefix
    (push 'yas-hippie-try-expand hippie-expand-try-functions-list))
  :config
  (progn
    ;; replace dabbrev-expand
    (global-set-key (kbd "M-/") 'hippie-expand)
    (define-key evil-insert-state-map (kbd "C-p") 'hippie-expand)
    (setq hippie-expand-try-functions-list '(
          ;; Try to expand word "dynamically", searching the current buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-list
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-line
          ;; Try to complete as an Emacs Lisp symbol, as many characters as
          ;; unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol
          dotemacs-try-complete-lisp-symbol-without-namespace))))

(use-package company                    ; Graphical (auto-)completion
  :ensure t
  :defer t
  :if (eq dotemacs-completion-engine 'company)
  :init
  (progn
    (setq company-idle-delay 0.2
          company-tooltip-align-annotations t
          ; company-tooltip-limit 10
          company-minimum-prefix-length 2
          ;; invert the navigation direction if the the completion popup-isearch-match
          ;; is displayed on top (happens near the bottom of windows)
          ; company-tooltip-flip-when-above t
          company-require-match nil
          ; company-dabbrev-code-ignore-case t
          ; company-dabbrev-code-everywhere t
          company-show-numbers t ;; Easy navigation to candidates with M-<n>
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil
          company-frontends '(company-pseudo-tooltip-frontend)
          company-clang-prefix-guesser 'company-mode/more-than-prefix-guesser)
          (defvar-local company-fci-mode-on-p nil)

    (defun company-turn-off-fci (&rest ignore)
      (when (boundp 'fci-mode)
        (setq company-fci-mode-on-p fci-mode)
        (when fci-mode (fci-mode -1))))

    (defun company-maybe-turn-on-fci (&rest ignore)
      (when company-fci-mode-on-p (fci-mode 1)))

    (add-hook 'company-completion-started-hook 'company-turn-off-fci)
    (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
    (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))
  :config
  (progn
    ;; key bindings
    (defun dotemacs-company-complete-common-or-cycle-backward ()
      "Complete common prefix or cycle backward."
      (interactive)
      (company-complete-common-or-cycle -1))

    (dotemacs-auto-completion-set-RET-key-behavior 'company)
    (dotemacs-auto-completion-set-TAB-key-behavior 'company)
    (dotemacs-auto-completion-setup-key-sequence 'company)
    (let ((map company-active-map))
      (define-key map (kbd "C-/") 'company-search-candidates)
      (define-key map (kbd "C-M-/") 'company-filter-candidates)
      (define-key map (kbd "C-d") 'company-show-doc-buffer)
      (define-key map (kbd "C-j") 'company-select-next)
      (define-key map (kbd "C-k") 'company-select-previous)
      (define-key map (kbd "C-l") 'company-complete-selection))

    ;; Nicer looking faces
    (custom-set-faces
     '(company-tooltip-common
       ((t (:inherit company-tooltip :weight bold :underline nil))))
     '(company-tooltip-common-selection
       ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

    ;; Transformers
    (defun dotemacs-company-transformer-cancel (candidates)
      "Cancel completion if prefix is in the list
`company-mode-completion-cancel-keywords'"
      (unless (member company-prefix company-mode-completion-cancel-keywords)
        candidates))
    (setq company-transformers '(dotemacs-company-transformer-cancel
                                   company-sort-by-occurrence)))
  :diminish (company-mode " ⓐ" " a"))

(use-package company-statistics
  :ensure t
  :if (eq dotemacs-completion-engine 'company)
  :defer t
  :init
  (progn
    (setq company-statistics-file (concat dotemacs-cache-directory
                                          "company-statistics-cache.el"))
    (add-hook 'company-mode-hook 'company-statistics-mode)))

(use-package company-quickhelp
  :defer t
  :disabled t
  :if (and auto-completion-enable-help-tooltip
           (not (version< emacs-version "24.4"))  ;; company-quickhelp from MELPA
                                                  ;; is not compatible with 24.3 anymore
           (eq dotemacs-completion-engine 'company)
           (display-graphic-p))
  :init (add-hook 'company-mode-hook 'company-quickhelp-mode))

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

(use-package auto-complete
  :ensure t
  :if (eq dotemacs-completion-engine 'auto-complete)
  :diminish auto-complete-mode)

(use-package auto-complete
  :ensure t
  :if (eq dotemacs-completion-engine 'auto-complete)
  :defer t
  :init
  (setq ac-auto-start 0
        ac-delay 0.2
        ac-quick-help-delay 1.
        ac-use-fuzzy t
        ; ac-auto-show-menu t
        ; ac-quick-help-height 30
        ; ac-show-menu-immediately-on-auto-complete t
        ; completion-ignored-extensions '(".xpt" ".a" ".so" ".o" ".d" ".elc" ".class" "~" ".ckp" ".bak" ".imp" ".lpt" ".bin" ".otl" ".err" ".lib" ".aux" ".elf" )
        ac-fuzzy-enable t
        ac-comphist-file (concat dotemacs-cache-directory "ac-comphist.dat")
        ;; use 'complete when auto-complete is disabled
        tab-always-indent 'complete
        ac-dwim t)
  :config
  (progn
    (require 'auto-complete-config)
    (setq-default ac-sources '(ac-source-abbrev
                               ac-source-dictionary
                               ac-source-words-in-same-mode-buffers))
    (after "yasnippet"
      (push 'ac-source-yasnippet ac-sources))

    (add-to-list 'completion-styles 'initials t)
    (define-key ac-completing-map (kbd "C-j") 'ac-next)
    (define-key ac-completing-map (kbd "C-k") 'ac-previous)
    (define-key ac-completing-map (kbd "<S-tab>") 'ac-previous))
  :diminish (auto-complete-mode " ⓐ" " a"))

(use-package ac-ispell
  :ensure t
  :defer t
  :if (eq dotemacs-completion-engine 'auto-complete)
  :init
  (progn
    (setq ac-ispell-requires 4)
    (eval-after-load 'auto-complete
      '(ac-ispell-setup))
    ))

(use-package init-yasnippet
  :load-path "config/"
  :defer t
  :commands (dotemacs-load-yasnippet
             dotemacs-auto-yasnippet-expand
             dotemacs-force-yasnippet-off))

(use-package yasnippet
  :ensure t
  :commands yas-global-mode
  :init
  (progn
    ;; disable yas minor mode map, use hippie-expand instead
    (setq yas-minor-mode-map (make-sparse-keymap)
          ;; allow nested expansions
          yas-triggers-in-field t
          ;; add key into candidate list
          helm-yas-display-key-on-candidate t)

    ;; this makes it easy to get out of a nested expansion
    (define-key yas-minor-mode-map
      (kbd "M-s-/") 'yas-next-field)

    (add-to-hooks 'dotemacs-load-yasnippet '(prog-mode-hook
                                             markdown-mode-hook
                                             org-mode-hook))

    (after "evil-leader"
      (dotemacs-add-toggle yasnippet
                           :status yas-minor-mode
                           :on (yas-minor-mode)
                           :off (yas-minor-mode -1)
                           :documentation "Enable yasnippet."
                           :evil-leader "ty"))

    (add-to-hooks 'dotemacs-force-yasnippet-off '(term-mode-hook
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
                                               (smartparens-mode 1)))))
  :diminish (yas-minor-mode . " ⓨ" ))

(use-package auto-yasnippet
  :ensure t
  :defer t
  :init
  (progn
    (setq aya-persist-snippets-dir (concat dotemacs-private-dir "snippets/"))
    (after "evil-leader"
      (evil-leader/set-key
        "iSc" 'aya-create
        "iSe" 'dotemacs-auto-yasnippet-expand
        "iSw" 'aya-persist-snippet))))

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
    (after "evil-leader"
      (evil-leader/set-key "is" 'dotemacs-helm-yas))
    (setq helm-c-yas-space-match-any-greedy t)))


;;; Spelling and syntax checking

;; Command Prefixes
(after "evil-leader"
  (dotemacs-declare-prefix "S" "spelling"))

(use-package ispell                     ; Spell checking
  :defer t
  :config
  (progn
    (setq ispell-program-name
          (or (executable-find "aspell")
              (executable-find "hunspell"))

          ; ispell-extra-args '("--sug-mode=ultra")
          ispell-dictionary "en_US"     ; Default dictionnary
          ispell-silently-savep t       ; Don't ask when saving the private dict
          ;; Increase the height of the choices window to take our header line
          ;; into account.
          ispell-choices-win-default-height 5)

    (unless ispell-program-name
      (warn "No spell checker available. Install Hunspell or ASpell."))))

(use-package flyspell                   ; On-the-fly spell checking
  :bind (("C-c u f s" . flyspell-mode))
  :defer t
  :init
  (progn
    (dolist (hook '(markdown-mode-hook text-mode-hook message-mode-hook org-mode-hook))
      (add-hook hook '(lambda () (flyspell-mode 1))))
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)

    (setq flyspell-use-meta-tab nil
          flyspell-issue-welcome-flag nil  ;; Make Flyspell less chatty
          flyspell-issue-message-flag nil)

    (after "evil-leader"
      (dotemacs-add-toggle spelling-checking
                           :status flyspell-mode
                           :on (flyspell-mode)
                           :off (flyspell-mode -1)
                           :documentation
                           "Enable flyspell for automatic spelling checking."
                           :evil-leader "tS")))
  :config
  (progn
    ;; Free C-M-i for completion
    (define-key flyspell-mode-map "\M-\t" nil)
    ;; Undefine mouse buttons which get in the way
    (define-key flyspell-mouse-map [down-mouse-2] nil)
    (define-key flyspell-mouse-map [mouse-2] nil)
    (flyspell-prog-mode))
  :diminish (flyspell-mode . " Ⓢ"))

(use-package init-flycheck         ; Personal Flycheck helpers
  :load-path "config/"
  :defer t
  :commands (dotemacs-discard-undesired-html-tidy-error
             dotemacs-flycheck-mode-line-status
             dotemacs-defface-flycheck-mode-line-color
             dotemacs-set-flycheck-mode-line-faces
             dotemacs-mode-line-flycheck-info-toggle))

(use-package flycheck                   ; On-the-fly syntax checking
  :ensure t
  :bind (("C-c l e" . list-flycheck-errors)
         ("C-c u f c" . flycheck-mode))
  :defer t
  :init
  (progn
    (setq flycheck-check-syntax-automatically '(save mode-enabled)
          flycheck-standard-error-navigation nil)

    ;; Each buffer gets its own idle-change-delay because of the
    ;; buffer-sensitive adjustment above.
    (make-variable-buffer-local 'flycheck-idle-change-delay)

    (add-hook 'flycheck-after-syntax-check-hook
              #'dotemacs-adjust-flycheck-automatic-syntax-eagerness)

    (after "evil-leader"
      (dotemacs-add-toggle syntax-checking
                           :status flycheck-mode
                           :on (flycheck-mode)
                           :off (flycheck-mode -1)
                           :documentation "Enable error and syntax checking."
                           :evil-leader "ts")))
  :config
  (progn
    (after "evil-leader"
      (evil-leader/set-key
        "ec" 'flycheck-clear
        "el" 'flycheck-list-errors
        "tmf" 'dotemacs-mode-line-flycheck-info-toggle))

    (dotemacs-set-flycheck-mode-line-faces)
    (setq flycheck-display-errors-function
          #'flycheck-display-error-messages-unless-error-list
          flycheck-mode-line
          '(:eval (dotemacs-flycheck-mode-line-status)))

    ;; Don't highlight undesired errors from html tidy
    (add-hook 'flycheck-process-error-functions
              #'dotemacs-discard-undesired-html-tidy-error)

    ;; Use italic face for checker name
    (set-face-attribute 'flycheck-error-list-checker-name nil
                        :inherit 'italic)

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Flycheck errors*" eos)
                    (display-buffer-reuse-window
                      display-buffer-in-side-window)
                    (side            . bottom)
                    (reusable-frames . visible)
                    (window-height   . 0.4)))

    ;; Custom fringe indicator
    (when (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'my-flycheck-fringe-indicator
        (vector #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00011100
                #b00111110
                #b00111110
                #b00111110
                #b00011100
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b01111111)))

    (flycheck-define-error-level 'error
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-error)
    (flycheck-define-error-level 'warning
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-warning)
    (flycheck-define-error-level 'info
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-info))
  :diminish (flycheck-mode . " ⓢ"))

(use-package flycheck-pos-tip
  :ensure t
; ;; flycheck errors on a tooltip (doesnt work on console)
  :if (and dotemacs-s-syntax-checking-enable-tooltips (display-graphic-p))
  :defer t
  :init
  (setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages))

(use-package helm-flycheck
  :ensure t
  :bind (("C-c ! L" . helm-flycheck)))


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
  :mode (("\\.mustache$" . mustache-mode)))

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
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :defer t
  :commands(dotemacs-preview-md-file)
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
    ;; Highlight and allow to open http link at point in programming buffers
    ;; goto-address-prog-mode only highlights links in strings and comments
    (add-hook 'prog-mode-hook 'goto-address-prog-mode)

    (setq dotemacs-prog-mode-hook #'dotemacs-prog-mode-defaults)
    (add-hook 'prog-mode-hook
              (lambda ()
                (run-hooks #'dotemacs-prog-mode-hook)))
    ))

(use-package prog-mode                  ; Prog Mode
  :bind (("C-c u p" . prettify-symbols-mode))
  :init
  (add-hook 'js2-mode-hook
            (lambda ()
              (push '("function" . 955) prettify-symbols-alist)
              (push '("return" . 8592) prettify-symbols-alist))))

; http://stackoverflow.com/questions/1085170/how-to-achieve-code-folding-effects-in-emacs
(use-package hs-minor-mode
  :defer t
  :init (progn
    ;; required for evil folding
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


;;; Evil
(use-package init-evil
  :load-path "config/")

(use-package evil
  :ensure t
  :defer t
  :init
  (progn
    ;; put back refresh of the cursor on post-command-hook see status of:
    ;; https://bitbucket.org/lyro/evil/issue/502/cursor-is-not-refreshed-in-some-cases
    (add-hook 'post-command-hook 'evil-refresh-cursor)

    ; (add-hook 'after-change-major-mode-hook #'dotemacs-major-mode-evil-state-adjust)

    ; (after "paren"
    ;   ;; the default behavior only highlights with the point one-after the closing paren
    ;   ;; this changes it such it will match with the point on the closing paren
    ;   (defadvice show-paren-function (around show-paren-closing-before activate)
    ;     (if (and (or
    ;               (evil-normal-state-p)
    ;               (evil-visual-state-p))
    ;              (eq (syntax-class (syntax-after (point))) 5))
    ;         (save-excursion
    ;           (forward-char)
    ;           ad-do-it)
    ;       ad-do-it)))

    (dotemacs-set-state-faces)

    (set-default-evil-emacs-state-cursor)
    (set-default-evil-evilified-state-cursor)
    (set-default-evil-normal-state-cursor)
    (set-default-evil-insert-state-cursor)
    (set-default-evil-visual-state-cursor)
    (set-default-evil-motion-state-cursor)
    (set-default-evil-lisp-state-cursor)
    (set-default-evil-iedit-state-cursor)
    (set-default-evil-iedit-insert-state-cursor)
    (set-default-evil-replace-state-cursor)
    (set-default-evil-operator-state-cursor)

    ; Don't move back the cursor one position when exiting insert mode
    (setq evil-move-cursor-back nil)
    ; (setq evil-search-module 'evil-search)
    ; (setq evil-magic 'very-magic)

    (evil-mode 1)

    (after "evil-leader"
      (use-package evil-evilified-state
        :load-path "extensions/")))
  :config
  (progn
    ; c-k/c-j for page down/up
    ;
    ; One thing that surprised me considering how complete Evil is, is the lack
    ; of Vim's Control-d/Control-u for page down/up. Probably because C-u is
    ; pretty important in Emacs (it's the shortcut to give a numeric parameter to
    ; other commands). I've in fact these mapped on my .vimrc to c-k/c-j
    ; (because I think they're more consistent with Vim's j/k movement keys) so
    ; that's how I mapped them in Emacs:
    (define-key evil-normal-state-map (kbd "C-k") 'evil-scroll-up)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-scroll-down)

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

    ;; evil ex-command key
    (define-key evil-normal-state-map (kbd dotemacs-command-key) 'evil-ex)
    (define-key evil-visual-state-map (kbd dotemacs-command-key) 'evil-ex)
    (define-key evil-motion-state-map (kbd dotemacs-command-key) 'evil-ex)
    ;; Make the current definition and/or comment visible.
    (define-key evil-normal-state-map "zf" 'reposition-window)
    ;; toggle maximize buffer
    (define-key evil-window-map (kbd "o") 'toggle-maximize-buffer)
    (define-key evil-window-map (kbd "C-o") 'toggle-maximize-buffer)

    (after "evil-leader"
      (dotemacs-define-micro-state scroll
        :doc "[k] page up [j] page down [K] half page up [J] half page down"
        :execute-binding-on-enter t
        :evil-leader "nn" "np" "nP" "nN"
        :bindings
        ;; page
        ("k" evil-scroll-page-up)
        ("j" evil-scroll-page-down)
        ;; half page
        ("K" dotemacs-scroll-half-page-up)
        ("J" dotemacs-scroll-half-page-down))

      (evil-leader/set-key "re" 'evil-show-registers))

    (unless dotemacs-enable-paste-micro-state
      (ad-disable-advice 'evil-paste-before 'after
                         'evil-paste-before-paste-micro-state)
      (ad-activate 'evil-paste-before)
      (ad-disable-advice 'evil-paste-after 'after
                         'evil-paste-after-paste-micro-state)
      (ad-activate 'evil-paste-after)
      (ad-disable-advice 'evil-visual-paste 'after
                         'evil-visual-paste-paste-micro-state)
      (ad-activate 'evil-visual-paste))

    ;; butter fingers
    (evil-ex-define-cmd "Q" 'evil-quit)
    (evil-ex-define-cmd "Qa" 'evil-quit-all)
    (evil-ex-define-cmd "QA" 'evil-quit-all)

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

    (define-key evil-normal-state-map (kbd "K") 'dotemacs-evil-smart-doc-lookup)

    (define-key evil-normal-state-map
      (kbd "gd") 'dotemacs-evil-smart-goto-definition)

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
           (push (cons (string-to-char ,key)
                       (if ,end
                           (cons ,start ,end)
                         ,start))
                 evil-surround-pairs-alist))))

    (add-to-hook 'prog-mode-hook '(dotemacs-standard-text-objects))

    ;; support smart-parens-strict-mode
    (after "smartparens"
      (defadvice evil-delete-backward-char-and-join
          (around dotemacs-evil-delete-backward-char-and-join activate)
        (if smartparens-strict-mode
            (call-interactively 'sp-backward-delete-char)
          ad-do-it)))))

(use-package evil-args
  :ensure t
  :init
  (progn
    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)))

(use-package evil-escape
  :ensure t
  :init (evil-escape-mode)
  :diminish evil-escape-mode)

(use-package evil-exchange
  :ensure t
  :init (evil-exchange-install))

(use-package evil-iedit-state
  :ensure t
  :init
  (progn
    (require 'evil-iedit-state)
    (defun dotemacs-evil-state-lazy-loading ()
      ;; activate leader in iedit and iedit-insert states
      (define-key evil-iedit-state-map
        (kbd evil-leader/leader) evil-leader--default-map))

    (after "evil-leader"
      (evil-leader/set-key "se" 'evil-iedit-state/iedit-mode)
      (add-hook 'find-file-hook #'dotemacs-evil-state-lazy-loading))))

(use-package evil-jumper
  :ensure t
  :init
  (progn
    (setq evil-jumper-file (concat dotemacs-cache-directory "evil-jumps")
          evil-jumper-auto-center t
          evil-jumper-auto-save-interval 3600)
    (evil-jumper-mode t)))

(use-package evil-leader
  :ensure t
  :defer t
  :init
  (progn
    (setq evil-leader/leader dotemacs-leader-key)
    (global-evil-leader-mode))
  :config
  (progn
    ;; Unset shortcuts which shadow evil leader
    (eval-after-load "compile"
      '(progn
         (define-key compilation-mode-map (kbd "h") nil)))
    ;; make leader available in visual and motion states
    (mapc (lambda (s)
            (eval `(define-key
                     ,(intern (format "evil-%S-state-map" s))
                     ,(kbd dotemacs-leader-key)
                     evil-leader--default-map)))
          '(motion visual))
    ;; emacs and insert states (make it also available in other states
    ;; for consistency and POLA.)
    (mapc (lambda (s)
            (eval `(define-key
                     ,(intern (format "evil-%S-state-map" s))
                     ,(kbd dotemacs-emacs-leader-key)
                     evil-leader--default-map)))
          '(emacs insert normal visual motion))
    ;; experimental: map `<leader> m` to `dotemacs-major-mode-leader-key`
    (when dotemacs-major-mode-leader-key
      (add-hook 'after-change-major-mode-hook
                'dotemacs-activate-major-mode-leader))))

(use-package evil-lisp-state
  :ensure t
  :init
  (progn
    (setq evil-lisp-state-global t)
    (setq evil-lisp-state-leader-prefix "k")))

(use-package evil-commentary
  :ensure t
  :defer t
  :init (evil-commentary-mode))

(use-package evil-nerd-commenter
  :disabled t
  :ensure t
  :commands (evilnc-comment-operator
             evilnc-comment-or-uncomment-lines
             evilnc-toggle-invert-comment-line-by-line
             evilnc-comment-or-uncomment-paragraphs
             evilnc-quick-comment-or-uncomment-to-the-line
             evilnc-copy-and-comment-lines)
  :init
  (progn
    (after "evil-leader"
      (evil-leader/set-key
        ";"  'evilnc-comment-operator
        "cl" 'evilnc-comment-or-uncomment-lines
        "ci" 'evilnc-toggle-invert-comment-line-by-line
        "cp" 'evilnc-comment-or-uncomment-paragraphs
        "ct" 'evilnc-quick-comment-or-uncomment-to-the-line
        "cy" 'evilnc-copy-and-comment-lines))))

(use-package evil-matchit
  :ensure t
  :defer t
  :init (global-evil-matchit-mode))

(use-package evil-indent-textobject
  :ensure t
  :defer t)

(use-package evil-easymotion
  :ensure t
  :defer t
  :init (after "evil"))

(use-package evil-numbers
  :ensure t
  :defer t
  :config
  (progn
    (defun dotemacs-evil-numbers-micro-state-doc ()
      "Display a short documentation in the mini buffer."
      (echo "+/= to increase the value or - to decrease it"))

    (defun dotemacs-evil-numbers-micro-state-overlay-map ()
      "Set a temporary overlay map to easily increase or decrease a number"
      (set-temporary-overlay-map
       (let ((map (make-sparse-keymap)))
         (define-key map (kbd "+") 'dotemacs-evil-numbers-increase)
         (define-key map (kbd "=") 'dotemacs-evil-numbers-increase)
         (define-key map (kbd "-") 'dotemacs-evil-numbers-decrease)
         map) t)
      (dotemacs-evil-numbers-micro-state-doc))

    (defun dotemacs-evil-numbers-increase (amount &optional no-region)
      "Increase number at point."
      (interactive "p*")
      (evil-numbers/inc-at-pt amount no-region)
      (dotemacs-evil-numbers-micro-state-overlay-map))
    (defun dotemacs-evil-numbers-decrease (amount)
      "Decrease number at point."
      (interactive "p*")
      (evil-numbers/dec-at-pt amount)
      (dotemacs-evil-numbers-micro-state-overlay-map))

    (after "evil-leader"
      (evil-leader/set-key "n+" 'dotemacs-evil-numbers-increase)
      (evil-leader/set-key "n=" 'dotemacs-evil-numbers-increase)
      (evil-leader/set-key "n-" 'dotemacs-evil-numbers-decrease))))

(use-package evil-search-highlight-persist
  :ensure t
  :init
  (progn
    (global-evil-search-highlight-persist)
    (after "evil-leader"
      (evil-leader/set-key "/" 'evil-search-highlight-persist-remove-all)) ;; sc
    (define-key evil-search-highlight-persist-map (kbd "C-x SPC") 'rectangle-mark-mode)

    (evil-ex-define-cmd "nohl[search]" 'dotemacs-turn-off-search-highlight-persist)
    (evil-ex-define-cmd "hl[search]" 'dotemacs-turn-on-search-highlight-persist)
    (define-key evil-ex-map "nohl" 'dotemacs-turn-off-search-highlight-persist)
    (define-key evil-ex-map "hl" 'dotemacs-turn-on-search-highlight-persist)

    (defun dotemacs-adaptive-evil-highlight-persist-face ()
      (set-face-attribute 'evil-search-highlight-persist-highlight-face nil
                          :inherit 'region
                          :background nil
                          :foreground nil))
    (dotemacs-adaptive-evil-highlight-persist-face)))

(use-package evil-surround
  :ensure t
  :defer t
  :init
  (progn
    (global-evil-surround-mode 1)
    ;; `s' for surround instead of `substitute'
    ;; see motivation for this change in the documentation
    (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
    (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)))

(use-package evil-terminal-cursor-changer
  :if (not (display-graphic-p))
  :ensure t
  :init
  (progn
    (require 'evil-terminal-cursor-changer)
    (setq evil-visual-state-cursor 'box ; █
          evil-insert-state-cursor 'bar ; ⎸
          evil-emacs-state-cursor 'hbar)) ; _
  :defer t)

(use-package evil-tutor
  :disabled t
  :ensure t
  :defer t
  :commands (evil-tutor-start
             evil-tutor-resume)
  :init
  (progn
    (setq evil-tutor-working-directory
          (concat dotemacs-cache-directory ".tutor/"))
    (evil-leader/set-key "hT" 'evil-tutor-start)))

(use-package evil-visualstar
  :ensure t
  :defer t
  :commands (evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (progn
    ; (global-evil-visualstar-mode)
    (define-key evil-visual-state-map (kbd "*")
      'evil-visualstar/begin-search-forward)
    (define-key evil-visual-state-map (kbd "#")
      'evil-visualstar/begin-search-backward)))


;;; Emacs Lisp
(bind-key "C-c u d" #'toggle-debug-on-error)

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
    (after "evil-leader"
      (evil-leader/set-key-for-mode 'emacs-lisp-mode
        "mgg" 'elisp-slime-nav-find-elisp-thing-at-point
        "mhh" 'elisp-slime-nav-describe-elisp-thing-at-point))
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
  :ensure t
  :defer t
  :commands rxt-fontify-regexp-at-point
  :init
  (progn
    (after "evil-leader"
      (dotemacs-declare-prefix "R" "pcre2el")
      (evil-leader/set-key
        "R/"  'rxt-explain
        "Rc"  'rxt-convert-syntax
        "Rx"  'rxt-convert-to-rx
        "R'"  'rxt-convert-to-strings
        "Rpe" 'rxt-pcre-to-elisp
        "R%"  'pcre-query-replace-regexp
        "Rpx" 'rxt-pcre-to-rx
        "Rps" 'rxt-pcre-to-sre
        "Rp'" 'rxt-pcre-to-strings
        "Rp/" 'rxt-explain-pcre
        "Re/" 'rxt-explain-elisp
        "Rep" 'rxt-elisp-to-pcre
        "Rex" 'rxt-elisp-to-rx
        "Res" 'rxt-elisp-to-sre
        "Re'" 'rxt-elisp-to-strings
        "Ret" 'rxt-toggle-elisp-rx
        "Rt"  'rxt-toggle-elisp-rx
        "Rh"  'rxt-fontify-regexp-at-point))))

(use-package macrostep                  ; Interactively expand macros in code
  :ensure t
  :defer t
  :init (after "lisp-mode"
          (bind-key "C-c e e" #'macrostep-expand emacs-lisp-mode-map)
          (bind-key "C-c e e" #'macrostep-expand lisp-interaction-mode-map)))

(use-package ielm                       ; Emacs Lisp REPL
  :bind (("C-c z" . ielm)))

(use-package elisp-mode                  ; Emacs Lisp editing
  :defer t
  :interpreter ("emacs" . emacs-lisp-mode)
  :mode ("/Cask\\'" . emacs-lisp-mode)
  :config (require 'ert))

(use-package init-elisp             ; Personal tools for Emacs Lisp
  :load-path "config/"
  :commands (dotemacs-elisp-find-cask-file
             dotemacs-add-use-package-to-imenu)
  :init (progn
          (add-hook 'emacs-lisp-mode-hook #'dotemacs-add-use-package-to-imenu)

          (after "lisp-mode"
            (bind-key "C-c f c" #'dotemacs-elisp-find-cask-file
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



;;; Clojure
(dotemacs-defvar-company-backends cider-mode)
(dotemacs-defvar-company-backends cider-repl-mode)

(use-package init-clojure
  :load-path "config/")

(use-package align-cljlet
  :defer t
  :ensure t
  :init
  (add-hook 'clojure-mode-hook (lambda () (require 'align-cljlet)))
  :config
  (evil-leader/set-key-for-mode 'clojure-mode
    "mfl" 'align-cljlet))

(use-package cider
  :defer t
  :ensure t
  :init
  (progn
    (setq cider-stacktrace-default-filters '(tooling dup)
          cider-repl-pop-to-buffer-on-connect nil
          cider-prompt-save-file-on-load nil
          cider-repl-use-clojure-font-lock t)
    (push "\\*cider-repl\.\+\\*" dotemacs-useful-buffers-regexp)
    (add-hook 'clojure-mode-hook 'cider-mode)
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (if dotemacs-smartparens-strict-mode
        (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)))
  :config
  (progn
    ;; add support for evil
    (push 'cider-stacktrace-mode evil-motion-state-modes)
    (push 'cider-popup-buffer-mode evil-motion-state-modes)

    (evilify cider-stacktrace-mode cider-stacktrace-mode-map)

    ;; open cider-doc directly and close it with q
    (setq cider-prompt-for-symbol nil)
    (evilify cider-docview-mode cider-docview-mode-map
             (kbd "q") 'cider-popup-buffer-quit)

    (evil-leader/set-key-for-mode 'clojure-mode
      "mhh" 'cider-doc
      "mhg" 'cider-grimoire
      "mhj" 'cider-javadoc

      "meb" 'cider-eval-buffer
      "mee" 'cider-eval-last-sexp
      "mef" 'cider-eval-defun-at-point
      "mer" 'cider-eval-region
      "mew" 'cider-eval-last-sexp-and-replace

      "mgb" 'cider-jump-back
      "mge" 'cider-jump-to-compilation-error
      "mgg" 'cider-jump-to-var
      "mgr" 'cider-jump-to-resource

      "msb" 'cider-load-buffer
      "msB" 'dotemacs-cider-send-buffer-in-repl-and-focus
      "msc" 'cider-connect
      "mse" 'dotemacs-cider-send-last-sexp-to-repl
      "msE" 'dotemacs-cider-send-last-sexp-to-repl-focus
      "msf" 'dotemacs-cider-send-function-to-repl
      "msF" 'dotemacs-cider-send-function-to-repl-focus
      "msi" 'cider-jack-in
      "msn" 'dotemacs-cider-send-ns-form-to-repl
      "msN" 'dotemacs-cider-send-ns-form-to-repl-focus
      "msq" 'cider-quit
      "msr" 'dotemacs-cider-send-region-to-repl
      "msR" 'dotemacs-cider-send-region-to-repl-focus
      "mss" 'cider-switch-to-repl-buffer

      "mta" 'dotemacs-cider-test-run-all-tests
      "mtr" 'dotemacs-cider-test-rerun-tests
      "mtt" 'dotemacs-cider-test-run-focused-test)
    (when dotemacs-clojure-enable-fancify-symbols
      (dotemacs-clojure-fancify-symbols 'cider-repl-mode)))

  (defadvice cider-jump-to-var (before add-evil-jump activate)
    (evil-set-jump)))

(eval-after-load 'eval-sexp-fu
  '(require 'cider-eval-sexp-fu))

(use-package clj-refactor
  :defer t
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  :config
  (progn
    (cljr-add-keybindings-with-prefix "C-c C-f")
    ;; not supported for now
    ;; (dotemacs-declare-prefix "mr" "clj-refactor")
    (evil-leader/set-key-for-mode 'clojure-mode
      "mrad" 'cljr-add-declaration
      "mrai" 'cljr-add-import-to-ns
      "mram" 'cljr-add-missing-libspec
      "mrap" 'cljr-add-project-dependency
      "mrar" 'cljr-add-require-to-ns
      "mrau" 'cljr-add-use-to-ns
      "mrcc" 'cljr-cycle-coll
      "mrci" 'cljr-cycle-if
      "mrcn" 'cljr-clean-ns
      "mrcp" 'cljr-cycle-privacy
      "mrdk" 'cljr-destructure-keys
      "mref" 'cljr-extract-function
      "mrel" 'cljr-expand-let
      "mrfu" 'cljr-find-usages
      "mrhd" 'cljr-hotload-dependency
      "mril" 'cljr-introduce-let
      "mrmf" 'cljr-move-form
      "mrml" 'cljr-move-to-let
      "mrpc" 'cljr-project-clean
      "mrpf" 'cljr-promote-function
      "mrrd" 'cljr-remove-debug-fns
      "mrrf" 'cljr-rename-file
      "mrrl" 'cljr-remove-let
      "mrrr" 'cljr-remove-unused-requires
      "mrrs" 'cljr-rename-symbol
      "mrru" 'cljr-replace-use
      "mrsn" 'cljr-sort-ns
      "mrsp" 'cljr-sort-project-dependencies
      "mrsr" 'cljr-stop-referring
      "mrtf" 'cljr-thread-first-all
      "mrth" 'cljr-thread
      "mrtl" 'cljr-thread-last-all
      "mrua" 'cljr-unwind-all
      "mruw" 'cljr-unwind)))

(use-package clojure-mode
  :defer t
  :ensure t
  :config
  (progn
    (when dotemacs-clojure-enable-fancify-symbols
      (dotemacs-clojure-fancify-symbols 'clojure-mode))))

(add-hook 'cider-mode-hook 'rainbow-delimiters-mode)

(unless (version< emacs-version "24.4")
  (add-hook 'cider-mode-hook 'subword-mode))

(when (eq dotemacs-completion-engine 'company)
  (after "company"
    (push 'company-capf company-backends-cider-mode)
    (dotemacs-add-company-hook cider-mode)

    (push 'company-capf company-backends-cider-repl-mode)
    (dotemacs-add-company-hook cider-repl-mode)))


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

;; Thanks to [[https://github.com/skeeto/impatient-mode][impatient-mode]] you
;; can see the effect of your HTML as you type it.
(use-package impatient-mode
  :ensure t
  :if window-system
  :defer t
  :init
  (progn
    ; (add-hook 'web-mode-hook 'impatient-mode)
    ))

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
    (evil-define-key 'insert emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
    (evil-define-key 'insert emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
    (evil-define-key 'emacs emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
    (evil-define-key 'emacs emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
    ))

(use-package web-mode                   ; Template editing
  :ensure t
  :defer t
  :mode "/templates?/.*\\.\\(php\\|html\\)\\'"
  :config
  (setq web-mode-markup-indent-offset 2))

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

;;; JavaScript
(use-package init-js
  :load-path "config/"
  :defer t
  :commands(dotemacs-js-ctrl-c-ctrl-c
            dotemacs-hide-test-functions
            dotemacs-tab-properly
            dotemacs-fetch-autolint-externs
            dotemacs-js-jump-to
            dotemacs-js-format-impl-name
            dotemacs-js-format-test-name
            dotemacs-js-jump-to-implementation-or-test))

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

;; magit
(use-package init-magit
  :load-path "config/"
  :defer t)

(use-package magit                      ; The one and only Git frontend
  :defer t
  :ensure t
  :bind (("C-c g"   . magit-status)
         ("C-c v g" . magit-status)
         ("C-c v v" . magit-status)
         ("C-c v g" . magit-blame-mode)
         ("C-c v l" . magit-file-log))
  :init
  (progn
    ; (after "evil-leader" (progn
    ;   ;; Set the initial evil state that certain major modes will be in.
    ;   (evil-set-initial-state 'magit-log-edit-mode 'emacs)
    ; ))

    ;; Seriously, Magit?! Set this variable before Magit is loaded to silence the
    ;; most stupid warning ever
    (setq magit-last-seen-setup-instructions "1.4.0"
          magit-completing-read-function 'magit-ido-completing-read
          magit-save-some-buffers 'dontask
          magit-stage-all-confirm nil
          magit-unstage-all-confirm nil
          magit-show-child-count t
          ;; Except when you ask something useful…
          magit-set-upstream-on-push t)

    (add-hook 'git-commit-mode-hook 'fci-mode)
    ;; must enable auto-fill-mode again because somehow fci-mode disable it
    (add-hook 'git-commit-mode-hook 'auto-fill-mode)
    ;; On Windows, we must use Git GUI to enter username and password
    ;; See: https://github.com/magit/magit/wiki/FAQ#windows-cannot-push-via-https
    (when (eq window-system 'w32)
      (setenv "GIT_ASKPASS" "git-gui--askpass"))

    (defun dotemacs-magit-diff-head ()
      "Execute `magit-diff' against current HEAD."
      (interactive)
      (magit-diff "HEAD"))

    (after "evil-leader"
      (evil-leader/set-key
        "gb" 'magit-blame-mode
        "gl" 'magit-log
        "gs" 'magit-status
        "gd" 'dotemacs-magit-diff-head
        "gC" 'magit-commit))

    (after "evil-evilified-state"
      (evilify magit-commit-mode magit-commit-mode-map
             (kbd "C-j") 'magit-goto-next-section
             (kbd "C-k") 'magit-goto-previous-section
             (kbd "C-n") 'magit-goto-next-section
             (kbd "C-p") 'magit-goto-previous-section
             (kbd "C-v") 'magit-revert-item)
      (evilify magit-log-mode magit-log-mode-map
             (kbd "C-j") 'magit-goto-next-section
             (kbd "C-k") 'magit-goto-previous-section
             (kbd "C-n") 'magit-goto-next-section
             (kbd "C-p") 'magit-goto-previous-section
             (kbd "C-v") 'magit-revert-item)
      (evilify magit-process-mode magit-process-mode-map
             (kbd "C-j") 'magit-goto-next-section
             (kbd "C-k") 'magit-goto-previous-section
             (kbd "C-n") 'magit-goto-next-section
             (kbd "C-p") 'magit-goto-previous-section
             (kbd "C-v") 'magit-revert-item)
      (evilify magit-branch-manager-mode magit-branch-manager-mode-map
             "K" 'magit-discard-item
             "L" 'magit-key-mode-popup-logging
             (kbd "C-j") 'magit-goto-next-section
             (kbd "C-k") 'magit-goto-previous-section
             (kbd "C-n") 'magit-goto-next-section
             (kbd "C-p") 'magit-goto-previous-section
             (kbd "C-v") 'magit-revert-item)
      (evilify magit-status-mode magit-status-mode-map
             "K" 'magit-discard-item
             "L" 'magit-key-mode-popup-logging
             "H" 'magit-key-mode-popup-diff-options
             (kbd "C-j") 'magit-goto-next-section
             (kbd "C-k") 'magit-goto-previous-section
             (kbd "C-n") 'magit-goto-next-section
             (kbd "C-p") 'magit-goto-previous-section
             (kbd "C-v") 'magit-revert-item)
      (evilify magit-diff-mode magit-diff-mode-map
             "K" 'magit-discard-item
             "L" 'magit-key-mode-popup-logging
             "H" 'magit-key-mode-popup-diff-options
             (kbd "C-j") 'magit-goto-next-section
             (kbd "C-k") 'magit-goto-previous-section
             (kbd "C-n") 'magit-goto-next-section
             (kbd "C-p") 'magit-goto-previous-section
             (kbd "C-v") 'magit-revert-item)))
  :config
  (progn
    (dotemacs-hide-lighter magit-auto-revert-mode)
    ;; full screen magit-status
    (when dotemacs-git-magit-status-fullscreen
      (defadvice magit-status (around magit-fullscreen activate)
        (window-configuration-to-register :magit-fullscreen)
        ad-do-it
        (delete-other-windows))

      (defun magit-quit-session ()
        "Restores the previous window configuration and kills the magit buffer"
        (interactive)
        (kill-buffer)
        (jump-to-register :magit-fullscreen))
      (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

    (defun magit-toggle-whitespace ()
      (interactive)
      (if (member "-w" magit-diff-options)
          (magit-dont-ignore-whitespace)
        (magit-ignore-whitespace)))

    (defun magit-ignore-whitespace ()
      (interactive)
      (add-to-list 'magit-diff-options "-w")
      (magit-refresh))

    (defun magit-dont-ignore-whitespace ()
      (interactive)
      (setq magit-diff-options (remove "-w" magit-diff-options))
      (magit-refresh))
    (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

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

(use-package magit-gitflow
  :ensure t
  :commands turn-on-magit-gitflow
  :init (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
  :diminish (magit-gitflow-mode . "Flow"))

(use-package magit-svn
  :if dotemacs-git-enable-magit-svn-plugin
  :ensure t
  :commands turn-on-magit-svn
  :init (add-hook 'magit-mode-hook 'turn-on-magit-svn)
  :config
  (progn
    (evil-define-key 'emacs magit-status-mode-map
      "N" 'magit-key-mode-popup-svn)))

(use-package git-commit-mode            ; Git commit message mode
  :ensure t
  :defer t
  :config
  (after "evil-leader"
    (evil-leader/set-key-for-mode 'git-commit-mode
      "mcc" 'git-commit-commit
      "mk" 'git-commit-abort)))

(use-package git-messenger
  :ensure t
  :defer t
  :init
  (after "evil-leader"
    (evil-leader/set-key
      "gm" 'git-messenger:popup-message)))

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
  :defer t
  :config
  (progn
    (after "evil-evilified-state"
      (evilify git-rebase-mode git-rebase-mode-map
              "J" 'git-rebase-move-line-down
              "K" 'git-rebase-move-line-up
              "u" 'git-rebase-undo
              "y" 'git-rebase-insert))

    (after "evil-leader"
      (evil-leader/set-key-for-mode 'git-rebase-mode
        "mcc" 'git-rebase-server-edit
        "mk" 'git-rebase-abort))))

(use-package git-timemachine            ; Go back in Git time
  :ensure t
  :defer t
  :bind (("C-c v t" . git-timemachine))
  :commands dotemacs-time-machine-micro-state
  :init
  (after "evil-leader"
    (evil-leader/set-key
      "gt" 'dotemacs-time-machine-micro-state))

  :config
  (progn

    (defun dotemacs-time-machine-ms-on-enter ()
      "Initiate git-timemachine")

    (dotemacs-define-micro-state time-machine
      :doc "[p] [N] previous [n] next [c] current [Y] copy hash [q] quit"
      :on-enter (dotemacs-time-machine-ms-on-enter)
      :on-exit (git-timemachine-quit)
      :persistent t
      :bindings
      ("c" git-timemachine-show-current-revision)
      ("p" git-timemachine-show-previous-revision)
      ("n" git-timemachine-show-next-revision)
      ("N" git-timemachine-show-previous-revision)
      ("Y" git-timemachine-kill-revision)
      ("q" nil :exit t))))


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

    (setq grep-find-ignored-files
          '("Thumbs.db"
            ".DS_Store"
            ".#*"
            "*.o"
            "*~"
            "*.pyc"
            "*.pyo"
            "*.png"
            "*.odg"
            "*.jpe?g"
            "*.gif"
            "*.rbc"
            "*.swp"
            "*.psd"
            "*.ai"
            "*.pdf"
            "*.mov"
            "*.aep"
            "*.dmg"
            "*.zip"
            "*.gz"
            "*.bmp"
            "*.dxl"
            "*.lo"
            "*.la"
            "*.gmo"
            "*.mo"
            "*.toc"
            "*.aux"
            "*.cp"
            "*.fn"
            "*.ky"
            "*.bin"
            "*.lbin"
            "*.so"
            "*.a"
            "*.ln"
            "*.blg"
            "*.bbl"
            "*.elc"
            "*.lof"
            "*.glo"
            "*.idx"
            "*.fmt"
            "*.class"
            "*.lib"
            "*.fsl"
            ))

    (setq grep-find-ignored-directories
          '("CVS"
          ; ".svn"
          ; ".hg"
          ; "_darcs"
          ; ".git"
          ; ".idea"
          ; ".hg"
          ; ".bzr"
          "elpa"
          "/assets"
          "/node_modules"
          "/build"
          "/tmp"
          "/log"
          "/vendor/rails"
          "/vendor/gems"
          "/vendor/plugins"
          "/bower_components"
          "/components"
          "/.git"
          "/.hg"
          "/.svn"
          "/.idea"
          "/.sass-cache"))

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
  ; (add-hook 'ag-mode-hook (lambda () (toggle-truncate-lines t)))
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
  :defer t
  :init
  (progn
    (after "evil-evilified-state"
      ;; evilify the helm-grep buffer
      (evilify helm-grep-mode helm-grep-mode-map
               (kbd "RET") 'helm-grep-mode-jump-other-window
               (kbd "q") 'quit-window))

    (after "evil-leader"
      (evil-leader/set-key
        ;; opened buffers scope
        "sb"  'dotemacs-helm-buffers-smart-do-search
        "sB"  'dotemacs-helm-buffers-smart-do-search-region-or-symbol
        "sab" 'helm-do-ag-buffers
        "saB" 'dotemacs-helm-buffers-do-ag-region-or-symbol
        "skb" 'dotemacs-helm-buffers-do-ack
        "skB" 'dotemacs-helm-buffers-do-ack-region-or-symbol
        "stb" 'dotemacs-helm-buffers-do-pt
        "stB" 'dotemacs-helm-buffers-do-pt-region-or-symbol
        ;; current file scope
        "ss"  'dotemacs-helm-file-smart-do-search
        "sS"  'dotemacs-helm-file-smart-do-search-region-or-symbol
        "saa" 'helm-ag-this-file
        "saA" 'dotemacs-helm-file-do-ag-region-or-symbol
        ;; files scope
        "sf"  'dotemacs-helm-files-smart-do-search
        "sF"  'dotemacs-helm-files-smart-do-search-region-or-symbol
        "saf" 'helm-do-ag
        "saF" 'dotemacs-helm-files-do-ag-region-or-symbol
        "skf" 'dotemacs-helm-files-do-ack
        "skF" 'dotemacs-helm-files-do-ack-region-or-symbol
        "stf" 'dotemacs-helm-files-do-pt
        "stF" 'dotemacs-helm-files-do-pt-region-or-symbol
        ;; current project scope
        "/"   'dotemacs-helm-project-smart-do-search
        "?"   'dotemacs-helm-project-smart-do-search-region-or-symbol
        "sp"  'dotemacs-helm-project-smart-do-search
        "sP"  'dotemacs-helm-project-smart-do-search-region-or-symbol
        "sap" 'dotemacs-helm-project-do-ag
        "saP" 'dotemacs-helm-project-do-ag-region-or-symbol
        "skp" 'dotemacs-helm-project-do-ack
        "skP" 'dotemacs-helm-project-do-ack-region-or-symbol
        "stp" 'dotemacs-helm-project-do-pt
        "stP" 'dotemacs-helm-project-do-pt-region-or-symbol)))
  :config
  (progn
    (setq helm-ag-fuzzy-match t
          helm-ag-use-grep-ignore-list t ;; Use `grep-find-ignored-files'
                                         ;; and `grep-find-ignored-directories'
                                         ;; as ignore pattern
          helm-ag-insert-at-point 'symbol
          helm-ag-source-type 'file-line))

    (after "evil-leader"
      (evil-define-key 'normal helm-ag-map "SPC" evil-leader--default-map))

    (after "evil-evilified-state"
      (evilify helm-ag-mode helm-ag-mode-map
               (kbd "RET") 'helm-ag-mode-jump-other-window
               (kbd "q") 'quit-window)))


;;; Project management for Interactively Do Things (IDO)

;; original `init-ido`
; (require 'ido)
; (setq ido-case-fold nil
;       ido-auto-merge-work-directories-length -1
;       ido-max-prospects 10)
; (setq ido-enable-prefix nil)
; (setq ido-use-virtual-buffers t)
; ;; disable ido faces to see flx highlights.
; (setq ido-use-faces nil)
; (setq ido-create-new-buffer 'always)
; (setq ido-use-filename-at-point 'guess)
; (setq ido-save-directory-list-file (concat dotemacs-cache-directory "ido.last"))
;
; (ido-mode t)
; (ido-everywhere t)
;
; (require 'ido-ubiquitous)
; (ido-ubiquitous-mode t)
;
; ;; Try out flx-ido for better flex matching between words
; (require 'flx-ido)
; (flx-ido-mode t)
;
; ;; flx-ido looks better with ido-vertical-mode
; (require 'ido-vertical-mode)
; (ido-vertical-mode)
;
; ;; C-n/p is more intuitive in vertical layout
; (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
;
; (require 'dash)
;
; ;; Always rescan buffer for imenu
; (set-default 'imenu-auto-rescan t)
;
; (add-to-list 'ido-ignore-directories "target")
; (add-to-list 'ido-ignore-directories "node_modules")
; (add-to-list 'ido-ignore-directories "bower_components")
;
; ;; Ido at point (C-,)
; (require 'ido-at-point)
; (ido-at-point-mode)
;
; ;; Use ido everywhere
; (require 'ido-ubiquitous)
; (ido-ubiquitous-mode 1)
;
; (provide 'init-ido)
;; end origianl `init-ido`
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
    (setq ido-enable-flex-matching t ;; enable fuzzy matching
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


;;; Project management with Projectile

;; To ignore files for grepping with `helm-projectile-grep/ack`, you must
;; customize either one of these four variables:
;; - `grep-find-ignored-files`: List of file names which rgrep and lgrep
;;   shall exclude. helm-projectile-grep also uses this variable.
;; - `grep-find-ignored-directories`: List of names of sub-directories which
;;   rgrep shall not recurse into. helm-projectile-grep also uses this
;;   variable.
;; - `projectile-globally-ignored-files`: A list of files globally ignored by
;;   Projectile.
;; - `projectile-globally-ignored-directories`: A list of directories
;;   globally ignored by Projectile.
;;
;; You can add to this to your `init.el`:
;;
;; ```
;; (add-to-list 'projectile-globally-ignored-files "*.png")
;; ```
;;
;; Add more if you want.
;;
;; Alternatively, `C-h v` then type `projectile-globally-ignored-files`. In the
;; customization window, press `INS` button and add "*.png", then press `State`
;; button and choose `Save for future session`.
;;
;; You can read [the guide][1] for `helm-projectile-grep/ack/ag`
;;
;; **EDIT**: Don't put it in `.projectile` files or your files won't be ignored when grepping.
;;
;; my ignore directory pattern "[\/]((assets|node_modules|build|tmp|log|vendor\/(rails|gems|plugins)|bower_components|components)|(\.(git|hg|svn|idea|sass-cache)))$"
;; my ignore file pattern "(\.(#.+|DS_Store|svn|png|jpe?g|gif|elc|rbc|pyc|swp|psd|ai|pdf|mov|aep|dmg|zip|gz|bmp)|(Thumbs\.db))$"
;;
;; [1]: http://tuhdo.github.io/helm-projectile.html#sec-9
(use-package projectile
  :ensure t
  :commands (projectile-ack
             projectile-ag
             projectile-compile-project
             projectile-dired
             projectile-grep
             projectile-find-dir
             projectile-find-file
             projectile-find-tag
             projectile-find-test-file
             projectile-invalidate-cache
             projectile-kill-buffers
             projectile-multi-occur
             projectile-project-root
             projectile-recentf
             projectile-regenerate-tags
             projectile-replace
             projectile-run-async-shell-command-in-root
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-vc)
  :init
  (progn
    (setq-default projectile-enable-caching t)
    (setq projectile-sort-order 'recentf
          projectile-cache-file (concat dotemacs-cache-directory
                                        "projectile.cache")
          projectile-known-projects-file (concat dotemacs-cache-directory
                                                   "projectile-bookmarks.eld")
          projectile-completion-system 'helm
          projectile-indexing-method 'alien ; force alien for Windwos
          projectile-find-dir-includes-top-level t
          projectile-mode-line '(:propertize
                                 (:eval (concat " " (projectile-project-name)))
                                 face font-lock-constant-face))

    (setq projectile-project-root-files '(
            ; "rebar.config"       ; Rebar project file
            "project.clj"        ; Leiningen project file
            ; "SConstruct"         ; Scons project file
            "pom.xml"            ; Maven project file
            ; "build.sbt"          ; SBT project file
            ; "build.gradle"       ; Gradle project file
            "Gemfile"            ; Bundler file
            ; "requirements.txt"   ; Pip file
            ; "setup.py"           ; Setuptools file
            ; "tox.ini"            ; Tox file
            "package.json"       ; npm package file
            "gulpfile.js"        ; Gulp build file
            "Gruntfile.js"       ; Grunt project file
            "bower.json"         ; Bower project file
            "composer.json"      ; Composer project file
            "Cargo.toml"         ; Cargo project file
            ; "mix.exs"            ; Elixir mix project file
            ))

    (setq projectile-project-root-files-bottom-up '(
            ".projectile" ; projectile project marker
            ".git"        ; Git VCS root dir
            ".hg"         ; Mercurial VCS root dir
            ; ".fslckout"   ; Fossil VCS root dir
            ; ".bzr"        ; Bazaar VCS root dir
            ; "_darcs"      ; Darcs VCS root dir
            ))

    (setq projectile-globally-ignored-files
          '("TAGS"
            "GRTAGS"
            "GTAGS"
            "GPATH"
            "Thumbs.db"
            ".DS_Store"
            ".#*"
            "*.o"
            "*~"
            "*.pyc"
            "*.pyo"
            "*.png"
            "*.odg"
            "*.jpe?g"
            "*.gif"
            "*.rbc"
            "*.swp"
            "*.psd"
            "*.ai"
            "*.pdf"
            "*.mov"
            "*.aep"
            "*.dmg"
            "*.zip"
            "*.gz"
            "*.bmp"
            "*.dxl"
            "*.lo"
            "*.la"
            "*.gmo"
            "*.mo"
            "*.toc"
            "*.aux"
            "*.cp"
            "*.fn"
            "*.ky"
            "*.bin"
            "*.lbin"
            "*.so"
            "*.a"
            "*.ln"
            "*.blg"
            "*.bbl"
            "*.elc"
            "*.lof"
            "*.glo"
            "*.idx"
            "*.fmt"
            "*.class"
            "*.lib"
            "*.fsl"))

    (setq projectile-globally-ignored-file-suffixes
          '("class"
            "elc"))

    (setq projectile-globally-ignored-directories
          '("CVS"
            ;".idea"
            ; ".eunit"
            ; ".git"
            ; ".hg"
            ; ".fslckout"
            ; ".bzr"
            ; "_darcs"
            ; ".tox"
            ; ".svn"
            "elpa"
            "/assets"
            "/node_modules"
            "/build"
            "/tmp"
            "/log"
            "/vendor/rails"
            "/vendor/gems"
            "/vendor/plugins"
            "/bower_components"
            "/components"
            "/.git"
            "/.hg"
            "/.cache"
            "/.svn"
            "/.idea"
            "/.sass-cache"))

    (after "evil-leader"
      (unless (boundp 'dotemacs-use-helm-projectile)
        (evil-leader/set-key
          "pb" 'projectile-switch-to-buffer
          "pd" 'projectile-find-dir
          "pe" 'projectile-recentf
          "pf" 'projectile-find-file
          "pg" 'projectile-grep
          "ph" 'helm-projectile
          "ps" 'projectile-switch-project))
      (evil-leader/set-key
        "p!" 'projectile-run-shell-command-in-root
        "p&" 'projectile-run-async-shell-command-in-root
        "pc" 'projectile-compile-project
        "pD" 'projectile-dired
        "pI" 'projectile-invalidate-cache
        "pk" 'projectile-kill-buffers
        "po" 'projectile-multi-occur
        "pr" 'projectile-replace
        "pR" 'projectile-regenerate-tags
        "py" 'projectile-find-tag
        "pT" 'projectile-find-test-file)))
  :config
  (progn
    ;; Remove dead projects when Emacs is idle
    ; (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)
    (projectile-global-mode))
  :diminish projectile-mode)

(use-package helm-projectile
  :ensure t
  :defer t
  :commands (helm-projectile-switch-to-buffer
             helm-projectile-find-dir
             helm-projectile-dired-find-dir
             helm-projectile-recentf
             helm-projectile-find-file
             helm-projectile-grep
             helm-projectile
             helm-projectile-switch-project)
  :init
  (progn
    ; (after "projectile" (helm-projectile-on)
    ; (add-to-list 'helm-projectile-sources-list 'helm-source-projectile-recentf-list)
    (setq projectile-switch-project-action #'helm-projectile)

    (defconst dotemacs-use-helm-projectile t
      "This variable is only defined if helm-projectile is used.")

    ;; needed for smart search if user's default tool is grep
    (defalias 'dotemacs-helm-project-do-grep 'helm-projectile-grep)
    (defalias 'dotemacs-helm-project-do-grep-region-or-symbol 'helm-projectile-grep)

    (after "evil-leader"
      (evil-leader/set-key
        "pb"  'helm-projectile-switch-to-buffer
        "pd"  'helm-projectile-find-dir
        "pe"  'helm-projectile-recentf
        "pf"  'helm-projectile-find-file
        "ph"  'helm-projectile
        "pp"  'helm-projectile-switch-project
        "pv"  'projectile-vc
        "sgp" 'helm-projectile-grep))))


;;; Project management with Project Explorer
(use-package project-explorer
  :disabled t
  :defer t
  :ensure t
  :commands (project-explorer-open
             project-explorer-toggle)
  :init
  (progn
    (after "evil-leader"
      (evil-leader/set-key
        "ft" 'project-explorer-open
        "fT" 'project-explorer-toggle))
    (setq pe/cache-directory (concat dotemacs-cache-directory "project-explorer")))
  :config
  (progn
    (after "evil"
      (define-key project-explorer-mode-map (kbd "C-l") 'evil-window-right)
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
        (kbd "M-o") 'pe/toggle-omit))))


;;; Project management with NeoTree
(use-package neotree
  :ensure t
  :defer t
  :commands neo-global--window-exists-p
  :init
  (progn
    (add-to-list 'evil-motion-state-modes 'neotree-mode)
    (setq neo-window-width 32
          neo-theme 'nerd
          neo-hidden-regexp-list '("\\(\\.\\(#.\\+\\|DS_Store\\|svn\\|png\\|jpe\\?g\\|gif\\|elc\\|rbc\\|pyc\\|swp\\|psd\\|ai\\|pdf\\|mov\\|aep\\|dmg\\|zip\\|gz\\|bmp\\|git\\|hg\\|svn\\|idea\\|sass-cache\\)\\|\\(Thumbs\\.db\\)\\|\\(assets\\|node_modules\\|build\\|tmp\\|log\\|vendor\\|bower_components\\|components\\)\\)$")
          neo-create-file-auto-open t
          neo-banner-message nil
          neo-show-updir-line nil
          neo-mode-line-type 'neotree
          neo-smart-open t
          neo-dont-be-alone t
          neo-persist-show nil
          neo-show-hidden-files t
          neo-auto-indent-point t)

    (defun dotemacs-init-neotree ()
      "Initialize the neotree mode.")

    (defun dotemacs-neotree-expand-or-open ()
      "Collapse a neotree node."
      (interactive)
      (let ((node (neo-buffer--get-filename-current-line)))
        (when node
          (if (file-directory-p node)
              (progn
                (neo-buffer--set-expand node t)
                (neo-buffer--refresh t)
                (when neo-auto-indent-point
                  (next-line)
                  (neo-point-auto-indent)))
            (call-interactively 'neotree-enter)))))

    (defun dotemacs-neotree-collapse ()
      "Collapse a neotree node."
      (interactive)
      (let ((node (neo-buffer--get-filename-current-line)))
        (when node
          (when (file-directory-p node)
            (neo-buffer--set-expand node nil)
            (neo-buffer--refresh t))
          (when neo-auto-indent-point
            (neo-point-auto-indent)))))

    (defun dotemacs-neotree-collapse-or-up ()
      "Collapse an expanded directory node or go to the parent node."
      (interactive)
      (let ((node (neo-buffer--get-filename-current-line)))
        (when node
          (if (file-directory-p node)
              (if (neo-buffer--expanded-node-p node)
                  (dotemacs-neotree-collapse)
                (neotree-select-up-node))
            (neotree-select-up-node)))))

    (defun neotree-find-project-root ()
      (interactive)
      (if (neo-global--window-exists-p)
          (neotree-hide)
        (neotree-find (projectile-project-root))))

    (defun dotemacs-neotree-key-bindings ()
      "Set the key bindings for a neotree buffer."
      (define-key evil-motion-state-local-map (kbd "TAB") 'neotree-stretch-toggle)
      (define-key evil-motion-state-local-map (kbd "RET") 'neotree-enter)
      (define-key evil-motion-state-local-map (kbd "|")   'neotree-enter-vertical-split)
      (define-key evil-motion-state-local-map (kbd "-")   'neotree-enter-horizontal-split)
      (define-key evil-motion-state-local-map (kbd "?")   'evil-search-backward)
      (define-key evil-motion-state-local-map (kbd "c")   'neotree-create-node)
      (define-key evil-motion-state-local-map (kbd "d")   'neotree-delete-node)
      (define-key evil-motion-state-local-map (kbd "g")   'neotree-refresh)
      (define-key evil-motion-state-local-map (kbd "h")   'dotemacs-neotree-collapse-or-up)
      (define-key evil-motion-state-local-map (kbd "H")   'neotree-select-previous-sibling-node)
      (define-key evil-motion-state-local-map (kbd "J")   'neotree-select-down-node)
      (define-key evil-motion-state-local-map (kbd "K")   'neotree-select-up-node)
      (define-key evil-motion-state-local-map (kbd "l")   'dotemacs-neotree-expand-or-open)
      (define-key evil-motion-state-local-map (kbd "L")   'neotree-select-next-sibling-node)
      (define-key evil-motion-state-local-map (kbd "q")   'neotree-hide)
      (define-key evil-motion-state-local-map (kbd "r")   'neotree-rename-node)
      (define-key evil-motion-state-local-map (kbd "R")   'neotree-change-root)
      (define-key evil-motion-state-local-map (kbd "s")   'neotree-hidden-file-toggle))

    (after "evil-leader"
      (evil-leader/set-key
        "ft" 'neotree-toggle
        "pt" 'neotree-find-project-root)))

  :config
    (add-to-hook 'neotree-mode-hook '(dotemacs-init-neotree
                                      dotemacs-neotree-key-bindings)))


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
    (after "evil-leader"
      (evil-leader/set-key
        "pp" 'dotemacs-persp-switch-project)
    )))


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

(use-package erc                        ; Powerful IRC client
  :defer t
  :config
  (progn
    ;; Default server and nick
    (setq erc-server "chat.freenode.net"
          erc-log-channels-directory (concat dotemacs-cache-directory "erc/logs")
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

    (after "evil-leader"
      (evil-leader/set-key-for-mode 'org-mode
        "mc" 'org-capture
        "md" 'org-deadline
        "me" 'org-export-dispatch
        "mf" 'org-set-effort
        "mi" 'org-clock-in
        "mj" 'helm-org-in-buffer-headings
        "mo" 'org-clock-out
        "mm" 'org-ctrl-c-ctrl-c
        "mq" 'org-clock-cancel
        "mr" 'org-refile
        "ms" 'org-schedule))

    (require 'ox-md)
    (require 'ox-ascii)
    (require 'ox-confluence)
    (require 'ox-html)
    (require 'org-bullets)

    (after "evil" (add-hook 'org-capture-mode-hook #'evil-emacs-state))

    (after "org-agenda"
      '(progn
         (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
         (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)
         ;; Since we could override SPC with <leader>, let's make RET do that functionality
         ;; TODO: Check if <leader> equals SPC
         ; (define-key org-agenda-mode-map
         ;   (kbd "RET") 'org-agenda-show-and-scroll-up)
         ; (define-key org-agenda-mode-map
         ;   (kbd "SPC") evil-leader--default-map)
         )))
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
    (after "evil-leader"
      (evil-leader/set-key
        "Ct"  'ort/capture-todo
        "CT"  'ort/capture-todo-check)
      (evil-leader/set-key-for-mode 'org-mode
        "mgt" 'ort/goto-todos))))

(use-package evil-org
  :disabled t
  :commands evil-org-mode
  :init
  (add-hook 'org-mode-hook 'evil-org-mode)
  :config
  (progn
    (after "evil-leader"
      (evil-leader/set-key-for-mode 'org-mode
        "a" nil "ma" 'org-agenda
        "c" nil "mA" 'org-archive-subtree
        "o" nil "mC" 'evil-org-recompute-clocks
        "l" nil "ml" 'evil-org-open-links
        "t" nil "mt" 'org-show-todo-tree)))
  :diminish (evil-org-mode . " ⓔ"))


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
  :defer t
  :init
  (progn
    (setq helm-descbinds-window-style 'split)
    (add-hook 'helm-mode-hook 'helm-descbinds-mode)
    (after "evil-leader"
      (evil-leader/set-key "hk" 'helm-descbinds))))

(use-package helm-make
  :ensure t
  :defer t
  :init
  (after "evil-leader"
    (evil-leader/set-key "hk" 'helm-make)))

(use-package help-mode
  :config
  (progn
    (define-key help-mode-map (kbd "n") 'next-line)
    (define-key help-mode-map (kbd "p") 'previous-line)
    (define-key help-mode-map (kbd "j") 'next-line)
    (define-key help-mode-map (kbd "k") 'previous-line)))

(use-package dash-at-point
  :ensure t
  :defer t
  :bind (("C-c h d" . dash-at-point)
         ("C-c h D" . dash-at-point-with-docset))
  :config (add-to-list 'dash-at-point-mode-alist
                       '(swift-mode . "ios,swift")))

(bind-key "C-c h b" #'describe-personal-keybindings)

(use-package guide-key-tip
  :ensure t
  :defer t
  :init
  (progn
    (defun dotemacs-toggle-guide-key ()
      "Toggle golden-ratio mode on and off."
      (interactive)
      (if (symbol-value guide-key-mode)
          (guide-key-mode -1)
        (guide-key-mode)))

    (defadvice guide-key/popup-guide-buffer-p
        (around dotemacs-inhibit-guide-buffer activate)
      "Prevent the popup of the guide-key buffer in some case."
      ;; a micro-state is running
      ;; or
      ;; bzg-big-fringe-mode is on
      (unless (or overriding-terminal-local-map
                  bzg-big-fringe-mode)
        ad-do-it))

    (after "evil-leader"
      (dotemacs-add-toggle guide-key
                      :status guide-key-mode
                      :on (guide-key-mode)
                      :off (guide-key-mode -1)
                      :documentation
                      "Display a buffer with available key bindings."
                      :evil-leader "tG"))

    (setq guide-key/guide-key-sequence `("C-x"
                                         "C-c"
                                         "C-w"
                                         ,dotemacs-leader-key
                                         ,dotemacs-emacs-leader-key
                                         ,dotemacs-major-mode-leader-key
                                         ,dotemacs-major-mode-emacs-leader-key
                                         ;; M-m in terminal
                                         "<ESC>m"
                                         ;; C-M-m in terminal
                                         "<ESC><RET>"
                                         "g"
                                         "\["
                                         "\]"
                                         "z"
                                         "C-h")
          guide-key/recursive-key-sequence-flag t
          guide-key/popup-window-position 'bottom
          guide-key/idle-delay dotemacs-guide-key-delay
          guide-key/text-scale-amount 0
          guide-key-tip/enabled nil)
    (setq guide-key/highlight-command-regexp
                 (cons dotemacs-prefix-command-string font-lock-warning-face))
    (guide-key-mode 1))
  :config
  (add-hook 'evil-leader-mode-hook
            #'(lambda () (guide-key/add-local-guide-key-sequence evil-leader/leader)))
  :diminish (guide-key-mode . " Ⓖ"))



;;; Documents
(use-package doc-view
  :defer t
  :init
  (after "evil-evilified-state"
         (evilify doc-view-mode doc-view-mode-map
           "/"  'dotemacs-doc-view-search-new-query
           "?"  'dotemacs-doc-view-search-new-query-backward
           "gg" 'doc-view-first-page
           "G"  'doc-view-last-page
           "gt" 'doc-view-goto-page
           "h"  'doc-view-previous-page
           "j"  'doc-view-next-line-or-next-page
           "k"  'doc-view-previous-line-or-previous-page
           "K"  'doc-view-kill-proc-and-buffer
           "l"  'doc-view-next-page
           "n"  'doc-view-search
           "N"  'doc-view-search-backward
           (kbd "C-d") 'doc-view-scroll-up-or-next-page
           (kbd "C-k") 'doc-view-kill-proc
           (kbd "C-u") 'doc-view-scroll-down-or-previous-page))
  :config
  (progn
    (defun dotemacs-doc-view-search-new-query ()
      "Initiate a new query."
      (interactive)
      (doc-view-search 'newquery))

    (defun dotemacs-doc-view-search-new-query-backward ()
      "Initiate a new query."
      (interactive)
      (doc-view-search 'newquery t))

    ;; fixed a weird issue where toggling display does not
    ;; swtich to text mode
    (defadvice doc-view-toggle-display
        (around dotemacs-doc-view-toggle-display activate)
      (if (eq major-mode 'doc-view-mode)
          (progn
            ad-do-it
            (text-mode)
            (doc-view-minor-mode))
        ad-do-it))))


;;; Config

;; Ansible
(use-package init-ansible
  :load-path "config/")

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package ansible
  :ensure t
  :defer t
  :init (progn
          (eval-after-load 'yaml-mode
            '(add-hook 'yaml-mode-hook 'ansible/ansible-maybe-enable))

          ;; ansible-mode requires ac-user-dictionary-files. If the
          ;; config is using company-mode this variable will not be
          ;; set, so we set it to a dummy value.
          ;;
          ;; Tracking here:
          ;; https://github.com/k1LoW/emacs-ansible/issues/2
          (after "company"
            (setq ac-user-dictionary-files '()))))

(use-package ansible-doc                ; Documentation lookup for Ansible
  :ensure t
  :defer t
  :init (eval-after-load 'yaml-mode
          '(add-hook 'yaml-mode-hook 'ansible/ansible-doc-maybe-enable))
  :diminish (ansible-doc-mode . "❓"))

;; Docker
(use-package dockerfile-mode
  :defer t
  :ensure t
  :config
  (progn
    (evil-leader/set-key-for-mode 'dockerfile-mode
       "mcb" 'dockerfile-build-buffer
     )))

;; Puppet
(use-package puppet-mode                ; Puppet manifests
  :defer t
  :ensure t
  :config
  ;; Fontify variables in Puppet comments
  (setq puppet-fontify-variables-in-comments t)
  :init
  (progn
    (evil-leader/set-key-for-mode 'puppet-mode
      "m{" 'beginning-of-defun
      "m}" 'end-of-defun
      "m$" 'puppet-interpolate
      "ma" 'puppet-align-block
      "m'" 'puppet-toggle-string-quotes
      "m;" 'puppet-clear-string
      "mj" 'imenu
      "mc" 'puppet-apply
      "mv" 'puppet-validate
      "ml" 'puppet-lint
    )))

(when (eq dotemacs-completion-engine 'company)
  (after "company"
    (dotemacs-add-company-hook puppet-mode)))

(use-package puppetfile-mode
  :ensure t
  :defer t)


;;; Misc
(use-package google-translate
  :ensure t
  :commands (google-translate-query-translate
             google-translate-at-point
             google-translate-query-translate-reverse
             google-translate-at-point-reverse)
  :init
  (after "evil-leader"
    (evil-leader/set-key
      "xgQ" 'google-translate-query-translate-reverse
      "xgq" 'google-translate-query-translate
      "xgT" 'google-translate-at-point-reverse
      "xgt" 'google-translate-at-point))
  :config
  (progn
    (require 'google-translate-default-ui)
    (setq google-translate-enable-ido-completion t)
    (setq google-translate-show-phonetic t)
    (setq google-translate-default-source-language "En")
    (setq google-translate-default-target-language "Sp")))

; (use-package init-bindings
;   :load-path "config/")

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
