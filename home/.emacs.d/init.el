;;; init.el --- Emacs configuration

;;; Commentary:

;; User key prefixes:
;;
;; - C-c A: Align
;; - C-c a: Ag
;; - C-c d: Data stuff
;; - C-c e: Edit commands, general and mode specific
;; - C-c f: Files
;; - C-c h: Helm/Help and documentation
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

(defcustom dotemacs-themes '(zenburn
                             solarized-dark
                             solarized-light
                             leuven
                             monokai
                             tao
                             ;; monster theme support
                             afternoon
                             ample
                             ample-zen
                             anti-zenburn
                             birds-of-paradise-plus
                             bubbleberry
                             busybee
                             cherry-blossom
                             clues
                             cyberpunk
                             dakrone
                             darkburn
                             darkmine
                             darktooth
                             django
                             espresso
                             firebelly
                             flatland
                             flatui
                             gandalf
                             gotham
                             grandshell
                             gruber-darker
                             gruvbox
                             hc-zenburn
                             heroku
                             inkpot
                             ir-black
                             jazz
                             light-soap
                             lush
                             material
                             minimal
                             molokai
                             monochrome
                             mustang
                             naquadah
                             niflheim
                             noctilux
                             obsidian
                             occidental
                             oldlace
                             organic-green
                             pastels-on-dark
                             phoenix-dark-mono
                             phoenix-dark-pink
                             planet
                             professional
                             purple-haze
                             seti
                             smyx
                             soft-charcoal
                             soft-morning
                             soft-stone
                             soothe
                             spacegray
                             subatomic
                             subatomic256
                             sunny-day
                             tango-2
                             tango-plus
                             tangotango
                             tronesque
                             twilight-anti-bright
                             twilight-bright
                             twilight
                             ujelly
                             underwater
                             zen-and-art)
  "List of themes, the first of the list is loaded when emacs starts.
Press <Leader> T n to cycle to the next theme in the list (works great
with 2 themes variants, one dark and one light")

(defcustom dotemacs--cur-theme nil
  "The current theme"
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

(defcustom dotemacs-guide-key-delay 1.0
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

(defcustom dotemacs-active-transparency 94
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's active or selected. Transparency
can be toggled through `toggle-transparency'."
  :group 'dotemacs)

(defcustom dotemacs-inactive-transparency 94
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

(defcustom dotemacs-auto-save-file-location 'cache
  "Location where to auto-save files. Possible values are `original' to
auto-save the file in-place, `cache' to auto-save the file to another
file stored in the cache directory and `nil' to disable auto-saving.
Default value is `cache'."
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

;; c-c++ settings
(defgroup dotemacs-c-c++ nil
  "Configuration options for c/c++."
  :group 'dotemacs
  :prefix 'dotemacs-c-c++)

(defcustom dotemacs-c-c++-enable-clang-support t
  "If non nil Clang related packages and configuration are enabled."
  :group 'dotemacs-c-c++)

(defcustom dotemacs-c-c++-default-mode-for-headers 'c-mode
  "Default mode to open header files. Can be `c-mode' or `c++-mode'."
  :group 'dotemacs-c-c++)

;; shell settings
(defgroup dotemacs-shell nil
  "Configuration options for shell."
  :group 'dotemacs
  :prefix 'dotemacs-shell)

(defcustom dotemacs-shell-default-shell (if (eq window-system 'w32)
                                'eshell
                              'ansi-term)
  "Default shell to use in emacs. Possible values are `eshell', `shell',
`term', `multi-term`,  and `ansi-term'."
  :group 'dotemacs-shell)

(defcustom dotemacs-shell-default-position 'bottom
  "Position of the shell. Possible values are `top', `bottom' and `full'."
  :group 'dotemacs-shell)

(defcustom dotemacs-shell-default-height 30
  "Height in percents for the shell window."
  :group 'dotemacs-shell)

(defcustom dotemacs-shell-default-term-shell "/bin/bash"
  "Default shell to use in `term' and `ansi-term' shells."
  :group 'dotemacs-shell)

(defcustom dotemacs-shell-enable-smart-eshell nil
  "If non-nil then `em-smart' is enabled. `em-smart' allows to quickly review
commands, modify old commands or enter a new one."
  :group 'dotemacs-shell)

;; ruby settings
(defgroup dotemacs-ruby nil
  "Configuration options for ruby."
  :group 'dotemacs
  :prefix 'dotemacs-ruby)

(defcustom dotemacs-ruby-version-manager 'rbenv
  "If non nil defines the Ruby version manager (i.e. rbenv, rvm)"
  :group 'dotemacs-ruby)

(defcustom dotemacs-ruby-enable-ruby-on-rails-support nil
  "If non nil we'll load support for Rails (haml, features, navigation)"
  :group 'dotemacs-ruby)

;; latex settings
(defgroup dotemacs-latex nil
  "Configuration options for latex."
  :group 'dotemacs
  :prefix 'dotemacs-latex)

(defcustom dotemacs-latex-build-command "LaTeX"
  "The default command to use with `SPC m b'"
  :group 'dotemacs-latex)

(defcustom dotemacs-latex-enable-auto-fill t
  "Whether to use auto-fill-mode or not in tex files."
  :group 'dotemacs-latex)

(defcustom dotemacs-latex-nofill-env '("equation"
                           "equation*"
                           "align"
                           "align*"
                           "tabular"
                           "tikzpicture")
  "List of environment names in which `auto-fill-mode' will be inhibited."
  :group 'dotemacs-latex)

;; haskell settings
(defgroup dotemacs-haskell nil
  "Configuration options for haskell."
  :group 'dotemacs
  :prefix 'dotemacs-haskell)

;; Use GHCI NG from https://github.com/chrisdone/ghci-ng
(defcustom dotemacs-haskell-enable-ghci-ng-support nil
  "If non-nil ghci-ng support is enabled"
  :group 'dotemacs-haskell)

(defcustom dotemacs-haskell-enable-shm-support nil
  "If non-nil structured-haskell-mode support is enabled"
  :group 'dotemacs-haskell)

(defcustom dotemacs-haskell-enable-hindent-style 'fundamental
  "If non-nil structured-haskell-mode support is enabled"
  :group 'dotemacs-haskell)

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

;; colors settings
(defgroup dotemacs-colors nil
  "Configuration options for colors."
  :group 'dotemacs
  :prefix 'dotemacs-colors)

(defcustom dotemacs-colors-engine
  'rainbow
  "The color identifier engine to use."
  :type '(radio
          (const :tag "none" none)
          (const :tag "rainbow-identifiers" rainbow)
          (const :tag "color-identifiers" color))
  :group 'dotemacs-colors)

(defcustom dotemacs-colors-theme-identifiers-sat&light
  '((jazz . (50 55))
    (gotham . (45 60))
    (leuven . (100 40))
    (material . (95 105))
    (monokai . (55 60))
    (solarized-dark . (65 55))
    (solarized-light . (60 55))
    (zenburn . (40 65)))
  "alist of theme symbols and pair of saturation and lightness values."
  :group 'dotemacs-colors)

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

(defcustom auto-completion-enable-snippets-in-popup t
  "If non nil show snippets in the auto-completion popup."
  :group 'dotemacs-ac)

(defcustom auto-completion-tab-key-behavior 'cycle
  "What the TAB key should do when auto-completion menu is active.
Possible values are `complete', `cycle' or `nil'."
  :group 'dotemacs-ac)

(defcustom auto-completion-complete-with-key-sequence nil
  "Provide a key sequence (string) to complete the current
selection."
  :group 'dotemacs-ac)

(defcustom auto-completion-enable-sort-by-usage t
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

(defcustom dotemacs-s-syntax-checking-enable-tooltips t
  "If non nil some feedback are displayed in tooltips."
  :group 'dotemacs-s)

;; buffer settings
(defgroup dotemacs-ibuffer nil
  "Configuration options for ibuffer"
  :group 'dotemacs
  :prefix 'dotemacs-ibuffer)

(defcustom dotemacs-ibuffer-group-buffers-by 'modes
  "If non nil ibuffer will group the buffers according to the passed symbol.
The supported values are `modes' to group by major-modes and `projects' to
group by projectile projects."
  :group 'dotemacs-ibuffer)

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
          ("fe" . "files-emacs/dotemacs")
          ("g" .  "git/versions-control")
          ("gf" . "file")
          ("gg" . "gist")
          ("h" .  "helm/help/highlight")
          ("hd" . "help-describe")
          ("i" .  "insertion")
          ("j" .  "join/split")
          ("k" .  "lisp")
          ("kd" . "lisp-delete")
          ("kD" . "lisp-delete-backward")
          ("n" .  "narrow/numbers")
          ("p" .  "projects")
          ("p$" . "projects/shell")
          ("q" .  "quit")
          ("r" .  "registers/rings")
          ("s" .  "search/symbol")
          ("sw" . "search-web")
          ("S" .  "spelling")
          ("t" .  "toggles")
          ("tC" . "toggles-colors")
          ("th" . "toggles-highlight")
          ("te" . "toggles-errors")
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
        dotemacs-key-binding-prefixes)

  (when (eq dotemacs-colors-engine 'rainbow)
    (setq colors/key-binding-prefixes '(("Ci" . "colors-identifiers")))
    (mapc (lambda (x) (dotemacs-declare-prefix (car x) (cdr x)))
          colors/key-binding-prefixes)))


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

(use-package core-buffers
  :load-path "core/")

(use-package core-evilify-keymap
  :load-path "core/")

(use-package core-toggle
  :load-path "core/")

(use-package core-auto-completion
  :load-path "core/")

(use-package core-use-package-ext
  :load-path "core/")

(use-package core-themes-support
  :load-path "core/")


;; Shell
(dotemacs-defvar-company-backends eshell-mode)

(use-package init-eshell
  :load-path "config/")

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

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (setq-local company-idle-delay 0.2)
      ;; The default frontend screws everything up in short windows like
      ;; terminal often are
      (setq-local company-frontends '(company-preview-frontend))
      (push 'company-capf company-backends-eshell-mode)
      (dotemacs-add-company-hook eshell-mode))))

(use-package eshell
  :defer t
  :ensure t
  :init
  (progn
    (setq eshell-cmpl-cycle-completions nil
          ;; auto truncate after 20k lines
          eshell-buffer-maximum-lines 20000
          ;; history size
          eshell-history-size 350
          ;; buffer shorthand -> echo foo > #'buffer
          eshell-buffer-shorthand t
          ;; my prompt is easy enough to see
          eshell-highlight-prompt nil
          ;; treat 'echo' like shell echo
          eshell-plain-echo-behavior t)

    (add-hook 'eshell-mode-hook 'dotemacs-init-eshell))
  :config
  (progn
    (require 'esh-opt)

    ;; quick commands
    (defalias 'e 'find-file-other-window)
    (defalias 'd 'dired)
    (setenv "PAGER" "cat")

    ;; support `em-smart'
    (when dotemacs-shell-enable-smart-eshell
      (require 'em-smart)
      (setq eshell-where-to-jump 'begin
            eshell-review-quick-commands nil
            eshell-smart-space-goes-to-end t)
      (add-hook 'eshell-mode-hook 'eshell-smart-initialize))

    ;; Visual commands
    (require 'em-term)
    (mapc (lambda (x) (push x eshell-visual-commands))
          '("el" "elinks" "htop" "less" "ssh" "tmux" "top"))

    ;; automatically truncate buffer after output
    (when (boundp 'eshell-output-filter-functions)
      (push 'eshell-truncate-buffer eshell-output-filter-functions))))

(use-package esh-help
  :defer t
  :ensure t
  :init (add-hook 'eshell-mode-hook 'eldoc-mode)
  :config (setup-esh-help-eldoc))

(use-package eshell-prompt-extras
  :commands epe-theme-lambda
  :ensure t
  :init
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(use-package multi-term
  :defer t
  :ensure t
  :init
  (progn
    (after "evil-leader"
      (evil-leader/set-key "ast" 'shell-pop-multi-term)))
  :config
  (progn
    (add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab))
    (after "evil-leader"
      (evil-leader/set-key "p$t" 'projectile-multi-term-in-root))))

(use-package shell-pop
  :defer t
  :ensure t
  :init
  (progn
    (setq shell-pop-window-position dotemacs-shell-default-position
          shell-pop-window-height   dotemacs-shell-default-height
          shell-pop-term-shell      dotemacs-shell-default-term-shell
          shell-pop-full-span t)
    (defmacro make-shell-pop-command (type &optional shell)
      (let* ((name (symbol-name type)))
        `(defun ,(intern (concat "shell-pop-" name)) (index)
           (interactive "P")
           (require 'shell-pop)
           (shell-pop--set-shell-type
            'shell-pop-shell-type
            (backquote (,name
                        ,(concat "*" name "*")
                        (lambda nil (funcall ',type ,shell)))))
           (shell-pop index))))
    (make-shell-pop-command eshell)
    (make-shell-pop-command shell)
    (make-shell-pop-command term shell-pop-term-shell)
    (make-shell-pop-command multiterm)
    (make-shell-pop-command ansi-term shell-pop-term-shell)

    (add-hook 'kill-buffer-hook 'dotemacs-term-kill-buffer-hook)
    (add-hook 'term-mode-hook 'ansi-term-handle-close)

    (after "evil-leader"
      (evil-leader/set-key
        "'"   'dotemacs-default-pop-shell
        "ase" 'shell-pop-eshell
        "asi" 'shell-pop-shell
        "asm" 'shell-pop-multiterm
        "ast" 'shell-pop-ansi-term
        "asT" 'shell-pop-term))))

;; shell
(add-hook 'shell-mode-hook 'shell-comint-input-sender-hook)

;; term
(after "evil-leader"
  ;; hack to fix pasting issue, the paste micro-state won't
  ;; work in term
  (evil-define-key 'normal term-raw-map "p" 'term-paste)
  (evil-define-key 'insert term-raw-map (kbd "C-c C-d") 'term-send-eof)
  (evil-define-key 'insert term-raw-map (kbd "<tab>") 'term-send-tab))

(dotemacs-use-package-add-hook helm
  :post-init
  (progn
    ;; eshell
    (defun dotemacs-helm-eshell-history ()
      "Correctly revert to insert state after selection."
      (interactive)
      (helm-eshell-history)
      (evil-insert-state))
    (defun dotemacs-helm-shell-history ()
      "Correctly revert to insert state after selection."
      (interactive)
      (helm-comint-input-ring)
      (evil-insert-state))
    (defun dotemacs-init-helm-eshell ()
      "Initialize helm-eshell."
      ;; this is buggy for now
      ;; (define-key eshell-mode-map (kbd "<tab>") 'helm-esh-pcomplete)
      (evil-leader/set-key-for-mode 'eshell-mode
        "mH" 'dotemacs-helm-eshell-history))
    (add-hook 'eshell-mode-hook 'dotemacs-init-helm-eshell)
    ;;shell
    (evil-leader/set-key-for-mode 'shell-mode
      "mH" 'dotemacs-helm-shell-history)))

(dotemacs-use-package-add-hook magit
  :post-init
  (progn
    ;; add a quick alias to open magit-status in current directory
    (defun dotemacs-eshell-magit-status ()
      "Function to open magit-status for the current directory"
      (interactive)
      (magit-status default-directory))
    (defalias 's 'dotemacs-eshell-magit-status)))


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
    (after "evil-leader"
      (evil-leader/set-key "bf" 'reveal-in-finder))

    ;; this is only applicable to GUI mode
    (when (display-graphic-p)
      (global-set-key (kbd "M-=") 'dotemacs-scale-up-font)
      (global-set-key (kbd "M--") 'dotemacs-scale-down-font)
      (global-set-key (kbd "M-0") 'dotemacs-reset-font-size)
      (global-set-key (kbd "M-n") 'new-frame)
      (global-set-key (kbd "M-v") 'yank)
      (global-set-key (kbd "M-c") 'evil-yank) ; kill-ring-save
      (global-set-key (kbd "M-X") 'kill-region)
      (global-set-key (kbd "M-z") 'undo-tree-undo)
      (global-set-key (kbd "M-Z") 'undo-tree-redo)
      (global-set-key (kbd "M-s") 'save-buffer)))
  :config
  (when (display-graphic-p)
    (setq ns-pop-up-frames nil            ; Don't pop up new frames from the
                                          ; workspace
          mac-option-key-is-meta t
          mac-option-modifier 'meta       ; Option is simply the natural Meta
          mac-command-key-is-meta t
          mac-command-modifier 'meta      ; But command is a lot easier to hit
          mac-right-command-modifier 'left
          mac-right-option-modifier 'none ; Keep right option for accented input
          ;; Just in case we ever need these keys
          mac-function-modifier 'hyper)))

(use-package init-macosx              ; Personal OS X tools
  :if (eq system-type 'darwin)
  :load-path "config/"
  :defer t
  :commands (dotemacs-id-of-bundle
             dotemacs-path-of-bundle
             dotemacs-homebrew-prefix
             dotemacs-homebrew-installed-p
             dotemacs-open-current-file
             dotemacs-chomp
             dotemacs-get-keychain-password)
  :init
  :config
  (progn
    ;; Ignore .DS_Store files with ido mode
    (add-to-list 'ido-ignore-files "\\.DS_Store"))
  :bind ("C-c f o" . dotemacs-open-current-file))

(use-package osx-trash                  ; Trash support for OS X
  :if (eq system-type 'darwin)
  :ensure t
  :init (osx-trash-setup))

(use-package pbcopy
  :if (and (eq system-type 'darwin) (not (display-graphic-p)))
  :ensure t
  :init (turn-on-pbcopy))

(use-package reveal-in-finder
  :if (eq system-type 'darwin)
  :ensure t
  :commands reveal-in-finder)


;;; User interface

;; Get rid of tool bar, menu bar and scroll bars.  On OS X we preserve the menu
;; bar, since the top menu bar is always visible anyway, and we'd just empty it
;; which is rather pointless.
(when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
  (tool-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
  (scroll-bar-mode -1))
;; tooltips in echo-aera
(when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
  (tooltip-mode -1))
(setq tooltip-use-echo-area t)
(unless (eq system-type 'darwin)
  (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
    (menu-bar-mode -1)))
;; for convenience and user support
(unless (fboundp 'tool-bar-mode)
  (dotemacs-message (concat "No graphical support detected, you won't be"
                             "able to launch a graphical instance of Emacs"
                             "with this build.")))


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

;; default theme
(let ((default-theme (car dotemacs-themes)))
  (dotemacs-load-theme default-theme)
  ;; used to prevent automatic deletion of used packages
  (setq dotemacs-used-theme-packages
        (delq nil (mapcar 'dotemacs-get-theme-package
                          dotemacs-themes)))
  (setq dotemacs--cur-theme default-theme)
  (setq-default dotemacs--cycle-themes (cdr dotemacs-themes)))

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

(use-package fill-column-indicator
  :defer t
  :ensure t
  :init
  (progn
    (setq fci-rule-width 1)
    (setq fci-rule-color "#D0BF8F")
    ;; manually register the minor mode since it does not define any
    ;; lighter
    (push '(fci-mode "") minor-mode-alist)
    (dotemacs-add-toggle fill-column-indicator
                          :status fci-mode
                          :on (turn-on-fci-mode)
                          :off (turn-off-fci-mode)
                          :documentation "Display the fill column indicator."
                          :evil-leader "tf"))
  :diminish fci-mode)

(use-package zoom-frm
  :commands (zoom-frm-unzoom
             zoom-frm-out
             zoom-frm-in)
  :ensure t
  :init
  (progn
    (dotemacs-define-micro-state zoom-frm
      :doc "[+] zoom frame in [-] zoom frame out [=] reset zoom"
      :evil-leader "zf"
      :use-minibuffer t
      :bindings
      ("+" dotemacs-zoom-frm-in)
      ("-" dotemacs-zoom-frm-out)
      ("=" dotemacs-zoom-frm-unzoom))

    (defun dotemacs-zoom-frm-do (arg)
      "Perform a zoom action depending on ARG value."
      (let ((zoom-action (cond ((eq arg 0) 'zoom-frm-unzoom)
                               ((< arg 0) 'zoom-frm-out)
                               ((> arg 0) 'zoom-frm-in)))
            (fm (cdr (assoc 'fullscreen (frame-parameters))))
            (fwp (* (frame-char-width) (frame-width)))
            (fhp (* (frame-char-height) (frame-height))))
        (when (equal fm 'maximized)
          (toggle-frame-maximized))
        (funcall zoom-action)
        (set-frame-size nil fwp fhp t)
        (when (equal fm 'maximized)
          (toggle-frame-maximized))))

    (defun dotemacs-zoom-frm-in ()
      "zoom in frame, but keep the same pixel size"
      (interactive)
      (dotemacs-zoom-frm-do 1))

    (defun dotemacs-zoom-frm-out ()
      "zoom out frame, but keep the same pixel size"
      (interactive)
      (dotemacs-zoom-frm-do -1))

    (defun dotemacs-zoom-frm-unzoom ()
      "Unzoom current frame, keeping the same pixel size"
      (interactive)
      (dotemacs-zoom-frm-do 0))

    ;; Font size, either with ctrl + mouse wheel
    (global-set-key (kbd "<C-wheel-up>") 'dotemacs-zoom-frm-in)
    (global-set-key (kbd "<C-wheel-down>") 'dotemacs-zoom-frm-out)))

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

(use-package rfringe
  :ensure t
  :defer t)


;;; Minibuffer and Helm
;; Display current keystrokes almost immediately in mini buffer
(setq echo-keystrokes 0.02)

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
  :bind (("C-c h h" . helm-resume))
  :defer 1
  :commands dotemacs-helm-find-files
  :init
  (progn
    (after "helm-config"
           (warn "`helm-config' loaded! Get rid of it ASAP!"))

    (setq helm-prevent-escaping-from-minibuffer t
          helm-bookmark-show-location t
          helm-display-header-line nil
          helm-split-window-in-side-p t
          helm-always-two-windows t
          helm-echo-input-in-header-line t)

    (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

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

    (setq helm-display-function 'dotemacs-display-helm-at-bottom)

    (add-hook 'helm-after-initialize-hook 'dotemacs-helm-prepare-display)
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

    (after "helm-mode" ; required
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
  :bind (("C-c h M-:" . helm-eval-expression-with-eldoc)
         ("C-c h *"   . helm-calcul-expression)))

(use-package helm-color                 ; Input colors with Helm
  :ensure helm
  :bind (("C-c h c" . helm-colors)))

(use-package helm-unicode               ; Unicode input with Helm
  :ensure t
  :bind ("C-c h 8" . helm-unicode))

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
             dotemacs-do-not-kill-important-buffers
             dotemacs-ibuffer-group-by-modes
             dotemacs-ibuffer-group-by-projects)
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

(use-package ibuffer                    ; Better buffer list
  :bind (([remap list-buffers] . ibuffer))
  ;; Show VC Status in ibuffer
  :init
  (progn
    (after "evil-evilified-state"
      (evil-leader/set-key "bB" 'ibuffer)
      (evilify ibuffer-mode ibuffer-mode-map))

    (global-set-key (kbd "C-x C-b") 'ibuffer)
    (add-hook 'ibuffer-hook 'dotemacs-ibuffer-group-by-modes)

    (setq ibuffer-expert t
          ibuffer-show-empty-filter-groups nil)))

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
  :defer t
  :init
  (progn
    (add-hook 'ibuffer-hook 'dotemacs-ibuffer-group-by-projects)))

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
  :config
  (progn
    (popwin-mode 1)
    (after "evil-leader"
      (evil-leader/set-key "wpm" 'popwin:messages)
      (evil-leader/set-key "wpp" 'popwin:close-popup-window))

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
                     popwin:special-display-config)))))

;;; File handling

;; don't create backup~ files
(setq backup-by-copying t
      make-backup-files nil
      create-lockfiles nil)

;; Keep backup files out of the way
(setq backup-directory-alist `((".*" . ,(concat dotemacs-cache-directory "backups"))))

(defconst dotemacs-auto-save-directory
  (expand-file-name (concat dotemacs-cache-directory "auto-save/"))
  "dotemacs auto-save directory")

;; Auto-save file
(setq auto-save-default (not (null dotemacs-auto-save-file-location)))
(setq auto-save-list-file-prefix (concat dotemacs-auto-save-directory))
;; always save TRAMP URLs to cache directory no matter what is the value
;; of `dotemacs-auto-save-file-location'
(let ((autosave-dir (concat dotemacs-auto-save-directory "dist/")))
  (setq auto-save-file-name-transforms
        `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,autosave-dir  t)))
  (unless (or (file-exists-p autosave-dir)
              (null dotemacs-auto-save-file-location))
    (make-directory autosave-dir t)))
;; Choose auto-save location
(case dotemacs-auto-save-file-location
  (cache (let ((autosave-dir (concat dotemacs-auto-save-directory "site/")))
           (add-to-list 'auto-save-file-name-transforms
                        `(".*" ,autosave-dir t) 'append)
           (unless (file-exists-p autosave-dir)
             (make-directory autosave-dir t))))
  (original (setq auto-save-visited-file-name t))
  (_ (setq auto-save-default nil
           auto-save-list-file-prefix nil)))

;; Transparently open compressed files
(auto-compression-mode t)

;; Delete files to trash
(setq delete-by-moving-to-trash t)

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
    ;; maybe absolute or relative name of the `ls' program used by
    ;; `insert-directory'.
    ;; brew install coreutils
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
    (setq bookmark-save-flag 1 ;; autosave each change
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

(use-package fasd
  :ensure t
  :init
  (progn
    (global-fasd-mode 1)

    (defun fasd-find-file-only ()
      (interactive)
      (fasd-find-file -1))

    (defun fasd-find-directory-only ()
      (interactive)
      (fasd-find-file 1))

    (after "evil-leader"
      (dotemacs-declare-prefix "fa" "fasd-find")
      (evil-leader/set-key "fad" 'fasd-find-directory-only)
      (evil-leader/set-key "faf" 'fasd-find-file-only)
      (evil-leader/set-key "fas" 'fasd-find-file))

    ;; we will fall back to using the default completing-read function, which is helm once helm is loaded.
    (setq fasd-completing-read-function 'nil)))

;; Additional bindings for built-ins
(bind-key "C-c f v d" #'add-dir-local-variable)
(bind-key "C-c f v l" #'add-file-local-variable)
(bind-key "C-c f v p" #'add-file-local-variable-prop-line)


;;; Navigation and scrolling

;; Handy way of getting back to previous places.
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(setq scroll-margin 3                   ; 0 to drag the point along while scrolling
      scroll-conservatively 1000        ; Never recenter the screen while scrolling
      scroll-error-top-bottom t         ; Move to beg/end of buffer before
                                        ; signalling an error
      ;; scroll-preserve-screen-position t
      ;; These settings make trackpad scrolling on OS X much more predictable
      ;; and smooth
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1))

;; Hack to fix a bug with tabulated-list.el
;; see: http://redd.it/2dgy52
(defun tabulated-list-revert (&rest ignored)
  "The `revert-buffer-function' for `tabulated-list-mode'.
It runs `tabulated-list-revert-hook', then calls `tabulated-list-print'."
  (interactive)
  (unless (derived-mode-p 'tabulated-list-mode)
    (error "The current buffer is not in Tabulated List mode"))
  (run-hooks 'tabulated-list-revert-hook)
  ;; hack is here
  ;; (tabulated-list-print t)
  (tabulated-list-print))

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

;; todo: add choice between avy-jump and ace-jump
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

(use-package ace-jump-mode
  :defer t
  :disabled t
  :ensure t
  :init
  (progn
    (evil-leader/set-key "SPC" 'evil-ace-jump-word-mode)
    (evil-leader/set-key "l" 'evil-ace-jump-line-mode))
  :config
  (progn
    (setq ace-jump-mode-scope 'global)
    (evil-leader/set-key "`" 'ace-jump-mode-pop-mark)))

(use-package ace-link                   ; Fast link jumping
  :commands dotemacs-ace-buffer-links
  :init
  (progn
    (after "info"
      (bind-key "C-c j l" #'ace-link-info Info-mode-map)
      '(define-key Info-mode-map "o" 'ace-link-info))
    (after "help-mode"
      (defvar help-mode-map)  ; Silence the byte compiler
      (bind-key "C-c j l" #'ace-link-help help-mode-map)
      '(define-key help-mode-map "o" 'ace-link-help))
    (after "eww"
      '(progn
         (define-key eww-link-keymap "o" 'ace-link-eww)
         (define-key eww-mode-map "o" 'ace-link-eww))))
  :config
  (progn
    (defvar dotemacs--link-pattern "~?/.+\\|\s\\[")
    (defun dotemacs-collect-buffer-links ()
      (let ((end (window-end))
            points)
        (save-excursion
          (goto-char (window-start))
          (while (re-search-forward dotemacs--link-pattern end t)
            (push (+ (match-beginning 0) 1) points))
          (nreverse points))))
    (defun dotemacs-ace-buffer-links ()
      "Ace jump to links in `emacs' buffer."
      (interactive)
      (ali-generic
       (dotemacs-collect-buffer-links)
       (forward-char 1)
       (widget-button-press (point))))))

(use-package ace-window                 ; Fast window switching
  :ensure t
  :defer t
  :bind (("C-x o" . ace-window)
         ("C-c o" . ace-window))
  :init
  (progn
    (after "evil-leader"
    (evil-leader/set-key
      "bM"  'ace-swap-window
      "wC"  'ace-delete-window
      "w <SPC>"  'ace-window))
    ;; set ace-window keys to home-row
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

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

;; The C-d rebinding that most shell-like buffers inherit from
;; comint-mode assumes non-evil configuration with its
;; `comint-delchar-or-maybe-eof' function, so we disable it
(after "comint"
  '(define-key comint-mode-map (kbd "C-d") nil))

;; enable electric indent
(setq electric-indent-mode 1)

;; Text
(setq longlines-show-hard-newlines t)

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
  :ensure t
  :if (unless (version< emacs-version "24.4"))
  :defer t
  :init
  (progn
    (unless (category-docstring ?U)
      (define-category ?U "Uppercase")
      (define-category ?u "Lowercase"))
    (modify-category-entry (cons ?A ?Z) ?U)
    (modify-category-entry (cons ?a ?z) ?u)
    (make-variable-buffer-local 'evil-cjk-word-separating-categories)
    (defun dotemacs-subword-enable-camel-case ()
      "Add support for camel case to subword."
      (if subword-mode
          (push '(?u . ?U) evil-cjk-word-separating-categories)
        (setq evil-cjk-word-separating-categories
              (default-value 'evil-cjk-word-separating-categories))))
    (add-hook 'subword-mode-hook 'dotemacs-subword-enable-camel-case)
    (dotemacs-add-toggle camel-case-motion
                          :status subword-mode
                          :on (subword-mode +1)
                          :off (subword-mode -1)
                          :documentation "Toggle CamelCase motion."
                          :evil-leader "tc")
    (dotemacs-add-toggle camel-case-motion-globally
                          :status subword-mode
                          :on (global-subword-mode +1)
                          :off (global-subword-mode -1)
                          :documentation "Globally toggle CamelCase motion."
                          :evil-leader "t C-c"))
  :diminish (subword-mode . " ⓒ"))

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
  :disabled t
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

    (add-hook 'LaTeX-mode-hook 'smartparens-mode)
    (add-hook 'markdown-mode-hook 'smartparens-mode)
    (add-hook 'inferior-python-mode-hook 'smartparens-mode)

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
  :diminish (smartparens-mode . " ⓟ"))


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

(use-package hi-lock                    ; Custom regexp highlights
  :init (global-hi-lock-mode)
  :diminish hi-lock-mode)


;;; Colors
(use-package init-colors
  :load-path "config/")

(use-package highlight-numbers          ; Fontify number literals
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook #'highlight-numbers-mode)
    (add-hook 'asm-mode-hook (lambda () (highlight-numbers-mode -1)))))

(use-package highlight-indentation
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-add-toggle highlight-indentation
                          :status highlight-indentation-mode
                          :on (highlight-indentation-mode)
                          :off (highlight-indentation-mode -1)
                          :documentation "Highlight indentation levels."
                          :evil-leader "thi")
    (dotemacs-add-toggle highlight-indentation-current-column
                          :status highlight-indentation-current-column-mode
                          :on (highlight-indentation-current-column-mode)
                          :off (highlight-indentation-current-column-mode -1)
                          :documentation "Highlight indentation level at point."
                          :evil-leader "thc")))

(use-package highlight-parentheses
  :defer t
  :ensure t
  :init
  (progn
    (when (eq dotemacs-highlight-delimiters 'current)
      (add-hook 'prog-mode-hook #'highlight-parentheses-mode))
    (evil-leader/set-key "tCp" 'highlight-parentheses-mode)
    (setq hl-paren-colors '("Springgreen3"
                            "IndianRed1"
                            "IndianRed3"
                            "IndianRed4")))
  :config
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold))

(use-package highlight-quoted
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-quoted-mode))

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

(use-package rainbow-delimiters         ; Highlight delimiters by depth,  is a "rainbow parentheses"-like
  :ensure t
  :defer t
  :init
  (progn
    (after "evil-leader"
      (evil-leader/set-key "tCd" 'rainbow-delimiters-mode))

    (when (eq dotemacs-highlight-delimiters 'all)
      (dolist (hook '(text-mode-hook prog-mode-hook))
        (add-hook hook #'rainbow-delimiters-mode)))))

;; Currently it supports Scala (scala-mode2), JavaScript (js-mode and js2-mode),
;; Ruby, Python, Emacs Lisp, Clojure, C, C++, and Java.
(use-package color-identifiers-mode
  :ensure t
  :if (eq dotemacs-colors-engine 'color)
  :commands color-identifiers-mode
  :init
  (progn
    (dolist (mode '(scala js js2 ruby python emacs-lisp clojure c java))
      (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
                (lambda ()
                  (color-identifiers-mode)))))
  :diminish color-identifiers-mode)

(use-package rainbow-identifiers
  :ensure t
  :if (eq dotemacs-colors-engine 'rainbow)
  :commands rainbow-identifiers-mode
  :init
  (progn
    (setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face
          rainbow-identifiers-cie-l*a*b*-saturation 100
          rainbow-identifiers-cie-l*a*b*-lightness 40
          ;; override theme faces
          rainbow-identifiers-faces-to-override '(highlight-quoted-symbol
                                                  font-lock-keyword-face
                                                  font-lock-function-name-face
                                                  font-lock-variable-name-face))

    (dotemacs-add-toggle rainbow-identifier-globally
                         :status rainbow-identifiers-mode
                         :on (rainbow-identifiers-mode)
                         :off (rainbow-identifiers-mode -1)
                         :documentation "Colorize identifiers globally."
                         :evil-leader "tCi")

    (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))
  (colors//tweak-theme-colors dotemacs--cur-theme)

  (defadvice dotemacs-post-theme-init (after colors/post-theme-init activate)
    "Adjust lightness and brightness of rainbow-identifiers on post theme init."
    (colors//tweak-theme-colors dotemacs--cur-theme))

  :config
  (progn
    ;; key bindings
    (evil-leader/set-key "Cis" 'colors/start-change-color-saturation)
    (evil-leader/set-key "Cil" 'colors/start-change-color-lightness)))

(use-package rainbow-mode               ; Fontify color values in code
  :commands rainbow-mode
  :ensure t
  :bind (("C-c t r" . rainbow-mode))
  :init
  (progn
    (evil-leader/set-key "tCc" 'rainbow-mode)
    (dolist (hook '(prog-mode-hook sgml-mode-hook css-mode-hook web-mode-hook))
      (add-hook hook #'rainbow-mode)))
  :diminish rainbow-mode)


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
  :disabled t
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
    (after "compile"
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
  :init (evil-commentary-mode)
  :diminish evil-commentary-mode)

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
  :init
  (progn
    ; (global-evil-matchit-mode)
    (dolist (hook '(LaTeX-mode-hook
                    web-mode-hook
                    mustache-mode-hook
                    handlebars-mode-hook))
      (add-hook hook '(lambda () (evil-matchit-mode))))))

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
    (defun dotemacs-surround-add-pair (trigger begin-or-fun &optional end)
      "Add a surround pair.
If `end' is nil `begin-or-fun' will be treated as a fun."
      (push (cons (if (stringp trigger)
                    (string-to-char trigger)
                    trigger)
                  (if end
                    (cons begin-or-fun end)
                    begin-or-fun))
            evil-surround-pairs-alist))

    (add-to-hooks (lambda ()
                    (dotemacs-surround-add-pair "`" "`"  "'"))
                  '(emacs-lisp-mode-hook lisp-mode-hook))

    (add-to-hooks (lambda ()
                    (dotemacs-surround-add-pair "~" "```"  "```"))
                  '(markdown-mode-hook))

    (add-hook 'LaTeX-mode-hook (lambda ()
                                 (dotemacs-surround-add-pair "~" "\\texttt{" "}")
                                 (dotemacs-surround-add-pair "=" "\\verb=" "=")
                                 (dotemacs-surround-add-pair "/" "\\emph{" "}")
                                 (dotemacs-surround-add-pair "*" "\\textbf{" "}")
                                 (dotemacs-surround-add-pair "P" "\\(" "\\)")))
    (add-to-hooks (lambda ()
                    (dotemacs-surround-add-pair "c" ":class:`" "`")
                    (dotemacs-surround-add-pair "f" ":func:`" "`")
                    (dotemacs-surround-add-pair "m" ":meth:`" "`")
                    (dotemacs-surround-add-pair "a" ":attr:`" "`")
                    (dotemacs-surround-add-pair "e" ":exc:`" "`"))
                  '(rst-mode-hook python-mode-hook))

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
    (define-key evil-visual-state-map (kbd "*")
      'evil-visualstar/begin-search-forward)
    (define-key evil-visual-state-map (kbd "#")
      'evil-visualstar/begin-search-backward)))

(use-package init-bindings
  :load-path "config/")


;;; Text editing
(use-package tildify
  :bind (("C-c e t" . tildify-region))
  :init (dolist (hook '(markdown-mode-hook
                        latex-mode-hook
                        rst-mode-hook))
          (add-hook hook #'tildify-mode))
  ;; Use the right space for LaTeX
  :config (add-hook 'LaTeX-mode-hook
                    (lambda () (setq-local tildify-space-string "~"))))

(use-package typo
  :ensure t
  :init (progn
          (typo-global-mode)
          (dolist (hook '(markdown-mode-hook rst-mode-hook))
            (add-hook hook 'typo-mode)))
  :diminish (typo-mode . "𝕿"))


;;; LaTeX with AUCTeX
(dotemacs-defvar-company-backends LaTeX-mode)

(use-package init-latex
  :load-path "config/")

(use-package tex-site                   ; AUCTeX initialization
  :ensure auctex)

(use-package tex                        ; TeX editing/processing
  :ensure auctex
  :defer t
  :init
  (progn
    (setq TeX-command-default dotemacs-latex-build-command
          TeX-auto-save t               ; Automatically save style information
          TeX-parse-self t              ; Parse documents to provide completion
                                        ; for packages, etc.
          TeX-electric-sub-and-superscript t ; Automatically insert braces after
                                        ; sub- and superscripts in math mode
          TeX-electric-math '("\\(" "\\)")
          ;; Don't insert magic quotes right away.
          TeX-quote-after-quote t
          ;; Don't ask for confirmation when cleaning
          TeX-clean-confirm nil
          TeX-syntactic-comment t
          ;; Provide forward and inverse search with SyncTeX
          TeX-source-correlate-mode t
          TeX-source-correlate-start-server nil
          ;; Setup reftex style (RefTeX is supported through extension)
          reftex-use-fonts t
          TeX-source-correlate-method 'synctex
          ;; Don't insert line-break at inline math
          LaTeX-fill-break-at-separators nil)

    (setq-default TeX-master nil        ; Ask for the master file
                  TeX-engine 'luatex    ; Use a modern engine
                  ;; Redundant in 11.88, but keep for older AUCTeX
                  TeX-PDF-mode t)

    (when dotemacs-latex-enable-auto-fill
      (add-hook 'LaTeX-mode-hook 'latex/auto-fill-mode))
    (add-hook 'LaTeX-mode-hook 'latex-math-mode))
  :config
  (progn
    ;; Move to chktex
    (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s")

    ;; Key bindings for plain TeX
    (evil-leader/set-key-for-mode 'tex-mode
      "m\\" 'TeX-insert-macro
      "mb" 'latex/build
      "mC" 'TeX-command-master
      ;; Find a way to rebind tex-fonts
      "mf" 'TeX-font
      "mv" 'TeX-view)

    ;; Key bindings for LaTeX
    (evil-leader/set-key-for-mode 'latex-mode
      "m\\" 'TeX-insert-macro
      "mb" 'latex/build
      "mc" 'LaTeX-close-environment
      "mC" 'TeX-command-master
      "me" 'LaTeX-environment
      ;; Find a way to rebind tex-fonts
      "mf" 'TeX-font
      "mhd" 'TeX-doc
      "mi" 'LaTeX-insert-item
      ;; TeX-doc is a very slow function
      "mpb" 'preview-buffer
      "mpc" 'preview-clearout
      "mpd" 'preview-document
      "mpe" 'preview-environment
      "mpf" 'preview-cache-preamble
      "mpp" 'preview-at-point
      "mpr" 'preview-region
      "mps" 'preview-section
      "mv" 'TeX-view)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook LaTeX-mode))))

(use-package company-auctex
  :if (eq dotemacs-completion-engine 'company)
  :defer t
  :init
  (progn
    (push 'company-auctex-labels company-backends-LaTeX-mode)
    (push 'company-auctex-bibs company-backends-LaTeX-mode)
    (push '(company-auctex-macros company-auctex-symbols company-auctex-environments)
          company-backends-LaTeX-mode)))

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

(use-package reftex                     ; TeX/BibTeX cross-reference management
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
  :config
  (progn
    (setq reftex-plug-into-AUCTeX '(nil nil t t t))

    (evil-leader/set-key-for-mode 'latex-mode
      "mrc"    'reftex-citation
      "mrg"    'reftex-grep-document
      "mri"    'reftex-index-selection-or-word
      "mrI"    'reftex-display-index
      "mr C-i" 'reftex-index
      "mrl"    'reftex-label
      "mrp"    'reftex-index-phrase-selection-or-word
      "mrP"    'reftex-index-visit-phrases-buffer
      "mrr"    'reftex-reference
      "mrs"    'reftex-search-document
      "mrt"    'reftex-toc
      "mrT"    'reftex-toc-recenter
      "mrv"    'reftex-view-crossref)

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

(dotemacs-use-package-add-hook flycheck
  :post-init
  (add-hook 'LaTeX-mode-hook 'flycheck-mode))

(dotemacs-use-package-add-hook flyspell
  :post-init
  (add-hook 'LaTeX-mode-hook 'flyspell-mode))

(dotemacs-use-package-add-hook yasnippet
  :post-init
  (add-hook 'LaTeX-mode-hook 'dotemacs-load-yasnippet))


;;; Markdown
(use-package init-markdown
  :load-path "config/"
  :defer t)

(use-package markdown-mode              ; Markdown
  :mode (("\\.m[k]d" . markdown-mode)
         ("\\.markdown$" . markdown-mode)
         ("\\.apib$" . markdown-mode))
  :ensure t
  :init
  (progn
    ;; http://www.tychoish.com/posts/imenu-for-markdown-and-writing/
    (setq markdown-imenu-generic-expression
          '(("title"  "^\\(.*\\)[\n]=+$" 1)
            ("h2-"    "^\\(.*\\)[\n]-+$" 1)
            ("h1"   "^# \\(.*\\)$" 1)
            ("h2"   "^## \\(.*\\)$" 1)
            ("h3"   "^### \\(.*\\)$" 1)
            ("h4"   "^#### \\(.*\\)$" 1)
            ("h5"   "^##### \\(.*\\)$" 1)
            ("h6"   "^###### \\(.*\\)$" 1)
            ("fn"   "^\\[\\^\\(.*\\)\\]" 1))))
  :config
  ;; Don't do terrible things with Github code blocks (```)
  (when (fboundp 'sp-local-pair)
    (sp-local-pair 'markdown-mode "`" nil :actions '(:rem autoskip))
    (sp-local-pair 'markdown-mode "'" nil :actions nil))
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

    (evil-leader/set-key-for-mode 'markdown-mode
      ;; Movement
      "m{"   'markdown-backward-paragraph
      "m}"   'markdown-forward-paragraph
      ;; Completion, and Cycling
      "m]"   'markdown-complete
      ;; Indentation
      "m>"   'markdown-indent-region
      "m<"   'markdown-exdent-region
      ;; Buffer-wide commands
      "mc]"  'markdown-complete-buffer
      "mcm"  'markdown-other-window
      "mcp"  'markdown-preview
      "mce"  'markdown-export
      "mcv"  'markdown-export-and-preview
      "mco"  'markdown-open
      "mcw"  'markdown-kill-ring-save
      "mcc"  'markdown-check-refs
      "mcn"  'markdown-cleanup-list-numbers
      "mcr"  'gh-md-render-buffer
      ;; headings
      "mhi"  'markdown-insert-header-dwim
      "mhI"  'markdown-insert-header-setext-dwim
      "mh1"  'markdown-insert-header-atx-1
      "mh2"  'markdown-insert-header-atx-2
      "mh3"  'markdown-insert-header-atx-3
      "mh4"  'markdown-insert-header-atx-4
      "mh5"  'markdown-insert-header-atx-5
      "mh6"  'markdown-insert-header-atx-6
      "mh!"  'markdown-insert-header-setext-1
      "mh@"  'markdown-insert-header-setext-2
      ;; Insertion of common elements
      "m-"   'markdown-insert-hr
      "mif"  'markdown-insert-footnote
      "mii"  'markdown-insert-image
      "mik"  'dotemacs-insert-keybinding-markdown
      "miI"  'markdown-insert-reference-image
      "mil"  'markdown-insert-link
      "miL"  'markdown-insert-reference-link-dwim
      "miw"  'markdown-insert-wiki-link
      "miu"  'markdown-insert-uri
      ;; Element removal
      "mk"   'markdown-kill-thing-at-point
      ;; List editing
      "mli"  'markdown-insert-list-item
      ;; region manipulation
      "mxb"  'markdown-insert-bold
      "mxi"  'markdown-insert-italic
      "mxc"  'markdown-insert-code
      "mxq"  'markdown-insert-blockquote
      "mxQ"  'markdown-blockquote-region
      "mxp"  'markdown-insert-pre
      "mxP"  'markdown-pre-region
      ;; Following and Jumping
      "mN"   'markdown-next-link
      "mo"   'markdown-follow-thing-at-point
      "mP"   'markdown-previous-link
      "m <RET>" 'markdown-jump)

    ;; Header navigation in normal state movements
    (evil-define-key 'normal markdown-mode-map
      "gj" 'outline-forward-same-level
      "gk" 'outline-backward-same-level
      "gh" 'outline-up-heading
      ;; next visible heading is not exactly what we want but close enough
      "gl" 'outline-next-visible-heading)

    ;; Promotion, Demotion
    (define-key markdown-mode-map (kbd "M-h") 'markdown-promote)
    (define-key markdown-mode-map (kbd "M-j") 'markdown-move-down)
    (define-key markdown-mode-map (kbd "M-k") 'markdown-move-up)
    (define-key markdown-mode-map (kbd "M-l") 'markdown-demote)))

(use-package markdown-toc
  :ensure t
  :defer t)

(use-package mmm-mode
  :commands mmm-parse-buffer
  :ensure t
  :init
  (evil-leader/set-key-for-mode 'markdown-mode
    ;; Highlight code blocks
    "mcs"   'mmm-parse-buffer)
  :config
  (progn
    (mmm-add-classes '((markdown-python
                        :submode python-mode
                        :face mmm-declaration-submode-face
                        :front "^```python[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-html
                        :submode web-mode
                        :face mmm-declaration-submode-face
                        :front "^```html[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-java
                        :submode java-mode
                        :face mmm-declaration-submode-face
                        :front "^```java[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-ruby
                        :submode ruby-mode
                        :face mmm-declaration-submode-face
                        :front "^```ruby[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-c
                        :submode c-mode
                        :face mmm-declaration-submode-face
                        :front "^```c[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-c++
                        :submode c++-mode
                        :face mmm-declaration-submode-face
                        :front "^```c\+\+[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-elisp
                        :submode emacs-lisp-mode
                        :face mmm-declaration-submode-face
                        :front "^```elisp[\n\r]+"
                        :back "^```$")))
    (setq mmm-global-mode t)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-python)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-java)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-ruby)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-c)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-c++)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-elisp)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-html)))

(dotemacs-use-package-add-hook flyspell
  :post-init
  (add-hook 'markdown-mode-hook 'flyspell-mode))


;;; Other markup languages
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

(dotemacs-use-package-add-hook flycheck
  :post-init
  (add-hook 'rst-mode-hook 'flycheck-mode))

(use-package mustache-mode              ; Mustache mode
  :ensure t
  :defer t
  :mode (("\\.mustache$" . mustache-mode)))

(use-package handlebars-mode
  :ensure t
  :mode (("\\.hbs$" . handlebars-mode)
         ("\\.handlebars$" . handlebars-mode))
  :init
  (progn
    (after "flycheck"
      (when-let (handlebars (executable-find "handlebars"))
                (setq flycheck-handlebars-executable handlebars)))))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (add-hook 'handlebars-mode-hook 'flycheck-mode))

(use-package jira-markup-mode           ; Jira markup
  :ensure t
  :defer t)

(use-package graphviz-dot-mode          ; Graphviz
  :ensure t
  :defer t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package systemd                    ; Mode for systemd unit files
  :ensure t
  :defer t)


;;; Programming utilities
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


;;; Emacs Lisp
(dotemacs-defvar-company-backends emacs-lisp-mode)

(bind-key "C-c u d" #'toggle-debug-on-error)

(use-package init-elisp             ; Personal tools for Emacs Lisp
  :load-path "config/"
  :commands (dotemacs-elisp-find-cask-file
             dotemacs-add-use-package-to-imenu)
  :init (progn
          (add-hook 'emacs-lisp-mode-hook #'dotemacs-add-use-package-to-imenu)

          (add-to-hook 'emacs-lisp-mode
                       '(lambda ()
                          (dotemacs-define-text-object ";"
                                                       "elisp-comment"
                                                       ";; "
                                                       "")))

          (after "lisp-mode"
            (bind-key "C-c f c" #'dotemacs-elisp-find-cask-file
                      emacs-lisp-mode-map))))

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
  :commands (flycheck-cask-setup)
  :init (add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(use-package flycheck-package           ; Check package conventions with Flycheck
  :ensure t
  :defer t
  :init (after "flycheck" (flycheck-package-setup)))

(use-package slime
  :ensure t
  :disabled t
  :commands slime-mode
  :init
  (progn
    (setq slime-contribs '(slime-fancy
                           slime-indentation
                           slime-sbcl-exts
                           slime-scratch)
          inferior-lisp-program "sbcl")
    ;; enable fuzzy matching in code buffer and SLIME REPL
    (setq slime-complete-symbol*-fancy t)
    (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
    ;; enabel smartparen in code buffer and SLIME REPL
    ;; (add-hook 'slime-repl-mode-hook #'smartparens-strict-mode)
    (defun slime/disable-smartparens ()
      (smartparens-strict-mode -1)
      (turn-off-smartparens-mode))
    (add-hook 'slime-repl-mode-hook #'slime/disable-smartparens)
    (add-to-hooks 'slime-mode '(lisp-mode-hook scheme-mode-hook)))
  :config
  (progn
    (slime-setup)
    (dolist (m `(,slime-mode-map ,slime-repl-mode-map))
      (define-key m [(tab)] 'slime-fuzzy-complete-symbol))
    (dolist (m '(lisp-mode
                 scheme-mode))
      (evil-leader/set-key-for-mode m
        "mcc" 'slime-compile-file
        "mcC" 'slime-compile-and-load-file
        "mcf" 'slime-compile-defun
        "mcr" 'slime-compile-region

        "meb" 'slime-eval-buffer
        "mef" 'slime-eval-defun
        "mee" 'slime-eval-last-sexp
        "mer" 'slime-eval-region

        "mgg" 'slime-inspect-definition
        "mgn" 'slime-next-note
        "mgN" 'slime-previous-note
        "mgp" 'slime-previous-note

        "mha" 'slime-apropos
        "mhd" 'slime-disassemble-symbol
        "mhh" 'slime-describe-function
        "mhH" 'slime-hyperspec-lookup

        "mse" 'slime-eval-last-expression-in-repl
        "msi" 'slime
        "msq" 'slime-quit-lisp

        "mtf" 'slime-toggle-fancy-trace))))

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

(use-package visual-regexp-steroids
  :defer t)

(use-package macrostep                  ; Interactively expand macros in code
  :ensure t
  :defer t
  :mode ("\\*.el\\'" . emacs-lisp-mode)
  :init
  (progn
    (after "lisp-mode"
           (bind-key "C-c e e" #'macrostep-expand emacs-lisp-mode-map)
           (bind-key "C-c e e" #'macrostep-expand lisp-interaction-mode-map))

    (dotemacs-define-micro-state macrostep
      :doc "[e] expand [c] collapse [n/N] next/previous [q] quit"
      :disable-evil-leader t
      :persistent t
      :evil-leader-for-mode (emacs-lisp-mode . "mdm")
      :bindings
      ("e" macrostep-expand)
      ("c" macrostep-collapse)
      ("n" macrostep-next-macro)
      ("N" macrostep-prev-macro)
      ("q" macrostep-collapse-all :exit t))))

(use-package ielm                       ; Emacs Lisp REPL
  :bind (("C-c z" . ielm))
  :config
  (defun ielm-indent-line ()
    (interactive)
    (let ((current-point (point)))
      (save-restriction
        (narrow-to-region (search-backward-regexp "^ELISP>") (goto-char current-point))
        (lisp-indent-line)))))

(use-package elisp-mode                  ; Emacs Lisp editing
  :defer t
  :interpreter ("emacs" . emacs-lisp-mode)
  :mode ("/Cask\\'" . emacs-lisp-mode)
  :config
  (progn
    (require 'ert)
    (evil-leader/set-key-for-mode 'emacs-lisp-mode
      "me$" 'lisp-state-eval-sexp-end-of-line
      "meb" 'eval-buffer
      "mec" 'dotemacs-eval-current-form
      "mee" 'eval-last-sexp
      "mer" 'dotemacs-eval-region
      "mef" 'eval-defun
      "mel" 'lisp-state-eval-sexp-end-of-line
      "m,"  'lisp-state-toggle-lisp-state
      "mtb" 'dotemacs-ert-run-tests-buffer
      "mtq" 'ert)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (push 'company-capf company-backends-emacs-lisp-mode)
      (dotemacs-add-company-hook emacs-lisp-mode))))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
    (add-hook 'lisp-mode-hook 'flycheck-mode)))


;;; Scala
(use-package init-scala
  :load-path "config/")

(use-package flycheck-auto-scalastyle   ; Scalastyle setup
  :load-path "config/"
  :commands (flycheck-auto-scalastyle-setup)
  :init
  (after "scala-mode2"
    (add-hook 'flycheck-mode-hook #'flycheck-auto-scalastyle-setup)))

(use-package noflet
  :ensure t
  :defer t)

(use-package scala-mode2                ; Scala editing
  :ensure t
  :defer t
  :init
  (dolist (ext '(".cfe" ".cfs" ".si" ".gen" ".lock"))
    (add-to-list 'completion-ignored-extensions ext))
  :config
  (progn
    (evil-define-key 'normal scala-mode-map "J" 'dotemacs-scala-join-line)

    ;; Compatibility with `aggressive-indent'
    (custom-set-variables
     '(scala-indent:align-forms t)
     '(scala-indent:align-parameters t)
     '(scala-indent:default-run-on-strategy scala-indent:operator-strategy))

    (require 'noflet)

    (defadvice scala-indent:indent-code-line (around retain-trailing-ws activate)
      "Keep trailing-whitespace when indenting."
      (noflet ((scala-lib:delete-trailing-whitespace ()))
        ad-do-it))))

(use-package flycheck-ensime
  :load-path "config/"
  :defer t
  :commands (flycheck-ensime-setup))

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
  :commands (ensime-mode)
  :init
  (progn
    (add-hook 'ensime-mode-hook 'scala/enable-eldoc)
    (add-hook 'scala-mode-hook 'scala/configure-flyspell)
    (add-hook 'scala-mode-hook 'scala/configure-ensime)
    (add-hook 'scala-mode-hook 'scala/maybe-start-ensime))
  :config
  (progn
    ;; Automatically open new Ensime sessions if needed
    (setq ensime-auto-connect 'always)

    (setq user-emacs-ensime-directory ".cache/ensime")

    ;; Enable Ensime for all Scala buffers.  We don't do this in :init,
    ;; because `ensime-mode' isn't autoloaded, and ensime-mode makes no
    ;; sense before the first session was started anyway
    (add-hook 'scala-mode-hook #'ensime-mode)

    (evil-define-key 'insert ensime-mode-map
      (kbd ".") 'scala/completing-dot
      (kbd "M-.") 'ensime-edit-definition
      (kbd "M-,") 'ensime-pop-find-definition-stack)

    (evil-define-key 'normal ensime-mode-map
      (kbd "M-.") 'ensime-edit-definition
      (kbd "M-,") 'ensime-pop-find-definition-stack)

    (evil-define-key 'normal ensime-popup-buffer-map
      (kbd "q") 'ensime-popup-buffer-quit-function)

    (evil-define-key 'normal ensime-inspector-mode-map
      (kbd "q") 'ensime-popup-buffer-quit-function)

    (evil-define-key 'normal ensime-refactor-info-map
      (kbd "q") 'dotemacs-ensime-refactor-cancel
      (kbd "c") 'dotemacs-ensime-refactor-accept
      (kbd "RET") 'dotemacs-ensime-refactor-accept)

    (evil-define-key 'normal ensime-compile-result-map
      (kbd "g") 'ensime-show-all-errors-and-warnings
      (kbd "TAB") 'forward-button
      (kbd "<backtab>") 'backward-button
      (kbd "M-n") 'forward-button
      (kbd "M-p") 'backward-button
      (kbd "n") 'forward-button
      (kbd "N") 'backward-button)

    (evil-leader/set-key-for-mode 'scala-mode
      "m/"     'ensime-search
      "m?"     'ensime-scalex

      "mbc"     'ensime-sbt-do-compile
      "mbC"     'ensime-sbt-do-clean
      "mbi"     'ensime-sbt-switch
      "mbp"     'ensime-sbt-do-package
      "mbr"     'ensime-sbt-do-run

      "mct"     'ensime-typecheck-current-file
      "mcT"     'ensime-typecheck-all

      "mdA"     'ensime-db-attach
      "mdb"     'ensime-db-set-break
      "mdB"     'ensime-db-clear-break
      "mdC"     'ensime-db-clear-all-breaks
      "mdc"     'ensime-db-continue
      "mdd"     'ensime-db-start
      "mdi"     'ensime-db-inspect-value-at-point
      "mdl"     'ensime-db-list-locals
      "mdn"     'ensime-db-next
      "mdo"     'ensime-db-step-out
      "mdq"     'ensime-db-quit
      "mdr"     'ensime-db-run
      "mds"     'ensime-db-step
      "mdt"     'ensime-db-backtrace

      "mee"     'ensime-print-errors-at-point
      "mel"     'ensime-show-all-errors-and-warnings
      "mes"     'ensime-stacktrace-switch

      "mgg"     'ensime-edit-definition
      "mgi"     'ensime-goto-impl
      "mgt"     'ensime-goto-test

      "mhh"     'ensime-show-doc-for-symbol-at-point
      "mhu"     'ensime-show-uses-of-symbol-at-point
      "mht"     'ensime-print-type-at-point

      "mii"     'ensime-inspect-type-at-point
      "miI"     'ensime-inspect-type-at-point-other-frame
      "mip"     'ensime-inspect-project-package

      "mnF"     'ensime-reload-open-files
      "mns"     'ensime
      "mnS"     'ensime-gen-and-restart

      "mrd"     'ensime-refactor-inline-local
      "mrD"     'ensime-undo-peek
      "mrf"     'ensime-format-source
      "mri"     'ensime-refactor-organize-imports
      "mrm"     'ensime-refactor-extract-method
      "mrr"     'ensime-refactor-rename
      "mrt"     'ensime-import-type-at-point
      "mrv"     'ensime-refactor-extract-local

      "mta"     'ensime-sbt-do-test
      "mtr"     'ensime-sbt-do-test-quick
      "mtt"     'ensime-sbt-do-test-only

      "msa"     'ensime-inf-load-file
      "msb"     'ensime-inf-eval-buffer
      "msB"     'ensime-inf-eval-buffer-switch
      "msi"     'ensime-inf-switch
      "msr"     'ensime-inf-eval-region
      "msR"     'ensime-inf-eval-region-switch

      "mz"      'ensime-expand-selection-command
      )

    ;; Don't use scala checker if ensime mode is active, since it provides
    ;; better error checking.
    (after "flycheck"
      '(progn
         (defun scala/disable-flycheck () (flycheck-mode -1))
         (add-hook 'ensime-mode-hook 'scala/disable-flycheck)))))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (add-hook 'scala-mode-hook 'flycheck-mode))

(use-package ensime-sbt
  :ensure ensime
  :defer t
  ;; Compile on save.
  :config (setq ensime-sbt-perform-on-save "test:compile"))


;;; Python
(dotemacs-defvar-company-backends python-mode)
(dotemacs-defvar-company-backends inferior-python-mode)
(dotemacs-defvar-company-backends pip-requirements-mode)

(use-package init-python
  :load-path "config/")

(use-package cython-mode
  :defer t
  :ensure t
  :init
  (progn
    (evil-leader/set-key-for-mode 'cython-mode
      "mhh" 'anaconda-mode-view-doc
      "mgg"  'anaconda-mode-goto)))

(use-package anaconda-mode              ; Powerful Python backend for Emacs
  :defer t
  :init (add-hook 'python-mode-hook #'anaconda-mode)
  :config
  (progn
    (after "evil-jumper"
      (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
        (evil-jumper--push)))
    (evil-leader/set-key-for-mode 'python-mode
      "mhh" 'anaconda-mode-view-doc
      "mgg"  'anaconda-mode-goto))
  :diminish anaconda-mode)

(use-package pip-requirements           ; requirements.txt files
  :defer t
  :ensure t)

(use-package pony-mode
  :defer t
  :ensure t
  :init
  (progn
    (evil-leader/set-key-for-mode 'python-mode
      ; d*j*ango f*a*bric
      "mjaf" 'pony-fabric
      "mjad" 'pony-fabric-deploy
      ; d*j*ango *f*iles
      "mjfs" 'pony-goto-settings
      "mjfc" 'pony-setting
      "mjft" 'pony-goto-template
      "mjfr" 'pony-resolve
      ; d*j*ango *i*nteractive
      "mjid" 'pony-db-shell
      "mjis" 'pony-shell
      ; d*j*ango *m*anage
      ; not including one-off management commands like "flush" and
      ; "startapp" even though they're implemented in pony-mode,
      ; because this is much handier
      "mjm" 'pony-manage
      ; d*j*ango *r*unserver
      "mjrd" 'pony-stopserver
      "mjro" 'pony-browser
      "mjrr" 'pony-restart-server
      "mjru" 'pony-runserver
      "mjrt" 'pony-temp-server
      ; d*j*ango *s*outh/*s*yncdb
      "mjsc" 'pony-south-convert
      "mjsh" 'pony-south-schemamigration
      "mjsi" 'pony-south-initial
      "mjsm" 'pony-south-migrate
      "mjss" 'pony-syncdb
      ; d*j*ango *t*est
      "mjtd" 'pony-test-down
      "mjte" 'pony-test-goto-err
      "mjto" 'pony-test-open
      "mjtt" 'pony-test
      "mjtu" 'pony-test-up)))

(use-package pyenv-mode
  :defer t
  :ensure t
  :init
  (progn
    (evil-leader/set-key-for-mode 'python-mode
      "mvs" 'pyenv-mode-set
      "mvu" 'pyenv-mode-unset)))

(use-package pyvenv
  :defer t
  :ensure t
  :init
  (evil-leader/set-key-for-mode 'python-mode
    "mV" 'pyvenv-workon))

(use-package pytest
  :defer t
  :ensure t
  :commands (pytest-one
             pytest-pdb-one
             pytest-all
             pytest-pdb-all
             pytest-module
             pytest-pdb-module)
  :init
  (progn
    (evil-leader/set-key-for-mode 'python-mode
      "mTa" 'pytest-pdb-all
      "mta" 'pytest-all
      "mTb" 'pytest-pdb-module
      "mtb" 'pytest-module
      "mTt" 'pytest-pdb-one
      "mtt" 'pytest-one
      "mTm" 'pytest-pdb-module
      "mtm" 'pytest-module))
  :config (add-to-list 'pytest-project-root-files "setup.cfg"))

(use-package python
  :defer t
  :ensure t
  :init
  (progn
    (after "stickyfunc-enhance"
      (add-to-hooks 'python-mode-hook 'dotemacs-lazy-load-stickyfunc-enhance))

    (after "eldoc"
      (add-hook 'python-mode-hook #'eldoc-mode))

    (add-hook 'inferior-python-mode-hook #'inferior-python-setup-hook)
    (add-all-to-hook 'python-mode-hook
                     'python-default
                     'python-setup-shell))
  :config
  (progn
    ;; PEP 8 compliant filling rules, 79 chars maximum
    (add-hook 'python-mode-hook (lambda () (setq fill-column 79)))
    (add-hook 'python-mode-hook #'subword-mode)

    ;; add support for `ahs-range-beginning-of-defun' for python-mode
    (after "auto-highlight-symbol"
      '(add-to-list 'ahs-plugin-bod-modes 'python-mode))

    (evil-leader/set-key-for-mode 'python-mode
      "mcc" 'dotemacs-python-execute-file
      "mcC" 'dotemacs-python-execute-file-focus
      "mdb" 'python-toggle-breakpoint
      "mri" 'python-remove-unused-imports
      "msB" 'python-shell-send-buffer-switch
      "msb" 'python-shell-send-buffer
      "msF" 'python-shell-send-defun-switch
      "msf" 'python-shell-send-defun
      "msi" 'python-start-or-switch-repl
      "msR" 'python-shell-send-region-switch
      "msr" 'python-shell-send-region)

    ;; the default in Emacs is M-n
    (define-key inferior-python-mode-map (kbd "C-j") 'comint-next-input)
    ;; the default in Emacs is M-p and this key binding overrides default C-k
    ;; which prevents Emacs users to kill line
    (define-key inferior-python-mode-map (kbd "C-k") 'comint-previous-input)
    ;; the default in Emacs is M-r; C-r to search backward old output
    ;; and should not be changed
    (define-key inferior-python-mode-map (kbd "C-r") 'comint-history-isearch-backward)
    ;; this key binding is for recentering buffer in Emacs
    ;; it would be troublesome if Emacs user
    ;; Vim users can use this key since they have other key
    (define-key inferior-python-mode-map (kbd "C-l") 'comint-clear-buffer)

    ;; add this optional key binding for Emacs user, since it is unbound
    (define-key inferior-python-mode-map (kbd "C-c M-l") 'comint-clear-buffer)

    (after "smartparens"
      (defadvice python-indent-dedent-line-backspace
          (around python/sp-backward-delete-char activate)
        (let ((pythonp (or (not smartparens-strict-mode)
                           (char-equal (char-before) ?\s))))
          (if pythonp
              ad-do-it
            (call-interactively 'sp-backward-delete-char)))))))

(use-package hy-mode
  :ensure t
  :defer t)

(use-package helm-pydoc
  :defer t
  :ensure t
  :init
  (evil-leader/set-key-for-mode 'python-mode "mhd" 'helm-pydoc))

(use-package flycheck-virtualenv        ; Setup Flycheck by virtualenv
  :load-path "config/"
  :commands (flycheck-virtualenv-setup)
  :init (add-hook 'flycheck-mode-hook #'flycheck-virtualenv-setup))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (add-hook 'python-mode-hook 'flycheck-mode))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook python-mode)
      (push 'company-capf company-backends-pip-requirements-mode)
      (dotemacs-add-company-hook pip-requirements-mode)

      (push 'company-capf company-backends-inferior-python-mode)
      (dotemacs-add-company-hook inferior-python-mode)
      (add-hook 'inferior-python-mode-hook (lambda ()
                                             (setq-local company-minimum-prefix-length 0)
                                             (setq-local company-idle-delay 0.5))))))

(use-package company-anaconda           ; Python backend for Company
  :ensure t
  :if (eq dotemacs-completion-engine 'company)
  :defer t
  :init
  (progn
    (push 'company-anaconda company-backends-python-mode)))


;;; Ruby
(dotemacs-defvar-company-backends enh-ruby-mode)

(use-package rbenv
  :ensure t
  :disabled t
  :if (eq dotemacs-ruby-version-manager 'rbenv)
  :defer t
  :init (global-rbenv-mode)
  :config (add-hook 'enh-ruby-mode-hook
                    (lambda () (rbenv-use-corresponding))))

(use-package rvm
  :ensure t
  :disabled t
  :defer t
  :if (eq dotemacs-ruby-version-manager 'rvm)
  :init (rvm-use-default)
  :config (add-hook 'enh-ruby-mode-hook
                    (lambda () (rvm-activate-corresponding-ruby))))

(use-package enh-ruby-mode
  :ensure t
  :mode (("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . enh-ruby-mode)
         ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\)\\'" . enh-ruby-mode))
  :config
  (progn
    (setq enh-ruby-deep-indent-paren nil
          enh-ruby-hanging-paren-deep-indent-level 2)
    (sp-with-modes '(ruby-mode enh-ruby-mode)
      (sp-local-pair "{" "}"
                     :pre-handlers '(sp-ruby-pre-handler)
                     :post-handlers '(sp-ruby-post-handler (dotemacs-smartparens-pair-newline-and-indent "RET"))
                     :suffix ""))))

(use-package ruby-tools
  :defer t
  :ensure t
  :init
  (add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)
  :diminish ruby-tools-mode)

(use-package bundler
  :defer t
  :ensure t
  :init
  (progn
    (evil-leader/set-key-for-mode 'enh-ruby-mode "mbc" 'bundle-check)
    (evil-leader/set-key-for-mode 'enh-ruby-mode "mbi" 'bundle-install)
    (evil-leader/set-key-for-mode 'enh-ruby-mode "mbs" 'bundle-console)
    (evil-leader/set-key-for-mode 'enh-ruby-mode "mbu" 'bundle-update)
    (evil-leader/set-key-for-mode 'enh-ruby-mode "mbx" 'bundle-exec)))

(use-package projectile-rails
  :if (when dotemacs-ruby-enable-ruby-on-rails-support)
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'enh-ruby-mode-hook 'projectile-rails-on))
  :diminish (projectile-rails-mode . " ⇋")
  :config
  (progn
    ;; Find files
    (evil-leader/set-key-for-mode 'enh-ruby-mode
      "mrfa" 'projectile-rails-find-locale
      "mrfc" 'projectile-rails-find-controller
      "mrfe" 'projectile-rails-find-environment
      "mrff" 'projectile-rails-find-feature
      "mrfh" 'projectile-rails-find-helper
      "mrfi" 'projectile-rails-find-initializer
      "mrfj" 'projectile-rails-find-javascript
      "mrfl" 'projectile-rails-find-lib
      "mrfm" 'projectile-rails-find-model
      "mrfn" 'projectile-rails-find-migration
      "mrfo" 'projectile-rails-find-log
      "mrfp" 'projectile-rails-find-spec
      "mrfr" 'projectile-rails-find-rake-task
      "mrfs" 'projectile-rails-find-stylesheet
      "mrft" 'projectile-rails-find-test
      "mrfu" 'projectile-rails-find-fixture
      "mrfv" 'projectile-rails-find-view
      "mrfy" 'projectile-rails-find-layout
      "mrf@" 'projectile-rails-find-mailer
      ;; Goto file
      "mrgc" 'projectile-rails-find-current-controller
      "mrgd" 'projectile-rails-goto-schema
      "mrge" 'projectile-rails-goto-seeds
      "mrgh" 'projectile-rails-find-current-helper
      "mrgj" 'projectile-rails-find-current-javascript
      "mrgg" 'projectile-rails-goto-gemfile
      "mrgm" 'projectile-rails-find-current-model
      "mrgn" 'projectile-rails-find-current-migration
      "mrgp" 'projectile-rails-find-current-spec
      "mrgr" 'projectile-rails-goto-routes
      "mrgs" 'projectile-rails-find-current-stylesheet
      "mrgt" 'projectile-rails-find-current-test
      "mrgu" 'projectile-rails-find-current-fixture
      "mrgv" 'projectile-rails-find-current-view
      "mrgz" 'projectile-rails-goto-spec-helper
      "mrg." 'projectile-rails-goto-file-at-point
      ;; Rails external commands
      "mrcc" 'projectile-rails-generate
      "mri" 'projectile-rails-console
      "mrr:" 'projectile-rails-rake
      "mrxs" 'projectile-rails-server
      ;; Refactoring 'projectile-rails-mode
      "mrRx" 'projectile-rails-extract-region)
    ;; Ex-commands
    (evil-ex-define-cmd "A" 'projectile-toggle-between-implementation-and-test)))

(use-package robe                       ; Ruby backend for Emacs
  :defer t
  :ensure t
  :init
  (progn
    (add-hook 'enh-ruby-mode-hook 'robe-mode)
    (when (eq dotemacs-completion-engine 'company)
      (push 'company-robe company-backends-enh-ruby-mode)))
  :diminish robe-mode
  :config
  (progn
    ;; robe mode specific
    (evil-leader/set-key-for-mode 'enh-ruby-mode "mgg" 'robe-jump)
    (evil-leader/set-key-for-mode 'enh-ruby-mode "mhd" 'robe-doc)
    (evil-leader/set-key-for-mode 'enh-ruby-mode "mrsr" 'robe-rails-refresh)
    ;; inf-enh-ruby-mode
    (evil-leader/set-key-for-mode 'enh-ruby-mode "msf" 'ruby-send-definition)
    (evil-leader/set-key-for-mode 'enh-ruby-mode "msF" 'ruby-send-definition-and-go)
    (evil-leader/set-key-for-mode 'enh-ruby-mode "msi" 'robe-start)
    (evil-leader/set-key-for-mode 'enh-ruby-mode "msr" 'ruby-send-region)
    (evil-leader/set-key-for-mode 'enh-ruby-mode "msR" 'ruby-send-region-and-go)
    (evil-leader/set-key-for-mode 'enh-ruby-mode "mss" 'ruby-switch-to-inf)))

(use-package yaml-mode                  ; YAML
  :ensure t
  :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
         ("Procfile\\'" . yaml-mode))
  :config (add-hook 'yaml-mode-hook
                    '(lambda ()
                       ; (electric-indent-local-mode -1)
                       (run-hooks 'prog-mode-hook)
                       (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package feature-mode               ; Feature files for ecukes/cucumber
  :ensure t
  :if (when dotemacs-ruby-enable-ruby-on-rails-support)
  :mode ("\\.feature\\'" . feature-mode)
  :config
  (progn
    ;; Add standard hooks for Feature Mode, since it is no derived mode
    (add-hook 'feature-mode-hook #'whitespace-mode)
    (add-hook 'feature-mode-hook #'whitespace-cleanup-mode)
    (add-hook 'feature-mode-hook #'flyspell-mode)))

(use-package haml-mode
  :ensure t
  :if (when dotemacs-ruby-enable-ruby-on-rails-support)
  :defer t)

(use-package slim-mode
  :ensure t
  :if (when dotemacs-ruby-enable-ruby-on-rails-support)
  :defer t)

(use-package ruby-test-mode
  :defer t
  :ensure t
  :init (add-hook 'enh-ruby-mode-hook 'ruby-test-mode)
  :diminish ruby-test-mode
  :config
  (progn
    (evil-leader/set-key-for-mode 'enh-ruby-mode "mtb" 'ruby-test-run)
    (evil-leader/set-key-for-mode 'enh-ruby-mode "mtt" 'ruby-test-run-at-point)))

(use-package inf-ruby                   ; Ruby REPL
  :ensure t
  :defer t
  :init (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
  :config
  ;; Easily switch to Inf Ruby from compilation modes to Inf Ruby
  (inf-ruby-switch-setup))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (progn
    (add-hook 'enh-ruby-mode-hook 'flycheck-mode)
    (add-hook 'slim-mode-hook 'flycheck-mode)
    (add-hook 'haml-mode-hook 'flycheck-mode)
    (add-hook 'yaml-mode-hook 'flycheck-mode)
    (add-hook 'ruby-mode-hook 'flycheck-mode)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook enh-ruby-mode)
      (after "company-dabbrev-code"
        '(push 'enh-ruby-mode company-dabbrev-code-modes)))))


;;; Rust
(dotemacs-defvar-company-backends rust-mode)

(use-package init-rust
  :load-path "config/")

(use-package rust-mode                  ; Rust
  :ensure t
  :defer t
  :config
  (progn
    (when (fboundp 'sp-local-pair)
      ;; Don't pair lifetime specifiers
      (sp-local-pair 'rust-mode "'" nil :actions nil))

    (evil-leader/set-key-for-mode 'rust-mode
      "mcc" 'dotemacs-rust-cargo-build
      "mct" 'dotemacs-rust-cargo-test
      "mcd" 'dotemacs-rust-cargo-doc
      "mcx" 'dotemacs-rust-cargo-run)))

(use-package flycheck-rust              ; Flycheck setup for Rust
  :ensure t
  :commands (flycheck-rust-setup)
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (add-hook 'rust-mode-hook 'flycheck-mode))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook rust-mode))))

(use-package company-racer
  :if (eq dotemacs-completion-engine 'company)
  :ensure t
  :defer t
  :init
  (progn
    (push 'company-tern company-backends-rust-mode)))

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

(dotemacs-defvar-company-backends haskell-mode)
(dotemacs-defvar-company-backends haskell-cabal-mode)

(use-package init-haskell
  :load-path "config/")

(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (progn
    (bind-key "C-c h d" #'haskell-describe haskell-mode-map)
    (bind-key "C-c j i" #'haskell-navigate-imports haskell-mode-map)
    (bind-key "C-c f c" #'haskell-cabal-visit-file haskell-mode-map)

    ;; Haskell main editing mode key bindings.
    (add-hook 'haskell-mode-hook #'subword-mode)           ; Subword navigation
    (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode) ; Scan and navigate declarations
    ;; Insert module templates into new buffers
    (add-hook 'haskell-mode-hook #'haskell-auto-insert-module-template)
    (add-hook 'haskell-mode-hook #'dotemacs-init-haskell-mode)
    (add-hook 'haskell-cabal-mode-hook #'haskell-cabal-hook)

    ;; settings
    (setq haskell-process-type 'auto
          ;; Use notify.el (if you have it installed) at the end of running
          ;; Cabal commands or generally things worth notifying.
          haskell-notify-p t
          ;; To enable tags generation on save.
          haskell-tags-on-save t
          ;; Remove annoying error popups
          haskell-interactive-popup-error nil
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
          haskell-process-suggest-hayoo-imports t
          ;; Disable haskell-stylish on save, it breaks flycheck highlighting
          haskell-stylish-on-save nil)

    ;; key bindings
    (evil-leader/set-key-for-mode 'haskell-mode
      "mgg"  'haskell-mode-jump-to-def-or-tag
      "mf"   'haskell-mode-stylish-buffer

      "msb"  'haskell-process-load-or-reload
      "msc"  'haskell-interactive-mode-clear
      "mss"  'haskell-interactive-bring
      "msS"  'haskell-interactive-switch

      "mca"  'haskell-process-cabal
      "mcb"  'haskell-process-cabal-build
      "mcc"  'haskell-compile
      "mcv"  'haskell-cabal-visit-file

      "mhd"  'inferior-haskell-find-haddock
      "mhh"  'hoogle
      "mhi"  'haskell-process-do-info
      "mht"  'haskell-process-do-type
      "mhT"  'dotemacs-haskell-process-do-type-on-prev-line
      "mhy"  'hayoo

      "mdd"  'haskell-debug
      "mdb"  'haskell-debug/break-on-function
      "mdn"  'haskell-debug/next
      "mdN"  'haskell-debug/previous
      "mdB"  'haskell-debug/delete
      "mdc"  'haskell-debug/continue
      "mda"  'haskell-debug/abandon
      "mdr"  'haskell-debug/refresh)

    ;; Switch back to editor from REPL
    (evil-leader/set-key-for-mode 'haskell-interactive-mode
      "msS"  'haskell-interactive-switch)

    ;; Compile
    (evil-leader/set-key-for-mode 'haskell-cabal
      "mC"  'haskell-compile)

    ;; Cabal-file bindings
    (evil-leader/set-key-for-mode 'haskell-cabal-mode
      ;; "m="  'haskell-cabal-subsection-arrange-lines ;; Does a bad job, 'gg=G' works better
      "md" 'haskell-cabal-add-dependency
      "mb" 'haskell-cabal-goto-benchmark-section
      "me" 'haskell-cabal-goto-executable-section
      "mt" 'haskell-cabal-goto-test-suite-section
      "mm" 'haskell-cabal-goto-exposed-modules
      "ml" 'haskell-cabal-goto-library-section
      "mn" 'haskell-cabal-next-subsection
      "mp" 'haskell-cabal-previous-subsection
      "mN" 'haskell-cabal-next-section
      "mP" 'haskell-cabal-previous-section
      "mf" 'haskell-cabal-find-or-create-source-file)

    ;; Make "RET" behaviour in REPL saner
    (evil-define-key 'insert haskell-interactive-mode-map
      (kbd "RET") 'haskell-interactive-mode-return)
    (evil-define-key 'normal haskell-interactive-mode-map
      (kbd "RET") 'haskell-interactive-mode-return)

    ;;GHCi-ng
    (when dotemacs-haskell-enable-ghci-ng-support

      (when-let (ghci-ng (executable-find "ghci-ng"))
        ;; Use GHCI NG from https://github.com/chrisdone/ghci-ng
        (setq haskell-process-path-ghci ghci-ng)
        ;; haskell-process-type is set to auto, so setup ghci-ng for either case
        ;; if haskell-process-type == cabal-repl
        (add-to-list 'haskell-process-args-cabal-repl
                     '("--ghc-option=-ferror-spans" (concat "--with-ghc=" ghci-ng))))

      (evil-leader/set-key-for-mode 'haskell-mode
        "mu"   'haskell-mode-find-uses
        "mht"  'haskell-mode-show-type-at
        "mgg"  'haskell-mode-goto-loc))

    ;; Useful to have these keybindings for .cabal files, too.
    (after "haskell-cabal-mode-map"
      '(define-key haskell-cabal-mode-map
         [?\C-c ?\C-z] 'haskell-interactive-switch))))

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
  :init (unless dotemacs-haskell-enable-shm-support
          (add-hook 'haskell-mode-hook #'haskell-indentation-mode))
  :config
  (progn
     ;; Show indentation guides in insert or emacs state only.
     (defun dotemacs-haskell-indentation-show-guides ()
       "Show visual indentation guides."
       (when (and (boundp 'haskell-indentation-mode) haskell-indentation-mode)
         (haskell-indentation-enable-show-indentations)))

     (defun dotemacs-haskell-indentation-hide-guides ()
       "Hide visual indentation guides."
       (when (and (boundp 'haskell-indentation-mode) haskell-indentation-mode)
         (haskell-indentation-disable-show-indentations)))

     ;; first entry in normal state
     (add-hook 'evil-normal-state-entry-hook 'dotemacs-haskell-indentation-hide-guides)

     (add-hook 'evil-insert-state-entry-hook 'dotemacs-haskell-indentation-show-guides)
     (add-hook 'evil-emacs-state-entry-hook 'dotemacs-haskell-indentation-show-guides)
     (add-hook 'evil-insert-state-exit-hook 'dotemacs-haskell-indentation-hide-guides)
     (add-hook 'evil-emacs-state-exit-hook 'dotemacs-haskell-indentation-hide-guides)))

(use-package hindent                    ; Automated Haskell indentation
  :defer t
  :ensure t
  :if (stringp dotemacs-haskell-enable-hindent-style)
  :init
  (add-hook 'haskell-mode-hook #'hindent-mode)
  :config
  (progn
    (setq hindent-style dotemacs-haskell-enable-hindent-style)
    (evil-leader/set-key-for-mode 'haskell-mode
      "mF" 'hindent/reformat-decl)))

(use-package flycheck-haskell           ; Setup Flycheck from Cabal projects
  :ensure t
  :commands (flycheck-haskell-setup)
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

(use-package haskell-snippets
  :ensure t
  :defer t
  :init
  (progn
    ;; manually load the package since the current implementation is not lazy
    ;; loading friendly (funny coming from the haskell mode :-))
    (setq haskell-snippets-dir (dotemacs-get-package-directory
                                'haskell-snippets))

    (defun haskell-snippets-initialize ()
      (when (derived-mode-p 'haskell-mode)
        (let ((snip-dir (expand-file-name "snippets" haskell-snippets-dir)))
          (add-to-list 'yas-snippet-dirs snip-dir t)
          (yas-load-directory snip-dir))))

    ;; TODO only load once
    (add-hook 'haskell-mode-hook #'haskell-snippets-initialize)))

(use-package cmm-mode
  :ensure t
  :defer t)

(use-package ghc
  :ensure t
  :defer t
  :init (add-hook 'haskell-mode-hook 'ghc-init)
  :config
  (after "flycheck"
    ;; remove overlays from ghc-check.el if flycheck is enabled
    (set-face-attribute 'ghc-face-error nil :underline nil)
    (set-face-attribute 'ghc-face-warn nil :underline nil)))

(use-package shm
  :defer t
  :ensure t
  :if dotemacs-haskell-enable-shm-support
  :init
  (add-hook 'haskell-mode-hook 'structured-haskell-mode)
  :config
  (progn
    (when (require 'shm-case-split nil 'noerror)
      ;;TODO: Find some better bindings for case-splits
      (define-key shm-map (kbd "C-c S") 'shm/case-split)
      (define-key shm-map (kbd "C-c C-s") 'shm/do-case-split))

    (evil-define-key 'normal shm-map
      (kbd "RET") nil
      (kbd "C-k") nil
      (kbd "C-j") nil
      (kbd "D") 'shm/kill-line
      (kbd "R") 'shm/raise
      (kbd "P") 'shm/yank
      (kbd "RET") 'shm/newline-indent
      (kbd "RET") 'shm/newline-indent
      (kbd "M-RET") 'evil-ret
      )

    (evil-define-key 'operator shm-map
      (kbd ")") 'shm/forward-node
      (kbd "(") 'shm/backward-node)

    (evil-define-key 'motion shm-map
      (kbd ")") 'shm/forward-node
      (kbd "(") 'shm/backward-node)

    (define-key shm-map (kbd "C-j") nil)
    (define-key shm-map (kbd "C-k") nil)))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (add-hook 'haskell-mode-hook 'flycheck-mode))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook haskell-mode)
      (dotemacs-add-company-hook haskell-cabal-mode))))

(use-package company-ghc
  :if (eq dotemacs-completion-engine 'company)
  :ensure t
  :defer t
  :init
  (progn
    (push '(company-ghc company-dabbrev-code company-yasnippet)
          company-backends-haskell-mode)))

(use-package company-cabal
  :if (eq dotemacs-completion-engine 'company)
  :ensure t
  :defer t
  :init
  (progn
    (push '(company-cabal) company-backends-haskell-cabal-mode)))


;;; Go
(dotemacs-defvar-company-backends go-mode)

(use-package init-go
  :commands (load-gopath-file
             go/init-go-oracle
             dotemacs-go-run-package-tests)
  :load-path "config/")

(use-package go-rename
  :disabled t
  :init
  (evil-leader/set-key-for-mode 'go-mode "mr" 'go-rename))

(use-package go-mode
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save)

    (evil-leader/set-key-for-mode 'go-mode
      "mdp" 'godoc-at-point
      "mig" 'go-goto-imports
      "mia" 'go-import-add
      "mir" 'go-remove-unused-imports
      "mpb" 'go-play-buffer
      "mpr" 'go-play-region
      "mpd" 'go-download-play
      "mga" 'ff-find-other-file
      "mgg" 'godef-jump
      "mtp" 'dotemacs-go-run-package-tests)))

(use-package go-eldoc
  :disabled t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (add-hook 'go-mode-hook 'flycheck-mode))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook go-mode))))

(use-package company-go
  :if (eq dotemacs-completion-engine 'company)
  :ensure t
  :defer t
  :init
  (progn
    (push 'company-go company-backends-go-mode)))


;;; C/C++
(dotemacs-defvar-company-backends c-mode-common)
(dotemacs-defvar-company-backends cmake-mode)

(use-package init-c-c++
  :load-path "config/")

(use-package cc-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist `("\\.h$" . ,dotemacs-c-c++-default-mode-for-headers))
  :config
  (progn
    (require 'compile)
    (c-toggle-auto-newline 1)
    (evil-leader/set-key-for-mode 'c-mode
      "mga" 'projectile-find-other-file
      "mgA" 'projectile-find-other-file-other-window)
    (evil-leader/set-key-for-mode 'c++-mode
      "mga" 'projectile-find-other-file
      "mgA" 'projectile-find-other-file-other-window)))

(use-package clang-format
  :ensure t
  :if dotemacs-c-c++-enable-clang-support)

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
  :init (push 'company-cmake company-backends-cmake-mode))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook c-mode-common)
      (dotemacs-add-company-hook cmake-mode)

      (when dotemacs-c-c++-enable-clang-support
        (push 'company-clang company-backends-c-mode-common)
        ;; .clang_complete file loading
        ;; Sets the arguments for company-clang based on a project-specific text file.
        (setq company-clang-prefix-guesser 'company-mode/more-than-prefix-guesser)))))

(use-package company-c-headers
  :if (eq dotemacs-completion-engine 'company)
  :ensure t
  :defer t
  :init
  (progn
    (push 'company-c-headers company-backends-c-mode-common)))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (progn
    (add-to-hooks 'flycheck-mode '(c-mode-hook c++-mode-hook))))

(use-package gdb-mi
  :ensure t
  :defer t
  :init
  (setq
   ;; use gdb-many-windows by default when `M-x gdb'
   gdb-many-windows t
   ;; Non-nil means display source file containing the main routine at startup
   gdb-show-main t))

(dotemacs-use-package-add-hook helm-gtags
  :post-init
  (progn
    (dotemacs-helm-gtags-define-keys-for-mode 'c-mode)
    (dotemacs-helm-gtags-define-keys-for-mode 'c++-mode)))

(dotemacs-use-package-add-hook semantic
  :post-init
  (progn
    (semantic/enable-semantic-mode 'c-mode)
    (semantic/enable-semantic-mode 'c++-mode)))

(dotemacs-use-package-add-hook srefactor
  :post-init
  (progn
    (evil-leader/set-key-for-mode 'c-mode "mr" 'srefactor-refactor-at-point)
    (evil-leader/set-key-for-mode 'c++-mode "mr" 'srefactor-refactor-at-point)
    (add-to-hooks 'dotemacs-lazy-load-srefactor '(c-mode-hook c++-mode-hook)) ))

(dotemacs-use-package-add-hook stickyfunc-enhance
  :post-init
  (progn
    (add-to-hooks 'dotemacs-lazy-load-stickyfunc-enhance '(c-mode-hook c++-mode-hook) )))

(dotemacs-use-package-add-hook ycmd
  :post-init
  (progn
    (add-hook 'c++-mode-hook 'ycmd-mode)
    (evil-leader/set-key-for-mode 'c++-mode
      "mgg" 'ycmd-goto
      "mgG" 'ycmd-goto-imprecise)))

(dotemacs-use-package-add-hook company-ycmd
  :post-init
  (progn
    (push 'company-ycmd company-backends-c-mode-common)))


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
    (after "eval-sexp-fu"
      '(require 'cider-eval-sexp-fu))
    (setq cider-stacktrace-default-filters '(tooling dup)
          cider-repl-pop-to-buffer-on-connect nil
          cider-prompt-save-file-on-load nil
          cider-repl-use-clojure-font-lock t)
    (push "\\*cider-repl\.\+\\*" dotemacs-useful-buffers-regexp)
    (add-hook 'clojure-mode-hook 'cider-mode)
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-mode-hook 'rainbow-delimiters-mode)
    (unless (version< emacs-version "24.4")
      (add-hook 'cider-mode-hook 'subword-mode))
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

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (push 'company-capf company-backends-cider-mode)
      (dotemacs-add-company-hook cider-mode)

      (push 'company-capf company-backends-cider-repl-mode)
      (dotemacs-add-company-hook cider-repl-mode))))


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

(dotemacs-use-package-add-hook flycheck
  :post-init
  (add-hook 'tuareg-mode-hook 'flycheck-mode))

(use-package flycheck-ocaml             ; Check OCaml code with Merlin
  :ensure t
  :commands (flycheck-ocaml-setup)
  :init (add-hook 'flycheck-mode-hook #'flycheck-ocaml-setup))


;;; Web languages
(dotemacs-defvar-company-backends css-mode)
(dotemacs-defvar-company-backends web-mode)

(use-package init-web
  :load-path "config/")

;; Thanks to [[https://github.com/skeeto/impatient-mode][impatient-mode]] you
;; can see the effect of your HTML as you type it.
(use-package impatient-mode
  :ensure t
  :if window-system
  :defer t
  :init
  (progn
    (add-hook 'web-mode-hook 'impatient-mode)))

(use-package css-mode
  :defer t
  :ensure t
  :config
  (progn
    ;; Run Prog Mode hooks, because for whatever reason CSS Mode derives from
    ;; `fundamental-mode'.
    (add-hook 'css-mode-hook (lambda () (run-hooks #'dotemacs-prog-mode-hook)))

    ;; Mark css-indent-offset as safe local variable.
    (put 'css-indent-offset 'safe-local-variable #'integerp)))

(dotemacs-use-package-add-hook yasnippet
  :post-init
  (progn
    (add-hook 'css-mode-hook 'dotemacs-load-yasnippet)))

(use-package css-eldoc                  ; Basic Eldoc for CSS
  :ensure t
  :commands (turn-on-css-eldoc)
  :init (add-hook 'css-mode-hook #'turn-on-css-eldoc))

(use-package helm-css-scss
  :defer t
  :ensure t
  :init
  (after "scss-mode"
    '(evil-leader/set-key-for-mode 'scss-mode "mgh" 'helm-css-scss)))

(use-package web-mode                   ; Template editing
  :defer t
  :ensure t
  :config
  (progn
    ;; Only use smartparens in web-mode
    (setq web-mode-enable-auto-pairing nil)
    (setq web-mode-markup-indent-offset 2)

    (after "flycheck"
      (when-let (tidy5 (and (eq system-type 'darwin)
                               (executable-find "tidy5")))
        (setq flycheck-html-tidy-executable tidy5)
        (flycheck-add-mode 'html-tidy 'web-mode)))

    (sp-local-pair 'web-mode "<% " " %>")
    (sp-local-pair 'web-mode "{ " " }")
    (sp-local-pair 'web-mode "<%= "  "  %>")
    (sp-local-pair 'web-mode "<%# "  " %>")
    (sp-local-pair 'web-mode "<%$ "  " %>")
    (sp-local-pair 'web-mode "<%@ "  " %>")
    (sp-local-pair 'web-mode "<%: "  " %>")
    (sp-local-pair 'web-mode "{{ "  " }}")
    (sp-local-pair 'web-mode "{% "  " %}")
    (sp-local-pair 'web-mode "{%- "  " %}")
    (sp-local-pair 'web-mode "{# "  " #}")

    (evil-leader/set-key-for-mode 'web-mode
      "meh" 'web-mode-dom-errors-show
      "mgb" 'web-mode-element-beginning
      "mgc" 'web-mode-element-child
      "mgp" 'web-mode-element-parent
      "mgs" 'web-mode-element-sibling-next
      "mhp" 'web-mode-dom-xpath
      "mrc" 'web-mode-element-clone
      "mrd" 'web-mode-element-vanish
      "mrk" 'web-mode-element-kill
      "mrr" 'web-mode-element-rename
      "mrw" 'web-mode-element-wrap
      "mz" 'web-mode-fold-or-unfold
      ;; TODO element close would be nice but broken with evil.
      )

    (defvar dotemacs--web-mode-ms-doc-toggle 0
      "Display a short doc when nil, full doc otherwise.")

    (dotemacs-define-micro-state web-mode
      :doc (dotemacs-web-mode-ms-doc)
      :persistent t
      :evil-leader-for-mode (web-mode . "m.")
      :bindings
      ("<escape>" nil :exit t)
      ("?" dotemacs-web-mode-ms-toggle-doc)
      ("c" web-mode-element-clone)
      ("d" web-mode-element-vanish)
      ("D" web-mode-element-kill)
      ("j" web-mode-element-next)
      ("J" web-mode-element-sibling-next)
      ("k" web-mode-element-previous)
      ("K" web-mode-element-sibling-previous)
      ("h" web-mode-element-parent)
      ("l" web-mode-element-child)
      ("p" web-mode-dom-xpath)
      ("r" web-mode-element-rename)
      ("q" nil :exit t)
      ("w" web-mode-element-wrap)))

  :mode
  (("\\.phtml\\'"      . web-mode)
   ("\\.tpl\\.php\\'"  . web-mode)
   ("\\.html\\'"       . web-mode)
   ("\\.htm\\'"        . web-mode)
   ("\\.[gj]sp\\'"     . web-mode)
   ("\\.as[cp]x\\'"    . web-mode)
   ("\\.erb\\'"        . web-mode)
   ("\\.eco\\'"        . web-mode)
   ("\\.djhtml\\'"     . web-mode)))

(use-package emmet-mode
  :defer t
  :ensure t
  :init
  (progn
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'html-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode))
  :config
  (progn
    (evil-define-key 'insert emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
    (evil-define-key 'insert emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
    (evil-define-key 'emacs emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
    (evil-define-key 'emacs emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
  :diminish emmet-mode))

(use-package scss-mode
  :defer t
  :ensure t
  :mode ("\\.scss\\'" . scss-mode)
  :init
  (progn
    (add-hook 'scss-mode-hook 'rainbow-delimiters-mode)))

(use-package sass-mode
  :ensure t
  :defer t
  :mode ("\\.sass\\'" . sass-mode))

(use-package less-css-mode
  :defer t
  :ensure t
  :init
  (progn
    (add-hook 'less-css-mode-hook 'rainbow-delimiters-mode))
  :mode ("\\.less\\'" . less-css-mode))

(use-package tagedit
  :defer t
  :ensure t
  :config
  (progn
    (tagedit-add-experimental-features)
    (add-hook 'html-mode-hook (lambda () (tagedit-mode 1))))
  :diminish (tagedit-mode . " Ⓣ"))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (progn
    (add-hook 'web-mode-hook 'flycheck-mode)
    (add-hook 'css-mode-hook 'flycheck-mode)
    (add-hook 'scss-mode-hook 'flycheck-mode)
    (add-hook 'sass-mode-hook 'flycheck-mode)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook css-mode)
      (dotemacs-add-company-hook web-mode))))

(use-package company-web
  :if (eq dotemacs-completion-engine 'company)
  :ensure t
  :defer t
  :init
  (progn
    (push 'company-css company-backends-css-mode)
    (push 'company-web-html company-backends-web-mode)))

;;; PureScript
(use-package purescript-mode
  :defer t
  :ensure t
  :init
  (progn
    (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)
    (evil-leader/set-key-for-mode 'purescript-mode
      "mi="  'purescript-mode-format-imports
      "mi`"  'purescript-navigate-imports-return
      "mia"  'purescript-align-imports
      "min"  'purescript-navigate-imports)))

(after "flycheck"
  (flycheck-define-checker purs-check
    "Use purscheck to flycheck PureScript code."
    :command ("purscheck" source source-original temporary-file-name)
    :error-patterns
    ((error line-start
            (or (and "Error at " (file-name)    " line " line ", column " column ":" (zero-or-more " "))
                (and "\""        (file-name) "\" (line " line ", column " column "):"))
            (or (message (one-or-more not-newline))
                (and "\n"
                     (message
                      (zero-or-more " ") (one-or-more not-newline)
                      (zero-or-more "\n"
                                    (zero-or-more " ")
                                    (one-or-more not-newline)))))
            line-end))
    :modes purescript-mode)
  (add-to-list 'flycheck-checkers 'purs-check))

; Waiting on purscheck to make it to melpa
; (dotemacs-use-package-add-hook flycheck
;   :post-init
;   (add-hook 'purescript-mode-hook 'flycheck-mode))

(use-package psci
  :defer t
  :ensure t
  :init
  (progn
    (add-hook 'purescript-mode-hook 'inferior-psci-mode)
    (evil-leader/set-key-for-mode 'purescript-mode
      "msb" 'psci/load-current-file!
      "msi" 'psci
      "msm" 'psci/load-module!
      "msp" 'psci/load-project-modules!)))

;;; JavaScript
(dotemacs-defvar-company-backends js2-mode)

(use-package init-js
  :load-path "config/"
  :commands (dotemacs-js-ctrl-c-ctrl-c
             dotemacs-hide-test-functions
             dotemacs-tab-properly
             dotemacs-fetch-autolint-externs
             dotemacs-js-jump-to
             dotemacs-js-format-impl-name
             dotemacs-js-format-test-name
             dotemacs-js-jump-to-implementation-or-test
             dotemacs-js2-mode-defaults)
  :init
  (progn
    (setq dotemacs-js2-mode-hook #'dotemacs-js2-mode-defaults)
    (add-hook 'js2-mode-hook
              (lambda ()
                (run-hooks #'dotemacs-js2-mode-hook)))))

(use-package coffee-mode
  :defer t
  :ensure t
  :init
  (progn
    (after "flycheck"
      (when-let (coffeelint (executable-find "coffeelint"))
        (setq flycheck-coffee-coffeelint-executable coffeelint)))

    (defun javascript/coffee-indent ()
      (if (coffee-line-wants-indent)
          ;; We need to insert an additional tab because the last line was special.
          (coffee-insert-spaces (+ (coffee-previous-indent) coffee-tab-width))
        ;; otherwise keep at the same indentation level
        (coffee-insert-spaces (coffee-previous-indent)))
      )
    ;; indent to right position after `evil-open-below' and `evil-open-above'
    (add-hook 'coffee-mode-hook '(lambda ()
                                   (setq indent-line-function 'javascript/coffee-indent
                                         evil-shift-width coffee-tab-width)))))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (progn
    (add-hook 'coffee-mode-hook 'flycheck-mode)
    (add-hook 'js2-mode-hook 'flycheck-mode)
    (add-hook 'json-mode-hook 'flycheck-mode)))

(use-package js2-mode                   ; Javascript editing
  :defer t
  :ensure t
  :init
  (progn
    (after "flycheck"

      (when-let (jshint (executable-find "jshint"))
                (setq flycheck-javascript-jshint-executable jshint)
                (defun flycheck-jshint-disable ()
                  (interactive)
                  (add-to-list 'flycheck-disabled-checkers 'javascript-jshint))
                (defun flycheck-jshint-enable ()
                  (interactive)
                  (setq flycheck-disabled-checkers (remove 'javascript-jshint flycheck-disabled-checkers)))
                (evil-leader/set-key
                  "teh" 'flycheck-jshint-enable
                  "teH" 'flycheck-jshint-disable))

      (when-let (jscs (executable-find "jscs"))
                (setq flycheck-javascript-jscs-executable jscs)
                (defun flycheck-jscs-disable ()
                  (interactive)
                  (add-to-list 'flycheck-disabled-checkers 'javascript-jscs))
                (defun flycheck-jscs-enable ()
                  (interactive)
                  (setq flycheck-disabled-checkers (remove 'javascript-jscs flycheck-disabled-checkers)))
                (evil-leader/set-key
                  "tec" 'flycheck-jscs-enable
                  "teC" 'flycheck-jscs-disable))

      (when-let (eslint (executable-find "eslint"))
                (setq flycheck-javascript-eslint-executable eslint)
                (defun flycheck-eslint-disable ()
                  (interactive)
                  (add-to-list 'flycheck-disabled-checkers 'javascript-eslint))
                (defun flycheck-eslint-enable ()
                  (interactive)
                  (setq flycheck-disabled-checkers (remove 'javascript-eslint flycheck-disabled-checkers)))
                (evil-leader/set-key
                  "tee" 'flycheck-eslint-enable
                  "teE" 'flycheck-eslint-disable)))

    ;; Let flycheck handle parse errors
    (setq js2-show-parse-errors nil
          js2-strict-missing-semi-warning nil
          js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason

    (add-to-list 'auto-mode-alist '("\\.jshintrc$" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.eslintrc$" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook #'js2-highlight-unused-variables-mode)
    ;; required to make `<SPC> s l' to work correctly
    (add-hook 'js2-mode-hook 'js2-imenu-extras-mode))
  :config
  (progn
    (setq-default js2-basic-offset 2)
    (setq js2-global-externs '("angular" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "__dirname" "console" "JSON" "_" "assert" "refute" "buster" "require" "global" "exports" "module" "describe" "it" "before" "after" "beforeEach" "afterEach" "chai" "expect" "sinon" "test" "asyncTest" "ok" "equal" "notEqual" "deepEqual" "expect"))

    (evil-leader/set-key-for-mode 'js2-mode "mw" 'js2-mode-toggle-warnings-and-errors)
    (evil-leader/set-key-for-mode 'js2-mode "mzc" 'js2-mode-hide-element)
    (evil-leader/set-key-for-mode 'js2-mode "mzo" 'js2-mode-show-element)
    (evil-leader/set-key-for-mode 'js2-mode "mzr" 'js2-mode-show-all)
    (evil-leader/set-key-for-mode 'js2-mode "mze" 'js2-mode-toggle-element)
    (evil-leader/set-key-for-mode 'js2-mode "mzF" 'js2-mode-toggle-hide-functions)
    (evil-leader/set-key-for-mode 'js2-mode "mzC" 'js2-mode-toggle-hide-comments)))

(use-package js2-refactor
  :defer t
  :ensure t
  :init
  (progn
    (defun javascript/load-js2-refactor ()
      "Lazy load js2-refactor"
      (require 'js2-refactor))
    (add-hook 'js2-mode-hook 'javascript/load-js2-refactor))
  :config
  (progn
    (evil-leader/set-key-for-mode 'js2-mode "mr3i" 'js2r-ternary-to-if)

    (evil-leader/set-key-for-mode 'js2-mode "mrag" 'js2r-add-to-globals-annotation)
    (evil-leader/set-key-for-mode 'js2-mode "mrao" 'js2r-arguments-to-object)

    (evil-leader/set-key-for-mode 'js2-mode "mrba" 'js2r-forward-barf)

    (evil-leader/set-key-for-mode 'js2-mode "mrca" 'js2r-contract-array)
    (evil-leader/set-key-for-mode 'js2-mode "mrco" 'js2r-contract-object)
    (evil-leader/set-key-for-mode 'js2-mode "mrcu" 'js2r-contract-function)

    (evil-leader/set-key-for-mode 'js2-mode "mrea" 'js2r-expand-array)
    (evil-leader/set-key-for-mode 'js2-mode "mref" 'js2r-extract-function)
    (evil-leader/set-key-for-mode 'js2-mode "mrem" 'js2r-extract-method)
    (evil-leader/set-key-for-mode 'js2-mode "mreo" 'js2r-expand-object)
    (evil-leader/set-key-for-mode 'js2-mode "mreu" 'js2r-expand-function)
    (evil-leader/set-key-for-mode 'js2-mode "mrev" 'js2r-extract-var)

    (evil-leader/set-key-for-mode 'js2-mode "mrig" 'js2r-inject-global-in-iife)
    (evil-leader/set-key-for-mode 'js2-mode "mrip" 'js2r-introduce-parameter)
    (evil-leader/set-key-for-mode 'js2-mode "mriv" 'js2r-inline-var)

    (evil-leader/set-key-for-mode 'js2-mode "mrlp" 'js2r-localize-parameter)
    (evil-leader/set-key-for-mode 'js2-mode "mrlt" 'js2r-log-this)

    (evil-leader/set-key-for-mode 'js2-mode "mrrv" 'js2r-rename-var)

    (evil-leader/set-key-for-mode 'js2-mode "mrsl" 'js2r-forward-slurp)
    (evil-leader/set-key-for-mode 'js2-mode "mrss" 'js2r-split-string)
    (evil-leader/set-key-for-mode 'js2-mode "mrsv" 'js2r-split-var-declaration)

    (evil-leader/set-key-for-mode 'js2-mode "mrtf" 'js2r-toggle-function-expression-and-declaration)

    (evil-leader/set-key-for-mode 'js2-mode "mruw" 'js2r-unwrap)

    (evil-leader/set-key-for-mode 'js2-mode "mrvt" 'js2r-var-to-this)

    (evil-leader/set-key-for-mode 'js2-mode "mrwi" 'js2r-wrap-buffer-in-iife)
    (evil-leader/set-key-for-mode 'js2-mode "mrwl" 'js2r-wrap-in-for-loop)

    (evil-leader/set-key-for-mode 'js2-mode "mk" 'js2r-kill)
    (evil-leader/set-key-for-mode 'js2-mode "xmj" 'js2r-move-line-down)
    (evil-leader/set-key-for-mode 'js2-mode "xmk" 'js2r-move-line-up)))

(use-package json-mode                  ; JSON files
  :ensure t
  :defer t)

(use-package json-snatcher
  :defer t
  :ensure t
  :config
  (evil-leader/set-key-for-mode 'json-mode
    "mhp" 'jsons-print-path)
  )

(use-package json-reformat              ; Reformat JSON
  :ensure t
  :defer t
  :bind (("C-c e j" . json-reformat-region)))

(use-package tern
  :defer t
  :ensure t
  :init (add-hook 'js2-mode-hook 'tern-mode)
  :config
  (progn
    (evil-leader/set-key-for-mode 'js2-mode "mrrV" 'tern-rename-variable)
    (evil-leader/set-key-for-mode 'js2-mode "mhd" 'tern-get-docs)
    (evil-leader/set-key-for-mode 'js2-mode "mgg" 'tern-find-definition)
    (evil-leader/set-key-for-mode 'js2-mode "mgG" 'tern-find-definition-by-name)
    (evil-leader/set-key-for-mode 'js2-mode (kbd "m C-g") 'tern-pop-find-definition)
    (evil-leader/set-key-for-mode 'js2-mode "mht" 'tern-get-type)))

(use-package js-doc
  :defer t
  :ensure t
  :init
  (progn
    (defun javascript/load-js-doc ()
        "Lazy load js-doc"
      (require 'js-doc))
    (add-hook 'js2-mode-hook 'javascript/load-js-doc))
  :config
  (progn
    (evil-leader/set-key-for-mode 'js2-mode "mrdb" 'js-doc-insert-file-doc)
    (evil-leader/set-key-for-mode 'js2-mode "mrdf" 'js-doc-insert-function-doc)
    (evil-leader/set-key-for-mode 'js2-mode "mrdt" 'js-doc-insert-tag)
    (evil-leader/set-key-for-mode 'js2-mode "mrdh" 'js-doc-describe-tag)))

(use-package web-beautify
  :defer t
  :ensure t
  :init
  (progn
    (evil-leader/set-key-for-mode 'js2-mode  "m=" 'web-beautify-js)
    (evil-leader/set-key-for-mode 'json-mode "m=" 'web-beautify-js)
    (evil-leader/set-key-for-mode 'web-mode  "m=" 'web-beautify-html)
    (evil-leader/set-key-for-mode 'css-mode  "m=" 'web-beautify-css)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (push 'company-capf company-backends-js2-mode)
      (dotemacs-add-company-hook js2-mode))))

(use-package company-tern       ; JavaScript backend for Company
  :if (eq dotemacs-completion-engine 'company)
  :ensure t
  :defer t
  :init
  (progn
    (push 'company-tern company-backends-js2-mode)))


;;; Lua
(dotemacs-defvar-company-backends lua-mode)

(dotemacs-use-package-add-hook flycheck
  :post-init
  (add-hook 'lua-mode-hook 'flycheck-mode))

(use-package lua-mode
  :defer t
  :ensure t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :config
  (progn
    (setq lua-indent-level 2
          lua-indent-string-contents t)
    (evil-leader/set-key-for-mode 'lua-mode "md" 'lua-search-documentation)
    (evil-leader/set-key-for-mode 'lua-mode "msb" 'lua-send-buffer)
    (evil-leader/set-key-for-mode 'lua-mode "msf" 'lua-send-defun)
    (evil-leader/set-key-for-mode 'lua-mode "msl" 'lua-send-current-line)
    (evil-leader/set-key-for-mode 'lua-mode "msr" 'lua-send-region)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook lua-mode))))


;;; PHP
(dotemacs-defvar-company-backends php-mode)

(use-package drupal-mode
  :defer t)

; todo
; (defun php/post-init-eldoc ()
;   (add-hook 'php-mode-hook 'eldoc-mode)
;     (dotemacs-ggtags-enable-eldoc 'php-mode))
;
; (defun php/post-init-ggtags ()
;   (add-hook 'php-mode-hook 'ggtags-mode))
;
; (defun php/post-init-helm-gtags ()
;   (dotemacs-helm-gtags-define-keys-for-mode 'php-mode))

(use-package php-auto-yasnippets
  :defer t
  :ensure t)

(use-package php-extras
  :defer t)

(use-package php-mode                   ; Because sometimes you have to
  :defer t
  :mode ("\\.php\\'" . php-mode))

(use-package phpcbf
  :defer t
  :ensure t)

(use-package phpunit
  :defer t
  :ensure t)

(dotemacs-use-package-add-hook flycheck
  :post-init
  (add-hook 'php-mode-hook 'flycheck-mode))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook php-mode))))


;;; Stylus
(use-package init-stylus
  :load-path "config/"
  :defer t
  :commands dotemacs-stylus-mode-defaults)

(use-package stylus-mode
  :ensure t
  :defer t
  :mode ("\\.styl$" . stylus-mode)
  :init
  (progn
    (setq dotemacs-stylus-mode-hook #'dotemacs-stylus-mode-defaults)
    (add-hook 'stylus-mode-hook
              (lambda ()
                (run-hooks #'dotemacs-stylus-mode-hook)))))


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


;;; Racket
(use-package init-racket
  :load-path "config/")

(when (eq dotemacs-completion-engine 'company)
  ;; this is the only thing to do to enable company in racket-mode
  ;; because racket-mode handle everything for us when company
  ;; is loaded.
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (add-hook 'racket-mode-hook 'company-mode)))

  ;; Bug exists in Racket company backend that opens docs in new window when
  ;; company-quickhelp calls it. Note hook is appendended for proper ordering.
  (dotemacs-use-package-add-hook company-quickhelp
    :post-init
    (progn
      (add-hook 'company-mode-hook
          '(lambda ()
             (when (equal major-mode 'racket-mode)
               (company-quickhelp-mode -1))) t))))

(use-package racket-mode
  :defer t
  :ensure t
  :config
  (progn
    ;; smartparens configuration
    (after "smartparens"
      '(progn (add-to-list 'sp--lisp-modes 'racket-mode)
              (when (fboundp 'sp-local-pair)
                (sp-local-pair 'racket-mode "'" nil :actions nil)
                (sp-local-pair 'racket-mode "`" nil :actions nil))))

    (evil-leader/set-key-for-mode 'racket-mode
      ;; navigation
      "mg`" 'racket-unvisit
      "mgg" 'racket-visit-definition
      "mgm" 'racket-visit-module
      "mgr" 'racket-open-require-path
      ;; doc
      "mhd" 'racket-describe
      "mhh" 'racket-doc
      ;; insert
      "mil" 'racket-insert-lambda
      ;; REPL
      "msb" 'racket-run
      "msB" 'racket-run-and-switch-to-repl
      "mse" 'racket-send-last-sexp
      "msE" 'dotemacs-racket-send-last-sexp-focus
      "msf" 'racket-send-definition
      "msF" 'dotemacs-racket-send-definition-focus
      "msi" 'racket-repl
      "msr" 'racket-send-region
      "msR" 'dotemacs-racket-send-region-focus
      "mss" 'racket-repl
      ;; Tests
      "mtb" 'racket-test
      "mtB" 'dotemacs-racket-test-with-coverage)
    (define-key racket-mode-map (kbd "H-r") 'racket-run)
    ;; remove racket auto-insert of closing delimiter
    ;; see https://github.com/greghendershott/racket-mode/issues/140
    (define-key racket-mode-map ")" 'self-insert-command)
    (define-key racket-mode-map "]" 'self-insert-command)
    (define-key racket-mode-map "}" 'self-insert-command)))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (add-hook 'racket-mode-hook 'flycheck-mode))


;;; Java
(dotemacs-defvar-company-backends java-mode)

(use-package init-java
  :load-path "config/")

(use-package eclim
  :defer t
  :ensure emacs-eclim
  :init (add-hook 'java-mode-hook 'eclim-mode)
  :config
  (progn
    (setq help-at-pt-display-when-idle t
          help-at-pt-timer-delay 0.1)
    (help-at-pt-set-timer)

    (add-to-list 'minor-mode-alist
                 '(eclim-mode (:eval (eclim-modeline-string))))

    (evil-define-key 'insert java-mode-map
      (kbd ".") 'dotemacs-java-completing-dot
      (kbd "M-.") 'eclim-java-find-declaration
      (kbd "M-,") 'pop-tag-mark
      (kbd "M-<mouse-3>") 'eclim-java-find-declaration
      (kbd "<mouse-8>") 'pop-tag-mark)

    (evil-define-key 'normal java-mode-map
      (kbd "M-.") 'eclim-java-find-declaration
      (kbd "M-,") 'pop-tag-mark
      (kbd "M-<mouse-3>") 'eclim-java-find-declaration
      (kbd "<mouse-8>") 'pop-tag-mark)

    (evil-define-key 'normal eclim-problems-mode-map
      (kbd "a") 'eclim-problems-show-all
      (kbd "e") 'eclim-problems-show-errors
      (kbd "g") 'eclim-problems-buffer-refresh
      (kbd "q") 'eclim-quit-window
      (kbd "w") 'eclim-problems-show-warnings
      (kbd "f") 'eclim-problems-toggle-filefilter
      (kbd "c") 'eclim-problems-correct
      (kbd "RET") 'eclim-problems-open-current)

    (evil-define-key 'normal eclim-project-mode-map
      (kbd "N") 'eclim-project-create
      (kbd "m") 'eclim-project-mark-current
      (kbd "M") 'eclim-project-mark-all
      (kbd "u") 'eclim-project-unmark-current
      (kbd "U") 'eclim-project-unmark-all
      (kbd "o") 'eclim-project-open
      (kbd "c") 'eclim-project-close
      (kbd "i") 'eclim-project-info-mode
      (kbd "I") 'eclim-project-import
      (kbd "RET") 'eclim-project-goto
      (kbd "D") 'eclim-project-delete
      (kbd "p") 'eclim-project-update
      (kbd "g") 'eclim-project-mode-refresh
      (kbd "R") 'eclim-project-rename
      (kbd "q") 'eclim-quit-window)

    (evil-leader/set-key-for-mode 'java-mode
      "mea" 'eclim-problems-show-all
      "meb" 'eclim-problems
      "mec" 'eclim-problems-correct
      "mee" 'eclim-problems-show-errors
      "mef" 'eclim-problems-toggle-filefilter
      "men" 'eclim-problems-next-same-window
      "meo" 'eclim-problems-open
      "mep" 'eclim-problems-previous-same-window
      "mew" 'eclim-problems-show-warnings

      "mff" 'eclim-java-find-generic

      "mgg" 'eclim-java-find-declaration
      "mgt" 'eclim-java-find-type

      "mrc" 'eclim-java-constructor
      "mrg" 'eclim-java-generate-getter-and-setter
      "mrf" 'eclim-java-format
      "mri" 'eclim-java-import-organize
      "mrj" 'eclim-java-implement
      "mrr" 'eclim-java-refactor-rename-symbol-at-point

      "mhc" 'eclim-java-call-hierarchy
      "mhh" 'eclim-java-show-documentation-for-current-element
      "mhi" 'eclim-java-hierarchy
      "mhu" 'eclim-java-find-references

      "mmi" 'dotemacs-java-maven-clean-install
      "mmI" 'dotemacs-java-maven-install
      "mmp" 'eclim-maven-lifecycle-phases
      "mmr" 'eclim-maven-run
      "mmR" 'eclim-maven-lifecycle-phase-run
      "mmt" 'dotemacs-java-maven-test

      "maa" 'eclim-ant-run
      "mac" 'eclim-ant-clear-cache
      "mar" 'eclim-ant-run
      "mav" 'eclim-ant-validate

      "mpb" 'eclim-project-build
      "mpc" 'eclim-project-create
      "mpd" 'eclim-project-delete
      "mpg" 'eclim-project-goto
      "mpi" 'eclim-project-import
      "mpj" 'eclim-project-info-mode
      "mpk" 'eclim-project-close
      "mpo" 'eclim-project-open
      "mpp" 'eclim-project-mode
      "mpu" 'eclim-project-update

      "mtt" 'eclim-run-junit)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook java-mode))))

(use-package company-emacs-eclim
  :if (eq dotemacs-completion-engine 'company)
  :defer t
  :init
  (push 'company-emacs-eclim company-backends-java-mode))


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

(dotemacs-use-package-add-hook flycheck
  :post-init
  (progn
    (add-hook 'swift-mode-hook 'flycheck-mode)
    (add-hook 'sh-mode-hook 'flycheck-mode)))


;; REST Client
(dotemacs-defvar-company-backends restclient-mode)

; (Emacs Rocks!)[http://emacsrocks.com/e15.html]
(use-package restclient                ; ReST REPL for Emacs
  :mode ("\\.http\\'" . restclient-mode)
  :ensure t
  :defer t
  :init
  (progn

    (defun restclient-http-send-current-raw-stay-in-window ()
      (interactive)
      (restclient-http-send-current t t))

    (evil-leader/set-key-for-mode 'restclient-mode
      "ms" 'restclient-http-send-current-stay-in-window
      "mS" 'restclient-http-send-current
      "mr" 'restclient-http-send-current-raw-stay-in-window
      "mR" 'restclient-http-send-current-raw
      )))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook restclient-mode))))

(use-package company-restclient    ; restclient backend for Company
  :if (eq dotemacs-completion-engine 'company)
  :ensure t
  :defer t
  :init
  (progn
    (push 'company-restclient company-backends-restclient-mode)))


;; Databases
(use-package init-sql
  :load-path "config/")

(use-package sql
  :defer t
  :ensure t
  :bind (("C-c d c" . sql-connect)
         ("C-c d m" . sql-mysql))
  :config
  (progn
    (dotemacs-load-private-file "sql-connections" 'noerror)

    (add-to-list 'display-buffer-alist
               `(,(rx bos "*SQL")
                 (display-buffer-reuse-window
                  display-buffer-in-side-window
                  (side            . bottom)
                  (reusable-frames . visible)
                  (window-height   . 0.4))))

    (setq dotemacs-sql-highlightable sql-product-alist
          dotemacs-sql-startable (remove-if-not
                              (lambda (product) (sql-get-product-feature (car product) :sqli-program))
                              sql-product-alist)

          ;; should not set this to anything else than nil
          ;; the focus of SQLi is handled by dotemacs conventions
          sql-pop-to-buffer-after-send-region nil)

    (evil-leader/set-key-for-mode 'sql-mode
      ;; sqli buffer
      "mbb" 'sql-show-sqli-buffer
      "mbs" 'sql-set-sqli-buffer

      ;; dialects
      "mhk" 'dotemacs-sql-highlight

      ;; interactivity
      "msb" 'sql-send-buffer
      "msB" 'dotemacs-sql-send-buffer-and-focus
      "msi" 'dotemacs-sql-start
      ;; paragraph gets "f" here because they can be assimilated to functions.
      ;; If you separate your commands in a SQL file, this key will send the
      ;; command under the point, which is what you probably want.
      "msf" 'sql-send-paragraph
      "msF" 'dotemacs-sql-send-paragraph-and-focus
      "msq" 'sql-send-string
      "msQ" 'dotemacs-sql-send-string-and-focus
      "msr" 'sql-send-region
      "msR" 'dotemacs-sql-send-region-and-focus

      ;; listing
      "mla" 'sql-list-all
      "mlt" 'sql-list-table)

    (evil-leader/set-key-for-mode 'sql-interactive-mode
      ;; sqli buffer
      "mbr" 'sql-rename-buffer
      "mbS" 'sql-save-connection)

    (add-hook 'sql-interactive-mode-hook
              (lambda () (toggle-truncate-lines t)))))

(use-package sql-indent
  :ensure t
  :defer t)


;;; Version control
(use-package vc-hooks                   ; Simple version control
  :defer t
  :config
  ;; Always follow symlinks to files in VCS repos
  (setq vc-follow-symlinks t))

(use-package diff-mode
  :defer t
  :ensure t
  :config
  (evilify diff-mode diff-mode-map
           "j" 'diff-hunk-next
           "k" 'diff-hunk-prev))

(use-package diff-hl                    ; Highlight hunks in fringe
  :ensure t
  :defer t
  :init
  (progn
    (setq diff-hl-side 'right)
    ;; Highlight changes to the current file in the fringe
    (global-diff-hl-mode)
    ;; Highlight changed files in the fringe of Dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

    ;; Fall back to the display margin, if the fringe is unavailable
    (unless (display-graphic-p)
      (setq diff-hl-side 'left)
      (diff-hl-margin-mode))

    (evil-leader/set-key
        "ghr" 'diff-hl-revert-hunk
        "ghN" 'diff-hl-previous-hunk
        "ghn" 'diff-hl-next-hunk
        "ghg" 'diff-hl-diff-goto-hunk)))

;; magit
(use-package init-magit
  :load-path "config/")

(use-package magit                      ; The one and only Git frontend
  :ensure t
  :bind (("C-c v g" . magit-status)
         ("C-c v v" . magit-status)
         ("C-c v b" . magit-blame-mode)
         ("C-c v l" . magit-log-all))
  :commands (magit-status
             magit-blame-mode
             magit-log
             magit-commit)
  :init
  (progn
    (setq magit-save-some-buffers 'dontask
          magit-stage-all-confirm nil
          magit-unstage-all-confirm nil
          magit-show-child-count t
          magit-completing-read-function 'magit-builtin-completing-read
          ;; Except when you ask something useful…
          magit-set-upstream-on-push t)

    (when (boundp 'fci-mode)
      (add-hook 'git-commit-mode-hook #'fci-mode))

    ;; On Windows, we must use Git GUI to enter username and password
    ;; See: https://github.com/magit/magit/wiki/FAQ#windows-cannot-push-via-https
    (when (eq window-system 'w32)
      (setenv "GIT_ASKPASS" "git-gui--askpass"))

    (after "projectile"
      (dotemacs-magit-set-repo-dirs-from-projectile))

    (evil-leader/set-key
      "gb" 'magit-blame
      "gl" 'magit-log-all
      "gL" 'magit-log-buffer-file
      "gs" 'magit-status
      "gd" 'dotemacs-magit-diff-head
      "gC" 'magit-commit))
  :config
  (progn
    ;; seems to be necessary at the time of release
    (require 'git-rebase)
    ;; mode maps

    (dotemacs-evilify-map magit-mode-map)
    (dotemacs-evilify-map magit-status-mode-map
      :mode magit-status-mode
      :bindings
      (kbd "C-S-j") 'magit-section-forward
      (kbd "C-S-k") 'magit-section-backward
      (kbd "C-n") 'magit-section-forward
      (kbd "C-p") 'magit-section-backward)
    (dotemacs-evilify-map magit-refs-mode-map
      :mode magit-refs-mode
      :bindings
      (kbd "C-S-j") 'magit-section-forward
      (kbd "C-S-k") 'magit-section-backward
      (kbd "C-n") 'magit-section-forward
      (kbd "C-p") 'magit-section-backward)
    (dotemacs-evilify-map magit-blame-mode-map
      :mode magit-blame-mode
      :bindings
      (kbd "C-S-j") 'magit-section-forward
      (kbd "C-S-k") 'magit-section-backward
      (kbd "C-n") 'magit-section-forward
      (kbd "C-p") 'magit-section-backward)
    (dotemacs-evilify-map magit-diff-mode-map
      :mode magit-diff-mode
      :bindings
      (kbd "C-S-j") 'magit-section-forward
      (kbd "C-S-k") 'magit-section-backward
      (kbd "C-n") 'magit-section-forward
      (kbd "C-p") 'magit-section-backward)
    (dotemacs-evilify-map magit-log-read-revs-map
      :mode magit-log-read-revs
      :bindings
      (kbd "C-S-j") 'magit-section-forward
      (kbd "C-S-k") 'magit-section-backward
      (kbd "C-n") 'magit-section-forward
      (kbd "C-p") 'magit-section-backward)
    (dotemacs-evilify-map magit-log-mode-map
      :mode magit-log-mode
      :bindings
      (kbd "C-S-j") 'magit-section-forward
      (kbd "C-S-k") 'magit-section-backward
      (kbd "C-n") 'magit-section-forward
      (kbd "C-p") 'magit-section-backward)
    (dotemacs-evilify-map magit-log-select-mode-map
      :mode magit-log-select-mode
      :bindings
      (kbd "C-S-j") 'magit-section-forward
      (kbd "C-S-k") 'magit-section-backward
      (kbd "C-n") 'magit-section-forward
      (kbd "C-p") 'magit-section-backward)
    (dotemacs-evilify-map magit-cherry-mode-map
      :mode magit-cherry-mode
      :bindings
      (kbd "C-S-j") 'magit-section-forward
      (kbd "C-S-k") 'magit-section-backward
      (kbd "C-n") 'magit-section-forward
      (kbd "C-p") 'magit-section-backward)
    (dotemacs-evilify-map magit-reflog-mode-map
      :mode magit-reflog-mode
      :bindings
      (kbd "C-S-j") 'magit-section-forward
      (kbd "C-S-k") 'magit-section-backward
      (kbd "C-n") 'magit-section-forward
      (kbd "C-p") 'magit-section-backward)
    (dotemacs-evilify-map magit-process-mode-map
      :mode magit-process-mode
      :bindings
      (kbd "C-S-j") 'magit-section-forward
      (kbd "C-S-k") 'magit-section-backward
      (kbd "C-n") 'magit-section-forward
      (kbd "C-p") 'magit-section-backward)
    (dotemacs-evilify-map git-rebase-mode-map
      :mode git-rebase-mode
      :bindings
      (kbd "C-S-j") 'magit-section-forward
      (kbd "C-S-k") 'magit-section-backward
      (kbd "C-n") 'magit-section-forward
      (kbd "C-p") 'magit-section-backward
      "J" 'git-rebase-move-line-down
      "K" 'git-rebase-move-line-up
      "u" 'git-rebase-undo
      "y" 'git-rebase-insert)

    ;; default state for additional modes
    (dolist (mode '(magit-popup-mode
                    magit-popup-sequence-mode))
      (add-to-list 'evil-emacs-state-modes mode))
    (dotemacs-evilify-configure-default-state 'magit-revision-mode)
    ;; section maps
    (dotemacs-evilify-map magit-tag-section-map)
    (dotemacs-evilify-map magit-untracked-section-map)
    (dotemacs-evilify-map magit-branch-section-map)
    (dotemacs-evilify-map magit-remote-section-map)
    (dotemacs-evilify-map magit-file-section-map)
    (dotemacs-evilify-map magit-hunk-section-map)
    (dotemacs-evilify-map magit-unstaged-section-map)
    (dotemacs-evilify-map magit-staged-section-map)
    (dotemacs-evilify-map magit-commit-section-map)
    (dotemacs-evilify-map magit-module-commit-section-map)
    (dotemacs-evilify-map magit-unpulled-section-map)
    (dotemacs-evilify-map magit-unpushed-section-map)
    (dotemacs-evilify-map magit-stashes-section-map)
    (dotemacs-evilify-map magit-stash-section-map)

    (add-hook 'projectile-switch-project-hook
      #'dotemacs-magit-set-repo-dirs-from-projectile)

    ;; full screen magit-status
    (when dotemacs-git-magit-status-fullscreen
      (setq magit-restore-window-configuration t)
      (setq magit-status-buffer-switch-function
            (lambda (buffer)
              (pop-to-buffer buffer)
              (delete-other-windows))))

    ;; rebase mode
    (evil-leader/set-key-for-mode 'git-rebase-mode
      "mcc" 'git-rebase-server-edit
      "mk" 'git-rebase-abort)
    ;; commit mode
    (evil-leader/set-key-for-mode 'git-commit-mode
      "mcc" 'git-commit-commit
      "mk" 'git-commit-abort)

    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
    (define-key magit-status-mode-map (kbd "C-S-w") 'magit-toggle-whitespace)))

(setq github-post-extensions '(magit-gh-pulls))
(use-package magit-gh-pulls
  :disabled t ; not compatible with magit 2.1
  :commands magit-gh-pulls-mode
  :init
  (progn
    (after "magit"
      '(progn
         (define-key magit-mode-map "#gg" 'dotemacs-load-gh-pulls-mode)
         (define-key magit-mode-map "#gf" 'dotemacs-fetch-gh-pulls-mode))))
  :diminish (magit-gh-pulls-mode . "Github-PR"))

(use-package magit-gitflow
  :ensure t
  :disabled t ; not compatible with magit 2.1
  :commands turn-on-magit-gitflow
  :init (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
  :diminish (magit-gitflow-mode . "Flow"))

(use-package magit-svn
  :if dotemacs-git-enable-magit-svn-plugin
  :disabled t ; not compatible with magit 2.1
  :ensure t
  :commands turn-on-magit-svn
  :init (add-hook 'magit-mode-hook 'turn-on-magit-svn)
  :config
  (progn
    (evil-define-key 'emacs magit-status-mode-map
      "N" 'magit-key-mode-popup-svn)))

;; git
(use-package git-commit-mode            ; Git commit message mode
  :disabled t ; not compatible with magit 2.1
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
  :disabled t ; not compatible with magit 2.1
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

;; github
(use-package gist
  :defer t
  :ensure t
  :init
  (progn
    (evilify gist-list-menu-mode gist-list-menu-mode-map
             "f" 'gist-fetch-current
             "K" 'gist-kill-current
             "o" 'gist-browse-current-url)

    (evil-leader/set-key
      "ggb" 'gist-buffer
      "ggB" 'gist-buffer-private
      "ggl" 'gist-list
      "ggr" 'gist-region
      "ggR" 'gist-region-private)))

(use-package github-browse-file
  :defer t
  :ensure t
  :init
  (evil-leader/set-key
    "gfb" 'github-browse-file))

(use-package git-link
  :defer t
  :ensure t
  :init
  (progn
    (evil-leader/set-key
      "gfl" 'git-link
      "gfL" 'dotemacs-git-link-copy-url-only
      "gfc" 'git-link-commit
      "gfC" 'dotemacs-git-link-commit-copy-url-only)

    ;; default is to open the generated link
    (setq git-link-open-in-browser t))
  :config
  (progn

    (defun git-link-ibaset (hostname dirname filename branch commit start end)
      (format "https://git.ibaset.com/%s/blob/%s/%s#%s"
        dirname
        (or branch commit)
        filename
        (if (and start end)
            (format "L%s-L%s" start end)
          (format "L%s" start))))

    (defun git-link-commit-ibaset (hostname dirname commit)
      (format "https://git.ibaset.com/%s/commit/%s" dirname commit))

    (add-to-list 'git-link-remote-alist
      '("git.ibaset.com" git-link-github))
    (add-to-list 'git-link-commit-remote-alist
      '("git.ibaset.com" git-link-commit-github))
    (add-to-list 'git-link-remote-alist
      '("andrew.git.ibaset.com" git-link-ibaset))
    (add-to-list 'git-link-commit-remote-alist
      '("andrew.git.ibaset.com" git-link-commit-ibaset))))


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
  :defer 1
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
          "pf" 'projectile-find-file
          "ph" 'helm-projectile
          "pr" 'projectile-recentf
          "ps" 'projectile-switch-project))
      (evil-leader/set-key
        "p!" 'projectile-run-shell-command-in-root
        "p&" 'projectile-run-async-shell-command-in-root
        "pc" 'projectile-compile-project
        "pD" 'projectile-dired
        "pG" 'projectile-regenerate-tags
        "pI" 'projectile-invalidate-cache
        "pk" 'projectile-kill-buffers
        "po" 'projectile-multi-occur
        "pR" 'projectile-replace
        "pT" 'projectile-find-test-file
        "py" 'projectile-find-tag)))
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
    ; disable till fixed https://github.com/emacs-helm/helm/issues/1088
    ; disable till fixed https://github.com/bbatsov/projectile/issues/794
    ; (helm-projectile-on)

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
        "pf"  'helm-projectile-find-file
        "ph"  'helm-projectile
        "pp"  'helm-projectile-switch-project
        "pr"  'helm-projectile-recentf
        "pv"  'projectile-vc
        "sgp" 'helm-projectile-grep))))


;;; Project management with Project Explorer
; todo: make choice between project explorer and neo-tree
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
        "tn" 'project-explorer-open
        "tN" 'project-explorer-toggle))
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
          neo-modern-sidebar t
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
        "tn" 'neotree-show
        "tN" 'neotree-hide
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
; todo add flyspell
(use-package init-org
  :load-path "config/")

(use-package org-plus-contrib
  :mode ("\\.org$" . org-mode)
  :ensure t
  :defer t
  :init
  (progn
    (setq org-replace-disputed-keys t ;; Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
          org-src-fontify-natively t ;; Fontify org-mode code blocks
          org-clock-persist-file (concat dotemacs-cache-directory "org-clock-save.el")
          org-log-done t
          org-startup-with-inline-images t
          org-startup-indented t)

    (after "org-indent"
      '(diminish org-indent-mode))

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
                 ("DONE" ("WAITING") ("CANCELLED"))))))

    (defmacro dotemacs-org-emphasize (fname char)
      "Make function for setting the emphasis in org mode"
      `(defun ,fname () (interactive)
              (org-emphasize ,char)))

    (after "evil-leader"
      (evil-leader/set-key-for-mode 'org-mode
        "m'" 'org-edit-special
        "mc" 'org-capture
        "md" 'org-deadline
        "me" 'org-export-dispatch
        "mf" 'org-set-effort

        ;; headings
        "mhi" 'org-insert-heading-after-current
        "mhI" 'org-insert-heading

        "mI" 'org-clock-in
        (if dotemacs-major-mode-leader-key
            (concat "m" dotemacs-major-mode-leader-key)
          "m,") 'org-ctrl-c-ctrl-c
          "mn" 'org-narrow-to-subtree
          "mN" 'widen
          "mO" 'org-clock-out
          "mq" 'org-clock-cancel
          "mR" 'org-refile
          "ms" 'org-schedule

          ;; insertion of common elements
          "mil" 'org-insert-link
          "mif" 'org-footnote-new
          "mik" 'dotemacs-insert-keybinding-org

          ;; images and other link types have no commands in org mode-line
          ;; could be inserted using yasnippet?
          ;; region manipulation
          "mxb" (dotemacs-org-emphasize dotemacs-org-bold ?*)
          "mxc" (dotemacs-org-emphasize dotemacs-org-code ?~)
          "mxi" (dotemacs-org-emphasize dotemacs-org-italic ?/)
          "mxr" (dotemacs-org-emphasize dotemacs-org-clear ? )
          "mxs" (dotemacs-org-emphasize dotemacs-org-strike-through ?+)
          "mxu" (dotemacs-org-emphasize dotemacs-org-underline ?_)
          "mxv" (dotemacs-org-emphasize dotemacs-org-verbose ?=)))

    (require 'ox-md)
    (require 'ox-ascii)
    (require 'ox-confluence)
    (require 'ox-html)
    (require 'org-bullets)

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
    (font-lock-add-keywords
     'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
                  (1 font-lock-comment-face prepend)
                  (2 font-lock-function-name-face)
                  (3 font-lock-comment-face prepend))))

    (require 'org-indent)
    (define-key global-map "\C-cl" 'org-store-link)
    (define-key global-map "\C-ca" 'org-agenda)

    (evil-leader/set-key
      "Cc" 'org-capture)))

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
  :commands evil-org-mode
  :ensure t
  :init
  (add-hook 'org-mode-hook 'evil-org-mode)
  :config
  (progn
    (after "evil-leader"
      (evil-leader/set-key-for-mode 'org-mode
        "a" nil "ma" 'org-agenda
        "b" nil "mb" 'org-tree-to-indirect-buffer
        "c" nil "mA" 'org-archive-subtree
        "o" nil "mC" 'evil-org-recompute-clocks
        "l" nil "ml" 'evil-org-open-links
        "t" nil "mt" 'org-show-todo-tree))
      (evil-define-key 'normal evil-org-mode-map
        "O" 'evil-open-above))
  :diminish (evil-org-mode . " ⓔ"))

(use-package org-pomodoro
  :defer t
  :ensure t
  :init
  (progn
    (when (system-is-mac)
      (setq org-pomodoro-audio-player "/usr/bin/afplay"))
    (evil-leader/set-key-for-mode 'org-mode
      "mp" 'org-pomodoro)))

(use-package org-present
  :defer t
  :ensure t
  :init
  (progn
    (after "evil-evilified-state"
      (evilify nil org-present-mode-keymap
        "h" 'org-present-prev
        "l" 'org-present-next
        "q" 'org-present-quit))
    (add-hook 'org-present-mode-hook 'dotemacs-org-present-start)
    (add-hook 'org-present-mode-quit-hook 'dotemacs-org-present-end)))

(use-package toc-org
  :ensure t
  :init
  (add-hook 'org-mode-hook 'toc-org-enable))

(use-package htmlize
  :ensure t
  :defer t)

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
      (evil-leader/set-key "?" 'helm-descbinds))))

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

(defvar dash-helm-dash-docset-path ""
  "Path containing dash docsets.")

(use-package helm-dash
  :defer t
  :ensure t
  :if (eq system-type 'gnu/linux)
  :init
  (progn
    (evil-leader/set-key "dd" 'helm-dash-at-point)
    (evil-leader/set-key "dD" 'helm-dash))
  :config
  (progn
    (defun dash//activate-package-docsets (path)
      "Add dash docsets from specified PATH."
      (setq helm-dash-docsets-path path
            helm-dash-common-docsets (helm-dash-installed-docsets))
      (message (format "activated %d docsets from: %s"
                       (length helm-dash-common-docsets) path)))
    (dash//activate-package-docsets dash-helm-dash-docset-path)))

(use-package dash-at-point
  :ensure t
  :if (eq system-type 'darwin)
  :defer t
  :bind (("C-c h d" . dash-at-point)
         ("C-c h D" . dash-at-point-with-docset))
  :init
  (progn
    (evil-leader/set-key "dd" 'dash-at-point)
    (evil-leader/set-key "dD" 'dash-at-point-with-docset))
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
          guide-key-tip/enabled (if window-system t))
    (setq guide-key/highlight-command-regexp
                 (cons dotemacs-prefix-command-string font-lock-warning-face))
    (guide-key-mode 1))
  ; :config
  ; (add-hook 'evil-leader-mode-hook
  ;           #'(lambda () (guide-key/add-local-guide-key-sequence evil-leader/leader)))
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

(use-package ansible
  ;; Tracking here:
  ;; https://github.com/k1LoW/emacs-ansible/issues/4
  ;; Ansible wants to hook into yasnippet and load its snippets always
  :disabled t
  :defer t
  :init
  (progn
    (after "yaml-mode"
      '(add-hook 'yaml-mode-hook 'ansible/ansible-maybe-enable))))

(use-package ansible-doc                ; Documentation lookup for Ansible
  :ensure t
  :defer t
  :init (after "yaml-mode"
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
(dotemacs-defvar-company-backends puppet-mode)

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
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook puppet-mode))))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (add-hook 'puppet-mode-hook 'flycheck-mode))

(use-package puppetfile-mode
  :ensure t
  :defer t)


;;; Finance
(dotemacs-defvar-company-backends ledger-mode)

(use-package ledger-mode
  :mode ("\\.\\(ledger\\|ldg\\)\\'" . ledger-mode)
  :ensure t
  :defer t
  :init
  (progn
    (setq ledger-post-amount-alignment-column 62)
    (evil-leader/set-key-for-mode 'ledger-mode
      "mhd"   'ledger-delete-current-transaction
      "ma"    'ledger-add-transaction
      "mb"    'ledger-post-edit-amount
      "mc"    'ledger-toggle-current
      "mC"    'ledger-mode-clean-buffer
      "ml"    'ledger-display-ledger-stats
      "mp"    'ledger-display-balance-at-point
      "mq"    'ledger-post-align-xact
      "mr"    'ledger-reconcile
      "mR"    'ledger-report
      "mt"    'ledger-insert-effective-date
      "my"    'ledger-set-year
      "m RET" 'ledger-set-month)
    (evilify ledger-report-mode ledger-report-mode-map)))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (add-hook 'ledger-mode-hook 'flycheck-mode))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (push 'company-capf company-backends-ledger-mode)
      (dotemacs-add-company-hook ledger-mode))))

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

;; search engine
(use-package engine-mode
  :commands (defengine dotemacs-search-engine-select)
  :ensure t
  :defines search-engine-alist
  :init
  (evil-leader/set-key
    "a/" 'dotemacs-search-engine-select)
  (setq search-engine-alist
        '((amazon
           :name "Amazon"
           :url "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%%3Daps&field-keywords=%s")
          (duck-duck-go
           :name "Duck Duck Go"
           :url "https://duckduckgo.com/?q=%s")
          (google
           :name "Google"
           :url "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")
          (google-images
           :name "Google Images"
           :url "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")
          (github
           :name "Github"
           :url "https://github.com/search?ref=simplesearch&q=%s")
          (google-maps
           :name "Google Maps"
           :url "http://maps.google.com/maps?q=%s")
          (twitter
           :name "Twitter"
           :url "https://twitter.com/search?q=%s")
          (project-gutenberg
           :name "Project Gutenberg"
           :url "http://www.gutenberg.org/ebooks/search.html/?format=html&default_prefix=all&sort_order=&query=%s")
          (youtube
           :name "YouTube"
           :url "http://www.youtube.com/results?aq=f&oq=&search_query=%s")
          (stack-overflow
           :name "Stack Overflow"
           :url "https://stackoverflow.com/search?q=%s")
          (wikipedia
           :name "Wikipedia"
           :url "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
          (wolfram-alpha
           :name "Wolfram Alpha"
           :url "http://www.wolframalpha.com/input/?i=%s")))
  :config
  (engine-mode t)
  (mapcar (lambda (engine)
            (let* ((cur-engine (car engine))
                   (engine-url (plist-get (cdr engine) :url)))
              (eval `(defengine ,cur-engine ,engine-url))))
          search-engine-alist)
  (defun dotemacs-search-engine-source (engines)
    "return a source for helm selection"
    `((name . "Search Engines")
      (candidates . ,(mapcar (lambda (engine)
                               (cons (plist-get (cdr engine) :name)
                                     (intern (format "engine/search-%S"
                                                     (car engine)))))
                             engines))
      (action . (lambda (candidate) (call-interactively candidate)))))
  (defun dotemacs-search-engine-select ()
    "set search engine to use"
    (interactive)
    (helm :sources (list (dotemacs-search-engine-source
                          search-engine-alist)))))

;; emoji
(use-package emoji-cheat-sheet-plus
  :ensure t
  :commands (emoji-cheat-sheet-plus-insert
             emoji-cheat-sheet-plus-buffer
             emoji-cheat-sheet-plus-display-mode)
  :init
  (progn
    (evil-leader/set-key "aE" 'emoji-cheat-sheet-plus-buffer)
    (evil-leader/set-key "ie" 'emoji-cheat-sheet-plus-insert)
    (evilify emoji-cheat-sheet-plus-buffer-mode
             emoji-cheat-sheet-plus-buffer-mode-map
             "<RET>" 'emoji-cheat-sheet-plus-echo-and-copy)
    (defun dotemacs-delay-emoji-cheat-sheet-hook ()
      "Work-around for org buffers."
      ;; we need to wait for org buffer to be fully loaded before
      ;; calling the emoji mode.
      ;; If we directly call the emoji mode at hook runtime then some
      ;; text properties are not applied correctly.
      (run-at-time 0.1 nil 'emoji-cheat-sheet-plus-display-mode))
    (add-hook 'org-mode-hook 'dotemacs-delay-emoji-cheat-sheet-hook)
    (add-to-hooks 'emoji-cheat-sheet-plus-display-mode '(markdown-mode))))

;; vagrant
(use-package vagrant
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-declare-prefix "V" "vagrant")
    (evil-leader/set-key
      "VD" 'vagrant-destroy
      "Ve" 'vagrant-edit
      "VH" 'vagrant-halt
      "Vp" 'vagrant-provision
      "Vr" 'vagrant-resume
      "Vs" 'vagrant-status
      "VS" 'vagrant-suspend
      "VV" 'vagrant-up)))

(use-package vagrant-tramp
  :defer t
  :ensure t
  :init
  (progn
    (defvar dotemacs--vagrant-tramp-loaded nil)
    (defadvice vagrant-tramp-term (before dotemacs-load-vagrant activate)
      "Lazy load vagrant-tramp."
      (unless dotemacs--vagrant-tramp-loaded
        (vagrant-tramp-enable)
        (setq dotemacs--vagrant-tramp-loaded t)))
    (evil-leader/set-key "Vt" 'vagrant-tramp-term)))


;;; Auto highlight symbol
(use-package init-auto-highlight-symbol
  :load-path "config/")

(use-package auto-highlight-symbol
  :defer t
  :ensure t
  :init
  (add-to-hooks 'auto-highlight-symbol-mode '(prog-mode-hook
                                              markdown-mode-hook))
  :config
  (progn
    (custom-set-variables
     '(ahs-case-fold-search nil)
     '(ahs-default-range (quote ahs-range-whole-buffer))
     ;; disable auto-highlight of symbol
     ;; current symbol should be highlight on demand with <SPC> s h
     '(ahs-idle-timer 0)
     '(ahs-idle-interval 0.25)
     '(ahs-inhibit-face-list nil))

    (defvar dotemacs-last-ahs-highlight-p nil
      "Info on the last searched highlighted symbol.")
    (make-variable-buffer-local 'dotemacs-last-ahs-highlight-p)

    (after "evil"
      '(progn
         (define-key evil-motion-state-map (kbd "*") 'dotemacs-quick-ahs-forward)
         (define-key evil-motion-state-map (kbd "#") 'dotemacs-quick-ahs-backward)))

    (evil-leader/set-key
      "sh"  'dotemacs-symbol-highlight
      "sH"  'dotemacs-goto-last-searched-ahs-symbol
      "sR"  'dotemacs-symbol-highlight-reset-range)

    (dotemacs-hide-lighter auto-highlight-symbol-mode)
    ;; micro-state to easily jump from a highlighted symbol to the others
    (dolist (sym '(ahs-forward
                   ahs-forward-definition
                   ahs-backward
                   ahs-backward-definition
                   ahs-back-to-start
                   ahs-change-range))
      (let* ((advice (intern (format "dotemacs-%s" (symbol-name sym)))))
        (eval `(defadvice ,sym (after ,advice activate)
                 (dotemacs-ahs-highlight-now-wrapper)
                 (setq dotemacs-last-ahs-highlight-p (ahs-highlight-p))
                 (dotemacs-auto-highlight-symbol-overlay-map)))))))


;;; Syntax Checking
(use-package init-syntax-checking
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
    (setq flycheck-standard-error-navigation nil)

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
        "eC" 'flycheck-compile
        ; https://github.com/flycheck/flycheck/pull/494
        "el" 'dotemacs-flycheck-pop-to-error-list ;flycheck-list-errors
        "eL" 'dotemacs-flycheck-hide-list-errors
        "es" 'flycheck-select-checker
        "ex" 'flycheck-disable-checker
        "e?" 'flycheck-describe-checker
        "tmf" 'dotemacs-mode-line-flycheck-info-toggle))

    (dotemacs-set-flycheck-mode-line-faces)
    (unless (display-graphic-p)
      (setq flycheck-display-errors-function
            #'flycheck-display-error-messages-unless-error-list
            flycheck-mode-line
            '(:eval (dotemacs-flycheck-mode-line-status))))

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

    (defun dotemacs-flycheck-pop-to-error-list ()
      (interactive)
      (flycheck-list-errors)
      (pop-to-buffer flycheck-error-list-buffer))

    (defun dotemacs-flycheck-hide-list-errors ()
      "Hide the error list for the current buffer."
      (interactive)
      (let ((buffer (get-buffer flycheck-error-list-buffer)))
        (when buffer
          (let ((window (get-buffer-window buffer)))
            (when window
              (unless (flycheck-overlays-at (point))
                (quit-window nil window)))))))

    (after "evil-evilified-state"
      (evilify flycheck-error-list-mode flycheck-error-list-mode-map
        "j" #'flycheck-error-list-next-error
        "k" #'flycheck-error-list-previous-error
        "RET" #'flycheck-error-list-goto-error))

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
  ;; flycheck errors on a tooltip (doesnt work on console)
  :if (and dotemacs-s-syntax-checking-enable-tooltips (display-graphic-p))
  :defer t
  :init
  (after "flycheck"
    (setq flycheck-display-errors-function
          #'flycheck-pos-tip-error-messages
          flycheck-mode-line
          '(:eval (dotemacs-flycheck-mode-line-status)))))

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
    (setq flyspell-use-meta-tab nil
          flyspell-issue-welcome-flag nil  ;; Make Flyspell less chatty
          flyspell-issue-message-flag nil)
    (dolist (hook '(text-mode-hook message-mode-hook))
      (add-hook hook '(lambda () (flyspell-mode 1))))
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
    ;; Undefine mouse buttons which get in the way
    (define-key flyspell-mouse-map [down-mouse-2] nil)
    (define-key flyspell-mouse-map [mouse-2] nil)
    (flyspell-prog-mode))
  :diminish (flyspell-mode . " Ⓢ"))

(use-package helm-flycheck
  :ensure t
  :commands helm-flycheck
  :init (evil-leader/set-key "ef" 'helm-flycheck))

(use-package helm-flyspell
  :ensure t
  :commands helm-flyspell-correct
  :init (evil-leader/set-key "Sc" 'helm-flyspell-correct))


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
  :diminish (company-mode . " ⓐ"))

(use-package company-statistics
  :ensure t
  :if (and auto-completion-enable-sort-by-usage
           (eq dotemacs-completion-engine 'company))
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
    (after "auto-complete"
      '(ac-ispell-setup))
    ))

(use-package init-yasnippet
  :load-path "config/"
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

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
