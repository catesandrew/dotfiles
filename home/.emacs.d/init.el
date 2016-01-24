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
;; - c-c l: List things
;; - C-c m: Multiple cursors
;; - C-c s: Symbol commands
;; - C-c t: Skeletons and templates
;; - C-c u: Miscellaneous utilities, including minor modes
;; - C-c v: Version control
;; - C-c w: Web stuff
;; - C-x x: Perspective

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 100000000)

;;; Code:

;; No splash screen please ... jeez
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)

(defvar dotemacs--default-mode-line mode-line-format
  "Backup of default mode line format.")

;; Set path to dependencies
(defgroup dotemacs nil
  "Custom configuration for dotemacs."
  :group 'local)

(defconst dotemacs-core-directory
  (expand-file-name (concat user-emacs-directory "core/"))
  "core directory.")

(defconst dotemacs-config-dir
  (expand-file-name (concat user-emacs-directory "config/"))
  "config directory.")

(defconst dotemacs-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "The storage location for various persistent files.")

(defconst dotemacs-auto-save-directory
  (expand-file-name (concat dotemacs-cache-directory "auto-save/"))
  "The auto-save directory")

(defconst dotemacs-quelpa-directory
  (concat dotemacs-cache-directory "quelpa/")
  "Quelpa directory")

(defconst dotemacs-quelpa-build-directory
  (expand-file-name "build" dotemacs-quelpa-directory)
  "Quelpa build directory")

(defconst dotemacs-quelpa-cache-directory
  (expand-file-name "cache" dotemacs-quelpa-directory)
  "Quelpa cache directory")

(defconst user-home-directory
  (expand-file-name "~/")
  "User home directory (~/).")

(defconst emacs-version-short (replace-regexp-in-string
                               "\\([0-9]+\\)\\.\\([0-9]+\\).*"
                               "\\1_\\2" emacs-version))

(defconst pcache-directory
  (concat dotemacs-cache-directory "pcache"))
(unless (file-exists-p dotemacs-cache-directory)
    (make-directory dotemacs-cache-directory))

(defconst dotemacs-editing-style 'vim
  "Always `vim', Evil is always enabled.")

(defcustom dotemacs-erc-nick
  'catesandrew
  "The erc nickname to use"
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
                             farmhouse
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
                             jbeans
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

(defcustom dotemacs-colorize-cursor-according-to-state t
  "If non nil the cursor color matches the state color in GUI
Emacs."
  :group 'dotemacs)

(defvaralias 'dotemacs-remap-Y-to-y$ 'evil-want-Y-yank-to-eol
  "If non nil `Y' is remapped to `y$'.")

(defcustom dotemacs-leader-key ","
  "The leader key."
  :group 'dotemacs)

(defcustom dotemacs-emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'"
  :group 'dotemacs)

;; major-mode-leader to 0x00A0 (NO_BREAK_SPACE)
(defcustom dotemacs-major-mode-leader-key " "
  "Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m`. Set it to `nil` to disable it."
  :group 'dotemacs)

(defcustom dotemacs-major-mode-emacs-leader-key nil
  "Major mode leader key accessible in `emacs state' and `insert state'"
  :group 'dotemacs)

; Source Code Pro for Powerline
; Pragmata Pro
(defcustom dotemacs-default-font '("Hasklig"
                                    :size 13
                                    :weight normal
                                    :width normal
                                    :powerline-scale 1.0)
  "Default font. `powerline-scale' allows to quickly tweak the mode-line
size to make separators look not too crappy."
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

(defcustom dotemacs-which-key-delay 0.4
  "Delay in seconds starting from the last keystroke after which
the which-key buffer will be shown if you have not completed a
key sequence. Setting this variable is equivalent to setting
`which-key-idle-delay'."
  :group 'dotemacs)

;; Possible options should be: right bottom right-then-bottom
(defcustom dotemacs-which-key-position 'bottom
  "Location of the which-key popup buffer. Possible choices are bottom,
right, and right-then-bottom. The last one will display on the
right if possible and fallback to bottom if not."
  :group 'dotemacs)

(defcustom dotemacs-search-tools '("ag" "pt" "ack" "grep")
  "List of search tool executable names. Dotemacs uses the first installed
tool of the list. Supported tools are `ag', `pt', `ack' and `grep'."
  :group 'dotemacs)

(defcustom dotemacs-startup-lists '(recents projects)
  "List of items to show in the startup buffer. If nil it is disabled.
Possible values are: `recents' `bookmarks' `projects'."
  :group 'dotemacs)

(defcustom dotemacs-startup-recent-list-size 5
  "Number of recent files to show in the startup buffer. Ignored if
`dotemacs-startup-lists' doesn't include `recents'."
  :group 'dotemacs)

;; Regexp for useful and useless buffers for smarter buffer switching
(defcustom dotemacs-useless-buffers-regexp '("*\.\+")
  "Regexp used to determine if a buffer is not useful."
  :group 'dotemacs)

(defcustom dotemacs-useful-buffers-regexp '("\\*\\(scratch\\|terminal\.\+\\|ansi-term\\|eshell\\)\\*")
  "Regexp used to define buffers that are useful despite matching
`dotemacs-useless-buffers-regexp'."
  :group 'dotemacs)

(defcustom dotemacs-active-transparency 96
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's active or selected. Transparency
can be toggled through `toggle-transparency'."
  :group 'dotemacs)

(defcustom dotemacs-inactive-transparency 96
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's inactive or deselected. Transparency
can be toggled through `toggle-transparency'."
  :group 'dotemacs)

(defcustom dotemacs-smartparens-strict-mode nil
  "If non-nil smartparens-strict-mode will be enabled in programming modes."
  :group 'dotemacs)

(defconst dotemacs-filepath (expand-file-name "." user-emacs-directory)
  "Filepath to the installed dotfile.")

(defvar dotemacs-line-numbers 'relative
  "If non nil line numbers are turned on in all `prog-mode' and `text-mode'.
derivatives. If set to `relative', also turns on relative line numbers.")

(defcustom dotemacs-persistent-server nil
  "If non nil advises quit functions to keep server open when quitting."
  :group 'dotemacs)

(defcustom dotemacs-smooth-scrolling t
  "If non nil smooth scrolling (native-scrolling) is enabled. Smooth scrolling
overrides the default behavior of Emacs which recenters the point when
it reaches the top or bottom of the screen."
  :group 'dotemacs)

(defcustom dotemacs-mode-line-unicode-symbols t
  "If non nil unicode symbols are displayed in the mode-line (eg. for lighters)"
  :group 'dotemacs)

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

(defvar dotemacs-whitespace-cleanup 'changed
  "Delete whitespace while saving buffer.

Possible values are `all', `trailing', `changed' or `nil'.
Default is `changed' (cleanup whitespace on changed lines)")


(defcustom dotemacs-highlight-delimiters 'all
  "Select a scope to highlight delimiters. Possible values are `any',
  `current', `all' or `nil'. Default is `all' (highlight any scope and
  emphasis the current one."
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

(defvar dotemacs-default-layout-name "Default"
  " Name of the default layout.")

(defvar dotemacs-display-default-layout nil
  "If non nil the default layout name is displayed in the mode-line.")

(defvar dotemacs-auto-resume-layouts nil
  "If non nil then the last auto saved layouts are resume automatically upon
start.")

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

(defcustom dotemacs-shell-default-term-shell shell-file-name
  "Default shell to use in `term' and `ansi-term' shells."
  :group 'dotemacs-shell)

(defcustom dotemacs-shell-enable-smart-eshell nil
  "If non-nil then `em-smart' is enabled. `em-smart' allows to quickly review
commands, modify old commands or enter a new one."
  :group 'dotemacs-shell)

(defcustom dotemacs-shell-protect-eshell-prompt t
  "If non-nil then eshell's prompt is protected. This means that
movement to the prompt is inhibited like for `comint-mode'
prompts and the prompt is made read-only"
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
  "Style to use for formatting with hindent; available are: fundamental
johan-tibell chris-done gibiansky. If nil hindent is disabled."
  :group 'dotemacs-haskell)

(defcustom dotemacs-haskell-enable-ghc-mod-support t
  "If non-nil ghc-mod support is enabled"
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

(defcustom dotemacs-evil-snipe-enable-alternate-f-and-t-behaviors nil
  "if non nil f/F/t/T behaviors are replaced by evil-snipe behavior."
  :group 'dotemacs-evil)

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

(defcustom dotemacs-ac-private-snippets-directory nil
  "Configurable private snippets directory."
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
(defgroup dotemacs-helm nil
  "Configuration options for helm"
  :group 'dotemacs
  :prefix 'dotemacs-helm)

(defcustom dotemacs-helm-resize nil
  "If non nil, `helm' will try to miminimize the space it uses.")

(defcustom dotemacs-helm-no-header t
  "if non nil, the helm header is hidden when there is only one source.")

(defcustom dotemacs-helm-position 'bottom
  "Position in which to show the `helm' mini-buffer.")

(defvar python-fill-column 79
  "Fill column value for python buffers")

(defvar syntax-checking-enable-by-default t
  "Enable syntax-checking by default.")

(defvar spell-checking-enable-by-default t
  "Enable spell checking by default.")

(defvar spell-checking-enable-auto-dictionary nil
  "Specify if auto-dictionary should be enabled or not.")

(defvar dotemacs-really-kill-emacs nil
  "prevent window manager close from closing instance.")

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


;;; Load Paths
(defun add-to-load-path (dir) (add-to-list 'load-path dir))

;; load paths
(mapc 'add-to-load-path
      `(
        ,dotemacs-core-directory
        ,dotemacs-config-dir
        ,dotemacs-user-settings-dir
        ))


;;; Core
(setq message-log-max 16384)

(require 'subr-x nil 'noerror)
(require 'core-auto-completion)
(require 'core-display-init)
(require 'core-themes-support)
(require 'core-fonts-support)
(require 'core-toggle)
(require 'core-micro-state)
(require 'core-use-package-ext)
(require 'core-keybindings)


;;; Macros

(defmacro dotemacs-bind (&rest commands)
  "Convience macro which creates a lambda interactive command."
  `(lambda ()
     (interactive)
     ,@commands))


;;; Core Package management
(require 'package)
(require 'core-funcs)
(require 'core-buffers)

(unless package--initialized
  (setq load-prefer-newer t)
  (setq package-archives '(("melpa" . "http://melpa.org/packages/")
                           ("org" . "http://orgmode.org/elpa/")
                           ; ("ELPA" . "http://tromey.com/elpa/")
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ))
  ;; optimization, no need to activate all the packages so early
  ;; http://stackoverflow.com/questions/11127109/
  (setq package-enable-at-startup nil)
  ;(package-initialize 'noactivate))
  (package-initialize))


;;; Initialization

(prefer-coding-system 'utf-8) ; with sugar on top

; (require 'rx)
; (require 'time-date)

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
(unless (eq system-type 'darwin)
  (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
    (menu-bar-mode -1)))
;; for convenience and user support
(unless (fboundp 'tool-bar-mode)
  (dotemacs-message (concat "No graphical support detected, you won't be"
                             "able to launch a graphical instance of Emacs"
                             "with this build.")))


;; font
(dotemacs|do-after-display-system-init
  (if (find-font (font-spec :name (car dotemacs-default-font)))
    (dotemacs-set-default-font dotemacs-default-font)
  (dotemacs-buffer/warning "Cannot find font \"%s\"!"
                           (car dotemacs-default-font))))
;; dotemacs init
(dotemacs-buffer/goto-buffer)
;; explicitly recreate the home buffer for the first GUI client
(dotemacs|do-after-display-system-init
  (kill-buffer (get-buffer dotemacs-buffer-name))
  (dotemacs-buffer/goto-buffer))
(setq initial-buffer-choice (lambda () (get-buffer dotemacs-buffer-name)))

;; fringes
(dotemacs|do-after-display-system-init
  (when (display-graphic-p)
    (custom-set-variables
      '(fringe-mode (quote (4 . 4)) nil (fringe)))
    (setq-default fringe-indicator-alist
                  '((truncation . nil) (continuation . nil)))))

;; mandatory dependencies
; (dotemacs-load-or-install-package 'dash t)
(dotemacs-load-or-install-package 's t)
;; bind-key is required by use-package
(dotemacs-load-or-install-package 'bind-key t)
(dotemacs-load-or-install-package 'use-package t)
;; package-build is required by quelpa
(dotemacs-load-or-install-package 'package-build t)

(setq quelpa-verbose init-file-debug
      use-package-verbose init-file-debug
      quelpa-dir dotemacs-quelpa-directory
      quelpa-build-dir dotemacs-quelpa-build-directory
      quelpa-persistent-cache-file dotemacs-quelpa-cache-directory
      quelpa-update-melpa-p nil)
(dotemacs-load-or-install-package 'quelpa t)
(dotemacs-load-or-install-package 'quelpa-use-package t)

;; inject use-package hooks for easy customization of
;; stock package configuration
(setq use-package-inject-hooks t)
; ;; which-key
(dotemacs-load-or-install-package 'which-key t)
;; evil and evil-leader must be installed at the beginning of the
;; boot sequence.
;; Use C-u as scroll-up (must be set before actually loading evil)
(dotemacs-load-or-install-package 'evil t)
(dotemacs-load-or-install-package 'evil-leader t)
(require 'core-evilified-state)


;;; Initialization

;; And disable the site default settings
(setq inhibit-default-init t)

;; save custom variables
(unless (bound-and-true-p custom-file)
  (setq custom-file dotemacs-custom-file))

;; start scratch in text mode (usefull to get a faster Emacs load time
;; because it avoids autoloads of elisp modes)
(setq initial-major-mode 'text-mode)

;; Disable case insensitivity for filename autocompletion in shell-mode
(setq pcomplete-ignore-case t) ;; Controls case sensitivity for pcomplete

(require 'init-funcs)

(when (and (dotemacs/system-is-mac) (version< emacs-version "25"))
  (warn "This configuration needs Emacs trunk, but this is %s!" emacs-version)
  (warn "brew install emacs --HEAD --srgb --use-git-head --with-cocoa --with-gnutls --with-rsvg --with-imagemagick"))

(when (dotemacs/system-is-mac)
  ;; Warn if the current build is more than a week old
  (run-with-idle-timer
   2 nil
   (lambda ()
     (let ((time-since-build (time-subtract (current-time) emacs-build-time)))
       (when (> (time-to-number-of-days time-since-build) 7)
         (lwarn 'emacs :warning "Your Emacs build is more than a week old!"))))))

(use-package init-util              ; Personal OS X tools
  :load-path "config/"
  :defer t
  :commands(my-recompile-init
            my-window-killer
            my-minibuffer-keyboard-quit
            my-set-transparency
            my-google
            my-copy-file-name-to-clipboard
            my-goto-scratch-buffer
            my-insert-last-kbd-macro
            my-buffer-to-unix-format
            my-buffer-to-dos-format))


;;; Key Binding Init

;; We define prefix commands only for the sake of which-key
(setq dotemacs-key-binding-prefixes
      '(("a"  "applications")
        ("ai" "irc")
        ("as" "shells")
        ("b"  "buffers")
        ("bm" "move")
        ("c"  "compile/comments")
        ("C"  "capture/colors")
        ("d"  "dash-at-point")
        ("e"  "errors")
        ("f"  "files")
        ("fC" "files/convert")
        ("fe" "emacs(dotemacs)")
        ("g"  "git/versions-control")
        ("gf" "file")
        ("gg" "github gist")
        ("h"  "helm/help/highlight")
        ("hd" "help-describe")
        ("i"  "insertion")
        ("j"  "join/split")
        ("k"  "lisp")
        ("kd" "delete")
        ("kD" "delete-backward")
        ("k`" "hybrid")
        ("n"  "narrow/numbers")
        ("p"  "projects")
        ("p$" "projects/shell")
        ("P"  "pandoc")
        ("q"  "quit")
        ("r"  "registers/rings")
        ("s"  "search/symbol")
        ("sa" "ag")
        ("sg" "grep")
        ("sk" "ack")
        ("st" "pt")
        ("sw" "web")
        ("S"  "spelling")
        ("t"  "toggles")
        ("tC" "colors")
        ("th" "highlight")
        ("tE" "editing-styles")
        ("tm" "modeline")
        ("T"  "toggles/themes")
        ("w"  "windows")
        ("wp" "popup")
        ("x"  "text")
        ("xa" "align")
        ("xd" "delete")
        ("xg" "google-translate")
        ("xl" "lines")
        ("xm" "move")
        ("xt" "transpose")
        ("xw" "words")
        ("z"  "zoom")))
(mapc (lambda (x) (apply #'dotemacs-declare-prefix x))
      dotemacs-key-binding-prefixes)

(when (eq dotemacs-colors-engine 'rainbow)
  (setq colors/key-binding-prefixes '(("Ci" . "colors-identifiers")))
  (mapc (lambda (x) (dotemacs-declare-prefix (car x) (cdr x)))
        colors/key-binding-prefixes))


;;; Evil Core

(use-package init-evil
  :ensure evil
  :load-path "config/")

(use-package evil
  :ensure t
  :init
  (progn
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

    ;; https://bitbucket.org/lyro/evil/issues/444/evils-undo-granularity-is-too-coarse
    ;; (setq evil-want-fine-undo t)

    ;; put back refresh of the cursor on post-command-hook see status of:
    ;; https://bitbucket.org/lyro/evil/issue/502/cursor-is-not-refreshed-in-some-cases
    ;; (add-hook 'post-command-hook 'evil-refresh-cursor)

    ; Don't move back the cursor one position when exiting insert mode
    (setq evil-move-cursor-back nil)
    ; (setq evil-search-module 'evil-search)
    ; (setq evil-magic 'very-magic)

    (evil-mode 1))
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
    (define-key evil-window-map (kbd "o") 'dotemacs/toggle-maximize-buffer)
    (define-key evil-window-map (kbd "C-o") 'dotemacs/toggle-maximize-buffer)

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

    ;; make cursor keys work
    (define-key evil-window-map (kbd "<left>") 'evil-window-left)
    (define-key evil-window-map (kbd "<right>") 'evil-window-right)
    (define-key evil-window-map (kbd "<up>") 'evil-window-up)
    (define-key evil-window-map (kbd "<down>") 'evil-window-down)

    (evil-leader/set-key "re" 'evil-show-registers)

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

    (dotemacs/add-to-hook 'prog-mode-hook '(dotemacs-standard-text-objects))

    ;; define text-object for entire buffer
    (evil-define-text-object evil-inner-buffer (count &optional beg end type)
      (evil-select-paren "\\`" "\\'" beg end type count nil))
    (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer)

    ;; support smart 1parens-strict-mode
    (defadvice evil-delete-backward-char-and-join
        (around dotemacs-evil-delete-backward-char-and-join activate)
      (if (bound-and-true-p smartparens-strict-mode)
          (call-interactively 'sp-backward-delete-char)
        ad-do-it))

    ;; Define history commands for comint
    (evil-define-key 'insert comint-mode-map
      (kbd "C-k") 'comint-next-input
      (kbd "C-j") 'comint-previous-input)
    (evil-define-key 'normal comint-mode-map
      (kbd "C-k") 'comint-next-input
      (kbd "C-j") 'comint-previous-input)))

(use-package evil-leader
  :ensure t
  :init
  (progn
    (setq evil-leader/leader dotemacs-leader-key)
    (global-evil-leader-mode)
    ;; This is the same hook used by evil-leader. We make sure that this
    ;; function is called after `evil-leader-mode' using the last argument
    (add-hook 'evil-local-mode-hook
      #'dotemacs-additional-leader-mode t))
  :config
  (progn
    ;; Unset shortcuts which shadow evil leader
    (with-eval-after-load 'compile
      (define-key compilation-mode-map (kbd "h") nil))
      ;; evil-leader does not get activated in existing buffers, so we have to
      ;; force it here
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (evil-leader-mode 1)
          (dotemacs-additional-leader-mode 1)))))


;;; Shell

;; Emacs built-in variables

;; move point to the end of buffer on new output
(setq comint-move-point-for-output t)

;; Add shell buffers to useful buffers list
(push "\\*\\(ansi-term\\|eshell\\|shell\\|terminal\.\+\\)\\*" dotemacs-useful-buffers-regexp)

;; Variables

;;; Terminal emulation and shells
(dotemacs-defvar-company-backends eshell-mode)

;;; Setup environment variables from the user's shell.
(use-package exec-path-from-shell
  :ensure t
  :init
  (progn
    (when (string-match-p "/zsh$" (getenv "SHELL"))
      ;; Use a non-interactive shell. We use a login shell, even though we have
      ;; our paths setup in .zshenv. However, OS X adds global settings to the
      ;; login profile. Notably, this affects /usr/texbin from MacTeX
      (setq exec-path-from-shell-arguments '("-l")))

    (dotemacs|do-after-display-system-init
     (when (memq window-system '(mac ns x))
       (exec-path-from-shell-initialize)))

    (setq user-mail-address (getenv "EMAIL")))
  :config
  (progn
    (dolist (var '("EMAIL" "PYTHONPATH" "INFOPATH"))
      (add-to-list 'exec-path-from-shell-variables var))))

(use-package init-eshell
  :load-path "config/")

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :pre-init
    (dotemacs-use-package-add-hook eshell
      :post-init
      (progn
        (push 'company-capf company-backends-eshell-mode)
        (dotemacs-add-company-hook eshell-mode))
      :post-config
      (progn
        (defun dotemacs-toggle-shell-auto-completion-based-on-path ()
          "Deactivates automatic completion on remote paths.
Retrieving completions for Eshell blocks Emacs. Over remote
connections the delay is often annoying, so it's better to let
the user activate the completion manually."
          (if (file-remote-p default-directory)
              (setq-local company-idle-delay nil)
            (setq-local company-idle-delay 0.2)))
        (add-hook 'eshell-directory-change-hook
                  'dotemacs-toggle-shell-auto-completion-based-on-path)

        ;; The default frontend screws everything up in short windows like
        ;; terminal often are
        (defun dotemacs-eshell-switch-company-frontend ()
          "Sets the company frontend to `company-preview-frontend' in e-shell mode."
          (setq-local company-frontends '(company-preview-frontend)))
        (add-hook 'eshell-mode-hook
                  'dotemacs-eshell-switch-company-frontend)))))

(use-package eshell
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-register-repl 'eshell 'eshell)
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

    (when dotemacs-shell-protect-eshell-prompt
      (add-hook 'eshell-after-prompt-hook 'dotemacs-protect-eshell-prompt))

    (autoload 'eshell-delchar-or-maybe-eof "em-rebind")

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
      (push 'eshell-truncate-buffer eshell-output-filter-functions))

    ;; These don't work well in normal state due to evil/emacs cursor
    ;; incompatibility
    (evil-define-key 'insert eshell-mode-map
      (kbd "C-k") 'eshell-previous-matching-input-from-input
      (kbd "C-j") 'eshell-next-matching-input-from-input)))

(use-package eshell-z
  :defer t
  :ensure t
  :init
  (with-eval-after-load 'eshell
    (require 'eshell-z)))

(use-package esh-help
  :defer t
  :ensure t
  :init (add-hook 'eshell-mode-hook 'eldoc-mode)
  :config (setup-esh-help-eldoc))

(use-package eshell-prompt-extras
  :ensure t
  :commands epe-theme-lambda
  :init
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(use-package multi-term
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-register-repl 'multi-term 'multi-term)
    (evil-leader/set-key "ast" 'shell-pop-multi-term))
  :config
  (progn
    (add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab))
    ;; multi-term commands to create terminals and move through them.
    (evil-leader/set-key-for-mode 'term-mode "c" 'multi-term)
    (evil-leader/set-key-for-mode 'term-mode "p" 'multi-term-prev)
    (evil-leader/set-key-for-mode 'term-mode "n" 'multi-term-next)
    (evil-leader/set-key "p$t" 'projectile-multi-term-in-root)))

(use-package comint
  :init
  (setq comint-prompt-read-only t))

(use-package xterm-color
  :ensure t
  :init
  (progn
    ;; Comint and Shell
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
    (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
    (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region)
    (with-eval-after-load 'esh-mode
      (add-hook 'eshell-mode-hook (lambda () (setq xterm-color-preserve-properties t)))
      (add-hook 'eshell-preoutput-filter-functions 'xterm-color-filter)
      (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))))

(use-package shell                      ; Dump shell in Emacs
  :init
  (dotemacs-register-repl 'shell 'shell)
  (add-hook 'shell-mode-hook 'shell-comint-input-sender-hook))

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

    (add-hook 'term-mode-hook 'ansi-term-handle-close)
    (add-hook 'term-mode-hook (lambda () (linum-mode -1)))

    (evil-leader/set-key
      "'"   'dotemacs-default-pop-shell
      "ase" 'shell-pop-eshell
      "asi" 'shell-pop-shell
      "asm" 'shell-pop-multiterm
      "ast" 'shell-pop-ansi-term
      "asT" 'shell-pop-term)))

(use-package term
  :init
  (progn
    (dotemacs-register-repl 'term 'term)
    (dotemacs-register-repl 'term 'ansi-term)
    (defun term-send-tab ()
      "Send tab in term mode."
      (interactive)
      (term-send-raw-string "\t"))

    ;; hack to fix pasting issue, the paste micro-state won't
    ;; work in term
    (evil-define-key 'normal term-raw-map "p" 'term-paste)
    (evil-define-key 'insert term-raw-map (kbd "C-c C-d") 'term-send-eof)
    (evil-define-key 'insert term-raw-map (kbd "C-c C-z") 'term-stop-subjob)
    (evil-define-key 'insert term-raw-map (kbd "<tab>") 'term-send-tab)

    (evil-define-key 'insert term-raw-map
      (kbd "C-k") 'term-send-up
      (kbd "C-j") 'term-send-down)
    (evil-define-key 'normal term-raw-map
      (kbd "C-k") 'term-send-up
      (kbd "C-j") 'term-send-down)))

(dotemacs-use-package-add-hook smooth-scrolling
  :post-init
  (dotemacs/add-to-hooks 'dotemacs//unset-scroll-margin
                          '(eshell-mode-hook
                            comint-mode-hook
                            term-mode-hook)))

(dotemacs-use-package-add-hook magit
  :post-init
  (defalias 's 'magit-status))


;;; Customization, init file and package management
(use-package cus-edit
  :defer t
  :init (load dotemacs-custom-file 'no-error 'no-message)
  :config
  (setq custom-buffer-done-kill nil            ; Kill when existing
        custom-buffer-verbose-help nil         ; Remove redundant help text
        ;; Show me the real variable name
        custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil))

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

    (evilify paradox-menu-mode paradox-menu-mode-map
             "H" 'paradox-menu-quick-help
             "J" 'paradox-next-describe
             "K" 'paradox-previous-describe
             "L" 'paradox-menu-view-commit-list
             "o" 'paradox-menu-visit-homepage)
    (evil-leader/set-key
      "aP" 'dotemacs-paradox-list-packages)))

(use-package bug-hunter                 ; Search init file for bugs
  :ensure t)

(use-package server                     ; The server of `emacsclient'
  :defer t
  :disabled t
  :init (server-mode)
  :diminish server-buffer-clients)


;;; OS X support
(use-package ns-win                     ; OS X window support
  :defer t
  :if (eq system-type 'darwin)
  :init
  (progn
    (evil-leader/set-key "bf" 'reveal-in-osx-finder)

    ;; this is only applicable to GUI mode
    (dotemacs|do-after-display-system-init
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
        (global-set-key (kbd "M-s") 'save-buffer))))
  :config
  (dotemacs|do-after-display-system-init
    (when (display-graphic-p)
      (setq ns-pop-up-frames nil            ; Don't pop up new frames from the
                                            ; workspace
            mac-control-modifier 'control   ; Make control to Control
            mac-option-modifier 'super      ; Make option do Super (`s` is for super)
            mac-command-modifier 'meta      ; Option is simply the natural Meta
                                            ; But command is a lot easier to hit.
                                            ; (`M` is for meta)
            mac-right-command-modifier 'left
            mac-right-option-modifier 'none ; Keep right option for accented input
            mac-function-modifier 'hyper    ; Just in case we ever need these
                                            ; keys. (`H` is for hyper)
            ))))

(use-package init-macosx              ; Personal OS X tools
  :if (eq system-type 'darwin)
  :load-path "config/")

(use-package osx-trash                  ; Trash support for OS X
  :if (eq system-type 'darwin)
  :ensure t
  :init (osx-trash-setup))

(use-package pbcopy
  :if (and (eq system-type 'darwin) (not (display-graphic-p)))
  :ensure t
  :init (turn-on-pbcopy))

(use-package launchctl
  :if (eq system-type 'darwin)
  :defer t
  :ensure t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.plist$" . nxml-mode))
    (evil-leader/set-key "al" 'launchctl))
  :config
  (progn
    (evilify launchctl-mode launchctl-mode-map
             (kbd "q") 'quit-window
             (kbd "s") 'tabulated-list-sort
             (kbd "g") 'launchctl-refresh
             (kbd "n") 'launchctl-new
             (kbd "e") 'launchctl-edit
             (kbd "v") 'launchctl-view
             (kbd "l") 'launchctl-load
             (kbd "u") 'launchctl-unload
             (kbd "r") 'launchctl-reload
             (kbd "S") 'launchctl-start
             (kbd "K") 'launchctl-stop
             (kbd "R") 'launchctl-restart
             (kbd "D") 'launchctl-remove
             (kbd "d") 'launchctl-disable
             (kbd "E") 'launchctl-enable
             (kbd "i") 'launchctl-info
             (kbd "f") 'launchctl-filter
             (kbd "=") 'launchctl-setenv
             (kbd "#") 'launchctl-unsetenv
             (kbd "h") 'launchctl-help)))

(use-package reveal-in-osx-finder           ; Reveal current buffer in finder
  :if (eq system-type 'darwin)
  :ensure t
  :commands reveal-in-osx-finder)

(when (eq system-type 'darwin)
  (dotemacs-use-package-add-hook helm
    ;; Use `mdfind' instead of `locate'.
    :pre-init
    ;; Disable fuzzy matchting to make mdfind work with helm-locate
    ;; https://github.com/emacs-helm/helm/issues/799
    (setq helm-locate-fuzzy-match nil)
    (setq helm-locate-command "mdfind -name %s %s")))


;;; User interface

(defun dotemacs//unset-scroll-margin ()
  "Set `scroll-margin` to zero."
  (setq-local scroll-margin 0))

(use-package smooth-scrolling
  :ensure t
  :defer t
  :if dotemacs-smooth-scrolling
  :init (setq smooth-scroll-margin 5
              scroll-conservatively 101
              scroll-preserve-screen-position t
              auto-window-vscroll nil)
  :config
  (progn
    (setq scroll-margin 5)
    ;; add hooks here only for emacs built-in packages
    (dotemacs/add-to-hooks 'dotemacs//unset-scroll-margin
                            '(messages-buffer-mode-hook))))

(unless dotemacs-smooth-scrolling
  ;; deactivate smooth-scrolling advices
  (ad-disable-advice 'previous-line 'after 'smooth-scroll-down)
  (ad-activate 'previous-line)
  (ad-disable-advice 'next-line 'after 'smooth-scroll-up)
  (ad-activate 'next-line)
  (ad-disable-advice 'isearch-repeat 'after 'isearch-smooth-scroll)
  (ad-activate 'isearch-repeat))

(use-package diminish
  :ensure t
  :init
  (progn
    ;; Minor modes abbrev --------------------------------------------------------
    (dotemacs|do-after-display-system-init
      (when (display-graphic-p)
        (with-eval-after-load 'eproject
          (diminish 'eproject-mode " eⓅ"))
        (with-eval-after-load 'flymake
          (diminish 'flymake-mode " Ⓕ2"))))
    ;; Minor Mode (hidden) ------------------------------------------------------
    (with-eval-after-load 'elisp-slime-nav
      (diminish 'elisp-slime-nav-mode))
    (with-eval-after-load 'hi-lock
      (diminish 'hi-lock-mode))
    (with-eval-after-load 'abbrev
      (diminish 'abbrev-mode))
    (with-eval-after-load 'subword
      (when (eval-when-compile (version< "24.3.1" emacs-version))
         (diminish 'subword-mode)))))

;; Just don’t show them. Use native Emacs controls:
(setq use-dialog-box nil)

;; Show line number in mode line
(setq line-number-mode t)
;; Show column number in mode line
(setq column-number-mode t)
(when dotemacs-line-numbers
  (add-hook 'text-mode-hook 'linum-mode))

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
      ;; no welcome buffer
      inhibit-startup-screen t
      ;; silence ad-handle-definition about advised functions getting redefined
      ad-redefinition-action 'accept
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
  :commands (dotemacs-insert-logo-into-scratch)
  :init (add-hook 'emacs-startup-hook #'dotemacs-insert-logo-into-scratch))

(use-package unicode-fonts              ; Map Unicode blocks to fonts
  :ensure t
  ;; Enable emoticon mappings
  :config (progn (setq unicode-fonts-skip-font-groups '(low-quality-glyphs)
                       unicode-fonts-use-prepend t)
                 (unicode-fonts-setup)))

(custom-set-faces
 '(linum ((t (:height 0.9 :family "Bebas Neue")))))

(use-package linum-relative
  :ensure t
  :commands (linum-relative-toggle linum-relative-on)
  :init
  (progn
    (when (eq dotemacs-line-numbers 'relative)
      (linum-relative-on))
    (evil-leader/set-key "tr" 'linum-relative-toggle))
  :config
  (progn
    (setq linum-relative-current-symbol "→")))

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
  :config (dotemacs-hide-lighter fci-mode))

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


;;; Perspective

(defvar dotemacs-layouts-directory
  (expand-file-name (concat dotemacs-cache-directory "layouts/"))
  "Save layouts in this directory.")

(defvar layouts-enable-autosave nil
  "If true, saves perspectives to file per `layouts-autosave-delay'")

(defvar layouts-autosave-delay 900
  "Delay in seconds between each layouts auto-save.")

(use-package init-perspective
  :load-path "config/")

(use-package persp-mode
  :diminish persp-mode
  :ensure t
  :init
  (progn
    (setq persp-auto-resume-time (if dotemacs-auto-resume-layouts 1 -1)
          persp-nil-name dotemacs-default-layout-name
          persp-reset-windows-on-nil-window-conf nil
          persp-set-last-persp-for-new-frames nil
          persp-save-dir dotemacs-layouts-directory)

    ;; always activate persp-mode
    (persp-mode)

    (defvar dotemacs--layouts-ms-doc-toggle 0
      "Display a short doc when nil, full doc otherwise.")

    (defvar dotemacs--last-selected-layout persp-nil-name
      "Previously selected layout.")

    (defvar dotemacs--custom-layout-alist nil
      "List of custom layouts with their bound keys.
 Do not modify directly, use provided `dotemacs-define-custom-layout'")

    (defvar dotemacs--layouts-autosave-timer nil
      "Timer for layouts auto-save.")

    (defun dotemacs/jump-to-last-layout ()
      "Open the previously selected layout, if it exists."
      (interactive)
      (unless (eq 'non-existent
                  (gethash dotemacs--last-selected-layout
                           *persp-hash* 'non-existent))
        (persp-switch dotemacs--last-selected-layout)))

    ;; Perspectives micro-state -------------------------------------------

    (defun dotemacs//layouts-ms-toggle-doc ()
      "Toggle the full documenation for the layouts micro-state."
      (interactive)
      (setq dotemacs--layouts-ms-doc-toggle
            (logxor dotemacs--layouts-ms-doc-toggle 1)))

    (defun dotemacs//layout-format-name (name pos)
      "Format the layout name given by NAME for display in mode-line."
      (let* ((layout-name (if (file-directory-p name)
                              (file-name-nondirectory (directory-file-name name))
                            name))
             (string-name (format "%s" layout-name))
             (current (equal name (dotemacs//current-layout-name)))
             (caption (concat (number-to-string (if (eq 9 pos) 0 (1+ pos)))
                              ":" string-name)))
        (if current
            (concat (when current "[") caption (when current "]"))
          caption)))

    (defvar dotemacs--layouts-ms-documentation
        "
  [?]                  toggle this help
  [0,9]                go to nth layout
  [tab]                last layout
  [a]                  add a buffer from another layout
  [A]                  add all buffers from another layout
  [b]                  select a buffer of the current layout
  [c]                  close layout (buffers are not closed)
  [C]                  close other layout(s) (buffers are not closed)
  [h]                  go to default layout
  [l]                  jump to a layout
  [L]                  load saved layouts
  [n] or [C-l]         next layout
  [N] or [p] or [C-h]  previous layout
  [o]                  custom layouts
  [r]                  remove current buffer from layout
  [R]                  rename or create layout
  [s]                  save all layouts
  [S]                  save layouts by names
  [t]                  show a buffer without adding it to current layout
  [w]                  workspaces micro-state
  [x]                  kill layout and its buffers
  [X]                  kill other layout(s) and their buffers")

     (defun dotemacs//layouts-ms-doc ()
       "Return the docstring for the layouts micro-state."
       (let* ((persp-list (or (persp-names-current-frame-fast-ordered)
                              (list persp-nil-name)))
              (formatted-persp-list
               (concat
                (mapconcat
                 (lambda (persp)
                   (dotemacs//layout-format-name
                    persp (position persp persp-list))) persp-list " | "))))
         (concat formatted-persp-list
                 (when (equal 1 dotemacs--layouts-ms-doc-toggle)
                   dotemacs--layouts-ms-documentation))))

     (dotemacs-define-micro-state layouts
       :doc (dotemacs//layouts-ms-doc)
       :use-minibuffer t
       :evil-leader "l"
       :bindings
       ;; need to exit in case number doesn't exist
       ("?" dotemacs//layouts-ms-toggle-doc)
       ("1" dotemacs/persp-switch-to-1 :exit t)
       ("2" dotemacs/persp-switch-to-2 :exit t)
       ("3" dotemacs/persp-switch-to-3 :exit t)
       ("4" dotemacs/persp-switch-to-4 :exit t)
       ("5" dotemacs/persp-switch-to-5 :exit t)
       ("6" dotemacs/persp-switch-to-6 :exit t)
       ("7" dotemacs/persp-switch-to-7 :exit t)
       ("8" dotemacs/persp-switch-to-8 :exit t)
       ("9" dotemacs/persp-switch-to-9 :exit t)
       ("0" dotemacs/persp-switch-to-0 :exit t)
       ("<tab>" dotemacs/jump-to-last-layout)
       ("<return>" nil :exit t)
       ("C-h" persp-prev)
       ("C-l" persp-next)
       ("a" persp-add-buffer :exit t)
       ("A" persp-import-buffers :exit t)
       ("b" dotemacs/persp-helm-mini :exit t)
       ("c" dotemacs/layouts-ms-close)
       ("C" dotemacs/layouts-ms-close-other :exit t)
       ("h" dotemacs/layout-goto-default :exit t)
       ("l" dotemacs/helm-perspectives :exit t)
       ("L" persp-load-state-from-file :exit t)
       ("n" persp-next)
       ("N" persp-prev)
       ("o" dotemacs/select-custom-layout :exit t)
       ("p" persp-prev)
       ("r" persp-remove-buffer :exit t)
       ("R" dotemacs/layouts-ms-rename :exit t)
       ("s" persp-save-state-to-file :exit t)
       ("S" persp-save-to-file-by-names :exit t)
       ("t" persp-temporarily-display-buffer :exit t)
       ("w" dotemacs/layout-workspaces-micro-state :exit t)
       ("x" dotemacs/layouts-ms-kill)
       ("X" dotemacs/layouts-ms-kill-other :exit t))

     (defun dotemacs/layout-switch-by-pos (pos)
       "Switch to perspective of position POS."
       (let ((persp-to-switch
              (nth pos (persp-names-current-frame-fast-ordered))))
         (if persp-to-switch
             (persp-switch persp-to-switch)
           (when (y-or-n-p
                  (concat "Perspective in this position doesn't exist.\n"
                          "Do you want to create one? "))
             (let ((persp-reset-windows-on-nil-window-conf t))
               (persp-switch nil)
               (dotemacs-home))))))

     ;; Define all `dotemacs/persp-switch-to-X' functions
     (dolist (i (number-sequence 9 0 -1))
       (eval `(defun ,(intern (format "dotemacs/persp-switch-to-%s" i)) nil
                ,(format "Switch to layout %s." i)
                (interactive)
                (dotemacs/layout-switch-by-pos ,(if (eq 0 i) 9 (1- i)))
                (dotemacs-layouts-micro-state))))

     (defun dotemacs/layout-goto-default ()
       "Go to `dotemacs-default-layout-name` layout"
       (interactive)
       (when dotemacs-default-layout-name
         (persp-switch dotemacs-default-layout-name)))

     (defun dotemacs/layouts-ms-rename ()
       "Rename a layout and get back to the perspectives micro-state."
       (interactive)
       (call-interactively 'persp-rename)
       (dotemacs-layouts-micro-state))

     (defun dotemacs/layouts-ms-close ()
       "Kill current perspective"
       (interactive)
       (persp-kill-without-buffers (dotemacs//current-layout-name)))

     (defun dotemacs/layouts-ms-close-other ()
       (interactive)
       (call-interactively 'dotemacs/helm-persp-close)
       (dotemacs-layouts-micro-state))

     (defun dotemacs/layouts-ms-kill ()
       "Kill current perspective"
       (interactive)
       (persp-kill (dotemacs//current-layout-name)))

     (defun dotemacs/layouts-ms-kill-other ()
       (interactive)
       (call-interactively 'dotemacs/helm-persp-kill)
       (dotemacs-layouts-micro-state))

     (defun dotemacs/layouts-ms-last ()
       "Switch to the last active perspective"
       (interactive)
       (persp-switch persp-last-persp-name))

     ;; Custom perspectives micro-state -------------------------------------

     (defun dotemacs//custom-layout-func-name (name)
       "Return the name of the custom-perspective function for NAME."
       (intern (concat "dotemacs/custom-perspective-" name)))

     (defmacro dotemacs-define-custom-layout (name &rest props)
       "Define a custom-perspective called NAME.

FUNC is a FUNCTION defined using NAME and the result of
`dotemacs//custom-layout-func-name', it takes care of
creating the perspective NAME and executing the expressions given
in the :body property to this macro.

NAME is a STRING.

Available PROPS:

`:binding STRING'
   Key to be bound to the function FUNC

`:body EXPRESSIONS'
  One or several EXPRESSIONS that are going to be evaluated after
  we change into the perspective NAME."
      (declare (indent 1))
      (let* ((func (dotemacs//custom-layout-func-name name))
             (binding (car (dotemacs-mplist-get props :binding)))
             (body (dotemacs-mplist-get props :body))
             (already-defined? (cdr (assoc binding
                                           dotemacs--custom-layout-alist))))
        `(progn
           (defun ,func ()
             ,(format "Open custom perspective %s" name)
             (interactive)
             (let ((initialize (not (gethash ,name *persp-hash*))))
               (persp-switch ,name)
               (when initialize
                 (delete-other-windows)
                 ,@body)))
           ;; Check for Clashes
           (if ,already-defined?
               (unless (equal ,already-defined? ,name)
                 (warn "Replacing existing binding \"%s\" for %s with %s"
                       ,binding ,already-defined? ,name )
                 (push '(,binding . ,name) dotemacs--custom-layout-alist))
             (push '(,binding . ,name) dotemacs--custom-layout-alist)))))

    (dotemacs-define-custom-layout "@Dotemacs"
      :binding "e"
      :body
      (dotemacs/find-dotfile))

    (defun dotemacs/select-custom-layout ()
      "Update the custom-perspectives microstate and then activate it."
      (interactive)
      (dotemacs//update-custom-layouts)
      (dotemacs-custom-layouts-micro-state))

    (defun dotemacs//custom-layouts-ms-documentation ()
      "Return the docstring for the custom perspectives micro-state."
      (if dotemacs--custom-layout-alist
          (mapconcat (lambda (custom-persp)
                       (format "[%s] %s"
                               (car custom-persp) (cdr custom-persp)))
                     dotemacs--custom-layout-alist " ")
        (warn (format "`dotemacs--custom-layout-alist' variable is empty" ))))

    (defun dotemacs//update-custom-layouts ()
      "Ensure the custom-perspectives micro-state is updated.
Takes each element in the list `dotemacs--custom-layout-alist'
format so they are supported by the
`dotemacs-custom-layouts-micro-state' macro."
      (let (bindings)
        (dolist (custom-persp dotemacs--custom-layout-alist bindings)
          (let* ((binding (car custom-persp))
                 (name (cdr custom-persp))
                 (func-name (dotemacs//custom-layout-func-name name)))
            (push (list binding func-name) bindings)))
        (eval `(dotemacs-define-micro-state custom-layouts
                 :doc (dotemacs//custom-layouts-ms-documentation)
                 :use-minibuffer t
                 :bindings
                 ,@bindings))))
    )
  :config
  (progn
    (defadvice persp-activate (before dotemacs//save-toggle-layout activate)
      (setq dotemacs--last-selected-layout persp-last-persp-name))
    (add-hook 'persp-mode-hook 'dotemacs//layout-autosave)
    ;; By default, persp mode wont affect either helm or ido
    (remove-hook 'ido-make-buffer-list-hook 'persp-restrict-ido-buffers)))

(dotemacs-use-package-add-hook spaceline-config
  :post-init
  (setq spaceline-display-default-perspective
        dotemacs-display-default-layout))

(dotemacs-use-package-add-hook eyebrowse
  :post-init
  (add-hook 'persp-before-switch-functions #'dotemacs/update-eyebrowse-for-perspective)
  (add-hook 'eyebrowse-post-window-switch-hook #'dotemacs/save-eyebrowse-for-perspective)
  (add-hook 'persp-activated-hook #'dotemacs/load-eyebrowse-for-perspective))

(dotemacs-use-package-add-hook helm
  :post-init
  (evil-leader/set-key
    "pl" 'dotemacs/helm-persp-switch-project))

(dotemacs-use-package-add-hook swiper
  :post-init
  (evil-leader/set-key
    "pl" 'dotemacs/ivy-persp-switch-project))


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
  :init (with-eval-after-load 'firestarter
          (setq firestarter-lighter
                '(:eval (dotemacs-firestarter-mode-line)))))

(defvar eyebrowse-display-help t
  "If non-nil additional help is displayed when selecting a workspace.")

(dotemacs-declare-prefix "W" "workspaces")

(use-package eyebrowse
  :ensure t
  :defer t
  :diminish eyebrowse-mode
  :init
  (progn
    (setq eyebrowse-new-workspace #'dotemacs-home
          eyebrowse-wrap-around t)
    (eyebrowse-mode)

    ;; vim-style tab switching
    (define-key evil-motion-state-map "gt" 'eyebrowse-next-window-config)
    (define-key evil-motion-state-map "gT" 'eyebrowse-prev-window-config)

    (defun dotemacs/workspaces-ms-rename ()
      "Rename a workspace and get back to micro-state."
      (interactive)
      (eyebrowse-rename-window-config (eyebrowse--get 'current-slot))
      (dotemacs-workspaces-micro-state))

    (defun dotemacs//workspaces-ms-get-slot-name (window-config)
      "Return the name for the given window-config"
      (let ((slot (car window-config))
            (caption (eyebrowse-format-slot window-config)))
        (if (= slot current-slot)
            (format "[%s]" caption)
          caption)))

    (defun dotemacs//workspaces-ms-get-window-configs ()
      "Return the list of window configs. Depends on value of
`eyebrowse-place-zero-at-the-end'."
      (--sort (if (eq (car other) 0)
                  t
                (< (car it) (car other)))
              (eyebrowse--get 'window-configs)))

    (defun dotemacs//workspaces-ms-documentation ()
      "Return the docstring for the workspaces micro-state."
      (let* ((current-slot (eyebrowse--get 'current-slot))
             (window-configs (dotemacs//workspaces-ms-get-window-configs)))
        (concat
         "<" (if window-configs
                 (concat
                  (mapconcat 'dotemacs//workspaces-ms-get-slot-name
                             window-configs "> <") ">")
               (when eyebrowse-display-help
                 (concat
                  "\n[0-9] to create/switch to a workspace, "
                  "[n] next, [p/N] previous, [TAB] back and forth, [c] close, "
                  "[r] rename"))))))

    (dotemacs-define-micro-state workspaces
      :doc (dotemacs//workspaces-ms-documentation)
      :use-minibuffer t
      :evil-leader "W"
      :bindings
      ("0" eyebrowse-switch-to-window-config-0)
      ("1" eyebrowse-switch-to-window-config-1)
      ("2" eyebrowse-switch-to-window-config-2)
      ("3" eyebrowse-switch-to-window-config-3)
      ("4" eyebrowse-switch-to-window-config-4)
      ("5" eyebrowse-switch-to-window-config-5)
      ("6" eyebrowse-switch-to-window-config-6)
      ("7" eyebrowse-switch-to-window-config-7)
      ("8" eyebrowse-switch-to-window-config-8)
      ("9" eyebrowse-switch-to-window-config-9)
      ("<tab>" eyebrowse-last-window-config)
      ("C-i" eyebrowse-last-window-config)
      ("c" eyebrowse-close-window-config)
      ("h" eyebrowse-prev-window-config)
      ("l" eyebrowse-next-window-config)
      ("n" eyebrowse-next-window-config)
      ("N" eyebrowse-prev-window-config)
      ("p" eyebrowse-prev-window-config)
      ("r" dotemacs/workspaces-ms-rename :exit t)
      ("w" eyebrowse-switch-to-window-config :exit t))

    (defun dotemacs-eyebrowse-switch ()
      "Hook eyebrowse to projectile and neotree."
      (interactive)
      (when (projectile-project-p)
        (message "eyebrowse switching to: %s" (projectile-project-root))
        (when (fboundp 'neotree-dir)
          (if (neo-global--window-exists-p)
              (neotree-dir (projectile-project-root))
            (progn
              (neotree-dir (projectile-project-root))
              (neotree-hide)
              (let ((origin-buffer-file-name (buffer-file-name)))
                (neotree-find (projectile-project-root))
                (neotree-find origin-buffer-file-name))
              (neotree-hide))))))
    (with-eval-after-load 'projectile
      (add-hook 'eyebrowse-post-window-switch-hook 'dotemacs-eyebrowse-switch))))


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

;; Helm: Unite/CtrlP style fuzzy file/buffer/anything searcher on steroids
;;
;; Helm does the same thing as Unite/CtrlP on Vim and does it really well. You
;; can also enable Helm to manage the command buffer, which is pretty awesome
;; with: (helm-mode 1)
(use-package init-helm
  :load-path "config/")

(use-package helm-flx
  :ensure t
  :defer t)

(dotemacs-use-package-add-hook helm
  :pre-config
  (progn
    ;; Disable for helm-find-files until performance issues are sorted
    ;; https://github.com/PythonNut/helm-flx/issues/9
    (setq helm-flx-for-helm-find-files nil)
    (helm-flx-mode)))

(use-package helm
  :ensure t
  :defer 1
  :commands dotemacs-helm-find-files
  :init
  (progn
    (with-eval-after-load 'helm-config
                          (warn "`helm-config' loaded! Get rid of it ASAP!"))

    ;; NOTE: Apple OS X users also need a version of grep that accepts --exclude-dir
    ;; brew tap homebrew/dupes
    ;; brew install homebrew/dupes/grep
    (when-let (gnu-grep (and (eq system-type 'darwin)
                             (executable-find "ggrep")))
              (setq helm-grep-default gnu-grep)
              (setq helm-grep-default-command (concat gnu-grep " --color=never -a -d skip %e -n%cH -e %p %f"))
              (setq helm-grep-default-recurse-command (concat gnu-grep " --color=never -a -d recurse %e -n%cH -e %p %f")))

    ;; https://github.com/syl20bnr/spacemacs/issues/1544
    ;; Vim users are used to CtrlP plugin.
    (setq helm-for-files-preferred-list '(helm-source-buffers-list
                                          helm-source-buffer-not-found
                                          helm-source-ls-git
                                          helm-source-ls-git-buffers
                                          helm-source-projectile-projects
                                          helm-source-projectile-files-list
                                          helm-source-recentf
                                          helm-source-bookmarks
                                          helm-source-file-cache
                                          helm-source-files-in-current-dir))
    ;; (define-key evil-normal-state-map (kbd "C-p") #'dotemacs-helm-multi-files)

    (setq helm-prevent-escaping-from-minibuffer t
          helm-bookmark-show-location t
          helm-display-header-line nil
          helm-split-window-in-side-p t
          helm-always-two-windows t
          helm-echo-input-in-header-line t
          helm-imenu-execute-action-at-once-if-one nil
          helm-org-format-outline-path t)

    ;; hide minibuffer in Helm session, since we use the header line already
    (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

    ;; fuzzy matching setting
    (setq helm-M-x-fuzzy-match t
          helm-apropos-fuzzy-match t
          helm-file-cache-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-lisp-fuzzy-completion t
          helm-recentf-fuzzy-match t
          helm-semantic-fuzzy-match t
          helm-buffers-fuzzy-matching t)

    ;; Use helm to provide :ls, unless ibuffer is used
    ;; (unless 'ibuffer
    ;;   (evil-ex-define-cmd "buffers" 'helm-buffers-list))

    ;; use helm by default for M-x
    (global-set-key (kbd "M-x") 'helm-M-x)

    (evil-leader/set-key
      "<f1>" 'helm-apropos
      "bb"   'helm-mini
      "Cl"   'helm-colors
      "ff"   'dotemacs-helm-find-files
      "fF"   'helm-find-files
      "fL"   'helm-locate
      "fm"   'dotemacs-helm-multi-files
      "fr"   'helm-recentf
      "hb"   'helm-filtered-bookmarks
      "hdF"  'dotemacs/helm-faces
      "hi"   'helm-info-at-point
      "hl"   'helm-resume
      "hm"   'helm-man-woman
      "iu"   'helm-ucs
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

    ;; define the key binding at the very end in order to allow the user
    ;; to overwrite any key binding
    (add-hook 'emacs-startup-hook
              (lambda ()
                (evil-leader/set-key dotemacs-command-key 'helm-M-x)))

    ;; Hide the cursor in helm buffers.
    (add-hook 'helm-after-initialize-hook 'dotemacs//hide-cursor-in-helm-buffer)

    ;; this or any specialized case of Helm buffer must be added AFTER
    ;; `dotemacs-helm-display-buffer-regexp'. Otherwise,
    ;; `dotemacs-helm-display-buffer-regexp' will be used before
    ;; `dotemacs-helm-display-help-buffer-regexp' and display
    ;; configuration for normal Helm buffer is applied for helm help
    ;; buffer, making the help buffer unable to be displayed.
    (setq helm-display-function 'dotemacs-display-helm-window)

    ;; Prepare necessary settings to make Helm display properly.
    (add-hook 'helm-after-initialize-hook 'dotemacs-helm-prepare-display)

    ;;  Restore popwin-mode after a Helm session finishes.
    (add-hook 'helm-cleanup-hook 'dotemacs-restore-previous-display-config)

    ;; Add minibuffer history with `helm-minibuffer-history'
    (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

    ;; Cleanup some helm related states when quitting.
    (add-hook 'helm-cleanup-hook 'dotemacs-helm-cleanup)

    (defface dotemacs-helm-navigation-ms-face
      `((t :background ,(face-attribute 'error :foreground) :foreground "black"))
      "Face for helm heder when helm micro-state is activated."
      :group 'dotemacs))
  :config
  (progn
    (helm-mode +1)

    (when (and dotemacs-helm-resize
               (or (eq dotemacs-helm-position 'bottom)
                   (eq dotemacs-helm-position 'top)))
      (setq helm-autoresize-min-height 10)
      (helm-autoresize-mode 1))

    ;; from https://www.reddit.com/r/emacs/comments/2z7nbv/lean_helm_window/
    (defvar helm-source-header-default-background
      (face-attribute 'helm-source-header :background))
    (defvar helm-source-header-default-foreground
      (face-attribute 'helm-source-header :foreground))
    (defvar helm-source-header-default-box
      (face-attribute 'helm-source-header :box))
    (defvar helm-source-header-default-height
      (face-attribute 'helm-source-header :height) )

    ;; Hide the `helm' header is there is only one source.
    (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)

    ;; helm-locate uses es (from everything on windows, which doesnt like fuzzy)
    (helm-locate-set-command)
    (setq helm-locate-fuzzy-match (string-match "locate" helm-locate-command))

    ;; Set the face of diretories for `.' and `..'
    (add-hook 'helm-find-files-before-init-hook 'dotemacs-set-dotted-directory)

    ;; alter helm-bookmark key bindings to be simpler
    (add-hook 'helm-mode-hook 'simpler-helm-bookmark-keybindings)

    ;; helm navigation on hjkl
    (dotemacs-helm-hjkl-navigation (member dotemacs-editing-style '(vim hybrid)))

    ;; Define functions to pick actions
    (dotimes (n 10)
      (let ((func (intern (format "dotemacs/helm-action-%d" n)))
            (doc (format "Select helm action #%d" n)))
        (eval `(defun ,func ()
                 ,doc
                 (intern)
                 (helm-select-nth-action ,(1- n))))))

    (dotemacs-define-micro-state helm-navigation
      :persistent t
      :disable-evil-leader t
      :define-key (helm-map . "M-SPC") (helm-map . "s-M-SPC")
      :on-enter (dotemacs-helm-navigation-ms-on-enter)
      :on-exit  (dotemacs-helm-navigation-ms-on-exit)
      :bindings
      ("1" dotemacs/helm-action-1 :exit t)
      ("2" dotemacs/helm-action-2 :exit t)
      ("3" dotemacs/helm-action-3 :exit t)
      ("4" dotemacs/helm-action-4 :exit t)
      ("5" dotemacs/helm-action-5 :exit t)
      ("6" dotemacs/helm-action-6 :exit t)
      ("7" dotemacs/helm-action-7 :exit t)
      ("8" dotemacs/helm-action-8 :exit t)
      ("9" dotemacs/helm-action-9 :exit t)
      ("0" dotemacs/helm-action-10 :exit t)
      ("<tab>" helm-select-action :exit t)
      ("C-i" helm-select-action :exit t)
      ("<RET>" helm-maybe-exit-minibuffer :exit t)
      ("?" nil :doc (dotemacs-helm-navigation-ms-full-doc))
      ("a" helm-select-action :post (dotemacs-helm-navigation-ms-set-face))
      ("e" dotemacs-helm-edit)
      ("g" helm-beginning-of-buffer)
      ("G" helm-end-of-buffer)
      ("h" helm-previous-source)
      ("j" helm-next-line)
      ("k" helm-previous-line)
      ("l" helm-next-source)
      ("q" nil :exit t)
      ("t" helm-toggle-visible-mark)
      ("T" helm-toggle-all-marks)
      ("v" helm-execute-persistent-action))

    ;; Swap default TAB and C-z commands.
    ;; For GUI.
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-find-files-map
      (kbd "S-<tab>") 'helm-find-files-up-one-level)
    (define-key helm-find-files-map
      (kbd "<backtab>") 'helm-find-files-up-one-level)
    ;; For terminal.
    (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
    (define-key helm-find-files-map
      (kbd "S-TAB") 'helm-find-files-up-one-level)
    (define-key helm-map (kbd "C-z") 'helm-select-action)

    (with-eval-after-load 'helm-mode ; required
      (dotemacs-hide-lighter helm-mode))))

(use-package helm-ls-git
  :defer t
  :ensure t
  :init
  (progn
    (require 'helm-ls-git))
  :config
  (progn
    (setq helm-ls-git-show-abs-or-relative 'relative) ))

(use-package helm-swoop
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'helm
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

    (evil-leader/set-key
      "ss"    'helm-swoop
      "sS"    'dotemacs-helm-swoop-region-or-symbol
      "s C-s" 'helm-multi-swoop-all)
    (defadvice helm-swoop (before add-evil-jump activate)
      (evil-set-jump))))

(use-package helm-misc                  ; Misc helm commands
  :ensure helm
  :bind (([remap switch-to-buffer] . helm-mini)))

(use-package helm-themes
  :ensure helm
  :defer t
  :init
  (evil-leader/set-key
    "Th" 'helm-themes))

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
  (evil-leader/set-key
    "hM"    'helm-switch-major-mode
    ;; "hm"    'helm-disable-minor-mode
    "h C-m" 'helm-enable-minor-mode))


;;; Buffer, Windows and Frames
(setq truncate-partial-width-windows nil ; Make side by side buffers function
                                         ; the same as the main window.
      frame-resize-pixelwise t           ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

(use-package buffer-move
  :defer t
  :init
  (evil-leader/set-key
    "bmh" 'buf-move-left
    "bmj" 'buf-move-down
    "bmk" 'buf-move-up
    "bml" 'buf-move-right))

(defvar dotemacs//frame-width nil)
(defvar dotemacs//frame-height nil)

(use-package frame
  :bind (("C-c u F" . toggle-frame-fullscreen))
  :init
  (progn
    (defun dotemacs-set-frame-size ()
      (when (display-graphic-p)
        ;; for the height, subtract 60 pixels from the screen height (for
        ;; panels, menubars and whatnot), then divide by the height of a char to
        ;; get the height we want.
        ;; use 120 char wide window for largeish displays and smaller 90 column
        ;; windows for smaller displays.
        (let* ((fwp (if (> (x-display-pixel-width) 1680) 120 90))
               (fhp (/ (- (x-display-pixel-height) 60)
                       (frame-char-height))))
          (setq dotemacs//frame-width fwp)
          (setq dotemacs//frame-height fhp)
          (add-to-list 'initial-frame-alist `(width . ,dotemacs//frame-width))
          (add-to-list 'initial-frame-alist `(height . ,dotemacs//frame-height))
          (add-to-list 'default-frame-alist `(height . ,dotemacs//frame-height))
          (add-to-list 'default-frame-alist `(width  . ,dotemacs//frame-width))
          (set-frame-height (selected-frame) fhp)
          (set-frame-width (selected-frame) fwp))))

    (dotemacs|do-after-display-system-init
      (dotemacs-set-frame-size))

    ;; Kill `suspend-frame'
    (global-set-key (kbd "C-z") nil)
    (global-set-key (kbd "C-x C-z") nil)))

(use-package init-buffers          ; Personal buffer tools
  :load-path "config/"
  :config
  (progn
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
    (evil-leader/set-key "bB" 'ibuffer)

    (global-set-key (kbd "C-x C-b") 'ibuffer)
    (add-hook 'ibuffer-hook 'dotemacs-ibuffer-group-by-modes)

    (setq ibuffer-expert t
          ibuffer-show-empty-filter-groups nil)
    ;; Use ibuffer to provide :ls
    (evil-ex-define-cmd "buffers" 'ibuffer))
  :config
  (progn

    ;; Since we could override `,` with <leader>, let's make `;` do that
    ;; functionality
    (when (equal dotemacs-leader-key ",")
      (define-key ibuffer-mode-map
        (kbd ";") 'ibuffer-toggle-sorting-mode)
      (define-key ibuffer-mode-map
        (kbd ",") nil))
    (dotemacs-evilify-map ibuffer-mode-map
      :mode ibuffer-mode)))

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
  :config
  (progn
    (setq window-numbering-auto-assign-0-to-minibuffer nil)
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
      "9" 'select-window-9)
    (window-numbering-mode 1))

  (defun dotemacs-window-numbering-assign (windows)
    "Custom number assignment for special buffers."
    (mapc (lambda (w)
            (when (and (boundp 'neo-global--window)
                       (eq w neo-global--window))
              (window-numbering-assign w 0)))
          windows))
  (add-hook 'window-numbering-before-hook 'dotemacs-window-numbering-assign)
  (add-hook 'neo-after-create-hook '(lambda (w) (window-numbering-update))))

(use-package ediff-wind
  :defer t
  :config
  (progn
    ;;revert windows on exit - needs winner mode
    (when (fboundp 'winner-undo)
      (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

    ;; Prevent Ediff from spamming the frame
    (setq ediff-diff-options "-w"
            ediff-window-setup-function #'ediff-setup-windows-plain
            ediff-split-window-function #'split-window-horizontally)))

;; http://stackoverflow.com/a/4485083/740527
(use-package init-desktop
  :load-path "config/")

(use-package desktop                    ; Save buffers, windows and frames
  :defer t
  :init
  (setq desktop-dirname (concat dotemacs-cache-directory "desktop/")
        desktop-base-file-name (concat "emacs_" emacs-version-short
                                       ".desktop")
        desktop-base-lock-name (concat "emacs_" emacs-version-short
                                       ".desktop.lock")
        desktop-path (list desktop-dirname)
        desktop-load-locked-desktop nil
        ;; Fix the frameset warning at startup
        desktop-restore-frames nil
        ;; Save desktops a minute after Emacs was idle.
        desktop-auto-save-timeout 60)
  (desktop-save-mode 0)
  :config
  (progn
    ;; https://github.com/purcell/emacs.d/blob/master/lisp/init-sessions.el
    ;; Save a bunch of variables to the desktop file.
    ;; For lists, specify the length of the maximal saved data too.
    (setq desktop-globals-to-save
          (append '((comint-input-ring . 50)
                    desktop-missing-file-warning
                    (dired-regexp-history . 20)
                    (extended-command-history . 30)
                    (face-name-history . 20)
                    (file-name-history . 100)
                    (ido-buffer-history . 100)
                    (ido-last-directory-list . 100)
                    (ido-work-directory-list . 100)
                    (ido-work-file-list . 100)
                    (magit-read-rev-history . 50)
                    (minibuffer-history . 50)
                    (org-refile-history . 50)
                    (org-tags-history . 50)
                    (query-replace-history . 60)
                    (read-expression-history . 60)
                    (regexp-history . 60)
                    (regexp-search-ring . 20)
                    register-alist
                    (search-ring . 20)
                    (shell-command-history . 50)
                    tags-file-name
                    tags-table-list)))

    ;; Don't save .gpg files. Restoring those files in emacsclients causes
    ;; a problem as the password prompt appears before the frame is loaded.
    (setq desktop-files-not-to-save
          (concat "\\(^/[^/:]*:\\|(ftp)$\\)" ; original value
                  "\\|\\(\\.gpg$\\)"
                  "\\|\\(\\.plstore$\\)"
                  "\\|\\(\\.desktop$\\)"
                  "COMMIT_EDITMSG\\'"
                  ;; If backup files with names like "file.sv.20150619_1641.bkp"
                  ;; are saved to the desktop file, emacsclient crashes at launch
                  ;; Need to debug why that's the case. But for now, simply not
                  ;; saving the .bkp files to the desktop file is a workable
                  ;; solution -- Fri Jun 19 16:45:50 EDT 2015 - kmodi
                  "\\|\\(\\.bkp$\\)"
                  "\\|\\(\\TAGS$\\)"))

    ;; Don't save the eww buffers
    (setq desktop-buffers-not-to-save (concat desktop-buffers-not-to-save
                                              "\\|\\(^eww\\(<[0-9]+>\\)*$\\)"))

    (dolist (mode '(magit-mode magit-log-mode))
      (add-to-list 'desktop-modes-not-to-save mode))))

(dotemacs-use-package-add-hook ignoramus
  :post-config
  (setq desktop-files-not-to-save (concat desktop-files-not-to-save
                                         ignoramus-boring-file-regexp)))

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
  :config
  (progn
    (popwin-mode 1)
    (evil-leader/set-key "wpm" 'popwin:messages)
    (evil-leader/set-key "wpp" 'popwin:close-popup-window)

    ;; don't use default value but manage it ourselves
    (setq popwin:special-display-config nil)

    ;; buffers that we manage
    (push '("*Help*"                 :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
    (push '("*compilation*"          :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
    (push '("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("*Async Shell Command*"  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '(" *undo-tree*"           :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
    (push '("*ert*"                  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("*grep*"                 :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("*nosetests*"            :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("^\\*WoMan.+\\*$" :regexp t           :position bottom                                   ) popwin:special-display-config)
    (push '("^\\*Flycheck.+\\*$" :regexp t
                                     :dedicated t :position bottom :stick t :noselect t              ) popwin:special-display-config)
      ;; Pin the weather forecast to the bottom window
    (push '("*Sunshine*"             :dedicated t :position bottom                                   ) popwin:special-display-config)
    ;; add cider error to popwin special buffers
    (push '("*cider-error*"          :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
    ;; add cider-doc to popwin
    (push '("*cider-doc*"            :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)

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
(cl-case dotemacs-auto-save-file-location
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
         ("C-c e y" . bury-buffer)))

(use-package tramp                      ; Access remote files
  :defer t
  :init
  (progn
    (setq tramp-ssh-controlmaster-options
      (concat
        "-o ControlPath=~/.ssh/conn-%%r@%%h:%%p"))
    (setq tramp-default-method "ssh"
          vc-ignore-dir-regexp
          (format "\\(%s\\)\\|\\(%s\\)"
                  vc-ignore-dir-regexp
                  tramp-file-name-regexp)))
  :config
  ;; Store auto-save files locally
  (setq tramp-auto-save-directory (concat dotemacs-cache-directory "tramp-auto-save")))

(use-package open-junk-file
  :ensure t
  :defer t
  :commands (open-junk-file)
  :init
  (evil-leader/set-key "fJ" 'open-junk-file)
  (setq open-junk-file-directory (concat dotemacs-cache-directory "junk/%Y/%m/%d-%H%M%S.")))

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

(use-package recentf                    ; Save recently visited files
  :defer t
  :init
  (progn
    ;; lazy load recentf
    (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                      (recentf-mode)
                                      (recentf-track-opened-file))))
    (setq recentf-save-file (concat dotemacs-cache-directory "recentf")
          recentf-max-saved-items 5000
          recentf-max-menu-items 10
          recentf-auto-save-timer (run-with-idle-timer 1800 t 'recentf-save-list)
          ;; Cleanup recent files only when Emacs is idle, but not when the mode
          ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
          ;; idles often enough to have the recent files list clean up regularly
          recentf-auto-cleanup 300))
  :config
  (progn
    (setq recentf-exclude (list "COMMIT_EDITMSG\\'"
                                (expand-file-name package-user-dir)
                                (expand-file-name dotemacs-cache-directory)))))

(dotemacs-use-package-add-hook ignoramus
  :post-config
  (with-eval-after-load 'recentf
    (setq recentf-exclude (append recentf-exclude
                                  (list ignoramus-boring-file-regexp)))))

(use-package ignoramus                  ; Ignore uninteresting files everywhere
  :ensure t
  :config
  (progn
    (defun dotemacs//ignoramus-setup (fcn)
      (funcall fcn)
      ;; neotree
      (setq neo-hidden-regexp-list (list ignoramus-boring-file-regexp))

      ;; helm-grep
      (with-eval-after-load 'helm-grep
        (setq helm-grep-ignored-files (cons ".#*" (delq nil (mapcar #'(lambda (pat)
                                                                        (concat "*" pat)) ignoramus-file-basename-endings))))
        (setq helm-grep-ignored-directories ignoramus-file-basename-exact-names))

      ;; according to what projectile expects
      ;; (setq projectile-globally-ignored-file-extensions (mapcar #'(lambda (ext)
      ;;                                                               (replace-regexp-in-string "\\`\\." "" ext))
      ;;                                                           ignoramus-file-basename-endings))
      (setq projectile-ignored-file-extensions (mapcar #'(lambda (ext)
                                                           (replace-regexp-in-string "\\`\\." "" ext))
                                                       ignoramus-file-basename-endings))
      ;; (setq projectile-globally-ignored-files ignoramus-file-basename-exact-names)
      (setq projectile-ignored-files ignoramus-file-basename-exact-names)
      ;; (setq projectile-globally-ignored-directories ignoramus-file-basename-exact-names)
      (setq projectile-ignored-directories ignoramus-file-basename-exact-names))

    (advice-add 'ignoramus-setup
                :around 'dotemacs//ignoramus-setup)

    (dolist (name '(
                    "pids"
                    ".grunt"
                    ".cache"
                    ".lock-wscript"
                    ".cask"
                    ".vagrant"
                    ".DS_Store"
                    "lib-cov"
                    "coverage"
                    ".builds"
                    ".bzr"
                    ".cdv"
                    ".classpath"
                    ".coverage"
                    ".git"
                    ".hg"
                    ".idea"
                    ".ido.last"
                    ".netrwhist"
                    ".pc"
                    ".project"
                    ".projectile"
                    ".puppet-bak"
                    ".rspec"
                    ".sass-cache"
                    ".scala_dependencies"
                    ".svn"
                    "_darcs"
                    "auto-save-list"
                    "bower_components"
                    "node_modules"
                    ".cache"
                    ".sx"
                    "elpa"
                    ".tox"
                    "virtualenv"))

      ;; Ignore some additional directories
      (add-to-list 'ignoramus-file-basename-exact-names name))

    (dolist (name '(
                    ".tern-port"
                    ".png"
                    ".jpg"
                    ".jpeg"
                    ".gif"
                    "-autoloads.el"
                    ".class"
                    ".elc"
                    ".min.js"
                    "-min.js"
                    ".min.css"
                    "-min.css"
                    ".pyc"
                    ".pyd"
                    ".pyo"
                    ".rbc"
                    ".sassc"
                    ".suo"
                    ".swo"
                    ".venv"
                    ".swp"
                    ".psd"
                    ".ai"
                    ".pdf"
                    ".mov"
                    ".aep"
                    ".dmg"
                    ".zip"
                    ".gz"
                    ".bmp"
                    ".7z"
                    ".jar"
                    ".rar"
                    ".zip"
                    ".gz"
                    ".bzip"
                    ".bz2"
                    ".xz"
                    ".lzma"
                    ".cab"
                    ".iso"
                    ".tar"
                    ".dmg"
                    ".xpi"
                    ".gem"
                    ".egg"
                    ".deb"
                    ".rpm"
                    ".msi"
                    ".msm"
                    ".msp"
                    ".pid"
                    ".seed"))

      ;; Ignore some additional filename endings
      (add-to-list 'ignoramus-file-basename-endings name))

    (dolist (name '(
                    "\\`\\.flycheck.*\\'"
                    "\\`.*_flymake\\..*'"
                    ))

      ;; Ignore some additional filename endings
      (add-to-list 'ignoramus-file-basename-regexps name))

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
  :ensure t
  :defer t
  :init
  (progn
    (setq bookmark-save-flag 1 ;; autosave each change
          ;; Store auto-save files locally
          bookmark-default-file (concat dotemacs-cache-directory "bookmarks")
          url-configuration-directory (concat dotemacs-cache-directory "url")
          eshell-directory-name (concat dotemacs-cache-directory "eshell" )
          tramp-persistency-file-name (concat dotemacs-cache-directory "tramp"))))

(use-package restart-emacs
  :defer t
  :init
  (evil-leader/set-key "qr" 'dotemacs/restart-emacs)
  (defun dotemacs/restart-emacs ()
    (interactive)
    (setq dotemacs-really-kill-emacs t)
    (restart-emacs)))

(use-package savehist                   ; Save minibuffer history
  :init
  (progn
    ;; Minibuffer history
    (setq savehist-file (concat dotemacs-cache-directory "savehist")
          enable-recursive-minibuffers t ; Allow commands in minibuffers
          history-length 1000
          savehist-additional-variables '(search
                                          ring
                                          mark-ring
                                          global-mark-ring
                                          search-ring
                                          regexp-search-ring
                                          extended-command-history)
          savehist-autosave-interval 180)
    (savehist-mode t)))

;; move cursor to the last position upon open
(use-package saveplace                  ; Save point position in files
  :init
  (progn
    (if (fboundp 'save-place-mode)
        ;; Emacs 25 has a proper mode for `save-place'
        (save-place-mode)
      (setq save-place t))
    ;; Save point position between sessions
    (setq save-place-file (concat dotemacs-cache-directory "places"))))

(setq view-read-only t)                 ; View read-only files

(use-package autorevert                 ; Auto-revert buffers of changed files
  :if (not noninteractive)
  :defer
  :init
  (progn
    (setq auto-revert-check-vc-info nil
          auto-revert-mode-text " ♻"
          auto-revert-tail-mode-text " ♻~"
          auto-revert-verbose nil)
    (defun auto-revert-turn-on-maybe ()
      (unless (file-remote-p default-directory)
        (auto-revert-mode)))
    (add-hook 'find-file-hook 'auto-revert-turn-on-maybe))
  :config
  (setq auto-revert-verbose nil ; Shut up, please!
        ;; Revert Dired buffers, too
        global-auto-revert-non-file-buffers t))

(use-package image-file                 ; Visit images as images
  :init (auto-image-file-mode))

(use-package launch                     ; Open files in external programs
  :ensure t
  :defer t)

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
         ("C-c w ." . dotemacs-browse-feature-url))
  :config
  (progn
    (add-hook 'find-file-hook
              (lambda ()
                (unless (eq major-mode 'org-mode)
                  (setq show-trailing-whitespace t))))
    (add-hook 'find-file-hook #'visual-line-mode)
    (add-hook 'find-file-hook #'dotemacs-find-file-check-large-file)))

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

    (dotemacs-declare-prefix "fa" "fasd-find")
    (evil-leader/set-key "fad" 'fasd-find-directory-only)
    (evil-leader/set-key "faf" 'fasd-find-file-only)
    (evil-leader/set-key "fas" 'fasd-find-file)

    ;; we will fall back to using the default completing-read function, which is helm once helm is loaded.
    (setq fasd-completing-read-function 'nil)))

(use-package ranger
  :defer t
  :ensure t
  :init
  (progn
    (evil-leader/set-key
      "ar" 'ranger
      "ad" 'deer)

    ;; set up image-dired to allow picture resize
    (setq image-dired-dir (concat dotemacs-cache-directory "image-dir"))
    (unless (file-directory-p image-dired-dir)
      (make-directory image-dired-dir))

    (setq ranger-show-literal nil
          ranger-preview-file t
          ranger-width-parents 0.15
          ranger-width-preview 0.65
          ranger-show-preview t
          ranger-parent-depth 1
          ranger-max-preview-size 10))
 :config
 (define-key ranger-mode-map (kbd "-") 'ranger-up-directory))

;; Additional bindings for built-ins
(bind-key "C-c f v d" #'add-dir-local-variable)
(bind-key "C-c f v l" #'add-file-local-variable)
(bind-key "C-c f v p" #'add-file-local-variable-prop-line)


;;; Navigation and scrolling

;; Handy way of getting back to previous places.
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

;; Keep focus while navigating help buffers
(setq help-window-select 't)

;; The commands defined in this layer are taken from various sources like
;; [[https://github.com/bbatsov/prelude][Prelude]].
(defun dotemacs-smart-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key (kbd "C-a") 'dotemacs-smart-move-beginning-of-line)

(defun dotemacs-backward-kill-word-or-region (&optional arg)
  "Calls `kill-region' when a region is active and
`backward-kill-word' otherwise. ARG is passed to
`backward-kill-word' if no region is active."
  (interactive "p")
  (if (region-active-p)
      ;; call interactively so kill-region handles rectangular selection
      ;; correctly (see https://github.com/syl20bnr/spacemacs/issues/3278)
      (call-interactively #'kill-region)
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") 'dotemacs-backward-kill-word-or-region)

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

(use-package avy                   ; Jump to characters in buffers
  :ensure t
  :commands (dotemacs-avy-open-url)
  :init
  (progn
    (setq avy-all-windows 'all-frames)
    (setq avy-background t)
    (evil-leader/set-key
      "SPC" 'avy-goto-word-or-subword-1 ; 'avy-goto-word-1
      "y" 'avy-goto-line ; 'avy-goto-char-2
      "xo" 'dotemacs-avy-open-url
    ))
  :config
  (progn
    (defun dotemacs-avy-goto-url()
      "Use avy to go to an URL in the buffer."
      (interactive)
      (avy--generic-jump "https?://" nil 'pre))
    (defun dotemacs-avy-open-url ()
      "Use avy to select an URL in the buffer and open it."
      (interactive)
      (save-excursion
        (dotemacs-avy-goto-url)
        (browse-url-at-point)))
    (evil-leader/set-key "`" 'avy-pop-mark)))

(use-package ace-jump-helm-line
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-q") 'ace-jump-helm-line)))

(use-package ace-link                   ; Fast link jumping
  :commands dotemacs-ace-buffer-links
  :init
  (progn
    (define-key dotemacs-buffer-mode-map "o" 'dotemacs-ace-buffer-links)
    (with-eval-after-load 'info
      (define-key Info-mode-map "o" 'ace-link-info))
    (with-eval-after-load 'help-mode
      (defvar help-mode-map)  ; Silence the byte compiler
      (define-key help-mode-map "o" 'ace-link-help))
    (with-eval-after-load 'eww
      (define-key eww-link-keymap "o" 'ace-link-eww)
      (define-key eww-mode-map "o" 'ace-link-eww)))
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
      (let ((res (avy--with-avy-keys dotemacs-ace-buffer-links
                    (avy--process
                        (dotemacs-collect-buffer-links)
                        #'avy--overlay-pre))))
            (when res
              (goto-char (1+ res))
              (widget-button-press (point)))))))

(use-package ace-window                 ; Fast window switching
  :ensure t
  :defer t
  :init
  (progn
    (evil-leader/set-key
      "bM"  'ace-swap-window
      "wC"  'ace-delete-window
      "w <SPC>"  'ace-window)
    ;; set ace-window keys to home-row
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

(use-package page-break-lines           ; Turn page breaks into lines
  :ensure t
  :init
  (progn
    (global-page-break-lines-mode)
    (dotemacs-hide-lighter page-break-lines-mode)))

(use-package outline                    ; Navigate outlines in buffers
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'outline-minor-mode))
  :diminish (outline-minor-mode . "📑"))

; (use-package nlinum                     ; Line numbers in display margin
;   :ensure t
;   :bind (("C-c u l" . nlinum-mode)))

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
(with-eval-after-load 'comint
  (define-key comint-mode-map (kbd "C-d") nil))

;; whitespace-cleanup configuration
(pcase dotemacs-whitespace-cleanup
  (`all (add-hook 'before-save-hook 'whitespace-cleanup))
  (`trailing (add-hook 'before-save-hook 'delete-trailing-whitespace)))

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

(setq kill-ring-max 200                 ; More killed items
      ;; Save the contents of the clipboard to kill ring before killing
      save-interprogram-paste-before-kill t)

(setq mark-ring-max 64
      global-mark-ring-max 128
      create-lockfiles nil)

;; Configure a reasonable fill column, indicate it in the buffer and enable
;; automatic filling
(setq-default fill-column 80)
(setq standard-indent 2)

; Also, =visual-line-mode= is so much better than =auto-fill-mode=. It doesn't
; actually break the text into multiple lines - it only looks that way.
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(dotemacs-diminish auto-fill-function " Ⓕ" " F")

; Exclude very large buffers from dabbrev
; From https://github.com/purcell/emacs.d/blob/master/lisp/init-auto-complete.el
(defun sanityinc/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))
(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)

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
  (dotemacs-define-micro-state move-text
    :doc "[J] move down [K] move up"
    :use-minibuffer t
    :execute-binding-on-enter t
    :evil-leader "xJ" "xK"
    :bindings
    ("J" move-text-down)
    ("K" move-text-up)))

(use-package helm-ring                  ; Helm commands for rings
  :ensure helm
  :bind (([remap yank-pop]        . helm-show-kill-ring)
         ([remap insert-register] . helm-register)))

(use-package delsel                     ; Delete the selection instead of insert
  :defer t
  :init (delete-selection-mode))

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
                          :documentation "Toggle camelCase motions"
                          :evil-leader "tc")
    (dotemacs-add-toggle camel-case-motion-globally
                          :status subword-mode
                          :on (global-subword-mode +1)
                          :off (global-subword-mode -1)
                          :documentation "Globally toggle camelCase motions"
                          :evil-leader "t C-c"))
  :config
  (dotemacs-diminish subword-mode " ⓒ" " c"))

(use-package adaptive-wrap              ; Choose wrap prefix automatically
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package visual-fill-column
  :ensure t
  :disabled t
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
  :defer t
  :init (evil-leader/set-key "v" 'er/expand-region)
  :config
  (progn
    ;; add search capability to expand-region
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
        (setq ad-return-value (cons new-msg new-bindings))))
    (setq expand-region-contract-fast-key "V"
          expand-region-reset-fast-key "r")))

(use-package undo-tree                  ; Branching undo
  :ensure t
  :init
  (global-undo-tree-mode)
  ;; https://bitbucket.org/lyro/evil/issues/522/when-saving-the-buffer-change-list-got
  ;; https://github.com/syl20bnr/spacemacs/issues/774
  ;; (setq undo-tree-auto-save-history t
  ;;       undo-tree-history-directory-alist
  ;;       `(("." . ,(concat dotemacs-cache-directory "undo"))))
  ;; (unless (file-exists-p (concat dotemacs-cache-directory "undo"))
  ;;     (make-directory (concat dotemacs-cache-directory "undo")))
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  :config
  (dotemacs-hide-lighter undo-tree-mode))

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
  :ensure t
  :bind (("C-c u w w" . whitespace-mode))
  :init
  (progn
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
                         :documentation "Display whitespace globally"
                         :evil-leader "t C-w")

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

    (add-hook 'diff-mode-hook #'whitespace-mode)
    (add-hook 'diff-mode-hook #'dotemacs-set-whitespace-style-for-diff)

    (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
      (progn
        (add-hook hook #'dotemacs-whitespace-mode-local)
        (add-hook hook #'dotemacs-set-whitespace-style-for-others))))
  :config
  (progn
    (set-face-attribute 'whitespace-space nil
                        :background nil
                        :foreground (face-attribute 'font-lock-warning-face :foreground))
    (set-face-attribute 'whitespace-tab nil
                        :background nil)
    (set-face-attribute 'whitespace-indentation nil
                        :background nil)
    (dotemacs-diminish whitespace-mode " ⓦ" " w")
    (dotemacs-diminish global-whitespace-mode " Ⓦ" " W")))

(use-package whitespace-cleanup-mode    ; Cleanup whitespace in buffers
  :ensure t
  :bind (("C-c u w c" . whitespace-cleanup-mode)
         ("C-c e w" . whitespace-cleanup))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode))
  :diminish (whitespace-cleanup-mode . "⌫"))

(use-package ws-butler
  :if (eq 'changed dotemacs-whitespace-cleanup)
  :ensure t
  :config
  (progn
    (ws-butler-global-mode 1)
    (dotemacs-hide-lighter ws-butler-mode)))

(use-package hungry-delete
  :defer t
  :init
  (dotemacs-add-toggle hungry-delete
    :status hungry-delete-mode
    :on (hungry-delete-mode)
    :off (hungry-delete-mode -1)
    :documentation "Delete consecutive horizontal whitespace with a single key."
    :evil-leader "td")
  :config
  (progn
    (setq-default hungry-delete-chars-to-skip " \t\f\v") ; only horizontal whitespace
    (define-key hungry-delete-mode-map (kbd "DEL") 'hungry-delete-backward)
    (define-key hungry-delete-mode-map (kbd "S-DEL") 'delete-backward-char)))

(use-package hl-line                    ; Highlight the current line
  :init (global-hl-line-mode 1))

(use-package indent-guide
  :defer t
  :ensure t
  :init
  (progn
    (setq indent-guide-delay 0.3)
    (dotemacs-add-toggle indent-guide
      :status indent-guide-mode
      :on (indent-guide-mode)
      :off (indent-guide-mode -1)
      :documentation
      "Highlight indentation level at point. (alternative to highlight-indentation)."
      :evil-leader "ti")
    (dotemacs-add-toggle indent-guide-globally
      :status indent-guide-mode
      :on (indent-guide-global-mode)
      :off (indent-guide-global-mode -1)
      :documentation
      "Highlight indentation level at point globally. (alternative to highlight-indentation)."
      :evil-leader "t C-i"))
  :config
  (dotemacs-diminish indent-guide-mode " ⓘ" " i"))

(use-package hl-anything ;; Highlight things at point, selections, enclosing parentheses
  :disabled t
  :init
  (progn
    (hl-highlight-mode)
    (setq-default hl-highlight-save-file (concat dotemacs-cache-directory ".hl-save"))
    (evil-leader/set-key
      "hc"  'hl-unhighlight-all-local
      "hC"  'hl-unhighlight-all-global
      "hh"  'hl-highlight-thingatpt-local
      "hH"  'hl-highlight-thingatpt-global
      "hn"  'hl-find-next-thing
      "hN"  'hl-find-prev-thing
      "hr"  'hl-restore-highlights
      "hs"  'hl-save-highlights))
  :config (dotemacs-hide-lighter hl-highlight-mode))

(use-package hi-lock                    ; Custom regexp highlights
  :init (global-hi-lock-mode)
  :diminish hi-lock-mode)


;;; Evil


(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :ensure t
  :init
  (setq evil-snipe-scope 'whole-buffer
        evil-snipe-enable-highlight t
        evil-snipe-enable-incremental-highlight t
        evil-snipe-auto-disable-substitute t
        evil-snipe-show-prompt nil
        evil-snipe-smart-case t)
  :config
  (progn
    (if dotemacs-evil-snipe-enable-alternate-f-and-t-behaviors
        (progn
          (setq evil-snipe-repeat-scope 'whole-buffer)
          (evil-snipe-override-mode 1))
      (evil-snipe-mode 1))))

(dotemacs-use-package-add-hook magit
  :post-init
  (if dotemacs-evil-snipe-enable-alternate-f-and-t-behaviors
      (progn
        (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
        (add-hook 'git-rebase-mode-hook 'turn-off-evil-snipe-override-mode))
    (add-hook 'magit-mode-hook 'turn-off-evil-snipe-mode)
    (add-hook 'git-rebase-mode-hook 'turn-off-evil-snipe-mode)))

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
  :config
  (dotemacs-hide-lighter evil-escape-mode))

(use-package evil-exchange
  :ensure t
  :init (evil-exchange-install))

(use-package iedit
  :defer t
  :ensure t
  :init
  (progn
    (setq iedit-current-symbol-default t
          iedit-only-at-symbol-boundaries t
          iedit-toggle-key-default nil))
  :config
  (progn
    (defun iedit-toggle-selection ()
      "Override default iedit function to be able to add arbitrary overlays.

It will toggle the overlay under point or create an overlay of one character."
      (interactive)
      (iedit-barf-if-buffering)
      (let ((ov (iedit-find-current-occurrence-overlay)))
        (if ov
            (iedit-restrict-region (overlay-start ov) (overlay-end ov) t)
          (save-excursion
            (push (iedit-make-occurrence-overlay (point) (1+ (point)))
                  iedit-occurrences-overlays))
          (setq iedit-mode
                (propertize
                 (concat " Iedit:" (number-to-string
                                    (length iedit-occurrences-overlays)))
                 'face 'font-lock-warning-face))
          (force-mode-line-update))))))

(use-package evil-iedit-state
  :ensure t
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init (evil-leader/set-key "se" 'evil-iedit-state/iedit-mode)
  :config
  ;; activate leader in iedit and iedit-insert states
  (define-key evil-iedit-state-map
    (kbd evil-leader/leader) evil-leader--default-map))

(use-package evil-jumper
  :ensure t
  :init
  (progn
    (setq evil-jumper-post-jump-hook 'recenter
          evil-jumper-auto-save-interval 600)
    (evil-jumper-mode t)))

(use-package evil-lisp-state
  :ensure t
  :init
  (progn
    (setq evil-lisp-state-global t)
    (setq evil-lisp-state-leader-prefix "k")))

(use-package evil-mc
  :ensure t
  :defer t)

; TODO: Add switch between evil-commentary and evil-nerd-commenter
(use-package evil-commentary
  :diminish evil-commentary-mode
  :ensure t
  :init
  (progn
    (evil-commentary-mode)
    (evil-leader/set-key ";" 'evil-commentary)))

(use-package evil-nerd-commenter
  :disabled t
  :ensure t
  :commands (evilnc-comment-operator)
  :init
  (progn
    ;; double all the commenting functions so that the inverse operations
    ;; can be called without setting a flag
    (defun dotemacs/comment-or-uncomment-lines-inverse (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line t))
        (evilnc-comment-or-uncomment-lines arg)))

    (defun dotemacs/comment-or-uncomment-lines (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line nil))
        (evilnc-comment-or-uncomment-lines arg)))

    (defun dotemacs/copy-and-comment-lines-inverse (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line t))
        (evilnc-copy-and-comment-lines arg)))

    (defun dotemacs/copy-and-comment-lines (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line nil))
        (evilnc-copy-and-comment-lines arg)))

    (defun dotemacs/quick-comment-or-uncomment-to-the-line-inverse
        (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line t))
        (evilnc-comment-or-uncomment-to-the-line arg)))

    (defun dotemacs/quick-comment-or-uncomment-to-the-line (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line nil))
        (evilnc-comment-or-uncomment-to-the-line arg)))

    (defun dotemacs/comment-or-uncomment-paragraphs-inverse (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line t))
        (evilnc-comment-or-uncomment-paragraphs arg)))

    (defun dotemacs/comment-or-uncomment-paragraphs (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line nil))
        (evilnc-comment-or-uncomment-paragraphs arg)))

    (define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
    (define-key evil-normal-state-map "gy" 'dotemacs/copy-and-comment-lines)

    (evil-leader/set-key
      ";"  'evilnc-comment-operator
      "cl" 'dotemacs/comment-or-uncomment-lines
      "cL" 'dotemacs/comment-or-uncomment-lines-inverse
      "cp" 'dotemacs/comment-or-uncomment-paragraphs
      "cP" 'dotemacs/comment-or-uncomment-paragraphs-inverse
      "ct" 'dotemacs/quick-comment-or-uncomment-to-the-line
      "cT" 'dotemacs/quick-comment-or-uncomment-to-the-line-inverse
      "cy" 'dotemacs/copy-and-comment-lines
      "cY" 'dotemacs/copy-and-comment-lines-inverse)))

(use-package evil-matchit
  :ensure t
  :init
  (dolist (hook '(LaTeX-mode-hook mustache-mode-hook handlebars-mode-hook ruby-mode-hook))
    (add-hook hook 'turn-on-evil-matchit-mode)))

(use-package evil-indent-plus
  :ensure t
  :init
  (evil-indent-plus-default-bindings))

(use-package evil-numbers
  :ensure t
  :config
  (progn
    (defun dotemacs-evil-numbers-micro-state-doc ()
      "Display a short documentation in the mini buffer."
      (dotemacs/echo "+/= to increase the value or - to decrease it"))

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

    (evil-leader/set-key "n+" 'dotemacs-evil-numbers-increase)
    (evil-leader/set-key "n=" 'dotemacs-evil-numbers-increase)
    (evil-leader/set-key "n-" 'dotemacs-evil-numbers-decrease)))

(use-package evil-search-highlight-persist
  :ensure t
  :init
  (progn
    (global-evil-search-highlight-persist)
    (evil-leader/set-key "/" 'evil-search-highlight-persist-remove-all)
    ;; (evil-leader/set-key "sc" 'evil-search-highlight-persist-remove-all)
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

    (dotemacs/add-to-hooks (lambda ()
                    (dotemacs-surround-add-pair "`" "`"  "'"))
                  '(emacs-lisp-mode-hook lisp-mode-hook))

    (dotemacs/add-to-hooks (lambda ()
                    (dotemacs-surround-add-pair "~" "```"  "```"))
                  '(markdown-mode-hook))

    (add-hook 'LaTeX-mode-hook (lambda ()
                                 (dotemacs-surround-add-pair "~" "\\texttt{" "}")
                                 (dotemacs-surround-add-pair "=" "\\verb=" "=")
                                 (dotemacs-surround-add-pair "/" "\\emph{" "}")
                                 (dotemacs-surround-add-pair "*" "\\textbf{" "}")
                                 (dotemacs-surround-add-pair "P" "\\(" "\\)")))
    (dotemacs/add-to-hooks (lambda ()
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
  :ensure t
  :init
  (progn
    (dotemacs|do-after-display-system-init
      (unless (display-graphic-p)
        (setq evil-visual-state-cursor 'box ; █
              evil-insert-state-cursor 'bar ; ⎸
              evil-emacs-state-cursor 'hbar ; _
        )))))

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
  :commands (evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (progn
    (define-key evil-visual-state-map (kbd "*")
      'evil-visualstar/begin-search-forward)
    (define-key evil-visual-state-map (kbd "#")
      'evil-visualstar/begin-search-backward)))

(use-package init-bindings
  :load-path "config/"
  :init
  (with-eval-after-load 'projectile
                        (dotemacs-toggle-transparency)))


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

(use-package company-math
  :if (eq dotemacs-completion-engine 'company)
  :ensure t
  :defer t
  :init (push 'company-math company-backends-LaTeX-mode))

(use-package company-auctex
  :if (eq dotemacs-completion-engine 'company)
  :defer t
  :init
  (progn
    (push 'company-auctex-labels company-backends-LaTeX-mode)
    (push 'company-auctex-bibs company-backends-LaTeX-mode)
    (push '(company-auctex-macros
            company-auctex-symbols
            company-auctex-environments) company-backends-LaTeX-mode)))

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
  :init (with-eval-after-load 'latex
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
  (dotemacs/add-flycheck-hook 'LaTeX-mode))

(dotemacs-use-package-add-hook flyspell
  :post-init
  (spell-checking/add-flyspell-hook 'LaTeX-mode))

(dotemacs-use-package-add-hook yasnippet
  :post-init
  (add-hook 'LaTeX-mode-hook 'dotemacs-load-yasnippet))


;;; Markdown
(dotemacs-defvar-company-backends markdown-mode)

(use-package init-markdown
  :load-path "config/"
  :defer t)

(use-package gh-md
  :ensure t
  :defer t
  :init
  (evil-leader/set-key-for-mode 'markdown-mode
    "mcr"  'gh-md-render-buffer))

(dotemacs-use-package-add-hook smartparens
  :post-init
  (progn
    (add-hook 'markdown-mode-hook 'smartparens-mode)))

(use-package markdown-mode              ; Markdown
  :mode (("\\.m[k]d" . markdown-mode)
         ("\\.markdown$" . markdown-mode)
         ("\\.apib$" . markdown-mode))
  :defer t
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
    (add-hook 'gfm-mode-hook #'dotemacs-whitespace-style-no-long-lines)

    (bind-key "C-c C-s C" #'markdown-insert-gfm-code-block markdown-mode-map)
    (bind-key "C-c C-s P" #'markdown-insert-gfm-code-block markdown-mode-map)

    ;; Declare prefixes and bind keys
    (dolist (prefix '(("mc" . "markdown/command")
                      ("mh" . "markdown/header")
                      ("mi" . "markdown/insert")
                      ("ml" . "markdown/lists")
                      ("mx" . "markdown/text")))
      (dotemacs-declare-prefix-for-mode
       'markdown-mode (car prefix) (cdr prefix)))

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
      "mcc"  'markdown-check-refs
      "mce"  'markdown-export
      "mcm"  'markdown-other-window
      "mcn"  'markdown-cleanup-list-numbers
      "mco"  'markdown-open
      "mcp"  'markdown-preview
      "mcv"  'markdown-export-and-preview
      "mcw"  'markdown-kill-ring-save
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
      "mxC"  'markdown-insert-gfm-code-block
      "mxq"  'markdown-insert-blockquote
      "mxQ"  'markdown-blockquote-region
      "mxp"  'markdown-insert-pre
      "mxP"  'markdown-pre-region
      ;; Following and Jumping
      "mN"   'markdown-next-link
      "mf"   'markdown-follow-thing-at-point
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

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook markdown-mode)
      (push 'company-capf company-backends-markdown-mode)))
  (dotemacs-use-package-add-hook company-emoji
    :post-init
    (progn
      (push 'company-emoji company-backends-markdown-mode))))

(dotemacs-use-package-add-hook flyspell
  :post-init
  (spell-checking/add-flyspell-hook 'markdown-mode))

(dotemacs-use-package-add-hook emoji-cheat-sheet-plus
  :post-init
  (add-hook 'markdown-mode-hook 'emoji-cheat-sheet-plus-display-mode))


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
  (dotemacs/add-flycheck-hook 'rst-mode))

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
    (with-eval-after-load 'flycheck
      (when-let (handlebars (executable-find "handlebars"))
                (setq flycheck-handlebars-executable handlebars)))))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'handlebars-mode))

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
    ;; Highlight and follow bug references in comments and strings
    (add-hook 'prog-mode-hook 'bug-reference-prog-mode)

    (setq dotemacs-prog-mode-hook #'dotemacs-prog-mode-defaults)
    (add-hook 'prog-mode-hook
              (lambda ()
                (run-hooks #'dotemacs-prog-mode-hook)))
    ))

(use-package aggressive-indent
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-add-toggle aggressive-indent
      :status aggressive-indent-mode
      :on (aggressive-indent-mode)
      :off (aggressive-indent-mode -1)
      :documentation "Always keep code indented."
      :evil-leader "tI")
    (dotemacs-add-toggle aggressive-indent-globally
      :status aggressive-indent-mode
      :on (global-aggressive-indent-mode)
      :off (global-aggressive-indent-mode -1)
      :documentation "Always keep code indented globally."
      :evil-leader "t C-I"))
  :config
  (progn
    (add-hook 'diff-auto-refine-mode-hook 'dotemacs-toggle-aggressive-indent-off)
    (dotemacs-diminish aggressive-indent-mode " Ⓘ" " I")))

(use-package prog-mode                  ; Prog Mode
  :bind (("C-c u p" . prettify-symbols-mode))
  :init
  (add-hook 'js2-mode-hook
            (lambda ()
              (push '("not" . 172) prettify-symbols-alist) ;
              (push '("!" . 172) prettify-symbols-alist) ;
              (push '("forall" . 8704) prettify-symbols-alist) ;
              (push '("::" . 8759) prettify-symbols-alist) ;
              ; (push '("." . 8728) prettify-symbols-alist) ;
              (push '("~>" . 8669) prettify-symbols-alist) ;
              ; (push '("()" . #X2205) prettify-symbols-alist) ;
              (push '("==" . #X225F) prettify-symbols-alist) ;
              (push '("!=" . #X2260) prettify-symbols-alist) ;
              (push '("===" . #X2261) prettify-symbols-alist) ;
              (push '("!==" . #X2262) prettify-symbols-alist) ;
              (push '(">=" . #X2265) prettify-symbols-alist) ;
              (push '("<=" . #X2264) prettify-symbols-alist) ;
              (push '("!!" . #X203C) prettify-symbols-alist) ;
              (push '("&&" . #X2227) prettify-symbols-alist) ;
              (push '("||" . #X2228) prettify-symbols-alist) ;
              ; (push '("null" . 00D8) prettify-symbols-alist) ;
              (push '("sqrt" . #X221A) prettify-symbols-alist) ;
              (push '("undefined" . #X22A5) prettify-symbols-alist) ;
              (push '("pi" . #X3C0) prettify-symbols-alist) ;
              (push '("function" . 955) prettify-symbols-alist) ; λ
              (push '("->" . 8594) prettify-symbols-alist) ; →
              (push '("-<" . 8610) prettify-symbols-alist) ;
              (push '("<-" . 8592) prettify-symbols-alist) ;
              (push '("=>" . 8658) prettify-symbols-alist) ; ⇒
              ; (push '("map" . 8614) prettify-symbols-alist) ; ↦
              (push '("return" . 8592) prettify-symbols-alist))))

;; http://stackoverflow.com/questions/1085170/how-to-achieve-code-folding-effects-in-emacs
(use-package hs-minor-mode
  :defer t
  :init
  (progn
    ;; Space to toggle folds.
    (define-key evil-motion-state-map (kbd "SPC") 'evil-toggle-fold)
    ;; TODO: Figure out how to only map " " when in hs-minor-mode
    ;; (evil-define-key 'motion hs-minor-mode-map " " 'evil-toggle-fold)

    ;; required for evil folding
    (setq hs-set-up-overlay 'dotemacs-fold-overlay)))

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
    (add-hook 'ielm-mode-hook #'eldoc-mode)
    ;; don't display eldoc on modeline
    (dotemacs-hide-lighter eldoc-mode)))

(use-package auto-compile
  :defer t
  :ensure t
  :diminish (auto-compile-mode . "")
  :init
  (progn
    (setq auto-compile-display-buffer nil
          ;; lets spaceline manage the mode-line
          auto-compile-use-mode-line nil
          auto-compile-mode-line-counter t)
    (add-hook 'emacs-lisp-mode-hook 'auto-compile-mode))
  :config
  (progn
    (evil-leader/set-key-for-mode 'emacs-lisp-mode
      "mcl" 'auto-compile-display-log)))


;;; Emacs Lisp
(dotemacs-defvar-company-backends emacs-lisp-mode)
(dotemacs-defvar-company-backends ielm-mode)

;; (use-package init-elisp             ; Personal tools for Emacs Lisp
;;   :load-path "config/")

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
    (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (dotemacs-declare-prefix-for-mode mode "mg" "find-symbol")
      (dotemacs-declare-prefix-for-mode mode "mh" "help")
      (evil-leader/set-key-for-mode mode
        "mgg" 'elisp-slime-nav-find-elisp-thing-at-point
        "mhh" 'elisp-slime-nav-describe-elisp-thing-at-point)))
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
  :init (with-eval-after-load 'flycheck (flycheck-package-setup)))

(use-package slime
  :ensure t
  :commands slime-mode
  :init
  (progn
    (setq slime-contribs '(slime-fancy
                           slime-indentation
                           slime-sbcl-exts
                           slime-scratch))

    (when-let (clisp (executable-find "clisp"))
      (setq inferior-lisp-program clisp))

    ;; enable fuzzy matching in code buffer and SLIME REPL
    (setq slime-complete-symbol*-fancy t)
    (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
    (defun slime/disable-smartparens ()
      (smartparens-strict-mode -1)
      (turn-off-smartparens-mode))
    (add-hook 'slime-repl-mode-hook #'slime/disable-smartparens)
    (dotemacs/add-to-hooks 'slime-mode '(lisp-mode-hook)))
  :config
  (progn
    (slime-setup)
    (dolist (m `(,slime-mode-map ,slime-repl-mode-map))
      (define-key m [(tab)] 'slime-fuzzy-complete-symbol))
    ;; TODO: Add bindings for the SLIME debugger?
    (evil-leader/set-key-for-mode 'lisp-mode
      "mcc" 'slime-compile-file
      "mcC" 'slime-compile-and-load-file
      "mcl" 'slime-load-file
      "mcf" 'slime-compile-defun
      "mcr" 'slime-compile-region
      "mcn" 'slime-remove-notes

      "meb" 'slime-eval-buffer
      "mef" 'slime-eval-defun
      "meF" 'slime-undefine-function
      "mee" 'slime-eval-last-sexp
      "mer" 'slime-eval-region

      "mgg" 'slime-inspect-definition
      "mgb" 'slime-pop-find-definition-stack
      "mgn" 'slime-next-note
      "mgN" 'slime-previous-note

      "mha" 'slime-apropos
      "mhA" 'slime-apropos-all
      "mhd" 'slime-disassemble-symbol
      "mhh" 'slime-describe-symbol
      "mhH" 'slime-hyperspec-lookup
      "mhp" 'slime-apropos-package
      "mht" 'slime-toggle-trace-fdefinition
      "mhT" 'slime-untrace-all
      "mh<" 'slime-who-calls
      "mh>" 'slime-calls-who
      ;; TODO: Add key bindings for who binds/sets globals?
      "mhr" 'slime-who-references
      "mhm" 'slime-who-macroexpands
      "mhs" 'slime-who-specializes

      "mma" 'slime-macroexpand-all
      "mmo" 'slime-macroexpand-1

      "mse" 'slime-eval-last-expression-in-repl
      "msi" 'slime
      "msq" 'slime-quit-lisp

      "mtf" 'slime-toggle-fancy-trace)))

(dotemacs-defvar-company-backends geiser-mode)

(use-package geiser
  :ensure t
  :commands run-geiser
  :config
  (progn
    (evil-leader/set-key-for-mode 'scheme-mode
      "mcc" 'geiser-compile-current-buffer
      "mcp" 'geiser-add-to-load-path

      "mgg" 'geiser-edit-symbol-at-point
      "mgb" 'geiser-pop-symbol-stack
      "mgm" 'geiser-edit-module
      "mgn" 'next-error
      "mgN" 'previous-error

      "mhh" 'geiser-doc-symbol-at-point
      "mhd" 'geiser-doc-look-up-manual
      "mhm" 'geiser-doc-module
      "mh<" 'geiser-xref-callers
      "mh>" 'geiser-xref-callees

      "mil" 'geiser-insert-lambda

      "mme" 'geiser-expand-last-sexp
      "mmf" 'geiser-expand-definition
      "mmx" 'geiser-expand-region

      "msi" 'geiser-mode-switch-to-repl
      "msb" 'geiser-eval-buffer
      "msB" 'geiser-eval-buffer-and-go
      "msf" 'geiser-eval-definition
      "msF" 'geiser-eval-definition-and-go
      "mse" 'geiser-eval-last-sexp
      "msr" 'geiser-eval-region
      "msR" 'geiser-eval-region-and-go)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      ;; Geiser provides completion as long as company mode is loaded.
      (dotemacs-add-company-hook geiser-mode))))

(use-package pcre2el                    ; Convert regexps to RX and back
  :ensure t
  :defer t
  :commands rxt-fontify-regexp-at-point
  :init
  (progn
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
      "Rh"  'rxt-fontify-regexp-at-point)))

(use-package visual-regexp-steroids
  :defer t)

(use-package macrostep                  ; Interactively expand macros in code
  :ensure t
  :defer t
  :mode ("\\*.el\\'" . emacs-lisp-mode)
  :init
  (progn
    (evil-define-key 'normal macrostep-keymap "q" 'macrostep-collapse-all)
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
  :config
  (progn
    (defun ielm-indent-line ()
      (interactive)
      (let ((current-point (point)))
        (save-restriction
          (narrow-to-region (search-backward-regexp "^ELISP>") (goto-char current-point))
          (lisp-indent-line))))
    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (dotemacs-declare-prefix-for-mode mode "ms" "ielm")
      (evil-leader/set-key-for-mode mode
        "msi" 'ielm))))

(use-package elisp-mode                  ; Emacs Lisp editing
  :defer t
  :interpreter ("emacs" . emacs-lisp-mode)
  :mode ("/Cask\\'" . emacs-lisp-mode)
  :config
  (progn
    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (dotemacs-declare-prefix-for-mode mode "mc" "compile")
      (dotemacs-declare-prefix-for-mode mode "me" "eval")
      (dotemacs-declare-prefix-for-mode mode "mt" "tests")
      (evil-leader/set-key-for-mode mode
        "mcc" 'emacs-lisp-byte-compile
        "me$" 'lisp-state-eval-sexp-end-of-line
        "meb" 'eval-buffer
        "mee" 'eval-last-sexp
        "mer" 'eval-region
        "mef" 'eval-defun
        "mel" 'lisp-state-eval-sexp-end-of-line
        "m,"  'lisp-state-toggle-lisp-state
        "mtb" 'dotemacs-ert-run-tests-buffer
        "mtq" 'ert))))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (push 'company-capf company-backends-emacs-lisp-mode)
      (push '(company-files company-capf) company-backends-ielm-mode)
      (dotemacs-add-company-hook ielm-mode)
      (dotemacs-add-company-hook emacs-lisp-mode))))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (progn
    (dolist (mode '(emacs-lisp-mode lisp-mode))
      (dotemacs/add-flycheck-hook mode))
    ;; Don't activate flycheck by default in elisp
    ;; because of too much false warnings
    ;; (dotemacs-add-flycheck-hook 'emacs-lisp-mode-hook)

    ;; Make flycheck recognize packages in loadpath
    ;; i.e (require 'company) will not give an error now
    (setq flycheck-emacs-lisp-load-path 'inherit)))

(with-eval-after-load 'evil
  (progn
    (dotemacs-define-text-object ";" "elisp-comment" ";; " "")))

(dotemacs-use-package-add-hook semantic
  :post-init
  (progn
    (semantic/enable-semantic-mode 'emacs-lisp-mode)
    (with-eval-after-load 'semantic
      (semantic-default-elisp-setup))))

(dotemacs-use-package-add-hook srefactor
  :post-init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'dotemacs-lazy-load-srefactor)
    (use-package srefactor-lisp
      :commands (srefactor-lisp-format-buffer
                 srefactor-lisp-format-defun
                 srefactor-lisp-format-sexp
                 srefactor-lisp-one-line)
      :init
      (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
        (dotemacs-declare-prefix-for-mode mode "=" "srefactor")
        (evil-leader/set-key-for-mode mode
          "=b" 'srefactor-lisp-format-buffer
          "=d" 'srefactor-lisp-format-defun
          "=o" 'srefactor-lisp-one-line
          "=s" 'srefactor-lisp-format-sexp)))))

(dotemacs-use-package-add-hook smartparens
  :post-init
  (progn
    (if (version< emacs-version "24.4")
        (ad-disable-advice 'preceding-sexp 'around 'evil)
      (advice-remove 'elisp--preceding-sexp 'evil--preceding-sexp))

    ;; but alwayws enable for lisp mode
    (dotemacs/add-to-hooks 'smartparens-strict-mode '(lisp-mode))

    (defun dotemacs-eval-current-form-sp (&optional arg)
      "Call `eval-last-sexp' after moving out of one level of
parentheses. Will exit any strings and/or comments first.
Requires smartparens because all movement is done using
`sp-up-sexp'. An optional ARG can be used which is passed to
`sp-up-sexp' to move out of more than one sexp."
      (interactive "p")
      (require 'smartparens)
      (save-excursion
        (let ((max 10))
          (while (and (> max 0)
                      (sp-point-in-string-or-comment))
            (decf max)
            (sp-up-sexp)))
        (sp-up-sexp arg)
        (call-interactively 'eval-last-sexp)))

    (defun dotemacs-eval-current-symbol-sp ()
      "Call `eval-last-sexp' on the symbol underneath the
point. Requires smartparens because all movement is done using
`sp-forward-symbol'."
      (interactive)
      (require 'smartparens)
      (save-excursion
        (sp-forward-symbol)
        (call-interactively 'eval-last-sexp)))

    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (evil-leader/set-key-for-mode mode
        "ec" 'dotemacs-eval-current-form-sp
        "es" 'dotemacs-eval-current-symbol-sp))))


;;; Scala
(use-package init-scala
  :load-path "config/")

(use-package flycheck-auto-scalastyle   ; Scalastyle setup
  :load-path "config/"
  :commands (flycheck-auto-scalastyle-setup)
  :init
  (with-eval-after-load 'scala-mode2
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
    (setq scala-indent:align-forms t
          scala-indent:align-parameters t
          scala-indent:default-run-on-strategy scala-indent:operator-strategy)

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
  :config
  (progn
    (setq sbt:sbt-prompt-regexp
          (rx bol (or (and (optional "scala") ">") ; Default prompt
                      ;; Sbt Prompt plugin
                      (and "[" (1+ (not (any "]")))"] " (1+ word) ":"))
              (0+ " ")))

    (evil-leader/set-key-for-mode 'scala-mode
      "mbb" 'sbt-command)

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

    (dolist (prefix '(("mb" . "scala/build")
                      ("mc" . "scala/check")
                      ("md" . "scala/debug")
                      ("me" . "scala/errors")
                      ("mg" . "scala/goto")
                      ("mh" . "scala/docs")
                      ("mi" . "scala/inspect")
                      ("mn" . "scala/ensime")
                      ("mr" . "scala/refactor")
                      ("mt" . "scala/test")
                      ("ms" . "scala/repl")))
      (dotemacs-declare-prefix-for-mode 'scala-mode (car prefix) (cdr prefix)))

    (evil-leader/set-key-for-mode 'scala-mode
      "m/"     'ensime-search

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
      "mgp"     'ensime-pop-find-definition-stack
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

      "mta"     'ensime-sbt-do-test-dwim
      "mtr"     'ensime-sbt-do-test-quick-dwim
      "mtt"     'ensime-sbt-do-test-only-dwim

      "msa"     'ensime-inf-load-file
      "msb"     'ensime-inf-eval-buffer
      "msB"     'ensime-inf-eval-buffer-switch
      "msi"     'ensime-inf-switch
      "msr"     'ensime-inf-eval-region
      "msR"     'ensime-inf-eval-region-switch

      "mz"      'ensime-expand-selection-command)

    ;; Enable Expand Region integration from Ensime.  Ignore load errors to
    ;; handle older Ensime versions gracefully.
    (with-eval-after-load 'expand-region
      (require 'ensime-expand-region nil 'noerror))

    ;; Don't use scala checker if ensime mode is active, since it provides
    ;; better error checking.
    (with-eval-after-load 'flycheck
      (defun scala/disable-flycheck-scala ()
        (push 'scala flycheck-disabled-checkers))
      (add-hook 'ensime-mode-hook 'scala/disable-flycheck-scala))))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'scala-mode))

(use-package ensime-sbt
  :ensure ensime
  :defer t
  ;; Compile on save.
  :config (setq ensime-sbt-perform-on-save "test:compile"))


;;; Python
(dotemacs-defvar-company-backends python-mode)
(dotemacs-defvar-company-backends inferior-python-mode)
(dotemacs-defvar-company-backends pip-requirements-mode)

(defvar python-enable-yapf-format-on-save nil
  "If non-nil, automatically format code with YAPF on save.")

(defvar python-test-runner 'nose
  "Test runner to use. Possible values are `nose' or `pytest'.")

(use-package init-python
  :load-path "config/")

(use-package cython-mode
  :defer t
  :ensure t
  :init
  (progn
    (evil-leader/set-key-for-mode 'cython-mode
      "mhh" 'anaconda-mode-show-doc
      "mgg" 'anaconda-mode-find-definitions
      "mga" 'anaconda-mode-find-assignments
      "mgu" 'anaconda-mode-find-references)
    (evilify anaconda-mode-view-mode anaconda-mode-view-mode-map
             (kbd "q") 'quit-window)))

(use-package anaconda-mode              ; Powerful Python backend for Emacs
  :defer t
  :init
  (progn
    (setq anaconda-mode-installation-directory
          (concat dotemacs-cache-directory "anaconda-mode"))
    (add-hook 'python-mode-hook 'anaconda-mode))
  :config
  (progn
    (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
      (evil-jumper--push))
    (evil-leader/set-key-for-mode 'python-mode
      "mhh" 'anaconda-mode-view-doc
      "mgu" 'anaconda-mode-usages
      "mgg"  'anaconda-mode-goto)
    (dotemacs-hide-lighter anaconda-mode)))

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

(use-package pylookup
  :quelpa (pylookup :fetcher github :repo "tsgates/pylookup")
  :commands (pylookup-lookup pylookup-update pylookup-update-all)
  :init
  (progn
    (evilify pylookup-mode pylookup-mode-map)
    (evil-leader/set-key-for-mode 'python-mode
      "mhH"  'pylookup-lookup))
  :config
  (progn
    (let ((dir dotemacs-quelpa-build-directory))
      (setq pylookup-dir (concat dir "pylookup/")
            pylookup-program (concat pylookup-dir "pylookup.py")
            pylookup-db-file (concat pylookup-dir "pylookup.db")))))

(use-package py-yapf
  :ensure t
  :init
  (evil-leader/set-key-for-mode 'python-mode "m=" 'py-yapf-buffer)
  :config
  (if python-enable-yapf-format-on-save
      (add-hook 'python-mode-hook 'py-yapf-enable-on-save)))

(use-package nose
  :ensure t
  :if (eq 'nose python-test-runner)
  :commands (nosetests-one
             nosetests-pdb-one
             nosetests-all
             nosetests-pdb-all
             nosetests-module
             nosetests-pdb-module
             nosetests-suite
             nosetests-pdb-suite)
  :init
  (evil-leader/set-key-for-mode 'python-mode
    "mtA" 'nosetests-pdb-all
    "mta" 'nosetests-all
    "mtB" 'nosetests-pdb-module
    "mtb" 'nosetests-module
    "mtT" 'nosetests-pdb-one
    "mtt" 'nosetests-one
    "mtM" 'nosetests-pdb-module
    "mtm" 'nosetests-module
    "mtS" 'nosetests-pdb-suite
    "mts" 'nosetests-suite)
  :config
  (progn
    (add-to-list 'nose-project-root-files "setup.cfg")
    (setq nose-use-verbose nil)))

(use-package pytest
  :if (eq 'pytest python-test-runner)
  :ensure t
  :defer t
  :commands (pytest-one
             pytest-pdb-one
             pytest-all
             pytest-pdb-all
             pytest-module
             pytest-pdb-module)
  :init (evil-leader/set-key-for-mode 'python-mode
          "mtA" 'pytest-pdb-all
          "mta" 'pytest-all
          "mtB" 'pytest-pdb-module
          "mtb" 'pytest-module
          "mtT" 'pytest-pdb-one
          "mtt" 'pytest-one
          "mtM" 'pytest-pdb-module
          "mtm" 'pytest-module)
  :config (add-to-list 'pytest-project-root-files "setup.cfg"))

(use-package python
  :defer t
  :ensure t
  :init
  (progn
    (with-eval-after-load 'eldoc
      (add-hook 'python-mode-hook #'eldoc-mode))

    (add-hook 'inferior-python-mode-hook #'inferior-python-setup-hook)
    (dotemacs/add-all-to-hook 'python-mode-hook
                              'python-default
                              'python-setup-shell))
  :config
  (progn
    ;; PEP 8 compliant filling rules, 79 chars maximum
    (add-hook 'python-mode-hook (lambda () (setq fill-column 79)))
    (add-hook 'python-mode-hook #'subword-mode)

    ;; add support for `ahs-range-beginning-of-defun' for python-mode
    (with-eval-after-load 'auto-highlight-symbol
      (add-to-list 'ahs-plugin-bod-modes 'python-mode))

    (dotemacs-declare-prefix-for-mode 'python-mode "mc" "execute")
    (dotemacs-declare-prefix-for-mode 'python-mode "md" "debug")
    (dotemacs-declare-prefix-for-mode 'python-mode "mh" "help")
    (dotemacs-declare-prefix-for-mode 'python-mode "mg" "goto")
    (dotemacs-declare-prefix-for-mode 'python-mode "mt" "test")
    (dotemacs-declare-prefix-for-mode 'python-mode "ms" "send to REPL")
    (dotemacs-declare-prefix-for-mode 'python-mode "mr" "refactor")
    (dotemacs-declare-prefix-for-mode 'python-mode "mv" "venv")
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
    (define-key inferior-python-mode-map (kbd "C-l") 'dotemacs/comint-clear-buffer)

    ;; add this optional key binding for Emacs user, since it is unbound
    (define-key inferior-python-mode-map (kbd "C-c M-l") 'dotemacs/comint-clear-buffer)

    ;; fix for issue #2569 (https://github.com/syl20bnr/spacemacs/issues/2569)
    ;; use `semantic-create-imenu-index' only when `semantic-mode' is enabled,
    ;; otherwise use `python-imenu-create-index'
    (defun dotemacs/python-imenu-create-index-python-or-semantic ()
      (if (bound-and-true-p semantic-mode)
          (semantic-create-imenu-index)
        (python-imenu-create-index)))

    (defadvice wisent-python-default-setup
        (after dotemacs/python-set-imenu-create-index-function activate)
      (setq imenu-create-index-function
            #'dotemacs/python-imenu-create-index-python-or-semantic))))

(with-eval-after-load 'evil-matchit
  (add-hook 'python-mode-hook 'turn-on-evil-matchit-mode))

(use-package evil-matchit-python
  :defer t
  :ensure evil-matchit
  :config
  (plist-put evilmi-plugins 'python-mode' ((evilmi-simple-get-tag evilmi-simple-jump)
                                           (evilmi-python-get-tag evilmi-python-jump))))

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
  (dotemacs/add-flycheck-hook 'python-mode))

(dotemacs-use-package-add-hook smartparens
  :post-init
  (defadvice python-indent-dedent-line-backspace
      (around python/sp-backward-delete-char activate)
    (let ((pythonp (or (not smartparens-strict-mode)
                       (char-equal (char-before) ?\s))))
      (if pythonp
          ad-do-it
        (call-interactively 'sp-backward-delete-char)))))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook python-mode)
      (push 'company-capf company-backends-pip-requirements-mode)
      (dotemacs-add-company-hook pip-requirements-mode)

      (dotemacs-add-company-hook inferior-python-mode)
      (push '(company-files company-capf) company-backends-inferior-python-mode)
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

(dotemacs-use-package-add-hook semantic
  :post-init
  (progn
    (semantic/enable-semantic-mode 'python-mode)
    (defadvice semantic-python-get-system-include-path (around semantic-python-skip-error-advice activate)
      "Don't cause error when Semantic cannot retrieve include
paths for Python then prevent the buffer to be switched. This
issue might be fixed in Emacs 25. Until then, we need it here to
fix this issue."
      (condition-case nil
          ad-do-it
        (error nil)))))

(dotemacs-use-package-add-hook stickyfunc-enhance
  :post-init
  (add-hook 'python-mode-hook 'dotemacs-lazy-load-stickyfunc-enhance))


;;; Ruby
(dotemacs-defvar-company-backends enh-ruby-mode)
(dotemacs-defvar-company-backends ruby-mode)

;; TODO: uncomment this when it becomes available
;; (dotemacs-defvar-company-backends haml-mode)

(defvar ruby-enable-enh-ruby-mode nil
  "If non-nil, use `enh-ruby-mode' package insted of the built-in Ruby Mode.

Otherwise use Enh Ruby Mode, which is the default.")

(use-package rbenv
  :ensure t
  :disabled t
  :if (eq dotemacs-ruby-version-manager 'rbenv)
  :defer t
  :init (global-rbenv-mode)
  :config (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
            (add-hook hook (lambda () (rbenv-use-corresponding)))))

(use-package rvm
  :ensure t
  :disabled t
  :defer t
  :if (eq dotemacs-ruby-version-manager 'rvm)
  :init (rvm-use-default)
  :config (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
            (add-hook hook (lambda () (rvm-activate-corresponding-ruby)))))

(use-package enh-ruby-mode
  :if (when ruby-enable-enh-ruby-mode)
  :ensure t
  :mode (("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . enh-ruby-mode)
         ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\)\\'" . enh-ruby-mode))
  :interpreter "ruby"
  :config
  (progn
    (setq enh-ruby-deep-indent-paren nil
          enh-ruby-hanging-paren-deep-indent-level 2)
    (sp-with-modes '(enh-ruby-mode)
      (sp-local-pair "{" "}"
                     :pre-handlers '(sp-ruby-pre-handler)
                     :post-handlers '(sp-ruby-post-handler (dotemacs-smartparens-pair-newline-and-indent "RET"))
                     :suffix ""))))

(use-package ruby-mode
  :if (unless ruby-enable-enh-ruby-mode)
  :ensure t
  :defer t
  :config
  (progn
    (evil-leader/set-key-for-mode 'ruby-mode
      "m'" 'ruby-toggle-string-quotes
      "m{" 'ruby-toggle-block)

    (sp-with-modes 'ruby-mode
      (sp-local-pair "{" "}"
                     :pre-handlers '(sp-ruby-pre-handler)
                     :post-handlers '(sp-ruby-post-handler (dotemacs-smartparens-pair-newline-and-indent "RET"))
                     :suffix ""))))

(use-package ruby-tools
  :defer t
  :ensure t
  :init
  (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
    (add-hook hook 'ruby-tools-mode))
  :config
  (progn
    (dotemacs-hide-lighter ruby-tools-mode)
    (dolist (mode '(ruby-mode enh-ruby-mode))
      (dotemacs-declare-prefix-for-mode mode "mx" "ruby/text")
      (evil-leader/set-key-for-mode mode
        "mx\'" 'ruby-tools-to-single-quote-string
        "mx\"" 'ruby-tools-to-double-quote-string
        "mx:" 'ruby-tools-to-symbol))))

(use-package bundler
  :defer t
  :ensure t
  :init
  (dolist (mode '(ruby-mode enh-ruby-mode))
    (dotemacs-declare-prefix-for-mode mode "mb" "ruby/bundle")
    (evil-leader/set-key-for-mode mode
      "mbc" 'bundle-check
      "mbi" 'bundle-install
      "mbs" 'bundle-console
      "mbu" 'bundle-update
      "mbx" 'bundle-exec)))

(use-package projectile-rails
  :if (when dotemacs-ruby-enable-ruby-on-rails-support)
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'projectile-mode-hook 'projectile-rails-on))
  :config
  (progn
    (dotemacs-diminish projectile-rails-mode " ⇋" " RoR")
    ;; Find files
    (dolist (mode '(ruby-mode enh-ruby-mode))
      (evil-leader/set-key-for-mode mode
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
        "mrRx" 'projectile-rails-extract-region))
    ;; Ex-commands
    (evil-ex-define-cmd "A" 'projectile-toggle-between-implementation-and-test)))

(use-package robe                       ; Ruby backend for Emacs
  :defer t
  :ensure t
  :init
  (progn
    (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
      (add-hook hook 'robe-mode))
    (when (eq dotemacs-completion-engine 'company)
      (push 'company-robe company-backends-enh-ruby-mode)
      (push 'company-robe company-backends-ruby-mode)))
  :config
  (progn
    (dotemacs-hide-lighter robe-mode)
    (dolist (mode '(ruby-mode enh-ruby-mode))
      (dotemacs-declare-prefix-for-mode mode "mg" "ruby/goto")
      (dotemacs-declare-prefix-for-mode mode "mh" "ruby/docs")
      (dotemacs-declare-prefix-for-mode mode "ms" "ruby/repl")
      (evil-leader/set-key-for-mode mode
        ;; robe mode specific
        "mgg" 'robe-jump
        "mhd" 'robe-doc
        "mrsr" 'robe-rails-refresh
        ;; inf-enh-ruby-mode
        "msf" 'ruby-send-definition
        "msF" 'ruby-send-definition-and-go
        "msi" 'robe-start
        "msr" 'ruby-send-region
        "msR" 'ruby-send-region-and-go
        "mss" 'ruby-switch-to-inf))))

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
    (spell-checking/add-flyspell-hook 'feature-mode)))

(use-package haml-mode
  :ensure t
  :if (when dotemacs-ruby-enable-ruby-on-rails-support)
  :defer t)

(use-package ruby-test-mode
  :defer t
  :ensure t
  :init (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
          (add-hook hook 'ruby-test-mode))
  :config
  (progn
    (dotemacs-hide-lighter ruby-test-mode)
    (dolist (mode '(ruby-mode enh-ruby-mode))
      (dotemacs-declare-prefix-for-mode mode "mt" "ruby/test")
      (evil-leader/set-key-for-mode mode "mtb" 'ruby-test-run)
      (evil-leader/set-key-for-mode mode "mtt" 'ruby-test-run-at-point))))

(use-package inf-ruby                   ; Ruby REPL
  :ensure t
  :defer t
  :init (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
  :config
  ;; Easily switch to Inf Ruby from compilation modes to Inf Ruby
  (inf-ruby-switch-setup))

(use-package evil-matchit-ruby
  :ensure evil-matchit
  :config
  (progn
    (plist-put evilmi-plugins 'enh-ruby-mode '((evilmi-simple-get-tag evilmi-simple-jump)
                                               (evilmi-ruby-get-tag evilmi-ruby-jump)))))

(dotemacs-use-package-add-hook evil-matchit
  :post-init
  (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
    (add-hook hook `turn-on-evil-matchit-mode)))

(dotemacs-use-package-add-hook rainbow-delimiters
  :post-init
  (progn
    (dotemacs/add-to-hooks 'rainbow-delimiters-mode '(haml-mode-hook))))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dolist (mode '(haml-mode yaml-mode ruby-mode enh-ruby-mode))
    (dotemacs/add-flycheck-hook mode)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook enh-ruby-mode)
      (dotemacs-add-company-hook ruby-mode)

      (with-eval-after-load 'company-dabbrev-code
        (dolist (mode '(ruby-mode enh-ruby-mode))
          (push mode company-dabbrev-code-modes))))))


;;; Rust
(dotemacs-defvar-company-backends rust-mode)

(defvar rust-enable-racer nil
  "If non-nil, load the racer package (this has an external dependency).")

(use-package init-rust
  :load-path "config/")

(use-package rust-mode                  ; Rust
  :ensure t
  :defer t
  :config
  (progn
    (evil-leader/set-key-for-mode 'rust-mode
      "mcc" 'dotemacs-rust-cargo-build
      "mct" 'dotemacs-rust-cargo-test
      "mcd" 'dotemacs-rust-cargo-doc
      "mcx" 'dotemacs-rust-cargo-run)))

(use-package racer
  :if rust-enable-racer
  :defer t
  :ensure t
  :init (dotemacs/add-to-hook 'rust-mode-hook '(racer-mode eldoc-mode)))

(use-package flycheck-rust              ; Flycheck setup for Rust
  :ensure t
  :commands (flycheck-rust-setup)
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'rust-mode))

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
;; cabal install stylish-haskell hlint ghc-mod hasktags
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
  :init
  (progn
    (defun dotemacs//force-haskell-mode-loading ()
      "Force `haskell-mode' loading when visiting cabal file."
      (require 'haskell-mode))
    (add-hook 'haskell-cabal-mode-hook
              'dotemacs//force-haskell-mode-loading)

    ;; settings
    (setq
      ;; Use notify.el (if you have it installed) at the end of running
      ;; Cabal commands or generally things worth notifying.
      haskell-notify-p t
      ;; To enable tags generation on save.
      haskell-tags-on-save t
      ;; Remove annoying error popups
      haskell-interactive-popup-errors nil
      ;; Better import handling
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
      haskell-stylish-on-save nil))
  :config
  (progn
    ;; hooks
    (add-hook 'haskell-mode-hook 'dotemacs-init-haskell-mode)
    (add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)

    ;; prefixes
    (dotemacs-declare-prefix-for-mode 'haskell-mode "mg" "haskell/navigation")
    (dotemacs-declare-prefix-for-mode 'haskell-mode "ms" "haskell/repl")
    (dotemacs-declare-prefix-for-mode 'haskell-mode "mc" "haskell/cabal")
    (dotemacs-declare-prefix-for-mode 'haskell-mode "mh" "haskell/documentation")
    (dotemacs-declare-prefix-for-mode 'haskell-mode "md" "haskell/debug")
    (dotemacs-declare-prefix-for-mode 'haskell-interactive-mode "ms" "haskell/repl")
    (dotemacs-declare-prefix-for-mode 'haskell-cabal-mode "ms" "haskell/repl")

    ;; key bindings
    (evil-leader/set-key-for-mode 'haskell-mode
      "mgg"  'haskell-mode-jump-to-def-or-tag
      "mgi"  'haskell-navigate-imports
      "mf"   'haskell-mode-stylish-buffer

      "msb"  'haskell-process-load-or-reload
      "msc"  'haskell-interactive-mode-clear
      "mss"  'dotemacs/haskell-interactive-bring
      "msS"  'haskell-interactive-switch

      "mca"  'haskell-process-cabal
      "mcb"  'haskell-process-cabal-build
      "mcc"  'haskell-compile
      "mcv"  'haskell-cabal-visit-file

      "mhd"  'inferior-haskell-find-haddock
      "mhh"  'hoogle
      "mhH"  'hoogle-lookup-from-local
      "mhi"  (lookup-key haskell-mode-map (kbd "C-c C-i"))
      "mht"  (lookup-key haskell-mode-map (kbd "C-c C-t"))
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

    ;; configure C-c C-l so it doesn't throw any errors
    (bind-key "C-c C-l" 'haskell-process-load-or-reload haskell-mode-map)

    ;; Switch back to editor from REPL
    (evil-leader/set-key-for-mode 'haskell-interactive-mode
      "msS"  'haskell-interactive-switch-back)

    ;; Compile
    (evil-leader/set-key-for-mode 'haskell-cabal
      "mC"  'haskell-compile)

    ;; Cabal-file bindings
    (evil-leader/set-key-for-mode 'haskell-cabal-mode
      ;; "m="   'haskell-cabal-subsection-arrange-lines ;; Does a bad job, 'gg=G' works better
      "md"   'haskell-cabal-add-dependency
      "mb"   'haskell-cabal-goto-benchmark-section
      "me"   'haskell-cabal-goto-executable-section
      "mt"   'haskell-cabal-goto-test-suite-section
      "mm"   'haskell-cabal-goto-exposed-modules
      "ml"   'haskell-cabal-goto-library-section
      "mn"   'haskell-cabal-next-subsection
      "mp"   'haskell-cabal-previous-subsection
      "msc"  'haskell-interactive-mode-clear
      "mss"  'dotemacs/haskell-interactive-bring
      "msS"  'haskell-interactive-switch
      "mN"   'haskell-cabal-next-section
      "mP"   'haskell-cabal-previous-section
      "mf"   'haskell-cabal-find-or-create-source-file)

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
        ;; function suggested in
        ;; https://github.com/chrisdone/ghci-ng#using-with-haskell-mode
        "mu"   'haskell-mode-find-uses
        "mht"  'haskell-mode-show-type-at
        "mgg"  'haskell-mode-goto-loc))

    ;; Useful to have these keybindings for .cabal files, too.
    (with-eval-after-load 'haskell-cabal-mode-map
      (define-key haskell-cabal-mode-map
        [?\C-c ?\C-z] 'haskell-interactive-switch))

    ;; align rules for Haskell
    (with-eval-after-load 'align
      (add-to-list 'align-rules-list
                   '(haskell-types
                     (regexp . "\\(\\s-+\\)\\(::\\|?\\)\\s-+")
                     (modes . '(haskell-mode literate-haskell-mode))))
      (add-to-list 'align-rules-list
                   '(haskell-assignment
                     (regexp . "\\(\\s-+\\)=\\s-+")
                     (modes . '(haskell-mode literate-haskell-mode))))
      (add-to-list 'align-rules-list
                   '(haskell-arrows
                     (regexp . "\\(\\s-+\\)\\(->\\|?\\)\\s-+")
                     (modes . '(haskell-mode literate-haskell-mode))))
      (add-to-list 'align-rules-list
                   '(haskell-left-arrows
                     (regexp . "\\(\\s-+\\)\\(<-\\|?\\)\\s-+")
                     (modes . '(haskell-mode literate-haskell-mode)))))))

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
  :commands flycheck-haskell-configure
  :init (add-hook 'flycheck-mode-hook 'flycheck-haskell-configure))

(use-package helm-hayoo
  :ensure t
  :defer t
  :init (with-eval-after-load 'haskell-mode
          (bind-key "C-c h h" #'helm-hayoo haskell-mode-map)))

(use-package helm-hoogle
  :ensure t
  :defer t
  :init (with-eval-after-load 'haskell-mode
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
      (let ((snip-dir (expand-file-name "snippets" haskell-snippets-dir)))
        (add-to-list 'yas-snippet-dirs snip-dir t)
        (yas-load-directory snip-dir)))

    ;; TODO only load once
    (with-eval-after-load 'yasnippet (haskell-snippets-initialize))))

(use-package cmm-mode
  :ensure t
  :defer t)

(use-package ghc
  :ensure t
  :if dotemacs-haskell-enable-ghc-mod-support
  :defer t
  :init (add-hook 'haskell-mode-hook 'ghc-init)
  :config
  (progn
    (dotemacs-declare-prefix-for-mode 'haskell-mode "mm" "haskell/ghc-mod")
    (evil-leader/set-key-for-mode 'haskell-mode
      "mmt" 'ghc-insert-template-or-signature
      "mmu" 'ghc-initial-code-from-signature
      "mma" 'ghc-auto
      "mmf" 'ghc-refine
      "mme" 'ghc-expand-th
      "mmn" 'ghc-goto-next-hole
      "mmp" 'ghc-goto-prev-hole
      "mm>"  'ghc-make-indent-deeper
      "mm<"  'ghc-make-indent-shallower)))

(dotemacs-use-package-add-hook flycheck
  :post-config
  (progn
    (with-eval-after-load 'ghc
      ;; remove overlays from ghc-check.el if flycheck is enabled
      (set-face-attribute 'ghc-face-error nil :underline nil)
      (set-face-attribute 'ghc-face-warn nil :underline nil))))

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
  (dotemacs/add-flycheck-hook 'haskell-mode))

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
  (push (if dotemacs-haskell-enable-ghc-mod-support
            '(company-ghc company-dabbrev-code company-yasnippet)
          '(company-dabbrev-code company-yasnippet))
        company-backends-haskell-mode))

(use-package company-cabal
  :if (eq dotemacs-completion-engine 'company)
  :ensure t
  :defer t
  :init
  (progn
    (push '(company-cabal) company-backends-haskell-cabal-mode)))


;;; Go
(dotemacs-defvar-company-backends go-mode)

(defvar go-use-gocheck-for-testing nil
  "If using gocheck for testing when running the tests -check.f
  will be used instead of -run to specify the test that will be
  ran. Gocheck is mandatory for testing suites.")

(use-package init-go
  :commands (load-gopath-file
             go/init-go-oracle
             dotemacs-go-run-package-tests)
  :load-path "config/")

(use-package go-rename
  :disabled t
  :init
  (evil-leader/set-key-for-mode 'go-mode "mrn" 'go-rename))

(dotemacs|do-after-display-system-init
 (when (memq window-system '(mac ns x))
   (exec-path-from-shell-copy-env "GOPATH")
   (exec-path-from-shell-copy-env "GO15VENDOREXPERIMENT")))

(use-package go-mode
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save)

    (evil-leader/set-key-for-mode 'go-mode
      "mhh" 'godoc-at-point
      "mig" 'go-goto-imports
      "mia" 'go-import-add
      "mir" 'go-remove-unused-imports
      "meb" 'go-play-buffer
      "mer" 'go-play-region
      "med" 'go-download-play
      "mxx" 'dotemacs/go-run-main
      "mga" 'ff-find-other-file
      "mgg" 'godef-jump
      "mtt" 'dotemacs/go-run-test-current-function
      "mts" 'dotemacs/go-run-test-current-suite
      "mtp" 'dotemacs/go-run-package-tests
      "mtP" 'dotemacs/go-run-package-tests-nested)))

(use-package go-eldoc
  :disabled t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'go-mode))

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
  (dolist (mode '(c-mode c++-mode))
    (dotemacs/add-flycheck-hook mode)))

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
    (dotemacs/add-to-hooks 'dotemacs-lazy-load-srefactor '(c-mode-hook c++-mode-hook)) ))

(dotemacs-use-package-add-hook stickyfunc-enhance
  :post-init
  (progn
    (dotemacs/add-to-hooks 'dotemacs-lazy-load-stickyfunc-enhance '(c-mode-hook c++-mode-hook) )))

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

(use-package srefactor
  :ensure t
  :init
  (progn
    (defun dotemacs-lazy-load-srefactor ()
      "Lazy load the package."
      (require 'srefactor)
      ;; currently, evil-mode overrides key mapping of srefactor menu
      ;; must expplicity enable evil-emacs-state. This is ok since
      ;; srefactor supports j,k,/ and ? commands when Evil is
      ;; available
      (add-hook 'srefactor-ui-menu-mode-hook 'evil-emacs-state))))

(use-package stickyfunc-enhance
  :ensure t
  :init
  (defun dotemacs-lazy-load-stickyfunc-enhance ()
    "Lazy load the package."
    (require 'stickyfunc-enhance)))


;;; Clojure
(dotemacs-defvar-company-backends cider-mode)
(dotemacs-defvar-company-backends cider-repl-mode)

(setq clojure/key-binding-prefixes '(("md" . "debug")
                                     ("me" . "evaluation")
                                     ("mg" . "goto")
                                     ("mh" . "documentation")
                                     ("mr" . "refactor")
                                     ("mra" . "add")
                                     ("mrc" . "cycle/clean")
                                     ("mrd" . "destructure")
                                     ("mre" . "extract/expand")
                                     ("mrf" . "find/function")
                                     ("mrh" . "hotload")
                                     ("mri" . "introduce/inline")
                                     ("mrm" . "move")
                                     ("mrp" . "project/promote")
                                     ("mrr" . "remove/rename/replace")
                                     ("mrs" . "show/sort/stop")
                                     ("mrt" . "thread")
                                     ("mru" . "unwind/update")
                                     ("ms" . "repl")
                                     ("mt" . "test")
                                     ("mT" . "toggle")
                                     ("mf" . "format")))
(mapc (lambda (x) (dotemacs-declare-prefix-for-mode
                   'clojure-mode (car x) (cdr x)))
            clojure/key-binding-prefixes)

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
    (with-eval-after-load 'eval-sexp-fu
      (require 'cider-eval-sexp-fu))
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

    (defun dotemacs-cider-display-error-buffer (&optional arg)
      "Displays the *cider-error* buffer in the current window.
If called with a prefix argument, uses the other-window instead."
      (interactive "P")
      (let ((buffer (get-buffer cider-error-buffer)))
        (when buffer
          (funcall (if (equal arg '(4))
                       'switch-to-buffer-other-window
                     'switch-to-buffer)
                   buffer))))

    (defun dotemacs-cider-toggle-repl-pretty-printing ()
      (interactive)
      (setq cider-repl-use-pretty-printing
            (if cider-repl-use-pretty-printing nil t))
      (message "Cider REPL pretty printing: %s"
               (if cider-repl-use-pretty-printing "ON" "OFF")))

    (defun dotemacs-cider-toggle-repl-font-locking ()
      (interactive)
      (setq cider-repl-use-clojure-font-lock
            (if cider-repl-use-pretty-printing nil t))
      (message "Cider REPL clojure-mode font-lock: %s"
               (if cider-repl-use-clojure-font-lock "ON" "OFF")))

    (defun dotemacs-cider-debug-setup ()
      (evil-make-overriding-map cider--debug-mode-map 'normal)
      (evil-normalize-keymaps))

    (add-hook 'cider--debug-mode-hook 'dotemacs-cider-debug-setup)

    (evilify cider-stacktrace-mode cider-stacktrace-mode-map
             (kbd "C-j") 'cider-stacktrace-next-cause
             (kbd "C-k") 'cider-stacktrace-previous-cause
             (kbd "TAB") 'cider-stacktrace-cycle-current-cause
             (kbd "0")   'cider-stacktrace-cycle-all-causes
             (kbd "1")   'cider-stacktrace-cycle-cause-1
             (kbd "2")   'cider-stacktrace-cycle-cause-2
             (kbd "3")   'cider-stacktrace-cycle-cause-3
             (kbd "4")   'cider-stacktrace-cycle-cause-4
             (kbd "5")   'cider-stacktrace-cycle-cause-5
             (kbd "a")   'cider-stacktrace-toggle-all
             (kbd "c")   'cider-stacktrace-toggle-clj
             (kbd "d")   'cider-stacktrace-toggle-duplicates
             (kbd "J")   'cider-stacktrace-toggle-java
             (kbd "r")   'cider-stacktrace-toggle-repl
             (kbd "T")   'cider-stacktrace-toggle-tooling)

    ;; open cider-doc directly and close it with q
    (setq cider-prompt-for-symbol nil)

    (evilify cider-docview-mode cider-docview-mode-map
             (kbd "q") 'cider-popup-buffer-quit)

    (evilify cider-inspector-mode cider-inspector-mode-map
             (kbd "L") 'cider-inspector-pop
             (kbd "n") 'cider-inspector-next-page
             (kbd "N") 'cider-inspector-previous-page
             (kbd "r") 'cider-inspector-refresh)

    (evilify cider-test-report-mode cider-test-report-mode-map
             (kbd "C-j") 'cider-test-next-result
             (kbd "C-k") 'cider-test-previous-result
             (kbd "RET") 'cider-test-jump
             (kbd "d")   'cider-test-ediff
             (kbd "e")   'cider-test-stacktrace
             (kbd "q")   'cider-popup-buffer-quit
             (kbd "r")   'cider-test-rerun-tests
             (kbd "t")   'cider-test-run-test
             (kbd "T")   'cider-test-run-tests)

    ;; TODO: having this work for cider-macroexpansion-mode would be nice,
    ;;       but the problem is that it uses clojure-mode as its major-mode

    (dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
      (evil-leader/set-key-for-mode m
        "mhh" 'cider-doc
        "mhg" 'cider-grimoire
        "mhj" 'cider-javadoc

        "meb" 'cider-eval-buffer
        "mee" 'cider-eval-last-sexp
        "mef" 'cider-eval-defun-at-point
        "mer" 'cider-eval-region
        "mew" 'cider-eval-last-sexp-and-replace

        "mfb" 'cider-format-buffer

        "mgb" 'cider-jump-back
        "mge" 'cider-jump-to-compilation-error
        "mgg" 'cider-find-var
        "mgr" 'cider-jump-to-resource

        "msb" 'cider-load-buffer
        "msB" 'dotemacs-cider-send-buffer-in-repl-and-focus
        "msc" 'cider-connect
        "mse" 'dotemacs-cider-send-last-sexp-to-repl
        "msE" 'dotemacs-cider-send-last-sexp-to-repl-focus
        "msf" 'dotemacs-cider-send-function-to-repl
        "msF" 'dotemacs-cider-send-function-to-repl-focus
        "msi" 'cider-jack-in
        "msI" 'cider-jack-in-clojurescript
        "msn" 'dotemacs-cider-send-ns-form-to-repl
        "msN" 'dotemacs-cider-send-ns-form-to-repl-focus
        "msq" 'cider-quit
        "msr" 'dotemacs-cider-send-region-to-repl
        "msR" 'dotemacs-cider-send-region-to-repl-focus
        "mss" 'cider-switch-to-repl-buffer
        "msx" 'cider-refresh
        "mTf" 'dotemacs-cider-toggle-repl-font-locking
        "mTp" 'dotemacs-cider-toggle-repl-pretty-printing

        "mta" 'dotemacs-cider-test-run-all-tests
        "mtr" 'dotemacs-cider-test-rerun-tests
        "mtt" 'dotemacs-cider-test-run-focused-test

        "mdb" 'cider-debug-defun-at-point
        "mde" 'dotemacs-cider-display-error-buffer
        "mdi" 'cider-inspect))

    (evil-leader/set-key-for-mode 'cider-repl-mode
      "mhh" 'cider-doc
      "mhg" 'cider-grimoire
      "mhj" 'cider-javadoc

      "mee" 'cider-eval-last-sexp
      "mef" 'cider-eval-defun-at-point
      "mer" 'cider-eval-region
      "mew" 'cider-eval-last-sexp-and-replace

      "mgb" 'cider-jump-back
      "mge" 'cider-jump-to-compilation-error
      "mgg" 'cider-find-var
      "mgr" 'cider-jump-to-resource
      "msc" 'cider-repl-clear-buffer

      "msn" 'cider-repl-set-ns
      "msq" 'cider-quit
      "mss" 'cider-switch-to-last-clojure-buffer
      "msx" 'cider-refresh
      "mTf" 'dotemacs-cider-toggle-repl-font-locking
      "mTp" 'dotemacs-cider-toggle-repl-pretty-printing

      "mdb" 'cider-debug-defun-at-point
      "mde" 'dotemacs-cider-display-error-buffer
      "mdi" 'cider-inspect)

    (evil-define-key 'normal cider-repl-mode-map
      "C-j" 'cider-repl-next-input
      "C-k" 'cider-repl-previous-input)

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

    (dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
      (evil-leader/set-key-for-mode m
        "mr?"  'cljr-describe-refactoring
        "mrad" 'cljr-add-declaration
        "mrai" 'cljr-add-import-to-ns
        "mram" 'cljr-add-missing-libspec
        "mrap" 'cljr-add-project-dependency
        "mrar" 'cljr-add-require-to-ns
        "mras" 'cljr-add-stubs
        "mrau" 'cljr-add-use-to-ns
        "mrcc" 'cljr-cycle-coll
        "mrci" 'cljr-cycle-if
        "mrcn" 'cljr-clean-ns
        "mrcp" 'cljr-cycle-privacy
        "mrdk" 'cljr-destructure-keys
        "mrec" 'cljr-extract-constant
        "mred" 'cljr-extract-def
        "mref" 'cljr-extract-function
        "mrel" 'cljr-expand-let
        "mrfe" 'cljr-create-fn-from-example
        "mrfu" 'cljr-find-usages
        "mrhd" 'cljr-hotload-dependency
        "mril" 'cljr-introduce-let
        "mris" 'cljr-inline-symbol
        "mrmf" 'cljr-move-form
        "mrml" 'cljr-move-to-let
        "mrpc" 'cljr-project-clean
        "mrpf" 'cljr-promote-function
        "mrrd" 'cljr-remove-debug-fns
        "mrrf" 'cljr-rename-file-or-dir
        "mrrl" 'cljr-remove-let
        "mrrr" 'cljr-remove-unused-requires
        "mrrs" 'cljr-rename-symbol
        "mrru" 'cljr-replace-use
        "mrsc" 'cljr-show-changelog
        "mrsn" 'cljr-sort-ns
        "mrsp" 'cljr-sort-project-dependencies
        "mrsr" 'cljr-stop-referring
        "mrtf" 'cljr-thread-first-all
        "mrth" 'cljr-thread
        "mrtl" 'cljr-thread-last-all
        "mrua" 'cljr-unwind-all
        "mrup" 'cljr-update-project-dependencies
        "mruw" 'cljr-unwind))

    (evil-leader/set-key-for-mode 'cider-repl-mode
      "mr?"  'cljr-describe-refactoring
      "mrap" 'cljr-add-project-dependency
      "mras" 'cljr-add-stubs
      "mrcc" 'cljr-cycle-coll
      "mrci" 'cljr-cycle-if
      "mrcp" 'cljr-cycle-privacy
      "mrdk" 'cljr-destructure-keys
      "mrel" 'cljr-expand-let
      "mrfu" 'cljr-find-usages
      "mrhd" 'cljr-hotload-dependency
      "mril" 'cljr-introduce-let
      "mrml" 'cljr-move-to-let
      "mrpc" 'cljr-project-clean
      "mrrl" 'cljr-remove-let
      "mrsp" 'cljr-sort-project-dependencies
      "mrsc" 'cljr-show-changelog
      "mrtf" 'cljr-thread-first-all
      "mrth" 'cljr-thread
      "mrtl" 'cljr-thread-last-all
      "mrua" 'cljr-unwind-all
      "mrup" 'cljr-update-project-dependencies
      "mruw" 'cljr-unwind)))

(use-package clojure-mode
  :defer t
  :ensure t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
    (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode)))
  :config
  (progn

    (defun dotemacs-clojure-mode-toggle-default-indent-style ()
      (interactive)
      (setq clojure-defun-style-default-indent
            (if clojure-defun-style-default-indent nil t))
      (message "Clojure-mode default indent style: %s"
               (if clojure-defun-style-default-indent "ON" "OFF")))

    (dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
      (evil-leader/set-key-for-mode m
        "mTi" 'dotemacs-clojure-mode-toggle-default-indent-style))

    (when dotemacs-clojure-enable-fancify-symbols
      (dolist (m '(clojure-mode clojurescript-mode clojurec-mode clojurex-mode))
        (dotemacs-clojure-fancify-symbols m)))

      (define-clojure-indent
        ;; Compojure
        (ANY 2)
        (DELETE 2)
        (GET 2)
        (HEAD 2)
        (POST 2)
        (PUT 2)
        (context 2)
        (defroutes 'defun)
        ;; Cucumber
        (After 1)
        (Before 1)
        (Given 2)
        (Then 2)
        (When 2)
        ;; Schema
        (s/defrecord 2)
        ;; test.check
        (for-all 'defun))))

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
  :init
  (progn
    ;; (dotemacs//init-ocaml-opam)
    (evil-leader/set-key-for-mode 'tuareg-mode
      "mga" 'tuareg-find-alternate-file
      "mcc" 'compile)
    ;; Make OCaml-generated files invisible to filename completion
    (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt" ".annot"))
      (add-to-list 'completion-ignored-extensions ext)))
  :config
  (progn
    (when (fboundp 'sp-local-pair)
      ;; don't auto-close apostrophes (type 'a = foo) and backticks (`Foo)
      (sp-local-pair 'tuareg-mode "'" nil :actions nil)
      (sp-local-pair 'tuareg-mode "`" nil :actions nil))

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
  (dotemacs/add-flycheck-hook 'tuareg-mode))

(use-package flycheck-ocaml             ; Check OCaml code with Merlin
  :ensure t
  :commands (flycheck-ocaml-setup)
  :init (add-hook 'flycheck-mode-hook #'flycheck-ocaml-setup))

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

(with-eval-after-load 'flycheck
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
;   (dotemacs/add-flycheck-hook 'purescript-mode))

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

;;; React
(dotemacs-defvar-company-backends react-mode)

; todo: update eslint to local for react-mode
(defun dotemacs-react-mode-defaults ()
  "Default react-mode coding hook."
  (unless (bound-and-true-p my-react-mh-ran)
    (set (make-local-variable 'my-react-mh-ran) t)))

(setq dotemacs-react-mode-hook #'dotemacs-react-mode-defaults)
(add-hook 'react-mode-hook
          (lambda ()
            (run-hooks #'dotemacs-react-mode-hook)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook react-mode)))

  (dotemacs-use-package-add-hook company-tern
    :post-init
    (progn
      (push 'company-capf company-backends-react-mode))))

(dotemacs-use-package-add-hook flycheck
  :post-config
  (progn
    (dotemacs-flycheck-executables-search)
    (when (bound-and-true-p dotemacs//flycheck-executables-searched)
      (when dotemacs//flycheck-executable-eslint
        (flycheck-add-mode 'javascript-eslint 'react-mode)))))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (progn
    (dotemacs/add-flycheck-hook 'react-mode)
    (add-hook 'react-mode-hook 'dotemacs-flycheck-init-react)))

(dotemacs-use-package-add-hook js-doc
  :post-init
  (add-hook 'react-mode-hook 'dotemacs-js-doc-require)
  (dotemacs-js-doc-set-key-bindings 'react-mode))

(dotemacs-use-package-add-hook js2-mode
  :post-init
  (add-hook 'react-mode-hook 'js2-imenu-extras-mode)
  (add-hook 'react-mode-hook 'js2-minor-mode))

(dotemacs-use-package-add-hook js2-refactor
  :post-init
  (add-hook 'react-mode-hook 'dotemacs-js2-refactor-require)
  (dotemacs-js2-refactor-set-key-bindings 'react-mode))

(dotemacs-use-package-add-hook tern
  :post-init
  (add-hook 'react-mode-hook 'tern-mode))

(dotemacs-use-package-add-hook web-beautify
  :post-init
  (evil-leader/set-key-for-mode 'react-mode  "m=" 'web-beautify-js))

(dotemacs-use-package-add-hook web-mode
  :post-init
  (define-derived-mode react-mode web-mode "react")
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . react-mode))
  (add-to-list 'auto-mode-alist '("\\.react.js\\'" . react-mode))
  (add-to-list 'magic-mode-alist '("/** @jsx React.DOM */" . react-mode))
  (defun dotemacs//setup-react-mode ()
    "Adjust web-mode to accommodate react-mode"
    (emmet-mode 0)
    ;; See https://github.com/CestDiego/emmet-mode/commit/3f2904196e856d31b9c95794d2682c4c7365db23
    (setq-local emmet-expand-jsx-className? t)
    ;; Force jsx content type
    (web-mode-set-content-type "jsx")
    ;; Why do we do this ?
    (defadvice web-mode-highlight-part (around tweak-jsx activate)
      (let ((web-mode-enable-part-face nil))
        ad-do-it)))
  (add-hook 'react-mode-hook 'dotemacs//setup-react-mode))

;;; Elm
(dotemacs-defvar-company-backends elm-mode)

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook elm-mode)
      (add-hook 'elm-mode-hook 'elm-oracle-setup-completion))))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'elm-mode))

(use-package flycheck-elm
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook 'flycheck-elm-setup t))

(use-package elm-mode
  :mode ("\\.elm\\'" . elm-mode)
  :ensure t
  :init
  (progn
    (defun dotemacs/init-elm-mode ()
      "Disable electric-indent-mode and let indentation cycling feature work"
      (if (fboundp 'electric-indent-local-mode)
          (electric-indent-local-mode -1)))

    (add-hook 'elm-mode-hook 'dotemacs/init-elm-mode))
  :config
  (progn
    (push "\\*elm\\*" dotemacs-useful-buffers-regexp)

    (defun dotemacs/elm-compile-buffer-output ()
      (interactive)
      (let* ((fname (format "%s.js" (downcase (file-name-base (buffer-file-name))))))
        (elm-compile--file (elm--buffer-local-file-name) fname)))

    (defun dotemacs/push-decl-elm-repl-focus ()
      "Send current function to the REPL and focus it in insert state."
      (interactive)
      (push-decl-elm-repl)
      (run-elm-interactive)
      (evil-insert-state))

    (defun dotemacs/push-elm-repl-focus ()
      "Send current region to the REPL and focus it in insert state."
      (push-elm-repl)
      (run-elm-interactive)
      (evil-insert-state))

    (evil-leader/set-key-for-mode 'elm-mode
      ;; make
      "mcb" 'elm-compile-buffer
      "mcB" 'dotemacs/elm-compile-buffer-output
      "mcm" 'elm-compile-main

      ;; oracle
      "mht" 'elm-oracle-type-at-point

      ;; repl
      "msi" 'elm-repl-load
      "msf" 'push-decl-elm-repl
      "msF" 'dotemacs/push-decl-elm-repl-focus
      "msr" 'push-elm-repl
      "msR" 'dotemacs/push-elm-repl-focus

      ;; reactor
      "mRn" 'elm-preview-buffer
      "mRm" 'elm-preview-main

      ;; package
      "mpi" 'elm-import
      "mpc" 'elm-package-catalog
      "mpd" 'elm-documentation-lookup)

    (evilify elm-package-mode elm-package-mode-map
             "g" 'elm-package-refresh
             "n" 'elm-package-next
             "p" 'elm-package-prev
             "v" 'elm-package-view
             "m" 'elm-package-mark
             "u" 'elm-package-unmark
             "x" 'elm-package-install
             "q" 'quit-window)))

(dotemacs-use-package-add-hook popwin
  :post-config
  (push '("*elm*" :tail t :noselect t) popwin:special-display-config)
  (push '("*elm-make*" :tail t :noselect t) popwin:special-display-config))

(dotemacs-use-package-add-hook smartparens
  :post-init
   (if dotemacs-smartparens-strict-mode
      (add-hook 'elm-mode-hook #'smartparens-strict-mode)
    (add-hook 'elm-mode-hook #'smartparens-mode)))

;;; JavaScript
(dotemacs-defvar-company-backends js2-mode)

(setq javascript/key-binding-prefixes '(("mh" . "documentation")
                                        ("mg" . "goto")
                                        ("mr" . "refactor")))
(mapc (lambda (x) (dotemacs-declare-prefix-for-mode
                   'js2-mode (car x) (cdr x)))
      javascript/key-binding-prefixes)

(use-package init-js
  :load-path "config/"
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
    (with-eval-after-load 'flycheck
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

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook js2-mode))))

(use-package company-tern       ; JavaScript backend for Company
  :if (eq dotemacs-completion-engine 'company)
  :ensure t
  :defer t
  :init
  (progn
    (push 'company-tern company-backends-js2-mode)))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (progn
    (dolist (mode '(coffee-mode js2-mode json-mode))
      (dotemacs/add-flycheck-hook mode))
    (add-hook 'js2-mode-hook 'dotemacs-flycheck-init-javascript)))

(dotemacs-use-package-add-hook flycheck
  :post-config
  (progn
    (dotemacs-flycheck-executables-search)
    (with-eval-after-load 'projectile
      (progn
        (add-hook 'projectile-after-switch-project-hook
                  (lambda ()
                    (dotemacs-eslint-set-local-eslint-from-projectile)
                    (dotemacs-flycheck-executables-updated)))

        (dotemacs-eslint-set-local-eslint-from-projectile)
        (dotemacs-flycheck-executables-updated)))))

(use-package js-doc
  :defer t
  :ensure t
  :init
  (progn
    (defun dotemacs-js-doc-require ()
      "Lazy load js-doc"
      (require 'js-doc))
    (add-hook 'js2-mode-hook 'dotemacs-js-doc-require)

    (setq js-doc-mail-address "catesandrew@gmail.com"
          js-doc-author (format "Andrew Cates <%s>" js-doc-mail-address)
          js-doc-url "https://git.ibaset.com/u/andrew"
          js-doc-license "SEE LICENSE IN LICENSE.md"
          js-doc-parameter-line " * @param {} %p\n")

    (setq js-doc-file-doc-lines
            '(js-doc-top-line
              " * @file\n"
              " * @name %F\n"
              " * @author %a\n"
              " * @license %l\n"
              js-doc-bottom-line))

    (setq js-doc-all-tag-alist
          '(("abstract" . "This member must be implemented (or overridden) by the inheritor.")
            ("virtual" . "This member must be implemented (or overridden) by the inheritor.")
            ("access" . "Specify the access level of this member (private, public, or protected).")
            ("alias" . "Treat a member as if it had a different name.")
            ("augments" . "Indicate that a symbol inherits from, ands adds to, a parent symbol.")
            ("extends" . "Indicate that a symbol inherits from, ands adds to, a parent symbol.")
            ("author" . "Identify the author of an item.")
            ("borrows" . "This object uses something from another object.")
            ("callback" . "Document a callback function.")
            ("class" . "This function is intended to be called with the \"new\" keyword.")
            ("constructor" . "This function is intended to be called with the \"new\" keyword.")
            ("classdesc" . "Use the following text to describe the entire class.")
            ("constant" . "Document an object as a constant.")
            ("const" . "Document an object as a constant.")
            ("constructs" . "This function member will be the constructor for the previous class.")
            ("copyright" . "Document some copyright information.")
            ("default" . "Document the default value.")
            ("defaultvalue" . "Document the default value.")
            ("deprecated" . "Document that this is no longer the preferred way.")
            ("description" . "Describe a symbol.")
            ("desc" . "Describe a symbol.")
            ("enum" . "Document a collection of related properties.")
            ("event" . "Document an event.")
            ("example" . "Provide an example of how to use a documented item.")
            ("exports" . "Identify the member that is exported by a JavaScript module.")
            ("external" . "Identifies an external class, namespace, or module.")
            ("host" . "Identifies an external class, namespace, or module.")
            ("file" . "Describe a file.")
            ("fileoverview" . "Describe a file.")
            ("overview" . "Describe a file.")
            ("fires" . "Describe the events this method may fire.")
            ("emits" . "Describe the events this method may fire.")
            ("function" . "Describe a function or method.")
            ("func" . "Describe a function or method.")
            ("method" . "Describe a function or method.")
            ("global" . "Document a global object.")
            ("ignore" . "Omit a symbol from the documentation.")
            ("implements" . "This symbol implements an interface.")
            ("inheritdoc" . "Indicate that a symbol should inherit its parent's documentation.")
            ("inner" . "Document an inner object.")
            ("instance" . "Document an instance member.")
            ("interface" . "This symbol is an interface that others can implement.")
            ("kind" . "What kind of symbol is this?")
            ("lends" . "Document properties on an object literal as if they belonged to a symbol with a given name.")
            ("license" . "Identify the license that applies to this code.")
            ("listens" . "List the events that a symbol listens for.")
            ("member" . "Document a member.")
            ("var" . "Document a member.")
            ("memberof" . "This symbol belongs to a parent symbol.")
            ("mixes" . "This object mixes in all the members from another object.")
            ("mixin" . "Document a mixin object.")
            ("module" . "Document a JavaScript module.")
            ("name" . "Document the name of an object.")
            ("namespace" . "Document a namespace object.")
            ("override" . "Indicate that a symbol overrides its parent.")
            ("param" . "Document the parameter to a function.")
            ("arg" . "Document the parameter to a function.")
            ("private" . "This symbol is meant to be private.")
            ("property" . "Document a property of an object.")
            ("prop" . "Document a property of an object.")
            ("protected" . "This symbol is meant to be protected.")
            ("public" . "This symbol is meant to be public.")
            ("readonly" . "This symbol is meant to be read-only.")
            ("requires" . "This file requires a JavaScript module.")
            ("returns" . "Document the return value of a function.")
            ("return" . "Document the return value of a function.")
            ("see" . "Refer to some other documentation for more information.")
            ("since" . "When was this feature added?")
            ("static" . "Document a static member.")
            ("summary" . "A shorter version of the full description.")
            ("this" . "What does the 'this' keyword refer to here?")
            ("throws" . "Describe what errors could be thrown.")
            ("exception" . "Describe what errors could be thrown.")
            ("todo" . "Document tasks to be completed.")
            ("tutorial" . "Insert a link to an included tutorial file.")
            ("type" . "Document the type of an object.")
            ("typedef" . "Document a custom type.")
            ("variation" . "Distinguish different objects with the same name.")
            ("version" . "Documents the version number of an item.")))

    (defun dotemacs-js-doc-set-key-bindings (mode)
      "Setup the key bindings for `js2-doc' for the given MODE."
      (evil-leader/set-key-for-mode mode "mrdb" 'js-doc-insert-file-doc)
      (evil-leader/set-key-for-mode mode "mrdf" 'js-doc-insert-function-doc)
      (evil-leader/set-key-for-mode mode "mrdt" 'js-doc-insert-tag)
      (evil-leader/set-key-for-mode mode "mrdh" 'js-doc-describe-tag))
    (dotemacs-js-doc-set-key-bindings 'js2-mode)))

(use-package js2-mode                   ; Javascript editing
  :defer t
  :ensure t
  :init
  (progn
    ;; Let flycheck handle parse errors
    (setq js2-show-parse-errors nil
          js2-use-font-lock-faces t
          js2-strict-missing-semi-warning nil
          js2-highlight-external-variables nil
          js2-include-browser-externs t
          js2-include-node-externs t
          js2-missing-semi-one-line-override t
          js2-strict-cond-assign-warning nil
          js2-strict-inconsistent-return-warning nil
          js2-strict-trailing-comma-warning nil
          js2-strict-var-hides-function-arg-warning nil
          js2-strict-var-redeclaration-warning nil
          js2-warn-about-unused-function-arguments nil
          js2-strict-trailing-comma-warning nil)

    (add-to-list 'auto-mode-alist '("\\.jshintrc$" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.jscsrc$" . json-mode))
    (add-to-list 'auto-mode-alist '("\\.eslintrc$" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook #'js2-highlight-unused-variables-mode))
  :config
  (progn
    (setq-default js2-basic-offset 2)
    (setq-default js-indent-level 2)

    (setq js2-global-externs '("__dirname" "_" "describe" "it" "before" "after" "beforeEach" "afterEach" "chai" "sinon" "asyncTest" "ok" "equal" "notEqual" "deepEqual" "expect"))

    (dotemacs-declare-prefix-for-mode 'js2-mode "mz" "folding")
    (evil-leader/set-key-for-mode 'js2-mode "mw" 'js2-mode-toggle-warnings-and-errors)
    (evil-leader/set-key-for-mode 'js2-mode "mzc" 'js2-mode-hide-element)
    (evil-leader/set-key-for-mode 'js2-mode "mzo" 'js2-mode-show-element)
    (evil-leader/set-key-for-mode 'js2-mode "mzr" 'js2-mode-show-all)
    (evil-leader/set-key-for-mode 'js2-mode "mze" 'js2-mode-toggle-element)
    (evil-leader/set-key-for-mode 'js2-mode "mzF" 'js2-mode-toggle-hide-functions)
    (evil-leader/set-key-for-mode 'js2-mode "mzC" 'js2-mode-toggle-hide-comments)))

(use-package evil-matchit-js2
  :defer t
  :ensure evil-matchit
  :init (add-hook `js2-mode-hook `turn-on-evil-matchit-mode))

(use-package js2-refactor
  :defer t
  :ensure t
  :init
  (progn
    (defun javascript/load-js2-refactor ()
      "Lazy load js2-refactor"
      (require 'js2-refactor))
    (add-hook 'js2-mode-hook 'javascript/load-js2-refactor)

    (defun dotemacs-js2-refactor-set-key-bindings (mode)
      (dotemacs-declare-prefix-for-mode 'js2-mode "mr3" "ternary")
      (evil-leader/set-key-for-mode 'js2-mode "mr3i" 'js2r-ternary-to-if)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mra" "add/args")
      (evil-leader/set-key-for-mode 'js2-mode "mrag" 'js2r-add-to-globals-annotation)
      (evil-leader/set-key-for-mode 'js2-mode "mrao" 'js2r-arguments-to-object)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mrb" "barf")
      (evil-leader/set-key-for-mode 'js2-mode "mrba" 'js2r-forward-barf)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mrc" "contract")
      (evil-leader/set-key-for-mode 'js2-mode "mrca" 'js2r-contract-array)
      (evil-leader/set-key-for-mode 'js2-mode "mrco" 'js2r-contract-object)
      (evil-leader/set-key-for-mode 'js2-mode "mrcu" 'js2r-contract-function)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mre" "expand/extract")
      (evil-leader/set-key-for-mode 'js2-mode "mrea" 'js2r-expand-array)
      (evil-leader/set-key-for-mode 'js2-mode "mref" 'js2r-extract-function)
      (evil-leader/set-key-for-mode 'js2-mode "mrem" 'js2r-extract-method)
      (evil-leader/set-key-for-mode 'js2-mode "mreo" 'js2r-expand-object)
      (evil-leader/set-key-for-mode 'js2-mode "mreu" 'js2r-expand-function)
      (evil-leader/set-key-for-mode 'js2-mode "mrev" 'js2r-extract-var)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mri" "inline/inject/introduct")
      (evil-leader/set-key-for-mode 'js2-mode "mrig" 'js2r-inject-global-in-iife)
      (evil-leader/set-key-for-mode 'js2-mode "mrip" 'js2r-introduce-parameter)
      (evil-leader/set-key-for-mode 'js2-mode "mriv" 'js2r-inline-var)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mrl" "localize/log")
      (evil-leader/set-key-for-mode 'js2-mode "mrlp" 'js2r-localize-parameter)
      (evil-leader/set-key-for-mode 'js2-mode "mrlt" 'js2r-log-this)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mrr" "rename")
      (evil-leader/set-key-for-mode 'js2-mode "mrrv" 'js2r-rename-var)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mrs" "split/slurp")
      (evil-leader/set-key-for-mode 'js2-mode "mrsl" 'js2r-forward-slurp)
      (evil-leader/set-key-for-mode 'js2-mode "mrss" 'js2r-split-string)
      (evil-leader/set-key-for-mode 'js2-mode "mrsv" 'js2r-split-var-declaration)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mrt" "toggle")
      (evil-leader/set-key-for-mode 'js2-mode "mrtf" 'js2r-toggle-function-expression-and-declaration)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mru" "unwrap")
      (evil-leader/set-key-for-mode 'js2-mode "mruw" 'js2r-unwrap)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mrv" "var")
      (evil-leader/set-key-for-mode 'js2-mode "mrvt" 'js2r-var-to-this)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mrw" "wrap")
      (evil-leader/set-key-for-mode 'js2-mode "mrwi" 'js2r-wrap-buffer-in-iife)
      (evil-leader/set-key-for-mode 'js2-mode "mrwl" 'js2r-wrap-in-for-loop)

      (evil-leader/set-key-for-mode 'js2-mode "mk" 'js2r-kill)
      (evil-leader/set-key-for-mode 'js2-mode "xmj" 'js2r-move-line-down)
      (evil-leader/set-key-for-mode 'js2-mode "xmk" 'js2r-move-line-up))

    (dotemacs-js2-refactor-set-key-bindings 'js2-mode)))

(use-package json-mode                  ; JSON files
  :ensure t
  :defer t)

(use-package json-snatcher
  :defer t
  :ensure t
  :config
  (evil-leader/set-key-for-mode 'json-mode
    "mhp" 'jsons-print-path))

(use-package js2-imenu-extras
  :ensure js2-mode
  :defer t
  :init
  (progn
    (require 'js2-imenu-extras)
    (setq js2-imenu-enabled-frameworks 'nil)
    (js2-imenu-extras-mode)
    ;; required to make `<LEADER> s l' to work correctly
    (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)))

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

(use-package web-beautify
  :defer t
  :ensure t
  :init
  (progn
    (evil-leader/set-key-for-mode 'js2-mode  "m=" 'web-beautify-js)
    (evil-leader/set-key-for-mode 'json-mode "m=" 'web-beautify-js)
    (evil-leader/set-key-for-mode 'web-mode  "m=" 'web-beautify-html)
    (evil-leader/set-key-for-mode 'css-mode  "m=" 'web-beautify-css)))


;;; Web languages
(dotemacs-defvar-company-backends css-mode)
(dotemacs-defvar-company-backends web-mode)
(dotemacs-defvar-company-backends jade-mode)
(dotemacs-defvar-company-backends slim-mode)

(use-package init-web
  :load-path "config/")

;; Thanks to [[https://github.com/skeeto/impatient-mode][impatient-mode]] you
;; can see the effect of your HTML as you type it.
(use-package impatient-mode
  :ensure t
  :defer t
  :init
  (progn
    (defun dotemacs-impatient-mode-hook()
      "my web mode hook for HTML REPL"
      (interactive)
      (impatient-mode)
      (httpd-start))
    (dotemacs|do-after-display-system-init
     (add-hook 'web-mode-hook 'dotemacs-impatient-mode-hook))))

(use-package css-mode
  :defer t
  :ensure t
  :init
  (progn
    ;; Mark `css-indent-offset' as safe-local variable
    (put 'css-indent-offset 'safe-local-variable #'integerp)

    (defun css-expand-statement ()
      "Expand CSS block"
      (interactive)
      (save-excursion
        (end-of-line)
        (search-backward "{")
        (forward-char 1)
        (while (or (eobp) (not (looking-at "}")))
        (let ((beg (point)))
          (newline)
          (search-forward ";")
          (indent-region beg (point))
          ))
        (newline)))

    (defun css-contract-statement ()
      "Contract CSS block"
      (interactive)
      (end-of-line)
      (search-backward "{")
      (while (not (looking-at "}"))
        (join-line -1)))

    (evil-leader/set-key-for-mode 'css-mode
      "mzc" 'css-contract-statement
      "mzo" 'css-expand-statement))
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
    (dotemacs/add-to-hooks 'dotemacs-load-yasnippet '(css-mode-hook
                                                      jade-mode-hook
                                                      slim-mode-hook))))

(use-package css-eldoc                  ; Basic Eldoc for CSS
  :ensure t
  :commands (turn-on-css-eldoc)
  :init (add-hook 'css-mode-hook #'turn-on-css-eldoc))

(use-package evil-matchit-web
  :defer t
  :ensure evil-matchit
  :init (add-hook `web-mode-hook `turn-on-evil-matchit-mode))

(use-package helm-css-scss
  :defer t
  :ensure t
  :init
  (dolist (mode '(css-mode scss-mode))
    (evil-leader/set-key-for-mode mode "mgh" 'helm-css-scss)))

(dotemacs-use-package-add-hook rainbow-delimiters
  :post-init
  (progn
    (dotemacs/add-to-hooks 'rainbow-delimiters-mode '(jade-mode-hook
                                                      less-css-mode-hook
                                                      scss-mode-hook
                                                      slim-mode-hook))))

(use-package web-mode                   ; Template editing
  :defer t
  :ensure t
  :config
  (progn
    ;; Only use smartparens in web-mode
    (setq web-mode-enable-auto-pairing nil
          web-mode-markup-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-sql-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-attr-indent-offset 2
          web-mode-style-padding 2
          web-mode-script-padding 2)

    (add-hook 'web-mode-hook
      (lambda ()
        (run-hooks #'dotemacs-prog-mode-hook)
        ; todo: verify this is not needed now
        ; (when (equal web-mode-content-type "jsx")
        ;   (setq-local cursor-type nil)
        ;   (with-eval-after-load 'flycheck
        ;     (flycheck-select-checker 'javascript-eslint)))
        ))

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
      ("gj" web-mode-element-sibling-next)
      ("k" web-mode-element-previous)
      ("K" web-mode-element-sibling-previous)
      ("gk" web-mode-element-sibling-previous)
      ("h" web-mode-element-parent)
      ("l" web-mode-element-child)
      ("p" web-mode-dom-xpath)
      ("r" web-mode-element-rename :exit t)
      ("q" nil :exit t)
      ("w" web-mode-element-wrap))

  ; todo: verify these settigns work: And if you want to have 2 space indent
  ; also for element's attributes, concatenations and contiguous function
  ; calls:
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

  :mode
  (("\\.phtml\\'"      . web-mode)
   ("\\.tpl\\.php\\'"  . web-mode)
   ("\\.twig\\'"       . web-mode)
   ("\\.html\\'"       . web-mode)
   ("\\.htm\\'"        . web-mode)
   ("\\.[gj]sp\\'"     . web-mode)
   ("\\.as[cp]x\\'"    . web-mode)
   ("\\.eex\\'"        . web-mode)
   ("\\.erb\\'"        . web-mode)
   ("\\.eco\\'"        . web-mode)
   ("\\.ejs\\'"        . web-mode)
   ("\\.djhtml\\'"     . web-mode)))

(dotemacs-use-package-add-hook flycheck
  :post-config
  (progn
    (dotemacs-flycheck-executables-search)
    (when (bound-and-true-p dotemacs//flycheck-executables-searched)
      (when dotemacs//flycheck-executable-tidy5
        (flycheck-add-mode 'html-tidy 'web-mode)))))

(use-package emmet-mode
  :defer t
  :ensure t
  :init
  (progn
    (setq emmet-indentation 2
          emmet-move-cursor-between-quotes t)
    (dotemacs/add-to-hooks 'emmet-mode '(css-mode-hook
                                         html-mode-hook
                                         web-mode-hook)))
  :config
  (progn
    (evil-define-key 'insert emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
    (evil-define-key 'insert emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
    (evil-define-key 'emacs emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
    (evil-define-key 'emacs emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
    (evil-define-key 'hybrid emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
    (evil-define-key 'hybrid emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
    (dotemacs-hide-lighter emmet-mode)))

(use-package jade-mode
  :defer t
  :ensure t)

(use-package slim-mode
  :ensure t
  :defer t)

(use-package scss-mode
  :defer t
  :ensure t
  :mode ("\\.scss\\'" . scss-mode))

(use-package sass-mode
  :ensure t
  :defer t
  :mode ("\\.sass\\'" . sass-mode))

(use-package less-css-mode
  :defer t
  :ensure t
  :mode ("\\.less\\'" . less-css-mode))

(dotemacs-use-package-add-hook smartparens
  :post-init
  (progn
    (dotemacs/add-to-hooks
     (if dotemacs-smartparens-strict-mode
         'smartparens-strict-mode
       'smartparens-mode)
     '(prog-mode-hook css-mode-hook scss-mode-hook sass-mode-hook less-css-mode-hook)))
  :post-config
  (sp-with-modes '(web-mode)
    (sp-local-pair "<% " " %>")
    (sp-local-pair "{ " " }")
    (sp-local-pair "<%= "  " %>")
    (sp-local-pair "<%# "  " %>")
    (sp-local-pair "<%$ "  " %>")
    (sp-local-pair "<%@ "  " %>")
    (sp-local-pair "<%: "  " %>")
    (sp-local-pair "{{ "  " }}")
    (sp-local-pair "{% "  " %}")
    (sp-local-pair "{%- "  " %}")
    (sp-local-pair "{# "  " #}")))

(use-package tagedit
  :defer t
  :ensure t
  :config
  (progn
    (tagedit-add-experimental-features)
    (dotemacs-diminish tagedit-mode " Ⓣ" " T")
    (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dolist (mode '(jade-mode less-mode slim-mode sass-mode css-mode scss-mode web-mode))
    (dotemacs/add-flycheck-hook mode)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook css-mode)
      (dotemacs-add-company-hook web-mode)
      (dotemacs-add-company-hook jade-mode)
      (dotemacs-add-company-hook slim-mode))))

(use-package company-web
  :if (eq dotemacs-completion-engine 'company)
  :ensure t
  :defer t
  :init
  (progn
    (push 'company-css company-backends-css-mode)
    (push 'company-web-slim company-backends-slim-mode)
    (push 'company-web-jade company-backends-jade-mode)
    (push '(company-web-html company-css) company-backends-web-mode)))


;;; Lua
(dotemacs-defvar-company-backends lua-mode)

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'lua-mode))

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
  :defer t
  :quelpa (php-extras :fetcher github :repo "arnested/php-extras"))

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
  (dotemacs/add-flycheck-hook 'php-mode))

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

; This lets you send HTML, CSS, and Javascript fragments to Google Chrome. You
; may need to start Chrome with `--allow-running-insecure-content`, if
; you're using the user script with HTTPS sites.
(use-package skewer-mode
  :ensure t
  :defer t
  :init (with-eval-after-load 'js2-mode
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
  ;; company-quickhelp calls it. Note hook is appended for proper ordering.
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
      "msB" 'dotemacs-racket-run-and-switch-to-repl
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
  (dotemacs/add-flycheck-hook 'racket-mode))


;;; Java
(dotemacs-defvar-company-backends java-mode)

;; Command prefixes
(setq java/key-binding-prefixes '(("me" . "errors")
                                  ("mf" . "find")
                                  ("mg" . "goto")
                                  ("mr" . "refactor")
                                  ("mh" . "documentation")
                                  ("mm" . "maven")
                                  ("ma" . "ant")
                                  ("mp" . "project")
                                  ("mt" . "test")))

(mapc (lambda(x) (dotemacs-declare-prefix-for-mode
                  'java-mode (car x) (cdr x)))
      java/key-binding-prefixes)

(use-package init-java
  :load-path "config/")

(use-package eclim
  :defer t
  :ensure emacs-eclim
  :diminish eclim-mode
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
      (kbd ":") 'dotemacs-java-completing-double-colon
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
  :config (with-eval-after-load 'flycheck
            (add-to-list 'flycheck-checkers 'swift)))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dolist (mode '(swift-mode sh-mode))
    (dotemacs/add-flycheck-hook mode)))


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


;;; gtags

(defun helm-gtags-dwim-other-window ()
  "helm-gtags-dwim in the other window"
  (interactive)
  (let ((helm-gtags--use-otherwin t)
        (split-height-threshold nil)
        (split-width-threshold 140))
    (helm-gtags-dwim)))

(defun dotemacs-helm-gtags-define-keys-for-mode (mode)
  "Define key bindings for the specific MODE."
  (when (fboundp mode)
    (let ((hook (intern (concat (symbol-name mode) "-hook"))))
      (add-hook hook 'helm-gtags-mode))
    (evil-leader/set-key-for-mode mode
      "mgc" 'helm-gtags-create-tags
      "mgd" 'helm-gtags-find-tag
      "mgf" 'helm-gtags-select-path
      "mgg" 'helm-gtags-dwim
      "mgG" 'helm-gtags-dwim-other-window
      "mgi" 'helm-gtags-tags-in-this-function
      "mgl" 'helm-gtags-parse-file
      "mgn" 'helm-gtags-next-history
      "mgp" 'helm-gtags-previous-history
      "mgr" 'helm-gtags-find-rtag
      "mgR" 'helm-gtags-resume
      "mgs" 'helm-gtags-select
      "mgS" 'helm-gtags-show-stack
      "mgu" 'helm-gtags-update-tags)))

(defun dotemacs-ggtags-enable-eldoc (mode)
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda ()
              (ggtags-mode 1)
              (eldoc-mode 1)
              (setq-local eldoc-documentation-function
                          #'ggtags-eldoc-function))))

(use-package ggtags
  :ensure t
  :defer t)

(use-package helm-gtags
  :defer t
  :ensure t
  :init
  (progn
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t)
    ;; modes that do not have a layer, define here
    (dotemacs-helm-gtags-define-keys-for-mode 'tcl-mode)
    (dotemacs-helm-gtags-define-keys-for-mode 'java-mode)
    (dotemacs-helm-gtags-define-keys-for-mode 'vhdl-mode)
    (dotemacs-helm-gtags-define-keys-for-mode 'shell-script-mode)
    (dotemacs-helm-gtags-define-keys-for-mode 'awk-mode)
    (dotemacs-helm-gtags-define-keys-for-mode 'asm-mode)
    (dotemacs-helm-gtags-define-keys-for-mode 'dired-mode)
    (dotemacs-helm-gtags-define-keys-for-mode 'compilation-mode)
    (dotemacs-helm-gtags-define-keys-for-mode 'shell-mode)

    (dotemacs-ggtags-enable-eldoc 'tcl-mode)
    (dotemacs-ggtags-enable-eldoc 'java-mode)
    (dotemacs-ggtags-enable-eldoc 'vhdl-mode))
  :config
  (progn
    ;; if anyone uses helm-gtags, they would want to use these key bindings
    (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
    (define-key helm-gtags-mode-map (kbd "C-x 4 .") 'helm-gtags-find-tag-other-window)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
    (define-key helm-gtags-mode-map (kbd "M-*") 'helm-gtags-pop-stack)))


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

(dotemacs-declare-prefix "gd" "diff")
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
    (dotemacs|do-after-display-system-init
      (unless (display-graphic-p)
        (setq diff-hl-side 'left)
        (diff-hl-margin-mode)))

    (evil-leader/set-key
      "gdg" 'diff-hl-diff-goto-hunk
      "gdn" 'diff-hl-next-hunk
      "gdN" 'diff-hl-previous-hunk
      "gdr" 'diff-hl-revert-hunk)))

;; git
(use-package git-commit                 ; Git commit message mode
  :ensure t
  :defer t)

(use-package git-messenger
  :ensure t
  :defer t
  :init
  (evil-leader/set-key
    "gm" 'git-messenger:popup-message)
  :config
  (define-key git-messenger-map [escape] 'git-messenger:popup-close))

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
    (evilify git-rebase-mode git-rebase-mode-map
            "J" 'git-rebase-move-line-down
            "K" 'git-rebase-move-line-up
            "u" 'git-rebase-undo
            "y" 'git-rebase-insert)

    (evil-leader/set-key-for-mode 'git-rebase-mode
      "mcc" 'git-rebase-server-edit
      "mk" 'git-rebase-abort)))

(use-package git-timemachine            ; Go back in Git time
  :ensure t
  :defer t
  :commands dotemacs-time-machine-micro-state
  :init
  (evil-leader/set-key
    "gt" 'dotemacs-time-machine-micro-state)
  :config
  (progn
    (dotemacs-define-micro-state time-machine
      :doc "[p] [N] previous [n] next [c] current [Y] copy hash [q] quit"
      :on-enter (let (golden-ratio-mode)
                  (unless (bound-and-true-p git-timemachine-mode)
                    (call-interactively 'git-timemachine)))
      :on-exit (when (bound-and-true-p git-timemachine-mode)
                 (git-timemachine-quit))
      :persistent t
      :bindings
      ("c" git-timemachine-show-current-revision)
      ("p" git-timemachine-show-previous-revision)
      ("n" git-timemachine-show-next-revision)
      ("N" git-timemachine-show-previous-revision)
      ("Y" git-timemachine-kill-revision)
      ("q" nil :exit t))))

;; Github
(setq github/key-binding-prefixes '(("gh" . "github")
                                    ("gg" . "github gist")))
(mapc (lambda (x) (dotemacs-declare-prefix (car x) (cdr x)))
      github/key-binding-prefixes)

(use-package gist
  :defer t
  :ensure t
  :init
  (progn
    (evilify gist-list-mode gist-list-menu-mode-map
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
    "gho" 'github-browse-file))

(use-package github-clone
  :defer t
  :ensure t
  :init
  (evil-leader/set-key
   "gh C-c" 'github-clone))

(use-package git-link
  :defer t
  :ensure t
  :init
  (progn
    (evil-leader/set-key
      "ghl" 'git-link
      "ghL" 'dotemacs-git-link-copy-url-only
      "ghc" 'git-link-commit
      "ghC" 'dotemacs-git-link-commit-copy-url-only)

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

;; magit-gh-pulls has to be loaded via a pre-config hook because the source
;; code makes assumptions about the status of the magit-mode keymaps that are
;; incompatible with the dotemacs' evilification feature. To avoid errors,
;; magit-gh-pulls must be loaded after magit, but before magit is configured by
;; dotemacs.

(dotemacs-use-package-add-hook magit
  :pre-config
  (progn
    (use-package magit-gh-pulls
      :ensure t
      :init
      (progn
        (define-key magit-mode-map "#" 'dotemacs-load-gh-pulls-mode))
      :config
      (dotemacs-diminish magit-gh-pulls-mode "Github-PR"))))

;; magit
(use-package init-magit
  :load-path "config/")

;; gravatars from magit use this to store their cache
(setq url-configuration-directory (concat dotemacs-cache-directory "url/"))

(use-package magit                      ; The one and only Git frontend
  :ensure t
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

    (with-eval-after-load 'projectile
      (dotemacs-magit-set-repo-dirs-from-projectile))

    (evil-leader/set-key
      "gb" 'dotemacs-git-blame-micro-state
      "gc" 'magit-commit
      "gC" 'magit-checkout
      "gdh" 'dotemacs-magit-diff-head
      "gi" 'magit-init
      "gl" 'magit-log-all
      "gL" 'magit-log-buffer-file
      "gs" 'magit-status)

    (dotemacs-define-micro-state git-blame
      :doc (concat "Press [b] again to blame further in the history, "
                   "[q] to go up or quit.")
      :on-enter (let (golden-ratio-mode)
                  (unless (bound-and-true-p magit-blame-mode)
                    (call-interactively 'magit-blame)))
      :persistent t
      :bindings
      ("b" magit-blame)
      ;; here we use the :exit keyword because we should exit the
      ;; micro-state only if the magit-blame-quit effectively disable
      ;; the magit-blame mode.
      ("q" nil :exit (progn (when (bound-and-true-p magit-blame-mode)
                              (magit-blame-quit))
                            (not (bound-and-true-p magit-blame-mode))))))
  :config
  (progn
    ;; seems to be necessary at the time of release
    (require 'git-rebase)

    (unless (boundp 'dotemacs-use-evil-magit)
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
      (dotemacs-evilify-map magit-hunk-section-map
        :mode magit-status-mode
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
      (dotemacs-evilify-map magit-stash-mode-map
        :mode magit-stash-mode
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
      (dotemacs-evilify-map magit-stash-section-map))

    (add-hook 'projectile-after-switch-project-hook
      #'dotemacs-magit-set-repo-dirs-from-projectile)

    ;; full screen magit-status
    (when dotemacs-git-magit-status-fullscreen
      (setq magit-display-buffer-function
            (lambda (buffer)
              (if (or
                   ;; the original should stay alive, so we can't go fullscreen
                   magit-display-buffer-noselect
                   ;; don't go fullscreen for certain magit buffers if current
                   ;; buffer is a magit buffer (we're conforming to
                   ;; `magit-display-buffer-traditional')
                   (and (derived-mode-p 'magit-mode)
                        (not (memq (with-current-buffer buffer major-mode)
                                   '(magit-process-mode
                                     magit-revision-mode
                                     magit-diff-mode
                                     magit-stash-mode
                                     magit-status-mode)))))
                  ;; open buffer according to original magit rules
                  (magit-display-buffer-traditional buffer)
                ;; open buffer in fullscreen
                (delete-other-windows)
                ;; make sure the window isn't dedicated, otherwise
                ;; `set-window-buffer' throws an error
                (set-window-dedicated-p nil nil)
                (set-window-buffer nil buffer)
                ;; return buffer's window
                (get-buffer-window buffer)))))

    (when dotemacs-major-mode-leader-key
      (add-hook 'with-editor-mode-hook 'evil-normalize-keymaps)
      (evil-define-key 'normal with-editor-mode-map
        (concat dotemacs-major-mode-leader-key "c") 'with-editor-finish
        (concat dotemacs-major-mode-leader-key "a") 'with-editor-cancel)
      (evil-define-key 'motion with-editor-mode-map
        (concat dotemacs-major-mode-leader-key "c") 'with-editor-finish
        (concat dotemacs-major-mode-leader-key "a") 'with-editor-cancel))

    (define-key magit-status-mode-map (kbd "C-S-w") 'magit-toggle-whitespace)))

(use-package evil-magit
  :defer t
  :ensure t
  :init
  (progn
    (defconst dotemacs-use-evil-magit t
      "This variable is only defined if evil-magit is used.")

    (with-eval-after-load 'magit
      (require 'evil-magit)
      ;; (evil-define-key 'motion magit-mode-map
      ;;   (kbd dotemacs-leader-key) dotemacs-default-map)
      )))

(use-package magit-gitflow
  :ensure t
  :commands turn-on-magit-gitflow
  :init
  (progn
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
      (with-eval-after-load 'magit
        (define-key magit-mode-map "%" 'magit-gitflow-popup)))
  :config (dotemacs-diminish magit-gitflow-mode "Flow"))

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


;;; Search
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
  :defer t
  :init
  (progn
    ;; This overrides the default C-s action in helm-projectile-switch-project
    ;; to search using ag/pt/whatever instead of just grep
    (with-eval-after-load 'helm-projectile
      (defun dotemacs-helm-project-smart-do-search-in-dir (dir)
        (interactive)
        (let ((default-directory dir))
          (dotemacs-helm-project-smart-do-search)))
      (define-key helm-projectile-projects-map
        (kbd "C-s")
        (lambda ()
          (interactive)
          (helm-exit-and-execute-action 'dotemacs-helm-project-smart-do-search-in-dir))))

    ;; evilify the helm-grep buffer
    (evilify helm-grep-mode helm-grep-mode-map
             (kbd "RET") 'helm-grep-mode-jump-other-window
             (kbd "q") 'quit-window)

    (evil-leader/set-key
      ;; helm-ag marks
      "s`"  'helm-ag-pop-stack
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
      ;; "/"   'dotemacs-helm-project-smart-do-search
      ;; "*"   'dotemacs-helm-project-smart-do-search-region-or-symbol
      "sp"  'dotemacs-helm-project-smart-do-search
      "sP"  'dotemacs-helm-project-smart-do-search-region-or-symbol
      "sap" 'dotemacs-helm-project-do-ag
      "saP" 'dotemacs-helm-project-do-ag-region-or-symbol
      "skp" 'dotemacs-helm-project-do-ack
      "skP" 'dotemacs-helm-project-do-ack-region-or-symbol
      "stp" 'dotemacs-helm-project-do-pt
      "stP" 'dotemacs-helm-project-do-pt-region-or-symbol))
  :config
  (progn
    ;; Use `grep-find-ignored-files' and `grep-find-ignored-directories' as
    ;; ignore pattern, but does not seem to be working, need to confirm
    (setq helm-ag-use-grep-ignore-list t)

    ;; example: (helm-ag-ignore-patterns '("*.md" "*.el"))
    ;; (setq helm-ag-ignore-patterns '(append grep-find-ignored-files
    ;;                                        grep-find-ignored-directories))

    (setq helm-ag-fuzzy-match t
          helm-ag-base-command "ag --nocolor --nogroup --hidden"
          helm-ag-insert-at-point 'symbol
          helm-ag-source-type 'file-line))

    (evil-define-key 'normal helm-ag-map (kbd evil-leader/leader) evil-leader--default-map)
    (evilify helm-ag-mode helm-ag-mode-map
             (kbd "RET") 'helm-ag-mode-jump-other-window
             (kbd "q") 'quit-window))


;;; Project management for Interactively Do Things (IDO)

(use-package ido
  :init
  (progn
    (setq ido-enable-flex-matching t ;; enable fuzzy matching
          ido-use-faces nil       ;; disable ido faces to see flx highlights.
          ido-enable-prefix nil
          ido-create-new-buffer 'always
          ido-use-filename-at-point 'guess
          ido-save-directory-list-file (concat dotemacs-cache-directory "ido.last")
          ido-default-file-method 'selected-window
          ido-auto-merge-work-directories-length 0))
  :config
  (progn
    (ido-mode t)))

(dotemacs-evilify-map package-menu-mode-map
                       :mode package-menu-mode)

(use-package ido-vertical-mode
  :ensure t
  :init
  (progn
    (ido-vertical-mode t)
    (defun dotemacs//ido-minibuffer-setup ()
      "Setup the minibuffer."
      ;; Since ido is implemented in a while loop where each
      ;; iteration setup a whole new minibuffer, we have to keep
      ;; track of any activated ido navigation micro-state and force
      ;; the reactivation at each iteration.
      (when dotemacs--ido-navigation-ms-enabled
        (dotemacs-ido-navigation-micro-state)))
    (add-hook 'ido-minibuffer-setup-hook 'dotemacs//ido-minibuffer-setup)

    (defun dotemacs//ido-setup ()
      (when dotemacs--ido-navigation-ms-face-cookie-minibuffer
        (face-remap-remove-relative
         dotemacs--ido-navigation-ms-face-cookie-minibuffer))
      ;; be sure to wipe any previous micro-state flag
      (setq dotemacs--ido-navigation-ms-enabled nil)
      ;; overwrite the key bindings for ido vertical mode only
      (define-key ido-completion-map (kbd "C-<return>") 'ido-select-text)
      ;; use M-RET in terminal
      (define-key ido-completion-map "\M-\r" 'ido-select-text)
      (define-key ido-completion-map (kbd "C-h") 'ido-delete-backward-updir)
      (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
      (define-key ido-completion-map (kbd "C-k") 'ido-prev-match)
      (define-key ido-completion-map (kbd "C-l") 'ido-exit-minibuffer)
      (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
      (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
      (define-key ido-completion-map (kbd "C-S-h") 'ido-prev-match-dir)
      (define-key ido-completion-map (kbd "C-S-j") 'next-history-element)
      (define-key ido-completion-map (kbd "C-S-k") 'previous-history-element)
      (define-key ido-completion-map (kbd "C-S-l") 'ido-next-match-dir)
      (define-key ido-completion-map (kbd "C-S-n") 'next-history-element)
      (define-key ido-completion-map (kbd "C-S-p") 'previous-history-element)
      ;; ido-other window maps
      (define-key ido-completion-map (kbd "C-o") 'dotemacs/ido-invoke-in-other-window)
      (define-key ido-completion-map (kbd "C-s") 'dotemacs/ido-invoke-in-vertical-split)
      (define-key ido-completion-map (kbd "C-t") 'dotemacs/ido-invoke-in-new-frame)
      (define-key ido-completion-map (kbd "C-v") 'dotemacs/ido-invoke-in-horizontal-split)
      ;; more natural navigation keys: up, down to change current item
      ;; left to go up dir
      ;; right to open the selected item
      (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
      (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
      (define-key ido-completion-map (kbd "<left>") 'ido-delete-backward-updir)
      (define-key ido-completion-map (kbd "<right>") 'ido-exit-minibuffer)
      ;; initiate micro-state
      (define-key ido-completion-map (kbd "M-SPC") 'dotemacs-ido-navigation-micro-state)
      (define-key ido-completion-map (kbd "s-M-SPC") 'dotemacs-ido-navigation-micro-state)
      )
    (add-hook 'ido-setup-hook 'dotemacs//ido-setup)

    (defun dotemacs/ido-invoke-in-other-window ()
      "signals ido mode to switch to (or create) another window after exiting"
      (interactive)
      (setq ido-exit-minibuffer-target-window 'other)
      (ido-exit-minibuffer))

    (defun dotemacs/ido-invoke-in-horizontal-split ()
      "signals ido mode to split horizontally and switch after exiting"
      (interactive)
      (setq ido-exit-minibuffer-target-window 'horizontal)
      (ido-exit-minibuffer))

    (defun dotemacs/ido-invoke-in-vertical-split ()
      "signals ido mode to split vertically and switch after exiting"
      (interactive)
      (setq ido-exit-minibuffer-target-window 'vertical)
      (ido-exit-minibuffer))

    (defun dotemacs/ido-invoke-in-new-frame ()
      "signals ido mode to create a new frame after exiting"
      (interactive)
      (setq ido-exit-minibuffer-target-window 'frame)
      (ido-exit-minibuffer))

    (defadvice ido-read-internal
        (around ido-read-internal-with-minibuffer-other-window activate)
      (let* (ido-exit-minibuffer-target-window
             (this-buffer (current-buffer))
             (result ad-do-it))
        (cond
         ((equal ido-exit-minibuffer-target-window 'other)
          (if (= 1 (count-windows))
              (dotemacs/split-window-horizontally-and-switch)
            (other-window 1)))
         ((equal ido-exit-minibuffer-target-window 'horizontal)
          (dotemacs/split-window-horizontally-and-switch))

         ((equal ido-exit-minibuffer-target-window 'vertical)
          (dotemacs/split-window-vertically-and-switch))
         ((equal ido-exit-minibuffer-target-window 'frame)
          (make-frame)))
        ;; why? Some ido commands, such as textmate.el's
        ;; textmate-goto-symbol don't switch the current buffer
        (switch-to-buffer this-buffer)
        result))

    (defvar dotemacs--ido-navigation-ms-enabled nil
      "Flag which is non nil when ido navigation micro-state is enabled.")

    (defvar dotemacs--ido-navigation-ms-face-cookie-minibuffer nil
      "Cookie pointing to the local face remapping.")

    (defface dotemacs-ido-navigation-ms-face
      `((t :background ,(face-attribute 'error :foreground)
           :foreground "black"
           :weight bold))
      "Face for ido minibuffer prompt when ido micro-state is activated."
      :group 'dotemacs)

    (defun dotemacs//ido-navigation-ms-set-face ()
      "Set faces for ido navigation micro-state."
      (setq dotemacs--ido-navigation-ms-face-cookie-minibuffer
            (face-remap-add-relative
             'minibuffer-prompt
             'dotemacs-ido-navigation-ms-face)))

    (defun dotemacs//ido-navigation-ms-on-enter ()
      "Initialization of ido micro-state."
    (setq dotemacs--ido-navigation-ms-enabled t)
      (dotemacs//ido-navigation-ms-set-face))

    (defun dotemacs//ido-navigation-ms-on-exit ()
      "Action to perform when exiting ido micro-state."
      (face-remap-remove-relative
       dotemacs--ido-navigation-ms-face-cookie-minibuffer))

    (defun dotemacs//ido-navigation-ms-full-doc ()
      "Full documentation for ido navigation micro-state."
      "
[?]          display this help
[e]          enter dired
[j] [k]      next/previous match
[J] [K]      sub/parent directory
[h]          delete backward or parent directory
[l]          select match
[n] [p]      next/previous directory in history
[o]          open in other window
[s]          open in a new horizontal split
[t]          open in other frame
[v]          open in a new vertical split
[q]          quit")

    (dotemacs-define-micro-state ido-navigation
      :persistent t
      :disable-evil-leader t
      :on-enter (dotemacs//ido-navigation-ms-on-enter)
      :on-exit  (dotemacs//ido-navigation-ms-on-exit)
      :bindings
      ("?" nil :doc (dotemacs//ido-navigation-ms-full-doc))
      ("<RET>" ido-exit-minibuffer :exit t)
      ("<escape>" nil :exit t)
      ("e" ido-select-text :exit t)
      ("h" ido-delete-backward-updir)
      ("j" ido-next-match)
      ("J" ido-next-match-dir)
      ("k" ido-prev-match)
      ("K" ido-prev-match-dir)
      ("l" ido-exit-minibuffer :exit t)
      ("n" ido-next-match-dir)
      ("o" dotemacs/ido-invoke-in-other-window :exit t)
      ("p" ido-prev-match-dir)
      ("q" nil :exit t)
      ("s" dotemacs/ido-invoke-in-vertical-split :exit t)
      ("t" dotemacs/ido-invoke-in-new-frame :exit t)
      ("v" dotemacs/ido-invoke-in-horizontal-split :exit t))))

(use-package flx-ido
  :ensure t
  :init (flx-ido-mode 1))


;;; Project management with Projectile

(use-package projectile
  :ensure t
  :defer 1.4
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
    (defun projectile-find-file-ignored ()
      "Projectile find file without ignore."
      (interactive)
      (let ((projectile-git-command "git ls-files -zco"))
        (call-interactively 'projectile-find-file)))

    (defun projectile-add-magit-repo-dirs-to-known-projects ()
      "Add `magit-repo-dirs' to `projectile-known-projects'."
      (interactive)
      (--each (mapcar 'cdr (magit-list-repos magit-repo-dirs))
        (projectile-add-known-project (file-name-as-directory
                                       (file-truename it)))))

    ;; note for Windows: GNU find or Cygwin find must be in path to enable
    ;; fast indexing
    (when (and (dotemacs/system-is-mswindows) (executable-find "find"))
      (setq projectile-indexing-method 'alien)
            projectile-generic-command "find . -type f")
    (setq projectile-sort-order 'recentf
          projectile-switch-project-action 'projectile-dired
          projectile-cache-file (concat dotemacs-cache-directory
                                        "projectile.cache")
          projectile-known-projects-file (concat dotemacs-cache-directory
                                                 "projectile-bookmarks.eld")
          projectile-find-dir-includes-top-level t
          projectile-require-project-root t
          projectile-verbose nil)

    ;; (setq projectile-enable-caching nil)
    ;; (defadvice projectile-mode (before maybe-use-cache activate)
    ;;   (when
    ;;     (--any? (and it (file-remote-p it))
    ;;             (list
    ;;               (buffer-file-name)
    ;;                list-buffers-directory
    ;;                default-directory))
    ;;     (setq-local projectile-enable-caching t)))

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

    (setq projectile-globally-ignored-file-suffixes
          '("class"
            "elc"))

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
      "pa" 'projectile-toggle-between-implementation-and-test
      "pc" 'projectile-compile-project
      "pD" 'projectile-dired
      "pG" 'projectile-regenerate-tags
      "pI" 'projectile-invalidate-cache
      "pk" 'projectile-kill-buffers
      "po" 'projectile-multi-occur
      "pR" 'projectile-replace
      "pT" 'projectile-find-test-file
      "py" 'projectile-find-tag))
  :config
  (progn
    ; (add-hook 'projectile-after-switch-project-hook 'pyenv-mode-set-local-version)
    (defun dotemacs//projectile-switch-project-by-name (fcn project-to-switch &optional args)
      (apply fcn project-to-switch args)
      (when (projectile-project-p)
        (message "Switching to project: %s" (projectile-project-root))
        (when (fboundp 'neotree-dir)
          (if (neo-global--window-exists-p)
              (neotree-dir (projectile-project-root))
            (progn
              (neotree-dir (projectile-project-root))
              (neotree-hide)
              (let ((origin-buffer-file-name (buffer-file-name)))
                (neotree-find (projectile-project-root))
                (neotree-find origin-buffer-file-name))
              (neotree-hide))))))
    (advice-add 'projectile-switch-project-by-name
                :around 'dotemacs//projectile-switch-project-by-name)

    ;; Remove dead projects when Emacs is idle
    (run-with-idle-timer 10 nil 'projectile-cleanup-known-projects)
    (projectile-global-mode)
    (dotemacs-hide-lighter projectile-mode)))

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
    (setq projectile-switch-project-action 'helm-projectile)

    (defconst dotemacs-use-helm-projectile t
      "This variable is only defined if helm-projectile is used.")

    ;; needed for smart search if user's default tool is grep
    (defalias 'dotemacs-helm-project-do-grep 'helm-projectile-grep)
    (defalias 'dotemacs-helm-project-do-grep-region-or-symbol 'helm-projectile-grep)

    (evil-leader/set-key
      "pb"  'helm-projectile-switch-to-buffer
      "pd"  'helm-projectile-find-dir
      "pf"  'helm-projectile-find-file
      "ph"  'helm-projectile
      "pp"  'helm-projectile-switch-project
      "pr"  'helm-projectile-recentf
      "pv"  'projectile-vc
      "sgp" 'helm-projectile-grep))
  :config
  (with-eval-after-load 'projectile
    (add-to-list 'helm-projectile-sources-list 'helm-source-projectile-recentf-list)))


;;; Project management with NeoTree
(use-package neotree
  :ensure t
  :defer t
  :commands neo-global--window-exists-p
  :init
  (progn
    (add-to-list 'evil-motion-state-modes 'neotree-mode)
    (setq neo-window-width 25
          neo-theme 'nerd
          neo-create-file-auto-open t
          neo-banner-message nil
          neo-show-updir-line nil
          neo-mode-line-type 'neotree
          neo-smart-open nil       ; if t, every time when the neotree window is
                                   ; opened, it will try to find current file
                                   ; and jump to node.

          neo-dont-be-alone t      ; Don't allow neotree to be the only open
                                   ; window
          neo-persist-show nil
          neo-show-hidden-files nil
          neo-auto-indent-point t
          neo-modern-sidebar t
          neo-vc-integration nil)

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
        (let ((origin-buffer-file-name (buffer-file-name)))
          (neotree-find (projectile-project-root))
          (neotree-find origin-buffer-file-name))))

    (defun dotemacs-neotree-key-bindings ()
      "Set the key bindings for a neotree buffer."
      (define-key evil-motion-state-local-map (kbd "TAB") 'neotree-stretch-toggle)
      (define-key evil-motion-state-local-map (kbd "RET") 'neotree-enter)
      (define-key evil-motion-state-local-map (kbd "|")   'neotree-enter-vertical-split)
      (define-key evil-motion-state-local-map (kbd "-")   'neotree-enter-horizontal-split)
      (define-key evil-motion-state-local-map (kbd "?")   'evil-search-backward)
      (define-key evil-motion-state-local-map (kbd "c")   'neotree-create-node)
      (define-key evil-motion-state-local-map (kbd "d")   'neotree-delete-node)
      (define-key evil-motion-state-local-map (kbd "gr")  'neotree-refresh)
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

    ;; neo-global--select-window
    (evil-leader/set-key
      "fn" 'neotree-show
      "fN" 'neotree-hide
      "ft" 'neotree-toggle
      "pt" 'neotree-find-project-root))

  :config
  (progn
    (when neo-persist-show
      (add-hook 'popwin:before-popup-hook
                (lambda () (setq neo-persist-show nil)))
      (add-hook 'popwin:after-popup-hook
                (lambda () (setq neo-persist-show t))))
    (dotemacs/add-to-hook 'neotree-mode-hook '(dotemacs-neotree-key-bindings))))


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


;;; Net & Web
(use-package browse-url                 ; Browse URLs
  :bind (("C-c w u" . browse-url)))

(use-package bug-reference              ; Turn bug refs into browsable buttons
  :defer t
  :init (progn (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
               (add-hook 'text-mode-hook #'bug-reference-mode)))

(use-package goto-addr                  ; Make links clickable
  :defer t
  :init (progn (add-hook 'prog-mode-hook #'goto-address-prog-mode)
               (add-hook 'text-mode-hook #'goto-address-mode)))

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

(use-package erc-gitter
  :quelpa (erc-gitter :fetcher github :repo "jleechpe/erc-gitter")
  :disabled t
  :defer t
  :ensure t
  :config
  (add-to-list 'erc-modules 'gitter))

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

(use-package jabber        ;; Jabber (XMPP) client for Emacs
  :defer t
  :ensure t
  :init (evil-leader/set-key "aj" 'jabber-connect-all)
  :config (evil-leader/set-key-for-mode 'jabber-roster-mode
            "ma" 'jabber-send-presence
            "mb" 'jabber-get-browse
            "md" 'jabber-disconnect
            "me" 'jabber-roster-edit-action-at-point
            "mg" 'jabber-display-roster
            "mi" 'jabber-get-disco-items
            "mj" 'jabber-muc-join
            "mo" 'jabber-roster-toggle-offline-display
            "mq" 'bury-buffer
            "ms" 'jabber-send-subscription-request
            "mv" 'jabber-get-version
            "m RET" 'jabber-roster-ret-action-at-point))


;;; Org Mode
(dotemacs-defvar-company-backends org-mode)

(use-package init-org
  :load-path "config/")

(use-package org-plus-contrib
  :mode ("\\.org$" . org-mode)
  :commands (org-clock-out org-occur-in-agenda-files)
  :ensure t
  :defer t
  :init
  (progn
    (setq org-replace-disputed-keys t ;; Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
          org-src-fontify-natively t ;; Fontify org-mode code blocks
          org-clock-persist-file (concat dotemacs-cache-directory "org-clock-save.el")
          org-id-locations-file
          (concat dotemacs-cache-directory ".org-id-locations")
          org-log-done t
          org-startup-with-inline-images t
          org-startup-indented t)

    (with-eval-after-load 'org-indent
      (dotemacs-hide-lighter org-indent-mode))

    (defmacro dotemacs-org-emphasize (fname char)
      "Make function for setting the emphasis in org mode"
      `(defun ,fname () (interactive)
              (org-emphasize ,char)))

    (evil-leader/set-key-for-mode 'org-mode
      "m'" 'org-edit-special
      "mc" 'org-capture
      "md" 'org-deadline
      "me" 'org-export-dispatch
      "mf" 'org-set-effort
      "mP" 'org-set-property

      "ma" 'org-agenda
      "mb" 'org-tree-to-indirect-buffer
      "mA" 'org-archive-subtree
      "ml" 'org-open-at-point
      "mT" 'org-show-todo-tree

      "m." 'org-time-stamp

      ;; headings
      "mhi" 'org-insert-heading-after-current
      "mhI" 'org-insert-heading

      ;; More cycling options (timestamps, headlines, items, properties)
      "mL" 'org-shiftright
      "mH" 'org-shiftleft
      "mJ" 'org-shiftdown
      "mK" 'org-shiftup

      ;; Change between TODO sets
      "m C-S-l" 'org-shiftcontrolright
      "m C-S-h" 'org-shiftcontrolleft
      "m C-S-j" 'org-shiftcontroldown
      "m C-S-k" 'org-shiftcontrolup

      ;; Subtree editing
      "mSl" 'org-demote-subtree
      "mSh" 'org-promote-subtree
      "mSj" 'org-move-subtree-down
      "mSk" 'org-move-subtree-up

      ;; tables
      "mta" 'org-table-align
      "mtb" 'org-table-blank-field
      "mtc" 'org-table-convert
      "mtdc" 'org-table-delete-column
      "mtdr" 'org-table-kill-row
      "mte" 'org-table-eval-formula
      "mtE" 'org-table-export
      "mth" 'org-table-previous-field
      "mtH" 'org-table-move-column-left
      "mtic" 'org-table-insert-column
      "mtih" 'org-table-insert-hline
      "mtiH" 'org-table-hline-and-move
      "mtir" 'org-table-insert-row
      "mtI" 'org-table-import
      "mtj" 'org-table-next-row
      "mtJ" 'org-table-move-row-down
      "mtK" 'org-table-move-row-up
      "mtl" 'org-table-next-field
      "mtL" 'org-table-move-column-right
      "mtn" 'org-table-create
      "mtN" 'org-table-create-with-table.el
      "mtr" 'org-table-recalculate
      "mts" 'org-table-sort-lines
      "mttf" 'org-table-toggle-formula-debugger
      "mtto" 'org-table-toggle-coordinate-overlays
      "mtw" 'org-table-wrap-region

      ;; Multi-purpose keys
      (if dotemacs-major-mode-leader-key
          (concat "m" dotemacs-major-mode-leader-key)
        "m,") 'org-ctrl-c-ctrl-c
        "m*" 'org-ctrl-c-star
        "m RET" 'org-ctrl-c-ret
        "m-" 'org-ctrl-c-minus
        "m^" 'org-sort
        "m/" 'org-sparse-tree

        "mI" 'org-clock-in
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
        "mxv" (dotemacs-org-emphasize dotemacs-org-verbose ?=))

    (require 'ox-md)
    (require 'ox-ascii)
    (require 'ox-confluence)
    (require 'ox-html)
    (require 'org-bullets)

    (with-eval-after-load 'org-agenda
       ;; Since we could override SPC with <leader>, let's make RET do that
       ;; functionality
      (when (equal dotemacs-leader-key "SPC")
       (define-key org-agenda-mode-map
         (kbd "RET") 'org-agenda-show-and-scroll-up)
       (define-key org-agenda-mode-map
         (kbd "SPC") evil-leader--default-map))

      (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
      (define-key org-agenda-mode-map "k" 'org-agenda-previous-line))

    ;; Add global evil-leader mappings. Used to access org-agenda
    ;; functionalities – and a few others commands – from any other mode.
    (dotemacs-declare-prefix "ao" "org")
    (evil-leader/set-key
      ;; org-agenda
      "ao#" 'org-agenda-list-stuck-projects
      "ao/" 'org-occur-in-agenda-files
      "aoa" 'org-agenda-list
      "aoe" 'org-store-agenda-views
      "aom" 'org-tags-view
      "aoo" 'org-agenda
      "aos" 'org-search-view
      "aot" 'org-todo-list
      ;; other
      "aoO" 'org-clock-out
      "aoc" 'org-capture
      "aol" 'org-store-link))
  :config
  (progn
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

    (font-lock-add-keywords
     'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
                  (1 font-lock-comment-face prepend)
                  (2 font-lock-function-name-face)
                  (3 font-lock-comment-face prepend))))

    (require 'org-indent)
    (define-key global-map "\C-cl" 'org-store-link)
    (define-key global-map "\C-ca" 'org-agenda)

    ;; Open links and files with RET in normal state
    (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)

    (evil-leader/set-key
      "Cc" 'org-capture)))

(use-package org-bullets
  :ensure t
  :defer t
  :init
  (progn
    (setq org-bullets-bullet-list '("✿" "❀" "☢" "☯" "✸" ))
    (add-hook 'org-mode-hook 'org-bullets-mode)))

(use-package org-repo-todo
  :ensure t
  :defer t
  :init
  (progn
    (evil-leader/set-key
      "Ct"  'ort/capture-todo
      "CT"  'ort/capture-checkitem)
    (evil-leader/set-key-for-mode 'org-mode
      "mgt" 'ort/goto-todos)))


(dotemacs-use-package-add-hook persp-mode
  :post-init
  (dotemacs-define-custom-layout "@Org"
                                  :binding "o"
                                  :body
                                  (find-file (first org-agenda-files))))

(use-package gnuplot
  :defer t
  :ensure t
  :init (evil-leader/set-key-for-mode 'org-mode
          "mtp" 'org-plot/gnuplot))

(use-package evil-org
  :load-path "extensions/"
  :commands evil-org-mode
  :init
  (add-hook 'org-mode-hook 'evil-org-mode)
  :config
  (progn
    (evil-leader/set-key-for-mode 'org-mode
      "mC" 'evil-org-recompute-clocks)
    (evil-define-key 'normal evil-org-mode-map
      "O" 'evil-open-above)
    (dotemacs-diminish evil-org-mode " ⓔ" " e")))

(dotemacs-use-package-add-hook evil-surround
  :post-init
  (defun dotemacs/add-org-surrounds ()
    (push '(?: . dotemacs//surround-drawer) evil-surround-pairs-alist))
  (add-hook 'org-mode-hook 'dotemacs/add-org-surrounds)
  (defun dotemacs//surround-drawer ()
    (let ((dname (read-from-minibuffer "" "")))
      (cons (format ":%s:" (or dname "")) ":END:"))))

(use-package org-mime
  :defer t
  :commands (org-mime-htmlize org-mime-org-buffer-htmlize)
  :init
  (progn
    (evil-leader/set-key-for-mode 'message-mode
      "mM" 'org-mime-htmlize)
    (evil-leader/set-key-for-mode 'org-mode
      "mm" 'org-mime-org-buffer-htmlize)))

(use-package org-pomodoro
  :defer t
  :ensure t
  :init
  (progn
    (when (dotemacs/system-is-mac)
      (setq org-pomodoro-audio-player "/usr/bin/afplay"))
    (evil-leader/set-key-for-mode 'org-mode
      "mp" 'org-pomodoro)))

(use-package org-present
  :defer t
  :ensure t
  :init
  (progn
    (evilify nil org-present-mode-keymap
      "h" 'org-present-prev
      "l" 'org-present-next
      "q" 'org-present-quit)
    (add-hook 'org-present-mode-hook 'dotemacs-org-present-start)
    (add-hook 'org-present-mode-quit-hook 'dotemacs-org-present-end)))

(use-package toc-org
  :ensure t
  :init
  (progn
    (setq toc-org-max-depth 10)
    (add-hook 'org-mode-hook 'toc-org-enable)))

(use-package htmlize
  :ensure t
  :defer t)

(use-package ox-pandoc
  :defer t
  :ensure t
  :init
  (with-eval-after-load 'org (require 'ox-pandoc)))

(dotemacs-use-package-add-hook emoji-cheat-sheet-plus
  :post-init
  (add-hook 'org-mode-hook 'dotemacs-delay-emoji-cheat-sheet-hook))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook org-mode)
      (push 'company-capf company-backends-org-mode)))
  (dotemacs-use-package-add-hook company-emoji
    :post-init
    (progn
      (push 'company-emoji company-backends-org-mode))))


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
    (evil-leader/set-key "?" 'helm-descbinds)))

(use-package helm-make
  :ensure t
  :defer t
  :init
  (evil-leader/set-key
   "cc" 'helm-make-projectile
   "cm" 'helm-make))

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
  :init
  (progn
    (evil-leader/set-key
      "dh" 'helm-dash-at-point
      "dH" 'helm-dash))
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

(use-package zeal-at-point
  :ensure t
  :if (eq system-type 'gnu/linux)
  :defer t
  :init
  (evil-leader/set-key
    "dd" 'zeal-at-point
    "dD" 'zeal-at-point-set-docset)
  :config
  ;; This lets users seach in multiple docsets
  (push '(web-mode . "html,css,javascript") zeal-at-point-mode-alist))

(use-package which-key
  :ensure t
  :init
  (progn
    (dotemacs-add-toggle which-key
      :status which-key-mode
      :on (which-key-mode)
      :off (which-key-mode -1)
      :documentation
      "Display a buffer with available key bindings."
      :evil-leader "tK")

    (let ((new-descriptions
           ;; being higher in this list means the replacement is applied later
           '(
             ("dotemacs/\\(.+\\)" . "\\1")
             ("dotemacs-\\(.+\\)" . "\\1")
             ("projectile-\\(.+\\)" . "\\1")
             ("dotemacs-toggle-\\(.+\\)" . "\\1")
             ("select-window-\\([0-9]\\)" . "window \\1")
             ("dotemacs-alternate-buffer" . "last buffer")
             ("dotemacs-toggle-mode-line-\\(.+\\)" . "\\1")
             ("avy-goto-word-or-subword-1" . "avy word")
             ("shell-command" . "shell cmd")
             ("dotemacs-default-pop-shell" . "open shell")
             ("dotemacs-helm-project-smart-do-search-region-or-symbol" . "smart search w/input")
             ("dotemacs-helm-project-smart-do-search" . "smart search")
             ("evil-search-highlight-persist-remove-all" . "remove srch hlght")
             ("helm-descbinds" . "show keybindings")
             ("sp-split-sexp" . "split sexp")
             ("avy-goto-line" . "avy line")
             ("universal-argument" . "universal arg")
             ("er/expand-region" . "expand region")
             ("helm-apropos" . "apropos")
             ("evil-lisp-state-\\(.+\\)" . "\\1"))))
        (dolist (nd new-descriptions)
          ;; ensure the target matches the whole string
          (push (cons (concat "\\`" (car nd) "\\'") (cdr nd))
                which-key-description-replacement-alist)))

      (dolist (leader-key `(,dotemacs-leader-key ,dotemacs-emacs-leader-key))
        (which-key-add-key-based-replacements
         (concat leader-key " m")    "major mode commands"
         (concat leader-key " " dotemacs-command-key) "helm M-x"))

      (if (fboundp 'which-key-declare-prefixes)
          (which-key-declare-prefixes
            dotemacs-leader-key '("root" . "dotemacs root")
            dotemacs-emacs-leader-key '("root" . "dotemacs root")
            (concat dotemacs-leader-key " m")
            '("major-mode-cmd" . "Major mode commands")
            (concat dotemacs-emacs-leader-key " m")
            '("major-mode-cmd" . "Major mode commands"))

       ;; no need to use this after everyone updates which-key
       (setq which-key-prefix-title-alist
             `((,(listify-key-sequence
                  (kbd (concat dotemacs-leader-key " m"))) . "Major mode commands")
               (,(listify-key-sequence
                  (kbd (concat dotemacs-emacs-leader-key " m"))) . "Major mode commands")
               (,(listify-key-sequence
                  (kbd dotemacs-leader-key)) . "dotemacs root")
               (,(listify-key-sequence
                  (kbd dotemacs-emacs-leader-key)) . "dotemacs root")))
       (nconc which-key-prefix-title-alist dotemacs-prefix-titles))

      ;; disable special key handling for dotemacs, since it can be
      ;; disorienting if you don't understand it
      (pcase dotemacs-which-key-position
        (`right (which-key-setup-side-window-right))
        (`bottom (which-key-setup-side-window-bottom))
        (`right-then-bottom (which-key-setup-side-window-right-bottom)))

      (setq which-key-special-keys nil
            which-key-use-C-h-for-paging t
            which-key-prevent-C-h-from-cycling t
            which-key-echo-keystrokes 0.02
            which-key-max-description-length 32
            which-key-sort-order 'which-key-key-order-alpha
            which-key-idle-delay dotemacs-which-key-delay
            which-key-allow-evil-operators t)
      (which-key-mode)
      (dotemacs-diminish which-key-mode " Ⓚ" " K")))


;;; Documents
(use-package doc-view
  :defer t
  :init
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
         (kbd "C-u") 'doc-view-scroll-down-or-previous-page)
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


;;; Dired

(use-package dired-open
  :ensure t
  :defer t
  :init
  (progn
    (defun dotemacs-dired-open-file ()
      "Hook dired to translate to projectile and neotree."
      (interactive)
      (let ((file (ignore-errors (dired-get-file-for-visit))))
        (when file
          (find-file (expand-file-name file (projectile-project-root)))
          (run-hooks 'projectile-find-file-hook)
          (message "Projectile root found: %s" (projectile-project-root))
          (when (fboundp 'neotree-dir)
            (if (neo-global--window-exists-p)
                (neotree-dir (projectile-project-root))
              (progn
                (neotree-dir (projectile-project-root))
                (neotree-hide)
                (let ((origin-buffer-file-name (buffer-file-name)))
                  (neotree-find (projectile-project-root))
                  (neotree-find origin-buffer-file-name))
                (neotree-hide)))))))
    (with-eval-after-load 'projectile
      (setq dired-open-functions 'dotemacs-dired-open-file))))

(dotemacs-use-package-add-hook dired
  :post-config
  (with-eval-after-load 'projectile
    (evilify dired-mode dired-mode-map
      (kbd "RET") 'dired-open-file)))


;;; Tim Pope

;; vinegar
; This layer is a port contribution layer for vim-vinegar for emacs.
;
; A port of tpope's vinegar.vim
; [vinegar][https://github.com/tpope/vim-vinegar], simplifying =dired=
; with a limited number of details and exposing the ~-~ command in all
; buffers to enter dired.
;
; ** Features
;
; -  navigation up folders with ~-~ key
; -  simplify dired buffer to show only file names
; -  better evil/vim bindings for navigation within dired buffer
; -  keep only one active dired buffer
; -  Use dired-k extension to show time / vcs related information in
;    single bar
; -  right mouse click moves up directory if in blank space or shows context menu

(defvar vinegar-reuse-dired-buffer nil
  "If non-nil, reuses one dired buffer for navigation.")

(use-package init-vinegar
  :load-path "config/"
  :commands (vinegar/dired-setup))

; (dotemacs|do-after-display-system-init)
(use-package dired+
  :defer t
  :ensure t
  :init
  (progn
    (setq diredp-hide-details-initially-flag t
          diredp-hide-details-propagate-flag t
          ;; use single buffer for all dired navigation
          ;; disable font themeing from dired+
          font-lock-maximum-decoration (quote ((dired-mode . 1) (t . t))))
    (toggle-diredp-find-file-reuse-dir 1)))

(use-package dired                      ; Edit directories
  :defer t
  :config
  (progn
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

    (evilify dired-mode dired-mode-map
      "j"         'vinegar/move-down
      "k"         'vinegar/move-up
      "-"         'vinegar/up-directory
      "0"         'dired-back-to-start-of-files
      "="         'vinegar/dired-diff
      (kbd "C-j") 'dired-next-subdir
      (kbd "C-k") 'dired-prev-subdir
      "I"         'vinegar/dotfiles-toggle
      (kbd "~")   '(lambda ()(interactive) (find-alternate-file "~/"))
      (kbd "RET") (if vinegar-reuse-dired-buffer
                      'dired-find-alternate-file
                    'dired-find-file)
      "f"         'helm-find-files
      "J"         'dired-goto-file
      (kbd "C-f") 'find-name-dired
      "H"         'diredp-dired-recent-dirs
      "T"         'dired-tree-down
      "K"         'dired-do-kill-lines
      "r"         'revert-buffer
      (kbd "C-r") 'dired-do-redisplay
      "gg"        'vinegar/back-to-top
      "G"         'vinegar/jump-to-bottom)

    (when (eq system-type 'darwin)
      ;; OS X bsdtar is mostly compatible with GNU Tar
      (setq dired-guess-shell-gnutar "tar"))

    ;; Use `gls' if `coreutils' was installed prefixed ('g') otherwise, leave
    ;; alone. Manually add to config `(setq dired-use-ls-dired nil)' to surpesss
    ;; warnings, when not using `coreutils' version of 'ls' on OS X.
    ;; We must look for `gls' after `exec-path-from-shell' was initialized to
    ;; make sure that `gls' is in `exec-path'
    ;; See brew info coreutils
    (when-let (gnu-ls (and (eq system-type 'darwin)
                             (executable-find "gls")))
      (setq insert-directory-program gnu-ls
            dired-listing-switches "-aBhl --group-directories-first"))

    (when (or (memq system-type '(gnu gnu/linux))
              (string= (file-name-nondirectory insert-directory-program) "gls"))
      ;; If we are on a GNU system or have GNU ls, add some more `ls' switches:
      ;; `--group-directories-first' lists directories before files, and `-v'
      ;; sorts numbers in file names naturally, i.e. "image1" goes before
      ;; "image02"
      (setq dired-listing-switches
            (concat dired-listing-switches " --group-directories-first -v")))))

(use-package dired-x                    ; Additional tools for Dired
  :defer t
  :init
  (add-hook 'dired-mode-hook 'vinegar/dired-setup)
  :config
  (define-key evil-normal-state-map (kbd "-") 'dired-jump))

;; unimpaired

; This layer ports some of the functionality of tpope's vim-unimpaired
; [unimpaired][https://github.com/tpope/vim-unimpaired]
;
; This plugin provides several pairs of bracket maps using ~[~ to denote
; previous, and ~]~ as next.

(defun unimpaired/paste-above ()
  (interactive)
  (evil-insert-newline-above)
  (evil-paste-after 1))

(defun unimpaired/paste-below ()
  (interactive)
  (evil-insert-newline-below)
  (evil-paste-after 1))

;; from tpope's unimpaired

(define-key evil-normal-state-map (kbd "[ SPC") (lambda() (interactive)(evil-insert-newline-above) (forward-line)))
(define-key evil-normal-state-map (kbd "] SPC") (lambda() (interactive)(evil-insert-newline-below) (forward-line -1)))

(define-key evil-normal-state-map (kbd "[ e") 'move-text-up)
(define-key evil-normal-state-map (kbd "] e") 'move-text-down)

(define-key evil-visual-state-map (kbd "[ e") ":move'<--1")
(define-key evil-visual-state-map (kbd "] e") ":move'>+1")

;; (define-key evil-visual-state-map (kbd "[ e") 'move-text-up)
;; (define-key evil-visual-state-map (kbd "] e") 'move-text-down)

(define-key evil-normal-state-map (kbd "[ b") 'dotemacs-previous-useful-buffer)
(define-key evil-normal-state-map (kbd "] b") 'dotemacs-next-useful-buffer)

(define-key evil-normal-state-map (kbd "] l") 'dotemacs-next-error)
(define-key evil-normal-state-map (kbd "[ l") 'dotemacs-previous-error)

(define-key evil-normal-state-map (kbd "[ h") 'diff-hl-previous-hunk)
(define-key evil-normal-state-map (kbd "] h") 'diff-hl-next-hunk)

(define-key evil-normal-state-map (kbd "[ t") (lambda () (interactive)(raise-frame (previous-frame))))
(define-key evil-normal-state-map (kbd "] t") (lambda () (interactive)(raise-frame (next-frame))))

(define-key evil-normal-state-map (kbd "[ w") 'previous-multiframe-window)
(define-key evil-normal-state-map (kbd "] w") 'next-multiframe-window)

;; select pasted text
(define-key evil-normal-state-map (kbd "g p") (kbd "` [ v ` ]"))

;; paste above or below with newline
(define-key evil-normal-state-map (kbd "[ p") 'unimpaired/paste-above)
(define-key evil-normal-state-map (kbd "] p") 'unimpaired/paste-below)


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
    (with-eval-after-load 'yaml-mode
      (add-hook 'yaml-mode-hook 'ansible/ansible-maybe-enable))))

(use-package ansible-doc                ; Documentation lookup for Ansible
  :ensure t
  :defer t
  :init (with-eval-after-load 'yaml-mode
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

;; Terraform
(use-package terraform-mode
  :defer t
  :ensure t)

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
  (dotemacs/add-flycheck-hook 'puppet-mode))


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
  (dotemacs/add-flycheck-hook 'ledger-mode))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (push 'company-capf company-backends-ledger-mode)
      (dotemacs-add-company-hook ledger-mode))))


;; Miscellaneous

(defvar geolocation-enable-osx-location-service-support t
  "If non nil enable the OS X location service support.")

(defvar geolocation-enable-weather-forecast t
  "If non nil enable the weather forecast service.")

(defvar geolocation-enable-automatic-theme-changer nil
  "If non nil enable the automatic change of theme based on the current time.")

(use-package osx-location
  :if geolocation-enable-osx-location-service-support
  :ensure t
  :init
  (progn
    (add-hook 'osx-location-changed-hook
              (lambda ()
                (setq calendar-latitude osx-location-latitude
                      calendar-longitude osx-location-longitude)
                (unless (bound-and-true-p calendar-location-name)
                  (setq calendar-location-name
                        (format "%s, %s"
                                osx-location-latitude
                                osx-location-longitude)))))
    (osx-location-watch)))

(use-package sunshine
  :if geolocation-enable-weather-forecast
  :ensure t
  :defer t
  :init
  (progn
    (evil-leader/set-key
      "aw" 'sunshine-forecast
      "aW" 'sunshine-quick-forecast)

    (evilify sunshine-mode sunshine-mode-map
      (kbd "q") 'quit-window
      (kbd "i") 'sunshine-toggle-icons))
  :config
  (progn
    (setq sunshine-appid "bedbfc11dac244208e29f486c82412b6"
          sunshine-location "Huntington Beach, CA")

    ;; just in case location was not set by user, or on OS X,
    ;; if wasn't set up automatically, will not work with Emac's
    ;; default for ;; `calendar-location-name'
    (when (not (boundp 'sunshine-location))
      (setq sunshine-location (format "%s, %s"
                                      calendar-latitude
                                      calendar-longitude)))))

(use-package theme-changer
  :if geolocation-enable-automatic-theme-changer
  :ensure t
  :config
  (progn
    (when (> (length dotemacs-themes) 1)
      (change-theme (nth 0 dotemacs-themes)
                    (nth 1 dotemacs-themes)))))

(use-package lorem-ipsum
  :ensure t
  :commands (lorem-ipsum-insert-list
             lorem-ipsum-insert-paragraphs
             lorem-ipsum-insert-sentences)
  :init
  (progn
    (dotemacs-declare-prefix "il" "lorem ipsum")
    (evil-leader/set-key
      "ill" 'lorem-ipsum-insert-list
      "ilp" 'lorem-ipsum-insert-paragraphs
      "ils" 'lorem-ipsum-insert-sentences)))


;;; Google Translate
(use-package google-translate
  :ensure t
  :commands (google-translate-query-translate
             google-translate-at-point
             google-translate-query-translate-reverse
             google-translate-at-point-reverse)
  :init
  (evil-leader/set-key
    "xgQ" 'google-translate-query-translate-reverse
    "xgq" 'google-translate-query-translate
    "xgT" 'google-translate-at-point-reverse
    "xgt" 'google-translate-at-point)
  :config
  (progn
    (require 'google-translate-default-ui)
    (setq google-translate-enable-ido-completion t)
    (setq google-translate-show-phonetic t)
    (setq google-translate-default-source-language "en")
    (setq google-translate-default-target-language "sp")))


;; Search Engine
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


;; Emoji
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
      (run-at-time 0.1 nil 'emoji-cheat-sheet-plus-display-mode))))

(use-package company-emoji
  :if (eq dotemacs-completion-engine 'company)
  :defer t
  :init
  (setq company-emoji-insert-unicode nil))

(use-package spray
  :commands spray-mode
  :ensure t
  :init
  (progn
    (defun dotemacs-start-spray ()
      "Start spray speed reading on current buffer at current point."
      (interactive)
      (evil-insert-state)
      (spray-mode t)
      (internal-show-cursor (selected-window) nil))
    (evil-leader/set-key "asr" 'dotemacs-start-spray)

    (defadvice spray-quit (after dotemacs-quit-spray activate)
      "Correctly quit spray."
      (internal-show-cursor (selected-window) t)
      (evil-normal-state)))
  :config
  (progn
    (define-key spray-mode-map (kbd "h") 'spray-backward-word)
    (define-key spray-mode-map (kbd "l") 'spray-forward-word)
    (define-key spray-mode-map (kbd "q") 'spray-quit)))


;; Vagrant
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
        (vagrant-tramp-add-method)
        (setq dotemacs--vagrant-tramp-loaded t)))
    (evil-leader/set-key "Vt" 'vagrant-tramp-term)))


;; Pandoc
(use-package pandoc-mode ; http://joostkremers.github.io/pandoc-mode/
  :defer t
  :commands dotemacs-run-pandoc
  :config
  (progn
    (defun dotemacs-run-pandoc ()
      "Start pandoc for the buffer and open the menu"
      (interactive)
      (pandoc-mode)
      (pandoc-main-hydra/body))
    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))
  :init
  (progn
    (evil-leader/set-key "P/" 'dotemacs-run-pandoc)))

; As there’s not, yet, an EPUB reader for Emacs, you can still set up Emacs to
; be able to open .epub files to see what’s inside them, since they are, after
; all, just ZIP files
(setq auto-mode-alist
 (append (list '("\\.epub$" . archive-mode)) auto-mode-alist))

(setq auto-coding-alist
 (append (list '("\\.epub$" . no-conversion)) auto-coding-alist))


;;; Auto highlight symbol
(use-package init-auto-highlight-symbol
  :load-path "config/")

(use-package auto-highlight-symbol
  :defer t
  :ensure t
  :init
  (progn
    (setq ahs-case-fold-search nil
          ahs-default-range 'ahs-range-whole-buffer
          ;; by default disable auto-highlight of symbol
          ;; current symbol can always be highlighted with <leader> s h
          ahs-idle-timer 0
          ahs-idle-interval 0.25
          ahs-inhibit-face-list nil)

    (dotemacs-add-toggle automatic-symbol-highlight
      :status (timerp ahs-idle-timer)
      :on (progn
            (auto-highlight-symbol-mode)
            (setq ahs-idle-timer
                  (run-with-idle-timer ahs-idle-interval t
                                       'ahs-idle-function)))
      :off (when (timerp ahs-idle-timer)
             (auto-highlight-symbol-mode)
             (cancel-timer ahs-idle-timer)
             (setq ahs-idle-timer 0))
      :documentation "Automatic highlight of current symbol."
      :evil-leader "tha")

    (dotemacs/add-to-hooks 'auto-highlight-symbol-mode '(prog-mode-hook
                                                         markdown-mode-hook)))
  :config
  (progn
    (dotemacs-hide-lighter auto-highlight-symbol-mode)

    (defvar-local dotemacs-last-ahs-highlight-p nil
      "Info on the last searched highlighted symbol.")
    (defvar-local dotemacs--ahs-searching-forward t)

    (with-eval-after-load 'evil
      (define-key evil-motion-state-map (kbd "*")
        'dotemacs-enter-ahs-forward)
      (define-key evil-motion-state-map (kbd "#")
        'dotemacs-enter-ahs-backward))

    (evil-leader/set-key
      "sh" 'dotemacs-symbol-highlight
      "sH" 'dotemacs-goto-last-searched-ahs-symbol)

    ;; micro-state to easily jump from a highlighted symbol to the others
    (dolist (sym '(ahs-forward
                   ahs-forward-definition
                   ahs-backward
                   ahs-backward-definition
                   ahs-back-to-start
                   ahs-change-range))
      (let* ((advice (intern (format "dotemacs-%s" (symbol-name sym)))))
        (eval `(defadvice ,sym (around ,advice activate)
                 (dotemacs-ahs-highlight-now-wrapper)
                 ad-do-it
                 (dotemacs-ahs-highlight-now-wrapper)
                 (setq dotemacs-last-ahs-highlight-p (ahs-highlight-p))))))

    (dotemacs-define-micro-state symbol-highlight
      :doc (let* ((i 0)
                  (overlay-count (length ahs-overlay-list))
                  (overlay (format "%s" (nth i ahs-overlay-list)))
                  (current-overlay (format "%s" ahs-current-overlay))
                  (st (ahs-stat))
                  (plighter (ahs-current-plugin-prop 'lighter))
                  (plugin (format " <%s> " (cond ((string= plighter "HS") "D")
                                                 ((string= plighter "HSA") "B")
                                                 ((string= plighter "HSD") "F"))))
                  (propplugin (propertize plugin 'face
                                          `(:foreground "#ffffff"
                                                        :background ,(face-attribute
                                                                      'ahs-plugin-defalt-face :foreground)))))
             (while (not (string= overlay current-overlay))
               (setq i (1+ i))
               (setq overlay (format "%s" (nth i ahs-overlay-list))))
             (let* ((x/y (format "(%s/%s)" (- overlay-count i) overlay-count))
                    (propx/y (propertize x/y 'face ahs-plugin-whole-buffer-face))
                    (hidden (if (< 0 (- overlay-count (nth 4 st))) "*" ""))
                    (prophidden (propertize hidden 'face '(:weight bold))))
               (format "%s %s%s [n/N] move [e] edit [r] range [R] reset [d/D] definition [/] find in project [f] find in files [b] find in opened buffers [q] exit"
                       propplugin propx/y prophidden)))
      :on-exit (dotemacs-ahs-ms-on-exit)
      :bindings
      ("d" ahs-forward-definition)
      ("D" ahs-backward-definition)
      ("e" nil
       :post (evil-iedit-state/iedit-mode)
       :exit t)
      ("n" dotemacs-quick-ahs-forward)
      ("N" dotemacs-quick-ahs-backward)
      ("R" ahs-back-to-start)
      ("r" ahs-change-range)
      ("/" dotemacs-helm-project-smart-do-search-region-or-symbol :exit t)
      ("b" dotemacs-helm-buffers-smart-do-search-region-or-symbol :exit t)
      ("f" dotemacs-helm-files-smart-do-search-region-or-symbol :exit t)
      ("q" nil :exit t))))


;; NixOS
(dotemacs-defvar-company-backends nix-mode)

(use-package nix-mode   ; This layer adds tools for
  :ensure t             ; better integration of emacs in NixOS.
  :defer t)

(use-package nixos-options
  :ensure t
  :defer t)

(use-package helm-nixos-options
  :ensure t
  :defer t
  :config
  (evil-leader/set-key
    "h>" 'helm-nixos-options))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :pre-init
    (progn
      (push 'company-capf company-backends-nix-mode)
      (dotemacs-add-company-hook nix-mode))))

(use-package company-nixos-options
  :if (eq dotemacs-completion-engine 'company)
  :defer t
  :init
  (progn
    (push 'company-nixos-options company-backends-nix-mode)))


;; Asciidoc
(use-package adoc-mode
  ;; We will NOT default `.txt' files to AsciiDoc mode,
  ;; and `.asciidoc' extension is just plain stupid.
  :mode (("\\.adoc?$" . adoc-mode))
  :defer t
  :ensure t
  :config
  (progn
    ;; We have quite a lot of possible keybindings.
    ;; See `adoc-mode.el', its bottom part where the huge easy-menu
    ;; is defined and after that, where the various `tempo-template-*'
    ;; functions are defined.

    ;; See /doc/CONVENTIONS.md#plain-text-markup-languages
    (evil-leader/set-key-for-mode 'adoc-mode
      "mh1" 'tempo-template-adoc-title-1
      ;; Alternative method of inserting top-level heading
      "mhI" 'tempo-template-adoc-title-1
      "mh2" 'tempo-template-adoc-title-2
      ;; Alternative method of inserting the most usual heading
      "mhi" 'tempo-template-adoc-title-2
      "mh3" 'tempo-template-adoc-title-3
      "mh4" 'tempo-template-adoc-title-4
      "mh5" 'tempo-template-adoc-title-5
      "mxb" 'tempo-template-adoc-strong
      "mxi" 'tempo-template-adoc-emphasis)
    ;; yes, exactly like that. To "promote" title is to INCREASE its size.
    ;; `adoc-denote' does the opposite: increases its LEVEL,
    ;; which DECREASES its size.
    (define-key adoc-mode-map (kbd "M-h") 'adoc-denote)
    ;; see the comment about  adoc-denote above
    (define-key adoc-mode-map (kbd "M-l") 'adoc-promote)))


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
                          :evil-leader "thc"))
  :config
  (progn
    (dotemacs-diminish highlight-indentation-mode " ⓗ" " h")
    (dotemacs-diminish highlight-indentation-current-column-mode " ⓗⒸ" " hC")))

(use-package highlight-parentheses
  :defer t
  :ensure t
  :init
  (progn
    (when (member dotemacs-highlight-delimiters '(all current))
      (add-hook 'prog-mode-hook #'highlight-parentheses-mode))
    (setq hl-paren-delay 0.2)
    (evil-leader/set-key "tCp" 'highlight-parentheses-mode)
    (setq hl-paren-colors '("Springgreen3"
                            "IndianRed1"
                            "IndianRed3"
                            "IndianRed4")))
  :config
  (dotemacs-hide-lighter highlight-parentheses-mode)
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
    (evil-leader/set-key "tCd" 'rainbow-delimiters-mode)

    (when (member dotemacs-highlight-delimiters '(any all))
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
  :config
  (dotemacs-hide-lighter rainbow-mode))


;;; Paired delimiters
(use-package elec-pair                  ; Electric pairs
  :disabled t
  :init (electric-pair-mode))

(use-package paren                      ; Highlight paired delimiters
  :ensure t
  :init
  (progn
    (setq show-paren-delay 0)
    (dotemacs-add-toggle show-paren-mode
      :status show-paren-mode
      :on (show-paren-mode)
      :off (show-paren-mode -1)
      :documentation "Highlight matching pairs of parentheses."
      :evil-leader "tCP")
    (if (eq dotemacs-highlight-delimiters 'all)
      (show-paren-mode)))
  :config (setq show-paren-when-point-inside-paren t
                show-paren-when-point-in-periphery t))

(use-package init-smartparens      ; Personal Smartparens extensions
  :load-path "config/")

;; Use SmartParens instead of Paredit and Electric Pair
(use-package smartparens                ; Parenthesis editing and balancing
  :ensure t
  :defer t
  :commands (sp-split-sexp sp-newline)
  :init
  (progn
    (dotemacs/add-to-hooks (if dotemacs-smartparens-strict-mode
                                'smartparens-strict-mode
                              'smartparens-mode)
                            '(prog-mode-hook))

    (add-hook 'minibuffer-setup-hook 'dotemacs-conditionally-enable-smartparens-mode)

    ;; TODO move these hooks into their layers
    ;; (dolist (hook '(LaTeX-mode-hook web-moode-hook inferior-python-mode-hook))
    ;;   (add-hook hook #'smartparens-mode))

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
      :evil-leader "t C-p")

    (setq sp-show-pair-delay 0.2
          sp-autoskip-closing-pair 'always ; https://github.com/Fuco1/smartparens/issues/142
          ; fix paren highlighting in normal mode
          sp-show-pair-from-inside t
          sp-cancel-autoskip-on-backward-movement nil)

    (evil-leader/set-key
     "J"  'sp-split-sexp
     "jj" 'sp-newline))
  :config
  (progn
    (require 'smartparens-config)
    (dotemacs-diminish smartparens-mode " ⓟ" " p")

    (show-smartparens-global-mode +1)

    ;;; Additional pairs for various modes
    (sp-with-modes '(php-mode)
      (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
                                                 (dotemacs-php-handle-docstring "RET")))
      (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
      (sp-local-pair "(" nil :prefix "\\(\\sw\\|\\s_\\)*"))

    (sp-with-modes '(scala-mode)
      (sp-local-pair "'" nil :actions nil))

    (sp-with-modes '(text-mode)
      (sp-local-pair "`" "'" :actions '(insert wrap)))

    (sp-with-modes '(racket-mode)
      (sp-local-pair "'" nil :actions nil)
      (sp-local-pair "`" nil :actions nil))

    ;; TODO research sp-local-tag or evil-surround with modes
    (sp-with-modes '(tex-mode
                     plain-tex-mode
                     latex-mode)
      ; (sp-local-tag "i" "\"<" "\">")
      (sp-local-pair "$" " $")
      (sp-local-pair "\\[" " \\]")
      (sp-local-pair "\\(" " \\)")
      (sp-local-pair "\\{" " \\}")
      (sp-local-pair "\\left(" " \\right)")
      (sp-local-pair "\\left\\{" " \\right\\}"))

    (sp-with-modes 'org-mode
      (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'dotemacs-org-skip-asterisk)
      (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
      (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "«" "»"))

    (sp-with-modes '(markdown-mode
                     gfm-mode
                     rst-mode)
      ; (sp-local-tag "2" "**" "**")
      ; (sp-local-tag "s" "```scheme" "```")
      ; (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags)
      (sp-local-pair "*" "*" :wrap "C-*" :skip-match 'dotemacs-gfm-skip-asterisk)
      (sp-local-pair "_" "_" :wrap "C-_"))

    (sp-with-modes '(rust-mode)
      ;; Don't pair lifetime specifiers
      (sp-local-pair "'" nil :actions nil))

    (sp-with-modes '(malabar-mode c++-mode)
      (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))

    (sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
                                                        ("* ||\n[i]" "RET")))
    (sp-with-modes '(haskell-mode)
      (sp-local-pair "'" nil :unless '(dotemacs-after-symbol-p))
      (sp-local-pair "\\(" nil :actions nil))

    ;; Emacs Lisp
    (sp-with-modes '(emacs-lisp-mode
                     inferior-emacs-lisp-mode
                     lisp-interaction-mode
                     lisp-mode)
      (sp-local-pair "'" nil :actions nil)
      (sp-local-pair "(" nil :bind "M-(")
      (sp-local-pair "`" "'" :when '(sp-in-string-p) :actions '(insert wrap)))

    ;; don't create a pair with single quote in minibuffer
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

    (sp-pair "{" nil :post-handlers
             '(:add (dotemacs-smartparens-pair-newline-and-indent "RET")))
    (sp-pair "[" nil :post-handlers
             '(:add (dotemacs-smartparens-pair-newline-and-indent "RET")))))


;;; Syntax Checking
(use-package init-syntax-checking
  :load-path "config/"
  :defer t
  :commands (dotemacs/add-flycheck-hook
             dotemacs-discard-undesired-html-tidy-error
             dotemacs-flycheck-mode-line-status
             dotemacs-mode-line-flycheck-info-toggle
             dotemacs-eslint-set-local-eslint-from-projectile
             dotemacs-flycheck-executables-updated
             dotemacs-flycheck-executables-search
             dotemacs-flycheck-init-react
             dotemacs-flycheck-init-javascript))

(use-package flycheck                   ; On-the-fly syntax checking
  :ensure t
  :defer t
  :init
  (progn
    (setq flycheck-standard-error-navigation nil
          flycheck-completion-system 'ido)

    ;; Each buffer gets its own idle-change-delay because of the
    ;; buffer-sensitive adjustment above.
    (make-variable-buffer-local 'flycheck-idle-change-delay)

    (add-hook 'flycheck-after-syntax-check-hook
              #'dotemacs-adjust-flycheck-automatic-syntax-eagerness)

    (dotemacs-add-toggle syntax-checking
      :status flycheck-mode
      :on (flycheck-mode)
      :off (flycheck-mode -1)
      :documentation "Enable error and syntax checking."
      :evil-leader "ts"))
  :config
  (progn
    ;; Make flycheck recognize packages in loadpath
    ;; i.e (require 'company) will not give an error now
    (setq flycheck-emacs-lisp-load-path 'inherit)

    (dotemacs-diminish flycheck-mode " ⓢ" " s")

    (dotemacs-evilify-map flycheck-error-list-mode-map
                           :mode flycheck-error-list-mode
                           :bindings
                           "RET" 'flycheck-error-list-goto-error
                           "j" 'flycheck-error-list-next-error
                           "k" 'flycheck-error-list-previous-error)

    (evil-leader/set-key
      "ec" 'flycheck-clear
      "eh" 'flycheck-describe-checker
      "el" 'dotemacs-toggle-flycheck-error-list
      ; ; https://github.com/flycheck/flycheck/pull/494
      ; "el" 'dotemacs-flycheck-pop-to-error-list
      ; "eL" 'dotemacs-flycheck-hide-list-errors
      "es" 'flycheck-select-checker
      "eS" 'flycheck-set-checker-executable
      "ev" 'flycheck-verify-setup)

    (dotemacs|do-after-display-system-init
      (unless (display-graphic-p)
        (setq flycheck-display-errors-function
              #'flycheck-display-error-messages-unless-error-list
              flycheck-mode-line
              '(:eval (dotemacs-flycheck-mode-line-status)))))

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

    ;; toggle flycheck window
    (defun dotemacs-toggle-flycheck-error-list ()
      "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
      (interactive)
      (-if-let (window (flycheck-get-error-list-window))
          (quit-window nil window)
        (flycheck-list-errors)))

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
                #b00000000)))

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
      :fringe-face 'flycheck-fringe-info)))

(use-package flycheck-pos-tip
  :ensure t
  :if dotemacs-s-syntax-checking-enable-tooltips
  :defer t
  :init
  (with-eval-after-load 'flycheck
    (dotemacs|do-after-display-system-init
     (flycheck-pos-tip-mode))))

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

(use-package init-spell-checking
  :load-path "config/"
  :defer t
  :commands (spell-checking/add-flyspell-hook
             spell-checking/change-dictionary))

(use-package define-word
  :defer t
  :ensure t
  :init
  (evil-leader/set-key
    "xwd" 'define-word-at-point))

(use-package auto-dictionary
  :if spell-checking-enable-auto-dictionary
  :defer t
  :ensure t
  :init
  (progn
    (add-hook 'flyspell-mode-hook 'auto-dictionary-mode)
    ;; Select the buffer local dictionary if it was set, otherwise
    ;; auto-dictionary will replace it with a guessed one at each activation.
    ;; https://github.com/nschum/auto-dictionary-mode/issues/5
    (defun dotemacs//adict-set-local-dictionary ()
      "Set the local dictionary if not nil."
      (when (and (fboundp 'adict-change-dictionary)
                 ispell-local-dictionary)
        (adict-change-dictionary ispell-local-dictionary)))

    (add-hook 'auto-dictionary-mode-hook
              'dotemacs//adict-set-local-dictionary 'append)))

(use-package flyspell                   ; On-the-fly spell checking
  :defer t
  :init
  (progn
    (setq flyspell-use-meta-tab nil
          flyspell-issue-welcome-flag nil  ;; Make Flyspell less chatty
          flyspell-issue-message-flag nil)
    (dolist (mode '(org-mode text-mode message-mode))
      (spell-checking/add-flyspell-hook mode))

    (dotemacs-add-toggle spelling-checking
      :status flyspell-mode
      :on (if (derived-mode-p 'prog-mode)
              (flyspell-prog-mode)
            (flyspell-mode))
      :off (progn
             (flyspell-mode-off)
             ;; Also disable auto-dictionary when disabling spell-checking.
             (when (fboundp 'auto-dictionary-mode) (auto-dictionary-mode -1)))
      :documentation "Enable automatic spell checking."
      :evil-leader "tS")

    (evil-leader/set-key
      "Sb" 'flyspell-buffer
      "Sd" 'spell-checing/change-dictionary
      "Sn" 'flyspell-goto-next-error))
  :config
  (progn
    ;; Undefine mouse buttons which get in the way
    (define-key flyspell-mouse-map [down-mouse-2] nil)
    (define-key flyspell-mouse-map [mouse-2] nil)
    (dotemacs-diminish flyspell-mode " Ⓢ" " S")))

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
                      :documentation "Enable auto-completion."
                      :evil-leader "ta")

;; In `completion-at-point', do not pop up silly completion buffers for less
;; than five candidates.  Cycle instead.
(setq completion-cycle-threshold 5)

;; tell emacs where to read abbrev
(setq abbrev-file-name (concat dotemacs-cache-directory "abbrev_defs"))

(use-package hippie-exp                 ; Powerful expansion and completion
  :bind (([remap dabbrev-expand] . hippie-expand))
  :init
  :config
  (progn
    ;; replace dabbrev-expand
    (global-set-key (kbd "M-/") 'hippie-expand)
    (define-key evil-insert-state-map (kbd "C-p") 'hippie-expand)
    (setq hippie-expand-try-functions-list '(
          ;; Try to expand yasnippet snippets based on prefix
          yas-hippie-try-expand
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
          try-complete-lisp-symbol))))

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
          company-frontends '(company-pseudo-tooltip-frontend))
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
    (dotemacs-diminish company-mode " ⓐ" " a")
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
                                   company-sort-by-occurrence))))

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
  :init
  (progn
    (dotemacs|do-after-display-system-init
    (when (and auto-completion-enable-help-tooltip
               (not (version< emacs-version "24.4"))  ;; company-quickhelp from MELPA
                                                      ;; is not compatible with 24.3 anymore
               (eq dotemacs-completion-engine 'company)
               (display-graphic-p))
      (add-hook 'company-mode-hook 'company-quickhelp-mode)))))

(use-package helm-company
  :ensure t
  :if (eq dotemacs-completion-engine 'company)
  :defer t
  :init
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-/") 'helm-company)))

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
    (with-eval-after-load 'yasnippet
      (push 'ac-source-yasnippet ac-sources))

    (add-to-list 'completion-styles 'initials t)
    (define-key ac-completing-map (kbd "C-j") 'ac-next)
    (define-key ac-completing-map (kbd "C-k") 'ac-previous)
    (define-key ac-completing-map (kbd "<S-tab>") 'ac-previous)
    (dotemacs-diminish auto-complete-mode " ⓐ" " a")))

(use-package ac-ispell
  :ensure t
  :defer t
  :if (eq dotemacs-completion-engine 'auto-complete)
  :init
  (progn
    (setq ac-ispell-requires 4)
    (with-eval-after-load 'auto-complete
      (ac-ispell-setup))))

(use-package init-yasnippet
  :ensure yasnippet
  :load-path "config/"
  :commands (dotemacs-load-yasnippet
             dotemacs-auto-yasnippet-expand
             dotemacs-force-yasnippet-off))

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
    (evil-leader/set-key
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
    (evil-leader/set-key "is" 'dotemacs-helm-yas)
    (setq helm-c-yas-space-match-any-greedy t)))


;;; The mode line

(use-package spaceline-config
  :ensure spaceline
  :init
  (progn
    (dotemacs|do-after-display-system-init
     (setq-default powerline-default-separator
                   (if (display-graphic-p) 'wave 'utf-8)))

    (defun dotemacs-set-powerline-for-startup-buffers ()
      "Set the powerline for buffers created when Emacs starts."
      (dolist (buffer '("*Messages*" "*dotemacs*" "*scratch" "*Compile-Log*" "*Require Times*"))
        (when (get-buffer buffer)
          (dotemacs-restore-powerline buffer))))
    (add-hook 'emacs-startup-hook
              'dotemacs-set-powerline-for-startup-buffers))
  :config
  (progn
    (spaceline-toggle-battery-off)
    (spaceline-toggle-version-control-off)
    (spaceline-toggle-minor-modes-off)
    (spaceline-toggle-major-mode-off)
    (spaceline-toggle-org-clock-off)

    (defun dotemacs/customize-powerline-faces ()
      "Alter powerline face to make them work with more themes."
      (set-face-attribute 'powerline-inactive2 nil
                          :inherit 'font-lock-comment-face))
    (dotemacs/customize-powerline-faces)

    (dolist (spec '((minor-modes "tmm")
                    (major-mode "tmM")
                    (version-control "tmv")
                    (point-position "tmp")
                    (org-clock "tmc")))
      (let* ((segment (car spec))
             (status-var (intern (format "spaceline-%S-p" segment))))
        (eval `(dotemacs-add-toggle ,(intern (format "mode-line-%S" segment))
                 :status ,status-var
                 :on (setq ,status-var t)
                 :off (setq ,status-var nil)
                 :documentation ,(format "Show %s in the mode-line."
                                         (replace-regexp-in-string
                                          "-" " " (format "%S" segment)))
                 :evil-leader ,(cadr spec)))))
    (setq spaceline-org-clock-p nil)

    (defun dotemacs//evil-state-face ()
      (if (bound-and-true-p evil-state)
          (let ((state (if (eq 'operator evil-state) evil-previous-state evil-state)))
            (intern (format "dotemacs-%S-face" state)))
        'face-of-god))
    (setq spaceline-highlight-face-func 'dotemacs//evil-state-face)

    (let ((unicodep (dotemacs-symbol-value
                     dotemacs-mode-line-unicode-symbols)))
      (setq spaceline-window-numbers-unicode unicodep)
      (setq spaceline-workspace-numbers-unicode unicodep))

    (spaceline-spacemacs-theme)
    (spaceline-helm-mode t)
    ; (when (configuration-layer/package-usedp 'info+)
    ;   (spaceline-info-mode t))

    (defun dotemacs-restore-powerline (buffer)
      "Restore the powerline in buffer"
      (with-current-buffer buffer
        (setq-local mode-line-format (default-value 'mode-line-format))
        (powerline-set-selected-window)
        (powerline-reset)))

    (defun dotemacs//prepare-diminish ()
      (when spaceline-minor-modes-p
        (let ((unicodep (dotemacs-symbol-value
                         dotemacs-mode-line-unicode-symbols)))
          (dotemacs|do-after-display-system-init
           (setq spaceline-minor-modes-separator
                 (if unicodep (if (display-graphic-p) "" " ") "|")))
          (dolist (mm dotemacs--diminished-minor-modes)
            (let ((mode (car mm)))
              (when (and (boundp mode) (symbol-value mode))
                (let* ((unicode (cadr mm))
                       (ascii (caddr mm))
                       (dim (if unicodep
                                unicode
                              (if ascii ascii unicode))))
                  (diminish mode dim))))))))
    (add-hook 'spaceline-pre-hook 'dotemacs//prepare-diminish)))

(use-package evil-anzu                  ; Position/matches count for isearch
  :ensure t
  :init
  (global-anzu-mode t)
  :config
  (progn
    (dotemacs-hide-lighter anzu-mode)
    (setq anzu-search-threshold 1000
          anzu-cons-mode-line-p nil)
    ;; powerline integration
    (defun dotemacs-anzu-update-mode-line (here total)
      "Custom update function which does not propertize the status."
      (when anzu--state
        (let ((status (cl-case anzu--state
                        (search (format "(%s/%d%s)"
                                        (anzu--format-here-position here total)
                                        total (if anzu--overflow-p "+" "")))
                        (replace-query (format "(%d replace)" total))
                        (replace (format "(%d/%d)" here total)))))
          status)))
    (setq anzu-mode-line-update-function 'dotemacs-anzu-update-mode-line)))

(use-package rfringe
  :ensure t
  :defer t)

; (byte-recompile-directory (expand-file-name dotemacs-core-directory) 0)
; (byte-recompile-directory (expand-file-name dotemacs-config-dir) 0)
; (byte-recompile-file (expand-file-name "init.el" user-emacs-directory))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
