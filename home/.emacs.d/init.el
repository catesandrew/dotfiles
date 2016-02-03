;;; init.el --- Emacs configuration

;; This file is NOT part of GNU Emacs.

;;; License:

;;; Commentary:

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;;; Code:

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 100000000)

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

(defconst dotemacs-modules-dir
  (expand-file-name (concat user-emacs-directory "modules/"))
  "modules directory.")

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

(defvar dotemacs-remap-Y-to-y$ t
  "If non nil `Y' is remapped to `y$'.")

(defvar dotemacs-ex-substitute-global nil
  "If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.")

(defcustom dotemacs-leader-key ","
  "The leader key."
  :group 'dotemacs)

(defcustom dotemacs-emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'"
  :group 'dotemacs)

;; major-mode-leader to 0x00A0 (NO_BREAK_SPACE)
(defcustom dotemacs-major-mode-leader-key "\\"
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

;; ;; perf measurments
;; (with-current-buffer (get-buffer-create "*Require Times*")
;;   (insert "| feature | timestamp | elapsed |\n")
;;   (insert "|---------+-----------+---------|\n"))

;; (defadvice require (around require-advice activate)
;;   (let ((elapsed)
;;         (loaded (memq feature features))
;;         (start (current-time)))
;;     (prog1
;;         ad-do-it
;;       (unless loaded
;;         (with-current-buffer (get-buffer-create "*Require Times*")
;;           (goto-char (point-max))
;;           (setq elapsed (float-time (time-subtract (current-time) start)))
;;           (insert (format "| %s | %s | %f |\n"
;;                           feature
;;                           (format-time-string "%Y-%m-%d %H:%M:%S.%3N" (current-time))
;;                           elapsed)))))))


;;; Load Paths
(defun add-to-load-path (dir) (add-to-list 'load-path dir))

;; load paths
(mapc 'add-to-load-path
      `(
        ,dotemacs-core-directory
        ,dotemacs-modules-dir
        ,dotemacs-user-settings-dir
        ))


;;; Core Package management
(setq message-log-max 16384)

(require 'subr-x nil 'noerror)
(require 'core-auto-completion)
(require 'core-display-init)
(require 'core-themes-support)
(require 'core-fonts-support)
(require 'core-toggle)
(require 'core-micro-state)
(require 'core-use-package-ext)
(require 'package)
(require 'core-funcs)
(require 'core-buffers)

(defvar dotemacs-elpa-https t
  "If non nil ELPA repositories are contacted via HTTPS whenever it's
possible. Set it to nil if you have no way to use HTTPS in your
environment, otherwise it is strongly recommended to let it set to t.")

(unless package--initialized
  (let ((archives '(("melpa" . "melpa.org/packages/")
                    ("org"   . "orgmode.org/elpa/")
                    ("gnu"   . "elpa.gnu.org/packages/"))))
    (setq package-archives
          (mapcar (lambda (x)
                    (cons (car x) (concat
                                   (if (and dotemacs-elpa-https
                                            ;; for now org ELPA repository does
                                            ;; not support HTTPS
                                            ;; TODO when org ELPA repo support
                                            ;; HTTPS remove the check
                                            ;; `(not (equal "org" (car x)))'
                                            (not (equal "org" (car x))))
                                       "https://"
                                     "http://") (cdr x))))
                  archives)))
  ;; optimization, no need to activate all the packages so early
  ;; http://stackoverflow.com/questions/11127109/
  (setq package-enable-at-startup nil)
  (package-initialize)) ;; inactivate


;;; Initialization

(prefer-coding-system 'utf-8) ; with sugar on top

;; silence ad-handle-definition about advised functions getting redefined
(setq ad-redefinition-action 'accept)
;; this is for a smoother UX at startup (i.e. less graphical glitches)
(hidden-mode-line-mode)

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

;; mandatory dependencies
; (dotemacs-load-or-install-package 'dash t)
(dotemacs-load-or-install-package 's t)
;; (dotemacs-load-or-install-package 'f t)
(setq-default ;; evil-want-C-u-scroll t
              ;; evil-want-C-w-in-emacs-state t
              ;; `evil-want-C-i-jump' is set to nil to avoid `TAB' being
              ;; overlapped in terminal mode. The GUI specific `<C-i>' is used
              ;; instead (defined in the init of `evil-jumper' package).
              evil-want-C-i-jump nil)
(setq evil-want-Y-yank-to-eol dotemacs-remap-Y-to-y$
      evil-ex-substitute-global dotemacs-ex-substitute-global)
(dotemacs-load-or-install-package 'evil t)
(dotemacs-load-or-install-package 'bind-map t)
(dotemacs-load-or-install-package 'bind-key t)
(dotemacs-load-or-install-package 'which-key t)
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
(require 'core-keybindings)


;;; Initialization

;; And disable the site default settings
(setq inhibit-default-init t)

;; save custom variables
(unless (bound-and-true-p custom-file)
  (setq custom-file dotemacs-custom-file))

;; start scratch in text mode (usefull to get a faster Emacs load time
;; because it avoids autoloads of elisp modes)
(setq initial-major-mode 'text-mode)

(require 'module-vars)
(require 'module-common)
(require 'module-core)
(require 'module-utils)


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


;;; Evil
(use-package module-evil)


;;; Shell
(use-package module-eshell)
(use-package module-shell)


;;; User interface
(use-package module-smooth-scrolling)

(use-package bind-map
  :init
  (bind-map dotemacs-default-map
    :prefix-cmd dotemacs-cmds
    :keys (dotemacs-emacs-leader-key)
    :evil-keys (dotemacs-leader-key)
    :override-minor-modes t
    :override-mode-name dotemacs-leader-override-mode))

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

(use-package unicode-fonts              ; Map Unicode blocks to fonts
  :ensure t
  ;; Enable emoticon mappings
  :config (progn (setq unicode-fonts-skip-font-groups '(low-quality-glyphs)
                       unicode-fonts-use-prepend t)
                 (unicode-fonts-setup)))


;;; Perspective
(use-package module-perspective)


;;; Eyebrowse
(use-package module-eyebrowse)


;;; Helm
(use-package module-helm)


;;; Buffer, Windows and Frames
(use-package module-fringe)
(use-package module-frame)
(use-package module-buffer)
(use-package module-ibuffer)
(use-package module-window)
(use-package module-desktop)
(use-package module-popwin)


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


;;; Files
(use-package module-file            ; Personal file tools
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

;; Additional bindings for built-ins
(bind-key "C-c f v d" #'add-dir-local-variable)
(bind-key "C-c f v l" #'add-file-local-variable)
(bind-key "C-c f v p" #'add-file-local-variable-prop-line)

(use-package files
  ;; Revert the current buffer (re-read the contents from disk). Burying
  ;; a buffer (removing it from the current window and sending it to the bottom
  ;; of the stack) is very common for dismissing buffers.
  :bind (("C-c e u" . revert-buffer)
         ("C-c e y" . bury-buffer)))

(use-package open-junk-file
  :ensure t
  :defer t
  :commands (open-junk-file)
  :init
  (dotemacs-set-leader-keys "fJ" 'open-junk-file)
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

(use-package module-ignoramus)

(setq view-read-only t)                 ; View read-only files


;;; todo

(setq editorconfig-packages '(editorconfig))
(use-package editorconfig
  :defer t
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.editorconfig" . conf-unix-mode)))

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

(use-package module-jumping)

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

(use-package helm-imenu
  :ensure helm
  :bind (("C-c i" . helm-imenu-in-all-buffers)))


;;; Buffer Editing

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

(use-package module-buffer-editing           ; Personal editing helpers
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

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

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
  :init (dotemacs-set-leader-keys "v" 'er/expand-region)
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


;;; Whitespace
(use-package module-whitespace)


;;; Evil
(use-package module-evil-packages)
(use-package evil-evilified-state :load-path "evil/")
(define-key evil-evilified-state-map (kbd dotemacs-leader-key)
  dotemacs-default-map)


;;; EMacs
(use-package module-emacs) ; Customization, init file and package management


;;; OSX support
(use-package module-osx              ; Personal OS X tools
  :if (eq system-type 'darwin))


;;; Bindings
(use-package module-key-bindings
  :config (dotemacs-toggle-transparency-core))


;;; Text editing

(use-package writeroom-mode             ; Distraction-free editing
  :ensure t
  :bind (("C-c u r" . writeroom-mode)))

(use-package typo
  :ensure t
  :init (progn
          (typo-global-mode)
          (dolist (hook '(markdown-mode-hook rst-mode-hook))
            (add-hook hook 'typo-mode)))
  :diminish (typo-mode . "𝕿"))

;;; Documents
(use-package doc-view
  :defer t
  :init
       (evilified-state-evilify doc-view-mode doc-view-mode-map
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
    (dotemacs-set-leader-keys-for-major-mode 'adoc-mode
      "h1" 'tempo-template-adoc-title-1
      ;; Alternative method of inserting top-level heading
      "hI" 'tempo-template-adoc-title-1
      "h2" 'tempo-template-adoc-title-2
      ;; Alternative method of inserting the most usual heading
      "hi" 'tempo-template-adoc-title-2
      "h3" 'tempo-template-adoc-title-3
      "h4" 'tempo-template-adoc-title-4
      "h5" 'tempo-template-adoc-title-5
      "xb" 'tempo-template-adoc-strong
      "xi" 'tempo-template-adoc-emphasis)
    ;; yes, exactly like that. To "promote" title is to INCREASE its size.
    ;; `adoc-denote' does the opposite: increases its LEVEL,
    ;; which DECREASES its size.
    (define-key adoc-mode-map (kbd "M-h") 'adoc-denote)
    ;; see the comment about  adoc-denote above
    (define-key adoc-mode-map (kbd "M-l") 'adoc-promote)))


;;; Markup Languages
(use-package module-markup-languages)
(use-package module-latex)
(use-package module-markdown)
(use-package module-pandoc)

(use-package graphviz-dot-mode          ; Graphviz
  :ensure t
  :defer t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package systemd                    ; Mode for systemd unit files
  :ensure t
  :defer t)


;;; Programming Languages, Utils
(use-package module-programming
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
                (run-hooks #'dotemacs-prog-mode-hook)))))

(use-package module-compile          ; Personal helpers for compilation
  :commands (dotemacs-colorize-compilation-buffer)
  ;; Colorize output of Compilation Mode, see
  ;; http://stackoverflow.com/a/3072831/355252
  :init (add-hook 'compilation-filter-hook
                  #'dotemacs-colorize-compilation-buffer))

;; Programming Languages
(use-package module-elisp)
(use-package module-scala)
(use-package module-python)
(use-package module-ruby)
(use-package module-rust)
(use-package module-haskell)
(use-package module-go)
(use-package module-c-c++)
(use-package module-clojure)
(use-package module-ocaml)
(use-package module-purescript)
(use-package module-react)
(use-package module-elm)
(use-package module-javascript)
(use-package module-web)
(use-package module-lua)
(use-package module-php)
(use-package module-stylus)
(use-package module-skewer)
(use-package module-racket)
(use-package module-java)
(use-package module-restclient)
(use-package module-swift)
(use-package module-shell-script)

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
      (dotemacs-set-leader-keys-for-major-mode mode
        "ec" 'dotemacs-eval-current-form-sp
        "es" 'dotemacs-eval-current-symbol-sp))))

;;; Misc programming languages
(use-package cmake-mode                 ; CMake files
  :ensure t
  :defer t)

(use-package thrift                     ; Thrift interface files
  :ensure t
  :defer t
  :init (put 'thrift-indent-level 'safe-local-variable #'integerp))


;; Databases
(use-package module-sql)


;; Source Code tags, and metadata
;; e.g. etags, ebrowse, exuberant ctags, cscope, GNU Global and GTags
(use-package module-gtags)


;;; Version control
(use-package module-version-control)
(use-package module-git)
(use-package module-github)
(use-package module-magit)


;;; File Search
(use-package module-file-search)


;;; Interactively Do Things (IDO)
(use-package module-ido)


;;; Project Management
(use-package module-projectile)
(use-package module-neotree)


;;; IRC and IM
(use-package module-irc-im)


;;; Outline and TODOs
(use-package module-org)


;;; Online Help
(use-package module-help)
(use-package module-which-key)


;;; File Navigation
(use-package module-file-navigation)
(use-package module-ranger)
(use-package module-vinegar)
(use-package module-unimpaired)


;;; Virtual Machine
(use-package module-virtual-machine)
(use-package module-ansible)
(use-package module-puppet)
(use-package module-vagrant)
(use-package module-nixos)


;;; Finance
(use-package module-finance)


;;; Highlight Symbols, Numbers, Parentheses, etc.
(use-package module-auto-highlight-symbol)
(use-package module-highlight)


;;; Paired delimiters
(use-package module-paired-delimiters)
(use-package module-smartparens)


;;; Syntax Checking
(use-package module-flycheck)


;;; Spell Checking
(use-package module-spell-checking)
(use-package module-flyspell)


;;; Skeletons, completion and expansion
(use-package module-auto-complete)
(use-package module-hippie-exp)
(use-package module-company)
(use-package module-yasnippet)


;;; Space line
(use-package module-spaceline)


;;; Date and time
(use-package calendar                   ; Built-in calendar
  :bind ("C-c u c" . calendar)
  :config
  ;; In Europe we start on Monday
  (setq calendar-week-start-day 1))

(use-package time                       ; Show current time
  :defer t
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

(use-package sendmail                   ; Send mails from Emacs
  :defer t
  :config (setq send-mail-function 'smtpmail-send-it))

(use-package message                    ; Compose mails from Emacs
  :defer t
  :config (setq message-send-mail-function 'smtpmail-send-it
                ;; Don't keep message buffers around
                message-kill-buffer-on-exit t))


;;; Miscellaneous
(use-package module-location)
(use-package module-emoji)

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
    (dotemacs-set-leader-keys
      "ill" 'lorem-ipsum-insert-list
      "ilp" 'lorem-ipsum-insert-paragraphs
      "ils" 'lorem-ipsum-insert-sentences)))

;; Google Translate
(use-package google-translate
  :ensure t
  :commands (google-translate-query-translate
             google-translate-at-point
             google-translate-query-translate-reverse
             google-translate-at-point-reverse)
  :init
  (dotemacs-set-leader-keys
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

(use-package spray          ; Speed Reading
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
    (dotemacs-set-leader-keys "asr" 'dotemacs-start-spray)

    (defadvice spray-quit (after dotemacs-quit-spray activate)
      "Correctly quit spray."
      (internal-show-cursor (selected-window) t)
      (evil-normal-state)))
  :config
  (progn
    (define-key spray-mode-map (kbd "h") 'spray-backward-word)
    (define-key spray-mode-map (kbd "l") 'spray-forward-word)
    (define-key spray-mode-map (kbd "q") 'spray-quit)))

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

(use-package tildify
  :bind (("C-c e t" . tildify-region))
  :init (dolist (hook '(markdown-mode-hook
                        latex-mode-hook
                        rst-mode-hook))
          (add-hook hook #'tildify-mode))
  ;; Use the right space for LaTeX
  :config (add-hook 'LaTeX-mode-hook
                    (lambda () (setq-local tildify-space-string "~"))))

(use-package rfringe
  :ensure t
  :defer t)

(use-package proced                     ; Edit system processes
  ;; Proced isn't available on OS X
  :if (not (eq system-type 'darwin))
  :bind ("C-x p" . proced))

;;; todo
(use-package module-firestarter
  :disabled t
  :commands (dotemacs-firestarter-mode-line)
  :init (with-eval-after-load 'firestarter
          (setq firestarter-lighter
                '(:eval (dotemacs-firestarter-mode-line)))))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
