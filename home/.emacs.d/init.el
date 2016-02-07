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


;;; Load Paths

(defun add-to-load-path (dir) (add-to-list 'load-path dir))

(defconst dotemacs-core-directory
  (expand-file-name (concat user-emacs-directory "core/"))
  "The core directory.")

(defconst dotemacs-modules-dir
  (expand-file-name (concat user-emacs-directory "modules/"))
  "The modules directory.")

(defconst dotemacs-evil-dir
  (expand-file-name (concat user-emacs-directory "evil/"))
  "The evil directory.")

(defvar dotemacs-user-settings-dir (
  concat user-emacs-directory "users/" user-login-name)
  "The currently logged in user's storage location for settings.")

;; load paths
(mapc 'add-to-load-path
      `(
        ,dotemacs-core-directory
        ,dotemacs-evil-dir
        ,dotemacs-modules-dir
        ,dotemacs-user-settings-dir
        ))

(require 'core-vars)


;;; Initialization

;; Set path to dependencies
(defgroup dotemacs nil
  "Custom configuration for dotemacs."
  :group 'local)

(defconst dotemacs-quelpa-directory
  (concat dotemacs-cache-directory "quelpa/")
  "Quelpa directory.")

(defconst dotemacs-quelpa-build-directory
  (expand-file-name "build" dotemacs-quelpa-directory)
  "Quelpa build directory.")

(defconst dotemacs-quelpa-cache-directory
  (expand-file-name "cache" dotemacs-quelpa-directory)
  "Quelpa cache directory.")

(defconst user-home-directory
  (expand-file-name "~/")
  "User home directory (~/).")

(defconst pcache-directory
  (concat dotemacs-cache-directory "pcache"))
(unless (file-exists-p dotemacs-cache-directory)
    (make-directory dotemacs-cache-directory))

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


;;; Core Package management
(setq message-log-max 16384)
(defconst emacs-start-time (current-time))

(require 'subr-x nil 'noerror)
(require 'core-auto-completion)
(require 'core-display-init)
(require 'core-themes-support)
(require 'core-fonts-support)
(require 'core-toggle)
(require 'core-micro-state)
(require 'core-transient-state)
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
(unless (eq system-type 'darwin)
  (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
    (menu-bar-mode -1)))
;; tooltips in echo-aera
(when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
  (tooltip-mode -1))

(prefer-coding-system 'utf-8) ; with sugar on top

(setq-default ;; evil-want-C-u-scroll t
              ;; evil-want-C-w-in-emacs-state t
              ;; `evil-want-C-i-jump' is set to nil to avoid `TAB' being
              ;; overlapped in terminal mode. The GUI specific `<C-i>' is used
              ;; instead (defined in the init of `evil-jumper' package).
              evil-want-C-i-jump nil)

;; theme
(let ((default-theme (car dotemacs-themes)))
  (dotemacs-load-theme default-theme)
  ;; used to prevent automatic deletion of used packages
  (setq dotemacs-used-theme-packages
        (delq nil (mapcar 'dotemacs-get-theme-package
                          dotemacs-themes)))
  (setq dotemacs--cur-theme default-theme)
  (setq-default dotemacs--cycle-themes (cdr dotemacs-themes)))

;; font
(dotemacs|do-after-display-system-init
  (if (find-font (font-spec :name (car dotemacs-default-font)))
    (dotemacs-set-default-font dotemacs-default-font)
  (dotemacs-buffer/warning "Cannot find font \"%s\"!"
                           (car dotemacs-default-font))))

(setq inhibit-startup-screen t)
(dotemacs-buffer/goto-buffer)
(unless (display-graphic-p)
  ;; explicitly recreate the home buffer for the first GUI client
  (dotemacs|do-after-display-system-init
  (kill-buffer (get-buffer dotemacs-buffer-name))
  (dotemacs-buffer/goto-buffer)))
(setq initial-buffer-choice nil)
(setq inhibit-startup-screen t)
;; dependencies
; (dotemacs-load-or-install-package 'dash t)
(dotemacs-load-or-install-package 's t)
;; (dotemacs-load-or-install-package 'f t)
(setq evil-want-Y-yank-to-eol dotemacs-remap-Y-to-y$
      evil-ex-substitute-global dotemacs-ex-substitute-global)
(dotemacs-load-or-install-package 'evil t)
(dotemacs-load-or-install-package 'bind-map t)
(dotemacs-load-or-install-package 'bind-key t)
(dotemacs-load-or-install-package 'which-key t)
(dotemacs-load-or-install-package 'use-package t)
(setq use-package-verbose init-file-debug)
;; package-build is required by quelpa
(dotemacs-load-or-install-package 'package-build t)

(setq quelpa-verbose init-file-debug
      quelpa-dir dotemacs-quelpa-directory
      quelpa-build-dir dotemacs-quelpa-build-directory
      quelpa-persistent-cache-file dotemacs-quelpa-cache-directory
      quelpa-update-melpa-p nil)
(dotemacs-load-or-install-package 'quelpa t)
(dotemacs-load-or-install-package 'quelpa-use-package t)
;; required for some micro-states
(dotemacs-load-or-install-package 'hydra t)

;; inject use-package hooks for easy customization of
;; stock package configuration
(setq use-package-inject-hooks t)
(require 'core-keybindings)
(require 'use-package)
(require 'module-vars)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (when (fboundp dotemacs-scratch-mode)
     (with-current-buffer "*scratch*"
       (funcall dotemacs-scratch-mode)))
   ;; from jwiegley
   ;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
   (let ((elapsed (float-time
                   (time-subtract (current-time) emacs-start-time))))
     (dotemacs-buffer/append
      (format "\n[Emacs loaded in %.3fs]\n"
              elapsed)))))


;;; Prefixes

;; We define prefix commands only for the sake of which-key
(setq dotemacs-key-binding-prefixes
      '(("a"  "applications")
        ("ai"  "irc")
        ("as"  "shells")
        ;; ("b"   "buffers")
        ("bm"  "move")
        ("c"   "compile/comments")
        ("C"   "capture/colors")
        ("e"   "errors")
        ("f"   "files")
        ("fC"  "files/convert")
        ("fe"  "emacs(dotemacs)")
        ("fv"  "variables")
        ("g"   "git/versions-control")
        ("h"   "help")
        ("hd"  "help-describe")
        ("i"   "insertion")
        ("j"   "jump/join/split")
        ("k"   "lisp")
        ("kd"  "delete")
        ("kD"  "delete-backward")
        ("k`"  "hybrid")
        ("n"   "narrow/numbers")
        ("p"   "projects")
        ("p$"  "projects/shell")
        ("q"   "quit")
        ("r"   "registers/rings/resume")
        ("Re"  "elisp")
        ("Rp"  "pcre")
        ("s"   "search/symbol")
        ("sa"  "ag")
        ("sg"  "grep")
        ("sk"  "ack")
        ("st"  "pt")
        ("sw"  "web")
        ("t"   "toggles")
        ("tC"  "colors")
        ("tE"  "editing-styles")
        ("th"  "highlight")
        ("tm"  "modeline")
        ("T"   "UI toggles/themes")
        ("C-t" "other toggles")
        ("w"   "windows")
        ("wp"  "popup")
        ("x"   "text")
        ("xa"  "align")
        ("xd"  "delete")
        ("xg"  "google-translate")
        ("xl"  "lines")
        ("xm"  "move")
        ("xt"  "transpose")
        ("xw"  "words")
        ("z"  "zoom")))
(mapc (lambda (x) (apply #'dotemacs-declare-prefix x))
      dotemacs-key-binding-prefixes)


;;; Navigation

;; Auto refresh
(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; Make dired "guess" target directory for some operations, like copy to
;; directory visited in other split buffer.
(setq dired-dwim-target t)

;; no beep pleeeeeease ! (and no visual blinking too please)
(setq ring-bell-function 'ignore
      visible-bell nil)

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

;; Highlight and allow to open http link at point in programming buffers
;; goto-address-prog-mode only highlights links in strings and comments
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'text-mode-hook 'goto-address-mode)
;; Highlight and follow bug references in comments and strings
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

;; Keep focus while navigating help buffers
(setq help-window-select 't)

;; Scroll compilation to first error or end
(setq compilation-scroll-output 'first-error)


;;; Edit

;; start scratch in text mode (usefull to get a faster Emacs load time
;; because it avoids autoloads of elisp modes)
(setq initial-major-mode 'text-mode)

;; Disable tabs, but given them proper width
(setq-default indent-tabs-mode nil
              highlight-tabs t
              tab-width 8)

;; Text
(setq longlines-show-hard-newlines t)

;; Use system trash for file deletion should work on Windows and Linux distros
;; on OS X, see contrib/osx layer
(setq delete-by-moving-to-trash t)

;; auto fill breaks line beyond buffer's fill-column
(setq-default fill-column 80)
(dotemacs-diminish auto-fill-function " Ⓕ" " F")

;; persistent abbreviation file
(setq abbrev-file-name (concat dotemacs-cache-directory "abbrev_defs"))

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Single space between sentences is more widespread than double
(setq-default sentence-end-double-space nil)

;; The C-d rebinding that most shell-like buffers inherit from
;; comint-mode assumes non-evil configuration with its
;; `comint-delchar-or-maybe-eof' function, so we disable it
(with-eval-after-load 'comint
  (define-key comint-mode-map (kbd "C-d") nil))

;; whitespace-cleanup configuration
(pcase dotemacs-whitespace-cleanup
  (`all (add-hook 'before-save-hook 'whitespace-cleanup))
  (`trailing (add-hook 'before-save-hook 'delete-trailing-whitespace)))

; Exclude very large buffers from dabbrev
; From https://github.com/purcell/emacs.d/blob/master/lisp/init-auto-complete.el
(defun dotemacs/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))
(setq dabbrev-friend-buffer-function 'dotemacs/dabbrev-friend-buffer)


;;; User Interface

;; important for golden-ratio to better work
(setq window-combination-resize t)

;; fringes
(dotemacs|do-after-display-system-init
 (when (display-graphic-p)
   (custom-set-variables
    '(fringe-mode (quote (4 . 4)) nil (fringe)))
   (custom-set-faces
    '(linum ((t (:height 0.9 :family "Bebas Neue")))))
   (setq-default fringe-indicator-alist
                 '((truncation . nil) (continuation . nil)))))

;; Show column number in mode line
(setq column-number-mode t)
;; Activate linum-mode in all text-mode buffers if the setting is enabled.
(when dotemacs-line-numbers
  (add-hook 'text-mode-hook 'linum-mode))
;; line number
(setq linum-format "%4d")
;; highlight current line
(global-hl-line-mode t)
;; no blink
(blink-cursor-mode 0)
;; Answering just 'y' or 'n' will do
(fset 'yes-or-no-p #'y-or-n-p)
;; draw underline lower
(setq x-underline-at-descent-line t)
;; don't let the cursor go into minibuffer prompt
;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))


;;; Session

;; save custom variables
(unless (bound-and-true-p custom-file)
  (setq custom-file dotemacs-custom-file))

;; scratch buffer empty
(setq initial-scratch-message nil)
;; don't create backup~ files
(setq make-backup-files nil)

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

;; remove annoying ellipsis when printing sexp in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; cache files
(setq url-configuration-directory (concat dotemacs-cache-directory "url")
      eshell-directory-name (concat dotemacs-cache-directory "eshell" )
      tramp-persistency-file-name (concat dotemacs-cache-directory "tramp"))

;; Give us narrowing back! Seems pointless to warn. There's always undo.
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Same for region casing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Remove prompt if the file is opened in other clients
(defun server-remove-kill-buffer-hook ()
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook 'server-remove-kill-buffer-hook)


;;; Core Begin

(require 'module-common)
(require 'module-core)
(require 'module-utils)

;; enable electric indent
(electric-indent-mode)

;; And disable the site default settings
;; (setq inhibit-default-init t)

;; Keep backup files out of the way
(setq backup-directory-alist `((".*" . ,(concat dotemacs-cache-directory "backups"))))
;; don't create backup~ files
(setq backup-by-copying t)
(setq create-lockfiles nil)

;; Transparently open compressed files
;; (auto-compression-mode t)

;; Opt out from the startup message in the echo area by simply disabling this
;; ridiculously bizarre thing entirely.
;; (fset 'display-startup-echo-area-message #'ignore)

;; Show active region
;; (transient-mark-mode 1)
;; (make-variable-buffer-local 'transient-mark-mode)
;; (put 'transient-mark-mode 'permanent-local t)
;; (setq-default transient-mark-mode t)

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(setq indicate-empty-lines t
      require-final-newline t)

(setq mark-ring-max 64
      kill-ring-max 200
      global-mark-ring-max 128)


;;; Evil
(use-package module-evil)


;;; Shell
(use-package module-shell)
(use-package module-eshell)


;;; Buffer, Windows and Frames
(use-package module-fringe)
(use-package module-frame)
(use-package module-buffer)
(use-package module-ibuffer)
(use-package module-window)
(use-package module-desktop)
(use-package module-popwin)
(use-package popup :ensure t :defer t)
(use-package module-diminish)


;;; Files
(use-package module-file            ; Personal file tools
  :init
  (dotemacs-declare-prefix "fp" "personal tools")
  (dotemacs-set-leader-keys
    "fpD" 'dotemacs-delete-file-and-buffer
    "fpi" 'dotemacs-open-in-intellij
    "fpo" 'dotemacs-launch-dwim
    "fpR" 'dotemacs-rename-file-and-buffer
    "fpw" 'dotemacs-copy-filename-as-kill
    "fpu" 'dotemacs-find-user-init-file-other-window
    "fpw" 'dotemacs-browse-feature-url))


;;; Perspective, EyeBrowse, and Helm
(use-package module-perspective)
(use-package module-eyebrowse)
(use-package module-helm)

(use-package module-ignoramus)


;;; Navigation and scrolling
(use-package module-smooth-scrolling)
(use-package module-jumping)


;;; Editing
(use-package module-editing           ; Personal editing helpers
  ;; :bind (([remap kill-whole-line]        . dotemacs-smart-kill-whole-line)
  ;;        ([remap move-beginning-of-line] . dotemacs-back-to-indentation-or-beginning-of-line)
  ;;        ("C-<backspace>"                . dotemacs-smart-backward-kill-line)
  ;;        ("C-S-j"                        . dotemacs-smart-open-line)
  ;;        ;; Additional utilities
  ;;        ("C-c e d"                      . dotemacs-insert-current-date))
  )
(use-package module-whitespace)


;;; Evil Addons
(use-package evil-evilified-state :load-path "evil/")
(define-key evil-evilified-state-map (kbd dotemacs-leader-key)
  dotemacs-default-map)
(evilified-state-evilify-map package-menu-mode-map
  :mode package-menu-mode)
(evilified-state-evilify process-menu-mode process-menu-mode-map)
(use-package module-evil-packages)


;;; EMacs
(use-package module-emacs) ; Customization, init file and package management


;;; OSX support
(use-package module-osx              ; Personal OS X tools
  :if (eq system-type 'darwin))


;;; Key Bindings
(use-package bind-map
  :init
  (bind-map dotemacs-default-map
    :prefix-cmd dotemacs-cmds
    :keys (dotemacs-emacs-leader-key)
    :evil-keys (dotemacs-leader-key)
    :override-minor-modes t
    :override-mode-name dotemacs-leader-override-mode))
(use-package hydra
  :ensure t
  :init
  ;; turn off evil in corelv buffers
  (push '("\\*LV\\*") evil-buffer-regexps)

  (defun dotemacs//hydra-key-doc-function (key key-width doc doc-width)
    (format (format "[%%%ds] %%%ds" key-width (- -1 doc-width))
            key doc))
  (setq hydra-key-doc-function 'dotemacs//hydra-key-doc-function)
  (setq hydra-head-format "[%s] "))
(use-package module-key-bindings
  :config (dotemacs-toggle-transparency-core))



;;; Text Mode

;; `visual-line-mode` is so much better than `auto-fill-mode`. It doesn't
;; actually break the text into multiple lines - it only looks that way.
(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(use-package module-typography)
(use-package writeroom-mode             ; Distraction-free editing
  :ensure t
  :defer t)


;;; Markup Languages
(use-package module-markup-languages)
(use-package module-latex)
(use-package module-markdown)
(use-package module-pandoc)
(use-package module-asciidoc)
(use-package graphviz-dot-mode          ; Graphviz
  :ensure t
  :defer t
  :config
  (setq graphviz-dot-indent-width 4))


;;; Programming Languages, Utils
(use-package module-programming
  :init
  (progn
    (setq dotemacs-prog-mode-hook 'dotemacs-prog-mode-defaults)
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
(use-package module-clisp)
(use-package module-csv)
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
(use-package module-scheme)
(use-package module-shell-script)
(use-package module-semantic)
(use-package module-ycmd)

;; Misc programming languages
(use-package arduino-mode :defer t :ensure t)
(use-package faust-mode :defer t :ensure t :mode "\\.\\(dsp\\|lib\\)\\'")
(use-package scad-mode :defer t :ensure t)
(use-package qml-mode :defer t :ensure t :mode "\\.qml\\'")
(use-package julia-mode :defer t :ensure t)
(use-package matlab-mode :defer t :ensure t)
(use-package stan-mode :defer t :ensure t)
(use-package thrift :defer t :ensure t)
;; no associated extension because conflicts with more common Objective-C,
;; manually invoke for .m files.
(use-package wolfram-mode
  :defer t
  :ensure t
  :interpreter "\\(Wolfram\\|Mathematica\\)Script\\( -script\\)?")


;; Source Code tags, and metadata
;; e.g. etags, ebrowse, exuberant ctags, cscope, GNU Global and GTags
(use-package module-gtags)


;;; Version control
(use-package module-version-control)
(use-package module-git)
(use-package module-github)
(use-package module-magit)


;; Databases
(use-package module-sql)


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
(use-package module-ranger)
(use-package module-vinegar)
(use-package module-unimpaired)


;;; Virtual Machine/SysOp
(use-package module-virtual-machine)
(use-package module-ansible)
(use-package module-puppet)
(use-package module-vagrant)
(use-package module-nixos)
(use-package module-systemd)
(use-package module-tmux)


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


;;; Skeletons, completion and expansion
(use-package module-auto-complete)
(use-package module-hippie-exp)
(use-package module-yasnippet)


;;; Space line
(use-package module-spaceline)


;;; Date and time
(use-package calendar                   ; Built-in calendar
  :defer t
  :config
  ;; I start on Monday
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
(use-package module-net)


;;; Miscellaneous
(use-package module-location)
(use-package module-emoji)

(use-package google-translate
  :ensure t
  :init
  (progn
    (defun dotemacs/set-google-translate-languages (source target)
      "Set source language for google translate.
For instance pass En as source for English."
      (interactive
       "sEnter source language (ie. en): \nsEnter target language (ie. en): "
       source target)
      (message
       (format "Set google translate source language to %s and target to %s"
               source target))
      (setq google-translate-default-source-language (downcase source))
      (setq google-translate-default-target-language (downcase target)))
    (dotemacs-set-leader-keys
     "xgl" 'dotemacs/set-google-translate-languages
     "xgQ" 'google-translate-query-translate-reverse
     "xgq" 'google-translate-query-translate
     "xgT" 'google-translate-at-point-reverse
     "xgt" 'google-translate-at-point))
  (setq google-translate-enable-ido-completion t)
  (setq google-translate-show-phonetic t)
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "sp"))

(use-package spray          ; Speed Reading
  :commands spray-mode
  :ensure t
  :init
  (progn
    (defun speed-reading/start-spray ()
      "Start spray speed reading on current buffer at current point."
      (interactive)
      (evil-insert-state)
      (spray-mode t)
      (internal-show-cursor (selected-window) nil))
    (dotemacs-set-leader-keys "aR" 'speed-reading/start-spray)

    (defadvice spray-quit (after speed-reading//quit-spray activate)
      "Correctly quit spray."
      (internal-show-cursor (selected-window) t)
      (evil-normal-state)))
  :config
  (progn
    (define-key spray-mode-map (kbd "h") 'spray-backward-word)
    (define-key spray-mode-map (kbd "l") 'spray-forward-word)
    (define-key spray-mode-map (kbd "q") 'spray-quit)))

(dotemacs-use-package-add-hook which-key
  :post-init
  (push '("\\`speed-reading/\\(.+\\)\\'" . "\\1")
        which-key-description-replacement-alist))

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
