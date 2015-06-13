;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;;; Debugging
(setq message-log-max 10000)

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


(defcustom dotemacs-elisp-dir (expand-file-name "elisp" user-emacs-directory)
  "The storage location lisp."
  :group 'dotemacs)

(defcustom dotemacs-config-dir (expand-file-name "config" user-emacs-directory)
  "The config location lisp."
  :group 'dotemacs)

(defcustom dotemacs-user-settings-dir (concat user-emacs-directory "users/" user-login-name)
  "The currently logged in user's storage location for settings."
  :group 'dotemacs)

;; lunaryorn-private-dir
(defcustom dotemacs-private-dir (locate-user-emacs-file "private")
  "Directory for private settings."
  :group 'dotemacs)

;; lunaryorn-custom-file
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

(use-package init-util              ; Personal OS X tools
  :load-path "config/"
  )


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

    (exec-path-from-shell-initialize)

    (setq user-mail-address (getenv "EMAIL"))

    ;; Re-initialize the `Info-directory-list' from $INFOPATH. Since package.el
    ;; already initializes info, we need to explicitly add the $INFOPATH
    ;; directories to `Info-directory-list'.
    (after "info"
      (dolist (dir (parse-colon-path (getenv "INFOPATH")))
        (when dir
          (add-to-list 'Info-directory-list dir))))))


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
;; Answering just 'y' or 'n' will do
(fset 'yes-or-no-p #'y-or-n-p)
;; Opt out from the startup message in the echo area by simply disabling this
;; ridiculously bizarre thing entirely.
(fset 'display-startup-echo-area-message #'ignore)

(use-package init-scratch          ; My logo in the scratch buffer
  :commands (dotemacs-insert-logo
             dotemacs-insert-logo-into-scratch)
  :init (add-hook 'after-init-hook #'dotemacs-insert-logo-into-scratch))

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
  :bind (
         ;; Replace some standard bindings with Helm equivalents
         ([remap execute-extended-command] . helm-M-x)
         ([remap find-file]                . helm-find-files)
         ([remap switch-to-buffer]         . helm-mini)
         ([remap yank-pop]                 . helm-show-kill-ring)
         ([remap insert-register]          . helm-register)
         ([remap occur]                    . helm-occur)
         ;; Special helm bindings
         ("C-c b b"                        . helm-resume)
         ("C-c b C"                        . helm-colors)
         ("C-c b *"                        . helm-calcul-expression)
         ("C-c b M-:"                      . helm-eval-expression-with-eldoc)
         ;; Helm features in other maps
         ("C-c i"                          . helm-semantic-or-imenu)
         ("C-c h a"                        . helm-apropos)
         ("C-c h e"                        . helm-info-emacs)
         ("C-c h i"                        . helm-info-at-point)
         ("C-c h m"                        . helm-man-woman)
         ("C-c f r"                        . helm-recentf)
         ("C-c f l"                        . helm-locate-library))
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
                   :defer t)

      (after "projectile"
        (warn "projectile loaded, now load helm-projectile")
        (use-package helm-projectile
                     :ensure t
                     :defer t))

      (after "company"
        (warn "company loaded, now load helm-company")
        (use-package helm-company
                     :ensure t
                     :defer t)))

  :diminish helm-mode)

(use-package helm-unicode
  :ensure t
  :bind ("C-c b 8" . helm-unicode))


;;; Buffer, Windows and Frames
(setq frame-resize-pixelwise t          ; Resize by pixels
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

;; Keep backup and auto save files out of the way
(setq backup-directory-alist `((".*" . ,(concat dotemacs-cache-directory "backups")))
      auto-save-file-name-transforms `((".*" ,(concat dotemacs-cache-directory "backups") t))
      auto-save-list-file-prefix (concat dotemacs-cache-directory "auto-save-list/saves-"))

;; Delete files to trash
(setq delete-by-moving-to-trash t)

(use-package files
  ;; Revert the current buffer (re-read the contents from disk). Burying
  ;; a buffer (removing it from the current window and sending it to the bottom
  ;; of the stack) is very common for dismissing buffers.
  :bind (("C-c f u" . revert-buffer)
         ("C-c f y" . bury-buffer))
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
  :bind (("C-c f D" . dotemacs-delete-file-and-buffer)
         ("C-c f i" . dotemacs-open-in-intellij)
         ("C-c f o" . dotemacs-launch-dwim)
         ("C-c f R" . dotemacs-rename-file-and-buffer)
         ("C-c f w" . dotemacs-copy-filename-as-kill)
         ("C-c f u" . dotemacs-find-user-init-file-other-window)
         ("C-c w ." . dotemacs-browse-feature-url)))

;;; Additional bindings for built-ins
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

(use-package imenu-anywhere             ; IDO-based imenu across open buffers
  ;; The Helm matching doesn't seem to work properly…
  :disabled t
  :ensure t
  :bind (("C-c i" . helm-imenu-anywhere)))

(use-package nlinum                     ; Line numbers in display margin
  :ensure t
  :bind (("C-c t l" . nlinum-mode)))


;;; Basic editing

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


; ;;; Skeletons, completion and expansion
;
; ;; In `completion-at-point', do not pop up silly completion buffers for less
; ;; than five candidates.  Cycle instead.
; (setq completion-cycle-threshold 5)
;
; (use-package hippie-exp                 ; Powerful expansion and completion
;   :bind (([remap dabbrev-expand] . hippie-expand))
;   :config
;   (setq hippie-expand-try-functions-list
;         '(try-expand-dabbrev
;           try-expand-dabbrev-all-buffers
;           try-expand-dabbrev-from-kill
;           try-complete-file-name-partially
;           try-complete-file-name
;           try-expand-all-abbrevs
;           try-expand-list
;           try-complete-lisp-symbol-partially
;           try-complete-lisp-symbol
;           lunaryorn-try-complete-lisp-symbol-without-namespace)))
;
; (use-package lunaryorn-hippie-exp       ; Custom expansion functions
;   :load-path "lisp/"
;   :commands (lunaryorn-try-complete-lisp-symbol-without-namespace))
;
; (use-package company                    ; Graphical (auto-)completion
;   :ensure t
;   :init (global-company-mode)
;   :config
;   (progn
;     (setq company-tooltip-align-annotations t
;           ;; Easy navigation to candidates with M-<n>
;           company-show-numbers t))
;   :diminish company-mode)
;
; (use-package company-statistics
;   :ensure t
;   :defer t
;   :init (company-statistics-mode))
;
; (use-package company-math               ; Completion for Math symbols
;   :ensure t
;   :defer t
;   :init (with-eval-after-load 'company
;           ;; Add backends for math characters
;           (add-to-list 'company-backends 'company-math-symbols-unicode)
;           (add-to-list 'company-backends 'company-math-symbols-latex)))
;
; (use-package helm-company
;   :ensure t
;   :defer t
;   :init (with-eval-after-load 'company
;           ;; Use Company for completion
;           (bind-key [remap completion-at-point] #'helm-company company-mode-map)
;           (bind-key "C-:" #'helm-company company-mode-map)
;           (bind-key "C-:" #'helm-company company-active-map)))
;
; 
; ;;; Spelling and syntax checking
; (use-package ispell                     ; Spell checking
;   :defer t
;   :config
;   (progn
;     (setq ispell-program-name (if (eq system-type 'darwin)
;                                   (executable-find "aspell")
;                                 (executable-find "hunspell"))
;           ispell-dictionary "en_GB"     ; Default dictionnary
;           ispell-silently-savep t       ; Don't ask when saving the private dict
;           ;; Increase the height of the choices window to take our header line
;           ;; into account.
;           ispell-choices-win-default-height 5)
;
;     (unless ispell-program-name
;       (warn "No spell checker available.  Install Hunspell or ASpell for OS X."))))
;
; (use-package flyspell                   ; On-the-fly spell checking
;   :bind (("C-c t s" . flyspell-mode))
;   :init (progn (dolist (hook '(text-mode-hook message-mode-hook))
;                  (add-hook hook 'turn-on-flyspell))
;                (add-hook 'prog-mode-hook 'flyspell-prog-mode))
;   :config
;   (progn
;     (setq flyspell-use-meta-tab nil
;           ;; Make Flyspell less chatty
;           flyspell-issue-welcome-flag nil
;           flyspell-issue-message-flag nil)
;
;     ;; Free C-M-i for completion
;     (define-key flyspell-mode-map "\M-\t" nil))
;   :diminish (flyspell-mode . "✓"))
;
; (use-package flycheck                   ; On-the-fly syntax checking
;   :ensure t
;   :bind (("C-c l e" . list-flycheck-errors)
;          ("C-c t f" . flycheck-mode))
;   :init (global-flycheck-mode)
;   :config (progn
;             (setq flycheck-display-errors-function
;                   #'flycheck-display-error-messages-unless-error-list)
;
;             ;; Use italic face for checker name
;             (set-face-attribute 'flycheck-error-list-checker-name nil
;                                 :inherit 'italic)
;
;             (add-to-list 'display-buffer-alist
;                          `(,(rx bos "*Flycheck errors*" eos)
;                            (display-buffer-reuse-window
;                             display-buffer-in-side-window)
;                            (side            . bottom)
;                            (reusable-frames . visible)
;                            (window-height   . 0.4))))
;   :diminish flycheck-mode)
;
; (use-package helm-flycheck
;   :ensure t
;   :bind (("C-c ! L" . helm-flycheck)))
;
; (use-package lunaryorn-flycheck         ; Personal Flycheck helpers
;   :defer t
;   :commands (lunaryorn-discard-undesired-html-tidy-error
;              lunaryorn-flycheck-mode-line-status)
;   :init (with-eval-after-load 'flycheck
;           ;; Don't highlight undesired errors from html tidy
;           (add-hook 'flycheck-process-error-functions
;                     #'lunaryorn-discard-undesired-html-tidy-error)
;
;           (setq flycheck-mode-line
;                 '(:eval (lunaryorn-flycheck-mode-line-status)))))
;
; 
; ;;; Text editing
; (use-package tildify
;   :bind (("C-c e t" . tildify-region))
;   :init (dolist (hook '(markdown-mode-hook
;                         latex-mode-hook
;                         rst-mode-hook))
;           (add-hook hook #'tildify-mode))
;   ;; Use the right space for LaTeX
;   :config (add-hook 'latex-mode-hook
;                     (lambda () (setq-local tildify-space-string "~"))))
;
; (use-package typo
;   :ensure t
;   :bind (("C-c t t" . typo-mode))
;   :init (progn
;           (typo-global-mode)
;
;           (dolist (hook '(markdown-mode-hook
;                           rst-mode-hook))
;             (add-hook hook 'typo-mode)))
;   :diminish (typo-mode . "𝕿"))



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

(use-package toml-mode                  ; TOML
  :ensure t
  :defer t
  :mode ("\\.toml$" . toml-mode)
  :config
    (add-hook 'toml-mode-hook
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




;; Lets start with a smattering of sanity
;; (require 'init-sane-defaults)


(let ((debug-on-error t))
  ;; (cl-loop for file in (directory-files (concat user-emacs-directory "config/"))
  ;;   if (not (file-directory-p file))
  ;;     do (require (intern (file-name-base file)))))
  ;; (require 'init-core)

  ;; (require 'init-eshell)
  ;; (require 'init-erc)

  ;; (if (eq dotemacs-completion-engine 'company)
  ;;     (require 'init-company)
  ;;   (require 'init-auto-complete))

  ;; (require 'init-ido)
  ;; (require 'init-org)
  ;; (require 'init-dired)
  ;; (require 'init-magit)
  ;; (require 'init-vcs)
  ;; (require 'init-rgrep)
  ;; (require 'init-shell)
  ;; (require 'init-perspective)
  ;; (require 'init-ffip)

  ;; (require 'init-programming)
  ;; (require 'init-lisp)
  ;; (require 'init-vim)
  ;; (require 'init-stylus)
  ;; (require 'init-js)
  ;; (require 'init-clojure)
  ;; (require 'init-go)
  ;; (require 'init-web)
  ;; (require 'init-markup)

  ;; (require 'init-projectile)
  ;; (require 'init-helm)
  ;; (require 'init-flycheck)
  ;; (require 'init-yasnippet)
  ;; (require 'init-smartparens)
  ;; (require 'init-mustache)
  ;; (require 'init-hbs)
  ;; (require 'init-misc)

  ;; (require 'init-evil)
  ;; (require 'init-macros)
  ;; (require 'init-eyecandy)

  ;; (require 'init-bindings)
  )

;; (autoload 'skewer-start "init-skewer" nil t)
;; (autoload 'skewer-demo "init-skewer" nil t)

; (setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
; (dolist (file (directory-files defuns-dir t "\\w+"))
;   (when (file-regular-p file)
;     (load file)))

;; TODO https://github.com/IvanMalison/org-projectile

; (add-hook 'c-mode-common-hook
;           (lambda ()
;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;               (ggtags-mode 1))))
