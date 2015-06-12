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

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'set-fringe-mode) (set-fringe-mode -1))

;; TODO: Remove Cask. Cask is yet another dependency). Great for making packages;
;; horrible for configuration management.
; (require 'cask "~/.cask/cask.el")
; (cask-initialize)

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

(use-package paradox                    ; Better package menu
  :ensure t
  :bind (("C-c l p" . paradox-list-packages)
         ("C-c l P" . package-list-packages-no-fetch))
  :config
  ;; Don't ask for a token, please, and don't bug me about asynchronous updates
  (setq paradox-github-token t
        paradox-execute-asynchronously nil))

;; Set up appearance early
;; (require 'init-appearance)

;; Lets start with a smattering of sanity
;; (require 'init-sane-defaults)
;; (require 'init-util)

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


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

;; TODO convert to use-package https://github.com/jwiegley/use-package
; (use-package init-macosx              ; Personal OS X tools
;   :if (eq system-type 'darwin)
;   :load-path "config/"
;   :defer t)

;; TODO https://github.com/IvanMalison/org-projectile

; (add-hook 'c-mode-common-hook
;           (lambda ()
;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;               (ggtags-mode 1))))
