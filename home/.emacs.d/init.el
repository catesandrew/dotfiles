;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'set-fringe-mode) (set-fringe-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

(defvar current-user
      (getenv
       (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Emacs is powering up… Be patient, Master %s!" current-user)

;;________________________________________________________________
;;    Determine where we are
;;________________________________________________________________

(defvar system-type-as-string (prin1-to-string system-type))

(defvar on_windows_nt (string-match "windows-nt" system-type-as-string))
(defvar on_darwin     (string-match "darwin" system-type-as-string))
(defvar on_gnu_linux  (string-match "gnu/linux" system-type-as-string))
(defvar on_cygwin     (string-match "cygwin" system-type-as-string))
(defvar on_solaris    (string-match "usg-unix-v" system-type-as-string))

;; $brew --prefix cask --> /usr/local/opt/cask
(require 'cask "/usr/local/opt/cask/cask.el")
(cask-initialize)

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

(defcustom dotemacs-settings-dir (expand-file-name "settings" user-emacs-directory)
  "The storage location for settings."
  :group 'dotemacs)

(defcustom dotemacs-user-settings-dir (concat user-emacs-directory "users/" user-login-name)
  "The currently logged in user's storage location for settings."
  :group 'dotemacs)

;; Set up load path
(add-to-list 'load-path dotemacs-settings-dir)
(add-to-list 'load-path dotemacs-elisp-dir)
(add-to-list 'load-path (concat user-emacs-directory "/config"))

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Set up appearance early
(require 'appearance)

;; Settings for currently logged in user
(add-to-list 'load-path dotemacs-user-settings-dir)

;; Add external projects to load path
; (let ((base (concat user-emacs-directory "/elisp")))
(let ((base (concat user-emacs-directory "/elisp")))
  (add-to-list 'load-path base)
  (dolist (dir (directory-files base t "^[^.]"))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

(require 'cl)

;; Setup packages
(require 'init-packages)

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup environment variables from the user's shell.
(when on_darwin
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(let ((debug-on-error t))
  (require 'init-util)
  (require 'init-core)

  (require 'init-eshell)
  (require 'init-erc)

  (if (eq dotemacs-completion-engine 'company)
      (require 'init-company)
    (require 'init-auto-complete))

  (require 'init-ido)
  (require 'init-org)
  (require 'init-dired)
  (require 'init-magit)
  (require 'init-vcs)
  (require 'init-rgrep)
  (require 'init-shell)
  (require 'init-perspective)
  (require 'init-ffip)

  (require 'init-programming)
  (require 'init-lisp)
  (require 'init-vim)
  (require 'init-stylus)
  (require 'init-js)
  (require 'init-clojure)
  (require 'init-go)
  (require 'init-web)
  (require 'init-markup)

  (require 'init-projectile)
  (require 'init-helm)
  (require 'init-flycheck)
  ; (require 'init-yasnippet)
  (require 'init-smartparens)
  (require 'init-mustache)
  (require 'init-hbs)
  (require 'init-misc)

  (require 'init-evil)
  (require 'init-macros)
  (require 'init-eyecandy)
  (require 'init-overrides)

  (require 'init-bindings))

(autoload 'skewer-start "init-skewer" nil t)
(autoload 'skewer-demo "init-skewer" nil t)

(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(when on_darwin
  (require 'mac-osx))

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))
