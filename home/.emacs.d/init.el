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

;;________________________________________________________________
;;    Determine where we are
;;________________________________________________________________

(defvar system-type-as-string (prin1-to-string system-type))

(defvar on_windows_nt (string-match "windows-nt" system-type-as-string))
(defvar on_darwin     (string-match "darwin" system-type-as-string))
(defvar on_gnu_linux  (string-match "gnu/linux" system-type-as-string))
(defvar on_cygwin     (string-match "cygwin" system-type-as-string))
(defvar on_solaris    (string-match "usg-unix-v" system-type-as-string))

;; TODO: Remove Cask. Cask is yet another dependency). Great for making packages;
;; horrible for configuration management.
(require 'cask "~/.cask/cask.el")
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

(defcustom dotemacs-elisp-dir (expand-file-name "elisp" user-emacs-directory)
  "The storage location lisp."
  :group 'dotemacs)

(defcustom dotemacs-config-dir (expand-file-name "config" user-emacs-directory)
  "The config location lisp."
  :group 'dotemacs)

(defcustom dotemacs-user-settings-dir (concat user-emacs-directory "users/" user-login-name)
  "The currently logged in user's storage location for settings."
  :group 'dotemacs)

;; Set up load path(s)
(add-to-list 'load-path dotemacs-config-dir)
(add-to-list 'load-path dotemacs-elisp-dir)
(add-to-list 'load-path dotemacs-user-settings-dir)

;; Set up appearance early
(require 'init-appearance)

;; Add external projects to load path
(let ((base dotemacs-elisp-dir))
  (add-to-list 'load-path base)
  (dolist (dir (directory-files base t "^[^.]"))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))


;;; Package management

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(add-to-list 'package-archives '(("melpa" . "http://melpa.org/packages/")))
(add-to-list 'package-archives '(("org" . "http://orgmode.org/elpa/")))

;; The reason automatic package loading occurs after loading the init file is
;; that user options only receive their customized values after loading the init
;; file, including user options which affect the packaging system. In some
;; circumstances, you may want to load packages explicitly in your init file
;; (usually because some other code in your init file depends on a package). In
;; that case, your init file should call the function `package-initialize`. It is
;; up to you to ensure that relevant user options, such as `package-load-list`
;; (see below), are set up prior to the `package-initialize` call. You should also
;; set `package-enable-at-startup` to `nil`, to avoid loading the packages again
;; after processing the init file. Alternatively, you may choose to completely
;; inhibit package loading at startup, and invoke the command
;; `M-x package-initialize` to load your packages manually.
;; http://stackoverflow.com/questions/11127109/
(setq package-enable-at-startup nil)
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

;; Lets start with a smattering of sanity
(require 'init-sane-defaults)
(require 'init-util)

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Setup environment variables from the user's shell.
(when on_darwin
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(let ((debug-on-error t))
  (cl-loop for file in (directory-files (concat user-emacs-directory "config/"))
    if (not (file-directory-p file))
      do (require (intern (file-name-base file)))))

(autoload 'skewer-start "init-skewer" nil t)
(autoload 'skewer-demo "init-skewer" nil t)

(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; TODO convert to use-package https://github.com/jwiegley/use-package
(use-package init-macosx              ; Personal OS X tools
  :if (eq system-type 'darwin)
  :load-path "config/"
  :defer t)

;; TODO https://github.com/IvanMalison/org-projectile

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))
