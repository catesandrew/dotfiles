;;; module-osx.el --- OSX Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;;; Code:

(require 'use-package)
(require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
(require 'core-display-init)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)


;; warning
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


;; variables

;; Use the OS X Emoji font for Emoticons
(when (fboundp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    '(#x1F600 . #x1F64F)
                    (font-spec :name "Apple Color Emoji") nil 'prepend))


;; keybindings
(dotemacs/set-leader-keys "bf" 'reveal-in-osx-finder)

;; this is only applicable to GUI mode
(dotemacs|do-after-display-system-init
 (when (display-graphic-p)
   (setq ns-pop-up-frames nil)
   (setq mac-control-modifier 'control)
   (setq mac-function-modifier 'hyper)

   ;; Treat command as super
   (setq mac-command-key-is-meta t)
   (setq mac-command-modifier 'super)

   ;; Treat option as meta
   (setq mac-option-key-is-meta t)
   (setq mac-option-modifier 'meta)

   ;; Keybindings
   (global-set-key (kbd "s-=") 'dotemacs/scale-up-font)
   (global-set-key (kbd "s--") 'dotemacs/scale-down-font)
   (global-set-key (kbd "s-0") 'dotemacs/reset-font-size)
   (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
   (global-set-key (kbd "s-v") 'yank)
   (global-set-key (kbd "s-c") 'evil-yank)
   (global-set-key (kbd "s-a") 'mark-whole-buffer)
   (global-set-key (kbd "s-x") 'kill-region)
   (global-set-key (kbd "s-w") 'delete-window)
   (global-set-key (kbd "s-W") 'delete-frame)
   (global-set-key (kbd "s-n") 'make-frame)
   (global-set-key (kbd "s-z") 'undo-tree-undo)
   (global-set-key (kbd "s-Z") 'undo-tree-redo)
   (global-set-key (kbd "s-s")
                   (lambda ()
                     (interactive)
                     (call-interactively (key-binding "\C-x\C-s"))))
   (global-set-key (kbd "s-Z") 'undo-tree-redo)
   (global-set-key (kbd "C-s-f") 'dotemacs/toggle-frame-fullscreen)
   ;; Emacs sometimes registers C-s-f as this weird keycode
   (global-set-key (kbd "<C-s-268632070>") 'dotemacs/toggle-frame-fullscreen)))


;; funcs

(defun dotemacs/id-of-bundle (bundle)
  "Get the ID of a BUNDLE.

BUNDLE is the user-visible name of the bundle as string.  Return
the id of the bundle as string.

These bundle IDs are normally constant.  Thus you may use this
function to determine the ID once, and then hard-code it in your
code."
  (let ((script (format "id of app \"%s\"" bundle)))
    (car (process-lines "osascript" "-e" script))))

(defun dotemacs/path-of-bundle (id)
  "Get the path of a bundle with ID.

ID is the bundle ID (see `dotemacs/id-of-bundle' as string.  Return
the directory path of the bundle as string."
  (let ((query (format "kMDItemCFBundleIdentifier == '%s'" id)))
    (car (process-lines "mdfind" query))))

(defun dotemacs/homebrew-prefix (&optional formula)
  "Get the homebrew prefix for FORMULA.

Without FORMULA, get the homebrew prefix itself.

Return nil, if homebrew is not available, or if the prefix
directory does not exist."
  (let ((prefix (ignore-errors (car (apply #'process-lines "brew" "--prefix"
                                           (when formula (list formula)))))))
    (when (and prefix (file-directory-p prefix))
      prefix)))

(defun dotemacs/homebrew-installed-p (&optional formula)
  "Determine whether a homebrew FORMULA is installed.

Without FORMULA determine whether Homebrew itself is available."
  (if formula
      (dotemacs/homebrew-prefix formula)
    (executable-find "brew")))

;; Open files
(defun dotemacs/open-current-file ()
  "Open current file using shell `open` command"
  (interactive)
  (shell-command (concat "open " (buffer-file-name))))

;; Get keychain password

;; If I'm on OS X, I can fetch passwords etc. from my Keychain. This is much
;; more secure than storing them in configuration on disk:
(defun dotemacs/chomp (str)
  "Chomp leading and tailing whitespace from `str'."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str))) str)

(defun dotemacs/get-keychain-password (account-name)
  "Get `account-name' keychain password from OS X Keychain"
  (interactive "sAccount name: ")
  (when (executable-find "security")
    (dotemacs/chomp
     (shell-command-to-string
      (concat
       "security find-generic-password -wa "
       account-name)))))


;; packages
(dotemacs-use-package-add-hook dired
  :post-config
  ;; OS X bsdtar is mostly compatible with GNU Tar
  (setq dired-guess-shell-gnutar "tar")
  ;; Use `gls' if `coreutils' was installed prefixed ('g') otherwise, leave
  ;; alone. Manually add to config `(setq dired-use-ls-dired nil)' to
  ;; suppress warnings.
  (let ((gls (executable-find "gls")))
    (when gls
      (setq insert-directory-program gls
            dired-listing-switches "-aBhl --group-directories-first"))))

;; Use `mdfind' instead of `locate'.
(dotemacs-use-package-add-hook helm
  :post-config
  ;; Disable fuzzy matchting to make mdfind work with helm-locate
  ;; https://github.com/emacs-helm/helm/issues/799
  (setq helm-locate-fuzzy-match nil)
  (setq helm-locate-command "mdfind -name %s %s"))

(use-package launchctl
  :defer t
  :ensure t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.plist$" . nxml-mode))
    (dotemacs-set-leader-keys "al" 'launchctl))
  :config
  (progn
    (evilified-state-evilify launchctl-mode launchctl-mode-map
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

;; Trash support for OS X
(use-package osx-trash
  :ensure t
  :init (osx-trash-setup))

(use-package pbcopy
  :if (not (display-graphic-p))
  :ensure t
  :init (turn-on-pbcopy))

;; Reveal current buffer in finder
(use-package reveal-in-osx-finder
  :ensure t
  :commands reveal-in-osx-finder)

(with-eval-after-load 'term
  (define-key term-raw-map (kbd "s-v") 'term-paste))

(provide 'module-osx)
;;; module-osx.el ends here
