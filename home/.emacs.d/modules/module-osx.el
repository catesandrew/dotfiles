;;; module-osx.el --- OSX Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-vars)
;; (require 'core-funcs)
;; (require 'core-keybindings)
;; (require 'core-display-init)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

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

(defun dotemacs-id-of-bundle (bundle)
  "Get the ID of a BUNDLE.

BUNDLE is the user-visible name of the bundle as string.  Return
the id of the bundle as string.

These bundle IDs are normally constant.  Thus you may use this
function to determine the ID once, and then hard-code it in your
code."
  (let ((script (format "id of app \"%s\"" bundle)))
    (car (process-lines "osascript" "-e" script))))

(defun dotemacs-path-of-bundle (id)
  "Get the path of a bundle with ID.

ID is the bundle ID (see `my-id-of-bundle' as string.  Return
the directory path of the bundle as string."
  (let ((query (format "kMDItemCFBundleIdentifier == '%s'" id)))
    (car (process-lines "mdfind" query))))

(defun dotemacs-homebrew-prefix (&optional formula)
  "Get the homebrew prefix for FORMULA.

Without FORMULA, get the homebrew prefix itself.

Return nil, if homebrew is not available, or if the prefix
directory does not exist."
  (let ((prefix (ignore-errors (car (apply #'process-lines "brew" "--prefix"
                                           (when formula (list formula)))))))
    (when (and prefix (file-directory-p prefix))
      prefix)))

(defun dotemacs-homebrew-installed-p (&optional formula)
  "Determine whether a homebrew FORMULA is installed.

Without FORMULA determine whether Homebrew itself is available."
  (if formula
      (my-homebrew-prefix formula)
    (executable-find "brew")))

;; Open files
(defun dotemacs-open-current-file ()
  "Open current file using shell `open` command"
  (interactive)
  (shell-command (concat "open " (buffer-file-name))))

;; Get keychain password

;; If I'm on OS X, I can fetch passwords etc. from my Keychain. This
;; is much more secure than storing them in configuration on disk:

(defun dotemacs-chomp (str)
  "Chomp leading and tailing whitespace from `str'."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str))) str)

(defun dotemacs-get-keychain-password (account-name)
  "Get `account-name' keychain password from OS X Keychain"
  (interactive "sAccount name: ")
  (when (executable-find "security")
    (dotemacs-chomp
     (shell-command-to-string
      (concat
       "security find-generic-password -wa "
       account-name)))))

(use-package ns-win                     ; OS X window support
  :defer t
  :init
  (progn
    ;; Use the OS X Emoji font for Emoticons
    (set-fontset-font "fontset-default"
                      '(#x1F600 . #x1F64F)
                      (font-spec :name "Apple Color Emoji") nil 'prepend)

    (dotemacs-set-leader-keys "bf" 'reveal-in-osx-finder)

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

(use-package osx-trash                  ; Trash support for OS X
  :ensure t
  :init (osx-trash-setup))

(use-package pbcopy
  :if (unless (display-graphic-p))
  :ensure t
  :init (turn-on-pbcopy))

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

(use-package reveal-in-osx-finder           ; Reveal current buffer in finder
  :ensure t
  :commands reveal-in-osx-finder)

(dotemacs-use-package-add-hook helm
  ;; Use `mdfind' instead of `locate'.
  :pre-init
  ;; Disable fuzzy matchting to make mdfind work with helm-locate
  ;; https://github.com/emacs-helm/helm/issues/799
  (setq helm-locate-fuzzy-match nil)
  (setq helm-locate-command "mdfind -name %s %s"))

(provide 'module-osx)
;;; module-osx.el ends here
