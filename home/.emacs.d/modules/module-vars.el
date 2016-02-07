;;; module-vars.el --- Settings and variables
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;;; Code:

(require 'core-vars)

(defvar ycmd-server-command '("python" "/usr/local/src/ycmdycmd")
  "The path to the ycmd server.")

;; Regexp for useful and useless buffers for smarter buffer switching
(defvar dotemacs-useless-buffers-regexp '("*\.\+")
  "Regexp used to determine if a buffer is not useful.")

(defvar dotemacs-useful-buffers-regexp '("\\*scratch\\*")
  "Regexp used to define buffers that are useful despite matching
`emacs-useless-buffers-regexp'.")

;; from Prelude
(defvar dotemacs-indent-sensitive-modes
  '(coffee-mode
    python-mode
    slim-mode
    haml-mode
    yaml-mode
    makefile-mode
    makefile-gmake-mode
    makefile-imake-mode
    makefile-bsdmake-mode)
  "Modes for which auto-indenting is suppressed.")

(defconst dotemacs-editing-style 'vim
  "Always `vim', Evil is always enabled.")

(defvar dotemacs-line-numbers 'relative
  "If non nil line numbers are turned on in all `prog-mode' and `text-mode'.
derivatives.  If set to `relative', also turns on relative line numbers.")

(defconst dotemacs-auto-save-directory
  (expand-file-name (concat dotemacs-cache-directory "auto-save/"))
  "The auto-save directory.")

(defvar dotemacs-active-transparency 96
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's active or selected. Transparency
can be toggled through `toggle-transparency'.")

(defvar dotemacs-inactive-transparency 96
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's inactive or deselected. Transparency
can be toggled through `toggle-transparency'.")

(defvar dotemacs-smartparens-strict-mode nil
  "If non-nil smartparens-strict-mode will be enabled in programming modes.")

(defvar dotemacs-completion-engine 'company
  "The completion engine the use.  Use either `company` or `auto-complete`.")

(defvar auto-completion-private-snippets-directory nil
  "Configurable private snippets directory.")

(defvar python-fill-column 79
  "Fill column value for python buffers")

(defvar dotemacs-really-kill-emacs nil
  "prevent window manager close from closing instance.")

(defvar dotemacs-persistent-server nil
  "If non nil advises quit functions to keep server open when quitting.")

(defvar dotemacs-mode-line-unicode-symbols t
  "If non nil unicode symbols are displayed in the mode-line (eg. for lighters)")

(defvar dotemacs-fullscreen-at-startup nil
  "If non nil the frame is fullscreen when Emacs starts up (Emacs 24.4+ only).")

(defvar dotemacs-fullscreen-use-non-native nil
  "If non nil `dotemacs-toggle-fullscreen' will not use native fullscreen. Use
to disable fullscreen animations in OSX.")

(defvar dotemacs-maximized-at-startup nil
  "If non nil the frame is maximized when Emacs starts up (Emacs 24.4+ only).
Takes effect only if `dotemacs-fullscreen-at-startup' is nil.")

(defvar dotemacs-auto-save-file-location 'cache
  "Location where to auto-save files. Possible values are `original' to
auto-save the file in-place, `cache' to auto-save the file to another
file stored in the cache directory and `nil' to disable auto-saving.
Default value is `cache'.")

(defvar dotemacs-whitespace-cleanup 'changed
  "Delete whitespace while saving buffer.

Possible values are `all', `trailing', `changed' or `nil'.
Default is `changed' (cleanup whitespace on changed lines)")

(defvar dotemacs-show-trailing-whitespace nil)

(defvar dotemacs-highlight-delimiters 'all
  "Select a scope to highlight delimiters. Possible values are `any',
  `current', `all' or `nil'. Default is `all' (highlight any scope and
  emphasis the current one.")

(defvar dotemacs-private-dir (locate-user-emacs-file "private")
  "Directory for private settings.")

(defvar dotemacs-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(defvar dotemacs-default-layout-name "Default"
  " Name of the default layout.")

(defvar dotemacs-display-default-layout nil
  "If non nil the default layout name is displayed in the mode-line.")

(defvar dotemacs-auto-resume-layouts nil
  "If non nil then the last auto saved layouts are resume automatically upon
start.")

(defvar git-enable-magit-svn-plugin nil
  "If non nil `magit-svn' plugin is enabled.")

(defvar git-magit-status-fullscreen t
  "If non nil magit-status buffer is displayed in fullscreen.")

(defvar dotemacs//flycheck-executables-searched nil)
(defvar dotemacs//flycheck-executable-eslint nil)
(defvar dotemacs//flycheck-executable-jscs nil)
(defvar dotemacs//flycheck-executable-jshint nil)
(defvar dotemacs//flycheck-executable-tidy5 nil)

(defvar dotemacs-use-ido nil
  "If non nil then `ido' replaces `helm' for some commands. For now only
`find-files' (SPC f f) is replaced.")

;; only for backward compatibility
(defalias 'dotemacs-mode 'emacs-lisp-mode)

(provide 'module-vars)
;;; module-vars.el ends here
