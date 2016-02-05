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

;; Regexp for useful and useless buffers for smarter buffer switching
(defvar dotemacs-useless-buffers-regexp '("*\.\+")
  "Regexp used to determine if a buffer is not useful.")
(defvar dotemacs-useful-buffers-regexp '("\\*scratch\\*")
  "Regexp used to define buffers that are useful despite matching
`spacemacs-useless-buffers-regexp'.")

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

(defvar auto-completion-return-key-behavior 'complete
  "What the RET key should do when auto-completion menu is active.
Possible values are `complete' or `nil'.")

(defvar auto-completion-enable-snippets-in-popup t
  "If non nil show snippets in the auto-completion popup.")

(defvar auto-completion-tab-key-behavior 'cycle
  "What the TAB key should do when auto-completion menu is active.
Possible values are `complete', `cycle' or `nil'.")

(defvar auto-completion-complete-with-key-sequence nil
  "Provide a key sequence (string) to complete the current
selection.")

(defvar auto-completion-enable-sort-by-usage t
  "If non nil suggestions are sorted by how often they are used.")

(defvar auto-completion-enable-help-tooltip t
  "If non nil the docstring appears in a tooltip.")

(defvar company-mode-completion-cancel-keywords
  '("do"
    "then"
    "begin"
    "case")
  "Keywords on which to cancel completion so that you can use RET
to complet without blocking common line endings.")

(defvar dotemacs-ac-private-snippets-directory nil
  "Configurable private snippets directory.")

(defvar dotemacs-s-syntax-checking-enable-tooltips t
  "If non nil some feedback are displayed in tooltips.")

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

(defvar dotemacs-persistent-server nil
  "If non nil advises quit functions to keep server open when quitting.")

(defvar dotemacs-mode-line-unicode-symbols t
  "If non nil unicode symbols are displayed in the mode-line (eg. for lighters)")

(defvar dotemacs-fullscreen-use-non-native nil
  "If non nil `dotemacs-toggle-fullscreen' will not use native fullscreen. Use
to disable fullscreen animations in OSX.")

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

(defvar dotemacs-c-c++-enable-clang-support t
  "If non nil Clang related packages and configuration are enabled.")

(defvar dotemacs-c-c++-default-mode-for-headers 'c-mode
  "Default mode to open header files. Can be `c-mode' or `c++-mode'.")

;; Use GHCI NG from https://github.com/chrisdone/ghci-ng
(defvar dotemacs-haskell-enable-ghci-ng-support nil
  "If non-nil ghci-ng support is enabled")

(defvar dotemacs-haskell-enable-shm-support nil
  "If non-nil structured-haskell-mode support is enabled")

(defvar dotemacs-haskell-enable-hindent-style 'fundamental
  "Style to use for formatting with hindent; available are: fundamental
johan-tibell chris-done gibiansky. If nil hindent is disabled.")

(defvar dotemacs-haskell-enable-ghc-mod-support t
  "If non-nil ghc-mod support is enabled")

(defvar dotemacs-git-enable-magit-svn-plugin nil
  "If non nil `magit-svn' plugin is enabled.")

(defvar dotemacs-git-magit-status-fullscreen t
  "If non nil magit-status buffer is displayed in fullscreen.")

(defvar dotemacs-colors-engine 'rainbow
  "The color identifier engine to use.  Either `rainbow` or `color`.")

(defvar dotemacs-colors-theme-identifiers-sat&light
  '((jazz . (50 55))
    (gotham . (45 60))
    (leuven . (100 40))
    (material . (95 105))
    (monokai . (55 60))
    (solarized-dark . (65 55))
    (solarized-light . (60 55))
    (zenburn . (40 65)))
  "alist of theme symbols and pair of saturation and lightness values.")

(defvar dotemacs//flycheck-executables-searched nil)
(defvar dotemacs//flycheck-executable-eslint nil)
(defvar dotemacs//flycheck-executable-jscs nil)
(defvar dotemacs//flycheck-executable-jshint nil)
(defvar dotemacs//flycheck-executable-tidy5 nil)

(provide 'module-vars)
;;; module-vars.el ends here
