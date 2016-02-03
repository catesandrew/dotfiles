;;; core-vars.el --- Settings and variables

;; This file is NOT part of GNU Emacs.

;;; License:

;;; Commentary:

;;; Code:

(defconst dotemacs-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "The storage location for various persistent files.")

(defconst emacs-version-short (replace-regexp-in-string
                               "\\([0-9]+\\)\\.\\([0-9]+\\).*"
                               "\\1_\\2" emacs-version))

(defvar dotemacs-leader-key ","
  "The leader key.")

(defvar dotemacs-emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'")

;; major-mode-leader to 0x00A0 (NO_BREAK_SPACE)
(defvar dotemacs-major-mode-leader-key "\\"
  "Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m`. Set it to `nil` to disable it.")

(defvar dotemacs-major-mode-emacs-leader-key nil
  "Major mode leader key accessible in `emacs state' and `insert state'")

(defvar dotemacs-command-key ":"
  "The key used for Evil commands (ex-commands) and Emacs commands (M-x).
By default the command key is `:' so ex-commands are executed like in Vim
with `:' and Emacs commands are executed with `<leader> :'.")

(defvar dotemacs-remap-Y-to-y$ t
  "If non nil `Y' is remapped to `y$'.")

(defvar dotemacs-ex-substitute-global nil
  "If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.")

(provide 'core-vars)
;;; core-vars.el ends here
