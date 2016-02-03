;;; module-vars.el --- Settings and variables

;; This file is NOT part of GNU Emacs.

;;; License:

;;; Commentary:

;;; Code:

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

(provide 'module-vars)
;;; module-vars.el ends here
