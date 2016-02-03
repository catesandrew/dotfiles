;;; module-global.el --- Global functions and utilities
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:

;;; Code:

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

(provide 'module-global)
;;; module-global.el ends here
