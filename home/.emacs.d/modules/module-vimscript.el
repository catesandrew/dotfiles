;;; module-vimscript.el --- VIMScript Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'use-package)
(require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
(require 'core-auto-completion)
(require 'core-use-package-ext)
(require 'core-fonts-support)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(use-package vimrc-mode
  :mode "\\.vim[rc]?\\'"
  :mode "_vimrc\\'"
  :defer t
  :init
  (progn
    (defun dotemacs//vimrc-mode-hook ()
      "Hooked function for `vimrc-mode-hook'."
      (highlight-numbers-mode -1)
      (rainbow-delimiters-mode-disable))
    (add-hook 'vimrc-mode-hook 'dotemacs//vimrc-mode-hook)))

(use-package dactyl-mode
  :mode "pentadactylrc\\'"
  :mode "vimperatorrc\\'"
  :mode "_pentadactylrc\\'"
  :mode "_vimperatorrc\\'"
  :mode "\\.penta\\'"
  :mode "\\.vimp\\'"
  :defer t)

(provide 'module-vimscript)
;;; module-vimscript.el ends here
