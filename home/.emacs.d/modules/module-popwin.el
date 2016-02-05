;;; module-popwin.el --- Popwin Module
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

(use-package popwin
  :ensure t
  :config
  (progn
    (popwin-mode 1)
    (dotemacs-set-leader-keys "wpm" 'popwin:messages)
    (dotemacs-set-leader-keys "wpp" 'popwin:close-popup-window)

    ;; don't use default value but manage it ourselves
    (setq popwin:special-display-config nil)

    ;; buffers that we manage
    (push '("*Help*"                 :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
    (push '("*compilation*"          :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
    (push '("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("*Async Shell Command*"  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '(" *undo-tree*"           :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
    (push '("*ert*"                  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("*grep*"                 :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("*nosetests*"            :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("^\\*WoMan.+\\*$" :regexp t           :position bottom                                   ) popwin:special-display-config)
    (push '("^\\*Flycheck.+\\*$" :regexp t
                                     :dedicated t :position bottom :stick t :noselect t              ) popwin:special-display-config)
      ;; Pin the weather forecast to the bottom window
    (push '("*Sunshine*"             :dedicated t :position bottom                                   ) popwin:special-display-config)
    ;; add cider error to popwin special buffers
    (push '("*cider-error*"          :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
    ;; add cider-doc to popwin
    (push '("*cider-doc*"            :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
    (push '("*elm*" :tail t :noselect t) popwin:special-display-config)
    (push '("*elm-make*" :tail t :noselect t) popwin:special-display-config)
    (push '("*rspec-compilation*"    :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)

    (defun dotemacs-remove-popwin-display-config (str)
      "Removes the popwin display configurations that matches the passed STR"
      (setq popwin:special-display-config
            (-remove (lambda (x) (if (and (listp x) (stringp (car x)))
                                     (string-match str (car x))))
                     popwin:special-display-config)))))

(provide 'module-popwin)
;;; module-popwin.el ends here
