;;; module-which-key.el --- Which Key Module
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
(require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

;; Display current keystrokes almost immediately in mini buffer
(setq echo-keystrokes 0.02)

(defvar dotemacs-which-key-delay 0.4
  "Delay in seconds starting from the last keystroke after which
the which-key buffer will be shown if you have not completed a
key sequence.  Setting this variable is equivalent to setting
`which-key-idle-delay'.")

;; Possible options should be: right bottom right-then-bottom
(defvar dotemacs-which-key-position 'bottom
  "Location of the which-key popup buffer. Possible choices are bottom,
right, and right-then-bottom. The last one will display on the
right if possible and fallback to bottom if not.")

(use-package which-key
  :ensure t
  :init
  (progn
    (dotemacs-add-toggle which-key
      :status which-key-mode
      :on (which-key-mode)
      :off (which-key-mode -1)
      :documentation
      "Display a buffer with available key bindings."
      :evil-leader "tK")

    (dotemacs-set-leader-keys "hk" 'which-key-show-top-level)

    (let ((new-descriptions
           ;; being higher in this list means the replacement is applied later
           '(
             ("dotemacs/\\(.+\\)" . "\\1")
             ("dotemacs-\\(.+\\)" . "\\1")
             ("projectile-\\(.+\\)" . "\\1")
             ("dotemacs-toggle-\\(.+\\)" . "\\1")
             ("select-window-\\([0-9]\\)" . "window \\1")
             ("dotemacs-alternate-buffer" . "last buffer")
             ("dotemacs-toggle-mode-line-\\(.+\\)" . "\\1")
             ("avy-goto-word-or-subword-1" . "avy word")
             ("shell-command" . "shell cmd")
             ("dotemacs-default-pop-shell" . "open shell")
             ("dotemacs-helm-project-smart-do-search-region-or-symbol" . "smart search w/input")
             ("dotemacs-helm-project-smart-do-search" . "smart search")
             ("evil-search-highlight-persist-remove-all" . "remove srch hlght")
             ("helm-descbinds" . "show keybindings")
             ("sp-split-sexp" . "split sexp")
             ("avy-goto-line" . "avy line")
             ("universal-argument" . "universal arg")
             ("er/expand-region" . "expand region")
             ("helm-apropos" . "apropos")
             ("evil-lisp-state-\\(.+\\)" . "\\1"))))
        (dolist (nd new-descriptions)
          ;; ensure the target matches the whole string
          (push (cons (concat "\\`" (car nd) "\\'") (cdr nd))
                which-key-description-replacement-alist)))

      (dolist (leader-key `(,dotemacs-leader-key ,dotemacs-emacs-leader-key))
        (which-key-add-key-based-replacements
         (concat leader-key " m")    "major mode commands"
         (concat leader-key " " dotemacs-command-key) "M-x"))

      (which-key-declare-prefixes
        dotemacs-leader-key '("root" . "dotemacs root")
        dotemacs-emacs-leader-key '("root" . "dotemacs root")
        (concat dotemacs-leader-key " m")
        '("major-mode-cmd" . "Major mode commands")
        (concat dotemacs-emacs-leader-key " m")
        '("major-mode-cmd" . "Major mode commands"))

      ;; disable special key handling for dotemacs, since it can be
      ;; disorienting if you don't understand it
      (pcase dotemacs-which-key-position
        (`right (which-key-setup-side-window-right))
        (`bottom (which-key-setup-side-window-bottom))
        (`right-then-bottom (which-key-setup-side-window-right-bottom)))

      (setq which-key-special-keys nil
            which-key-use-C-h-for-paging t
            which-key-prevent-C-h-from-cycling t
            which-key-echo-keystrokes 0.02
            which-key-max-description-length 32
            which-key-sort-order 'which-key-key-order-alpha
            which-key-idle-delay dotemacs-which-key-delay
            which-key-allow-evil-operators t)
      (which-key-mode)
      (dotemacs-diminish which-key-mode " Ⓚ" " K")))

(provide 'module-which-key)
;;; module-which-key.el ends here
