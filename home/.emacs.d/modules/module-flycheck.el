;;; module-flycheck.el --- Flycheck Module
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
(require 'core-use-package-ext)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)


;; variables
(defvar syntax-checking-enable-tooltips t
  "If non nil some feedback are displayed in tooltips.")

(defvar syntax-checking-enable-by-default t
  "Enable syntax-checking by default.")


;; funcs
(defun dotemacs/add-flycheck-hook (mode)
  "Use flycheck in MODE by default, if `syntax-checking-enable-by-default' is
true."
  (when (and syntax-checking-enable-by-default
             (listp flycheck-global-modes)
             (not (eq 'not (car flycheck-global-modes))))
    (push mode flycheck-global-modes)))


;; packages
(use-package flycheck                   ; On-the-fly syntax checking
  :ensure t
  :defer t
  :init
  (progn
    (setq flycheck-standard-error-navigation nil
          flycheck-global-modes nil
          flycheck-completion-system 'ido)

    ;; Make flycheck recognize packages in loadpath i.e (require 'company) will
    ;; not give an error now
    (setq flycheck-emacs-lisp-load-path 'inherit)

    (dotemacs-add-toggle syntax-checking
      :status flycheck-mode
      :on (flycheck-mode)
      :off (flycheck-mode -1)
      :documentation "Enable error and syntax checking."
      :evil-leader "ts")

    (dotemacs-diminish flycheck-mode " ⓢ" " s")

    (when syntax-checking-enable-by-default
      (global-flycheck-mode 1))

    ;; Custom fringe indicator
    (when (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'my-flycheck-fringe-indicator
        (vector #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00011100
                #b00111110
                #b00111110
                #b00111110
                #b00011100
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000)))

    (flycheck-define-error-level 'error
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-error)

    (flycheck-define-error-level 'warning
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-warning)

    (flycheck-define-error-level 'info
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-info)

    ;; toggle flycheck window
    (defun dotemacs/toggle-flycheck-error-list ()
      "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
      (interactive)
      (-if-let (window (flycheck-get-error-list-window))
          (quit-window nil window)
        (flycheck-list-errors)))

    (defun dotemacs/goto-flycheck-error-list ()
      "Open and go to the error list buffer."
      (interactive)
      (unless (get-buffer-window (get-buffer flycheck-error-list-buffer))
        (flycheck-list-errors))
      (switch-to-buffer-other-window flycheck-error-list-buffer))

    (evilified-state-evilify-map flycheck-error-list-mode-map
      :mode flycheck-error-list-mode
      :bindings
      "RET" 'flycheck-error-list-goto-error
      "j" 'flycheck-error-list-next-error
      "k" 'flycheck-error-list-previous-error)

    (dotemacs-set-leader-keys
      "ec" 'flycheck-clear
      "eh" 'flycheck-describe-checker
      "el" 'dotemacs/toggle-flycheck-error-list
      "eL" 'dotemacs/goto-flycheck-error-list
      "es" 'flycheck-select-checker
      "eS" 'flycheck-set-checker-executable
      "ev" 'flycheck-verify-setup)))

(use-package flycheck-pos-tip
  :ensure t
  :if syntax-checking-enable-tooltips
  :defer t
  :init
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

(dotemacs-use-package-add-hook popwin
  :post-config
  (push '("^\\*Flycheck.+\\*$"
          :regexp t
          :dedicated t
          :position bottom
          :stick t
          :noselect t)
        popwin:special-display-config))

(use-package helm-flycheck
  :ensure t
  :commands helm-flycheck
  :init (dotemacs-set-leader-keys "ef" 'helm-flycheck))

(provide 'module-flycheck)
;;; module-flycheck.el ends here
