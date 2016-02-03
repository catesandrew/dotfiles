;;; module-flyspell.el --- Flyspell Module
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

(use-package flyspell                   ; On-the-fly spell checking
  :defer t
  :init
  (progn
    (setq flyspell-use-meta-tab nil
          flyspell-issue-welcome-flag nil  ;; Make Flyspell less chatty
          flyspell-issue-message-flag nil)
    (dolist (mode '(org-mode text-mode message-mode))
      (spell-checking/add-flyspell-hook mode))

    (dotemacs-add-toggle spelling-checking
      :status flyspell-mode
      :on (if (derived-mode-p 'prog-mode)
              (flyspell-prog-mode)
            (flyspell-mode))
      :off (progn
             (flyspell-mode-off)
             ;; Also disable auto-dictionary when disabling spell-checking.
             (when (fboundp 'auto-dictionary-mode) (auto-dictionary-mode -1)))
      :documentation "Enable automatic spell checking."
      :evil-leader "tS")

    (dotemacs-set-leader-keys
      "Sb" 'flyspell-buffer
      "Sd" 'spell-checing/change-dictionary
      "Sn" 'flyspell-goto-next-error))
  :config
  (progn
    ;; Undefine mouse buttons which get in the way
    (define-key flyspell-mouse-map [down-mouse-2] nil)
    (define-key flyspell-mouse-map [mouse-2] nil)
    (dotemacs-diminish flyspell-mode " Ⓢ" " S")))

(use-package helm-flyspell
  :ensure t
  :commands helm-flyspell-correct
  :init (dotemacs-set-leader-keys "Sc" 'helm-flyspell-correct))

(provide 'module-flyspell)
;;; module-flyspell.el ends here
