;;; module-spell-checking.el --- Spell Checking Module
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
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(defun spell-checking/change-dictionary ()
  "Change the dictionary. Use the ispell version if
auto-dictionary is not used, use the adict version otherwise."
  (interactive)
  (if (fboundp 'addict-change-dictionary)
      (adict-change-dictionary)
    (call-interactively 'ispell-change-dictionary)))

(use-package ispell                     ; Spell checking
  :defer t
  :config
  (progn
    (setq ispell-program-name
          (or (executable-find "aspell")
              (executable-find "hunspell"))

          ; ispell-extra-args '("--sug-mode=ultra")
          ispell-dictionary "en_US"     ; Default dictionnary
          ispell-silently-savep t       ; Don't ask when saving the private dict
          ;; Increase the height of the choices window to take our header line
          ;; into account.
          ispell-choices-win-default-height 5)

    (unless ispell-program-name
      (warn "No spell checker available. Install Hunspell or ASpell."))))

(use-package define-word
  :defer t
  :ensure t
  :init
  (dotemacs-set-leader-keys
    "xwd" 'define-word-at-point))

(use-package auto-dictionary
  :if spell-checking-enable-auto-dictionary
  :defer t
  :ensure t
  :init
  (progn
    (add-hook 'flyspell-mode-hook 'auto-dictionary-mode)
    ;; Select the buffer local dictionary if it was set, otherwise
    ;; auto-dictionary will replace it with a guessed one at each activation.
    ;; https://github.com/nschum/auto-dictionary-mode/issues/5
    (defun dotemacs//adict-set-local-dictionary ()
      "Set the local dictionary if not nil."
      (when (and (fboundp 'adict-change-dictionary)
                 ispell-local-dictionary)
        (adict-change-dictionary ispell-local-dictionary)))

    (add-hook 'auto-dictionary-mode-hook
              'dotemacs//adict-set-local-dictionary 'append)))

(provide 'module-spell-checking)
;;; module-spell-checking.el ends here
