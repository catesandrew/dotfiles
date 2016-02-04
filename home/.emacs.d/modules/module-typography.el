;;; module-typography.el --- Typography Module
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
(defvar typography-enable-typographic-editing nil
  "If non-nil automatically enable typographic editing.")

(use-package typo
  :defer t
  :ensure t
  :init
  (progn
    (when typography-enable-typographic-editing
      (add-hook 'text-mode-hook 'typo-mode))

    (dotemacs-add-toggle typographic-substitutions
      :status typo-mode
      :on (typo-mode)
      :off (typo-mode -1)
      :documentation "Enable typographic substitutions"
      :evil-leader "tT")
    (dotemacs-diminish typo-mode " Ⓣ" " T"))
  :config (setq-default typo-language "English"))

(use-package tildify
  :defer t
  :init
  (progn
    (when typography-enable-typographic-editing
      (add-hook 'text-mode-hook 'tildify-mode))

    (dotemacs-set-leader-keys
     "x~" 'tildify-region)

    ;; Use the symbolic non-breaking space for LaTeX
    (defun typography/tildify-latex-space ()
      "Set tildify space for LaTeX"
      (setq-local tildify-space-string "~"))
    (add-hook 'LaTeX-mode-hook 'typography/tildify-latex-space)

    (dotemacs-add-toggle tildify-space
      :status tildify-mode
      :on (tildify-mode)
      :off (tildify-mode -1)
      :documentation "Enable electric non-breaking space"
      :evil-leader "t~")
    (dotemacs-diminish tildify-mode " ~" " ~")))

(provide 'module-typography)
;;; module-typography.el ends here
