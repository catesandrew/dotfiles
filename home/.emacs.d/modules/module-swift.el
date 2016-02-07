;;; module-swift.el --- Swift Module
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

(dotemacs-use-package-add-hook flycheck
  :post-config
  (add-to-list 'flycheck-checkers 'swift))

(use-package swift-mode
  :mode ("\\.swift\\'" . swift-mode)
  :defer t
  :init
  (progn
    (dotemacs-advise-commands "store-initial-buffer-name"
                              (swift-mode-run-repl) around
                              "Store current buffer bane in bufffer local variable,
before activiting or switching to REPL."
                              (let ((initial-buffer (current-buffer)))
                                ad-do-it
                                (with-current-buffer swift-repl-buffer
                                  (setq swift-repl-mode-previous-buffer initial-buffer))))

    (defun dotemacs/swift-repl-mode-hook ()
      "Hook to run when starting an interactive swift mode repl"
      (make-variable-buffer-local 'swift-repl-mode-previous-buffer))
    (add-hook 'swift-repl-mode-hook 'spacemacs/swift-repl-mode-hook)

    (defun dotemacs/swift-repl-mode-switch-back ()
      "Switch back to from REPL to editor."
      (interactive)
      (if swift-repl-mode-previous-buffer
          (switch-to-buffer-other-window swift-repl-mode-previous-buffer)
        (message "No previous buffer"))))
  :config
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'swift-mode
      "msS" 'swift-mode-run-repl      ; run or switch to an existing swift repl
      "mss" 'swift-mode-run-repl
      "msb" 'swift-mode-send-buffer
      "msr" 'swift-mode-send-region)

    (with-eval-after-load 'swift-repl-mode-map
      ;; Switch back to editor from REPL
      (dotemacs-set-leader-keys-for-major-mode 'swift-repl-mode
        "mss"  'dotemacs/swift-repl-mode-switch-back)
      (define-key swift-repl-mode-map
        (kbd "C-c C-z") 'dotemacs/swift-repl-mode-switch-back))))

(provide 'module-swift)
;;; module-swift.el ends here
