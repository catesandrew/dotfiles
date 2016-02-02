;;; gtags
(require 'module-global)

(defun helm-gtags-dwim-other-window ()
  "helm-gtags-dwim in the other window"
  (interactive)
  (let ((helm-gtags--use-otherwin t)
        (split-height-threshold nil)
        (split-width-threshold 140))
    (helm-gtags-dwim)))

(defun dotemacs-helm-gtags-define-keys-for-mode (mode)
  "Define key bindings for the specific MODE."
  (when (fboundp mode)
    (let ((hook (intern (concat (symbol-name mode) "-hook"))))
      (add-hook hook 'helm-gtags-mode))
    (dotemacs-set-leader-keys-for-major-mode mode
      "gc" 'helm-gtags-create-tags
      "gd" 'helm-gtags-find-tag
      "gf" 'helm-gtags-select-path
      "gg" 'helm-gtags-dwim
      "gG" 'helm-gtags-dwim-other-window
      "gi" 'helm-gtags-tags-in-this-function
      "gl" 'helm-gtags-parse-file
      "gn" 'helm-gtags-next-history
      "gp" 'helm-gtags-previous-history
      "gr" 'helm-gtags-find-rtag
      "gR" 'helm-gtags-resume
      "gs" 'helm-gtags-select
      "gS" 'helm-gtags-show-stack
      "gu" 'helm-gtags-update-tags)))

(defun dotemacs-ggtags-enable-eldoc (mode)
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda ()
              (ggtags-mode 1)
              (eldoc-mode 1)
              (setq-local eldoc-documentation-function
                          #'ggtags-eldoc-function))))

(use-package ggtags
  :ensure t
  :defer t)

(use-package helm-gtags
  :defer t
  :ensure t
  :init
  (progn
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t)
    ;; modes that do not have a layer, define here
    (dotemacs-helm-gtags-define-keys-for-mode 'tcl-mode)
    (dotemacs-helm-gtags-define-keys-for-mode 'java-mode)
    (dotemacs-helm-gtags-define-keys-for-mode 'vhdl-mode)
    (dotemacs-helm-gtags-define-keys-for-mode 'shell-script-mode)
    (dotemacs-helm-gtags-define-keys-for-mode 'awk-mode)
    (dotemacs-helm-gtags-define-keys-for-mode 'asm-mode)
    (dotemacs-helm-gtags-define-keys-for-mode 'dired-mode)
    (dotemacs-helm-gtags-define-keys-for-mode 'compilation-mode)
    (dotemacs-helm-gtags-define-keys-for-mode 'shell-mode)

    (dotemacs-ggtags-enable-eldoc 'tcl-mode)
    (dotemacs-ggtags-enable-eldoc 'java-mode)
    (dotemacs-ggtags-enable-eldoc 'vhdl-mode))
  :config
  (progn
    ;; if anyone uses helm-gtags, they would want to use these key bindings
    (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
    (define-key helm-gtags-mode-map (kbd "C-x 4 .") 'helm-gtags-find-tag-other-window)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
    (define-key helm-gtags-mode-map (kbd "M-*") 'helm-gtags-pop-stack)))

(provide 'module-gtags)
;;; module-gtags.el ends here
