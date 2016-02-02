;;; Lua
(require 'module-global)

(dotemacs-defvar-company-backends lua-mode)

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'lua-mode))

(use-package lua-mode
  :defer t
  :ensure t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :config
  (progn
    (setq lua-indent-level 2
          lua-indent-string-contents t)
    (dotemacs-set-leader-keys-for-major-mode 'lua-mode "d" 'lua-search-documentation)
    (dotemacs-set-leader-keys-for-major-mode 'lua-mode "sb" 'lua-send-buffer)
    (dotemacs-set-leader-keys-for-major-mode 'lua-mode "sf" 'lua-send-defun)
    (dotemacs-set-leader-keys-for-major-mode 'lua-mode "sl" 'lua-send-current-line)
    (dotemacs-set-leader-keys-for-major-mode 'lua-mode "sr" 'lua-send-region)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook lua-mode))))

(provide 'module-lua)
;;; module-lua.el ends here
