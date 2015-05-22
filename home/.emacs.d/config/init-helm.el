; TODO: https://github.com/thierryvolpiatto/emacs-tv-config/blob/master/init-helm-thierry.el
; Helm: Unite/CtrlP style fuzzy file/buffer/anything searcher on steroids

; Helm does the same thing as Unite/CtrlP on Vim and does it really well. You
; can also enable Helm to manage the command buffer, which is pretty awesome
; with: (helm-mode 1)

(setq helm-command-prefix-key "C-c h")
(setq helm-quick-update t)
(setq helm-bookmark-show-location t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-M-x-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)
(setq helm-recentf-fuzzy-match t)
(setq helm-locate-fuzzy-match t)
(setq helm-file-cache-fuzzy-match t)
(setq helm-semantic-fuzzy-match t)
(setq helm-imenu-fuzzy-match t)
(setq helm-lisp-fuzzy-completion t)

(require 'helm)
(setq helm-swoop-pre-input-function #'ignore)
(setq helm-swoop-use-line-number-face t)
(setq helm-swoop-split-with-multiple-windows t)
(require 'helm-swoop)
(require 'helm-descbinds)

(with-eval-after-load "projectile-autoloads"
  (require 'helm-projectile))

(with-eval-after-load "company-autoloads"
  (require 'helm-company))

(require 'helm-config)
(with-eval-after-load 'helm
  (helm-autoresize-mode t))

(provide 'init-helm)
