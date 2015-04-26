;; show matching parens
(show-paren-mode t)
(setq show-paren-delay 0)


(display-time-mode t)
(size-indication-mode t)


(defun my-fold-overlay (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let ((col (save-excursion
                 (move-end-of-line 0)
                 (current-column)))
          (count (count-lines (overlay-start ov) (overlay-end ov))))
      (overlay-put ov 'after-string
                   (format "%s [ %d ] ... "
                           (make-string (- (window-width) col 32) (string-to-char "."))
                           count)))))
(setq hs-set-up-overlay 'my-fold-overlay)

;; Unclutter the modeline
(require 'diminish)
(diminish 'visual-line-mode)
(after 'undo-tree (diminish 'undo-tree-mode))
(after 'auto-complete (diminish 'auto-complete-mode))
(after 'projectile (diminish 'projectile-mode))
(after 'yasnippet (diminish 'yas-minor-mode))
(after 'guide-key (diminish 'guide-key-mode))
(after 'eldoc (diminish 'eldoc-mode))
(after 'smartparens (diminish 'smartparens-mode))
(after 'company (diminish 'company-mode))
(after 'elisp-slime-nav (diminish 'elisp-slime-nav-mode))
(after 'git-gutter+ (diminish 'git-gutter+-mode))
(after 'magit (diminish 'magit-auto-revert-mode))
(after 'highlight-symbol (diminish 'highlight-symbol-mode))
(after 'paredit (diminish 'paredit-mode))
(after 'tagedit (diminish 'tagedit-mode))
(after 'skewer-mode (diminish 'skewer-mode))
(after 'skewer-css (diminish 'skewer-css-mode))
(after 'skewer-html (diminish 'skewer-html-mode))
(after 'whitespace-cleanup-mode (diminish 'whitespace-cleanup-mode))
(after 'subword (diminish 'subword-mode))


(require 'smart-mode-line)
(setq sml/show-client t)
(setq sml/show-eol t)
(setq sml/show-frame-identification t)
(sml/setup)


; (require 'powerline)
; (powerline-default-theme)
; (powerline-center-evil-theme)
; (powerline-vim-theme)


; (require 'powerline-evil)
; (powerline-evil-vim-theme)



(if (fboundp 'global-prettify-symbols-mode)
    (progn
      (global-prettify-symbols-mode)
      (add-hook 'js2-mode-hook
                (lambda ()
                  (push '("function" . 955) prettify-symbols-alist)
                  (push '("return" . 8592) prettify-symbols-alist))))
  (progn
    (require 'pretty-symbols)
    (require 'pretty-symbols)
    (diminish 'pretty-symbols-mode)
    (add-to-list 'pretty-symbol-categories 'js)
    (add-to-list 'pretty-symbol-patterns '(955 js "\\<function\\>" (js2-mode)))
    (add-to-list 'pretty-symbol-patterns '(8592 js "\\<return\\>" (js2-mode)))
    (add-hook 'find-file-hook 'pretty-symbols-mode)))


(require 'color-identifiers-mode)
(global-color-identifiers-mode)
(diminish 'color-identifiers-mode)


;; (require-package 'fancy-narrow)
;; (fancy-narrow-mode)


(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.3)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

(require 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

(require 'highlight-quoted)
(add-hook 'prog-mode-hook 'highlight-quoted-mode)


(add-hook 'find-file-hook 'hl-line-mode)


;; relative line numbers
(add-hook 'prog-mode-hook 'relative-line-numbers-mode t)
(add-hook 'prog-mode-hook 'line-number-mode t)
(add-hook 'prog-mode-hook 'column-number-mode t)


(provide 'init-eyecandy)
