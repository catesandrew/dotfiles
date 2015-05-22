;; show matching parens
(show-paren-mode t)
(setq show-paren-delay 0)

; set transparency of background
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))

(display-time-mode t)
(size-indication-mode t)

;; nyan-cat
; (nyan-mode +1)
; (nyan-start-animation)

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
(with-eval-after-load 'undo-tree (diminish 'undo-tree-mode))
(with-eval-after-load 'auto-complete (diminish 'auto-complete-mode))
(with-eval-after-load 'projectile (diminish 'projectile-mode))
(with-eval-after-load 'yasnippet (diminish 'yas-minor-mode))
(with-eval-after-load 'guide-key (diminish 'guide-key-mode))
(with-eval-after-load 'eldoc (diminish 'eldoc-mode))
(with-eval-after-load 'smartparens (diminish 'smartparens-mode))
(with-eval-after-load 'company (diminish 'company-mode))
(with-eval-after-load 'elisp-slime-nav (diminish 'elisp-slime-nav-mode))
(with-eval-after-load 'git-gutter+ (diminish 'git-gutter+-mode))
(with-eval-after-load 'magit (diminish 'magit-auto-revert-mode))
(with-eval-after-load 'highlight-symbol (diminish 'highlight-symbol-mode))
(with-eval-after-load 'paredit (diminish 'paredit-mode))
(with-eval-after-load 'tagedit (diminish 'tagedit-mode))
(with-eval-after-load 'skewer-mode (diminish 'skewer-mode))
(with-eval-after-load 'skewer-css (diminish 'skewer-css-mode))
(with-eval-after-load 'skewer-html (diminish 'skewer-html-mode))
(with-eval-after-load 'whitespace-cleanup-mode (diminish 'whitespace-cleanup-mode))
(with-eval-after-load 'subword (diminish 'subword-mode))


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


(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.3)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

(require 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

(require 'highlight-quoted)
(add-hook 'prog-mode-hook 'highlight-quoted-mode)


(add-hook 'find-file-hook 'hl-line-mode)


(add-hook 'prog-mode-hook 'line-number-mode t)
(add-hook 'prog-mode-hook 'column-number-mode t)


(provide 'init-eyecandy)
