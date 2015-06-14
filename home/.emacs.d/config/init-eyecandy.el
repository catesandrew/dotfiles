; set transparency of background
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))

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
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Unclutter the modeline
(require 'diminish)
(diminish 'visual-line-mode)
(with-eval-after-load 'yasnippet (diminish 'yas-minor-mode))
(with-eval-after-load 'guide-key (diminish 'guide-key-mode))
(with-eval-after-load 'eldoc (diminish 'eldoc-mode))
(with-eval-after-load 'git-gutter+ (diminish 'git-gutter+-mode))
(with-eval-after-load 'tagedit (diminish 'tagedit-mode))
(with-eval-after-load 'skewer-mode (diminish 'skewer-mode))
(with-eval-after-load 'skewer-css (diminish 'skewer-css-mode))
(with-eval-after-load 'skewer-html (diminish 'skewer-html-mode))


(delayed-init
 (require-package 'color-identifiers-mode)
 (global-color-identifiers-mode)
 (diminish 'color-identifiers-mode))

(provide 'init-eyecandy)
