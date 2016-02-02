;;; Programming Head
(require 'module-global)

(defun dotemacs-fold-overlay (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let ((col (save-excursion
                 (move-end-of-line 0)
                 (current-column)))
          (count (count-lines (overlay-start ov) (overlay-end ov))))
      (overlay-put ov 'after-string
                   (format "%s [ %d ] … "
                           (make-string (- (window-width) col 32) (string-to-char "."))
                           count)))))

(defun dotemacs-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

;; required for evil folding
(defun dotemacs-enable-hs-minor-mode ()
  "Enable hs-minor-mode for code folding."
  (ignore-errors
    (hs-minor-mode)
    (dotemacs-hide-lighter hs-minor-mode)))

(defun dotemacs-highlight-TODO-words ()
  "Highlight keywords in comments."
  (interactive)
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|BUG\\|CHECK\\|DONE\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\)\\>:?\\)"
          1 font-lock-warning-face t))))

;; To augment and/or counteract these defaults your own function
;; to dotemacs-prog-mode-hook, using:
;;
;; (add-hook 'dotemacs-prog-mode-hook 'dotemacs-prog-mode-defaults t)
;;
;; (the final optional t sets the *append* argument)

(defun dotemacs-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."

  (when spell-checking-enable-by-default
    (flyspell-prog-mode))

  ;; Underscore "_" is now a word character in programming mode
  (modify-syntax-entry ?_ "w")
  (unless (bound-and-true-p my-pmh-ran)
    ;; add buffer-local indicator for whether prog-mode-hook has run.
    (set (make-local-variable 'my-pmh-ran) t)

    (when dotemacs-show-trailing-whitespace
      (set-face-attribute 'trailing-whitespace nil
                          :background (face-attribute 'font-lock-comment-face
                                                      :foreground))
      (setq show-trailing-whitespace 1))

    ;; disable line wrap
    (unless (bound-and-true-p truncate-lines)
      ; (set (make-local-variable 'truncate-lines) t)
      (setq truncate-lines t))

    ;; enable line number-mode
    (when dotemacs-line-numbers
      (unless (bound-and-true-p linum-mode)
        (linum-mode)))

    (when (bound-and-true-p visual-line-mode)
      (setq visual-line-mode nil))

    ; TODO: Check and disable visual-fill-column-mode
    ; (when (bound-and-true-p visual-fill-column-mode)
    ;   (visual-fill-column-mode--disable))

    ; (smartparens-mode +1)

    ; how-to-check-whether-a-minor-mode-e-g-flymake-mode-is-on
    ; http://stackoverflow.com/questions/10088168/
    ; however, hs-minor-mode already set in `init.el`
    ; (unless (bound-and-true-p hs-minor-mode)
    ;   (hs-minor-mode t))
    (dotemacs-enable-hs-minor-mode)
    (dotemacs-local-comment-auto-fill)
    (dotemacs-highlight-TODO-words)))

(use-package aggressive-indent
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-add-toggle aggressive-indent
      :status aggressive-indent-mode
      :on (aggressive-indent-mode)
      :off (aggressive-indent-mode -1)
      :documentation "Always keep code indented."
      :evil-leader "tI")
    (dotemacs-add-toggle aggressive-indent-globally
      :status aggressive-indent-mode
      :on (global-aggressive-indent-mode)
      :off (global-aggressive-indent-mode -1)
      :documentation "Always keep code indented globally."
      :evil-leader "t C-I"))
  :config
  (progn
    (add-hook 'diff-auto-refine-mode-hook 'dotemacs-toggle-aggressive-indent-off)
    (dotemacs-diminish aggressive-indent-mode " Ⓘ" " I")))

; TODO if this is useful, add a toggle
(use-package elide-head                 ; Elide lengthy GPL headers
  :init (add-hook 'prog-mode-hook #'elide-head))

(use-package prog-mode                  ; Prog Mode
  :defer
  :init
  (add-hook 'js2-mode-hook
            (lambda ()
              (push '("not" . 172) prettify-symbols-alist) ;
              (push '("!" . 172) prettify-symbols-alist) ;
              (push '("forall" . 8704) prettify-symbols-alist) ;
              (push '("::" . 8759) prettify-symbols-alist) ;
              ; (push '("." . 8728) prettify-symbols-alist) ;
              (push '("~>" . 8669) prettify-symbols-alist) ;
              ; (push '("()" . #X2205) prettify-symbols-alist) ;
              (push '("==" . #X225F) prettify-symbols-alist) ;
              (push '("!=" . #X2260) prettify-symbols-alist) ;
              (push '("===" . #X2261) prettify-symbols-alist) ;
              (push '("!==" . #X2262) prettify-symbols-alist) ;
              (push '(">=" . #X2265) prettify-symbols-alist) ;
              (push '("<=" . #X2264) prettify-symbols-alist) ;
              (push '("!!" . #X203C) prettify-symbols-alist) ;
              (push '("&&" . #X2227) prettify-symbols-alist) ;
              (push '("||" . #X2228) prettify-symbols-alist) ;
              ; (push '("null" . 00D8) prettify-symbols-alist) ;
              (push '("sqrt" . #X221A) prettify-symbols-alist) ;
              (push '("undefined" . #X22A5) prettify-symbols-alist) ;
              (push '("pi" . #X3C0) prettify-symbols-alist) ;
              (push '("function" . 955) prettify-symbols-alist) ; λ
              (push '("->" . 8594) prettify-symbols-alist) ; →
              (push '("-<" . 8610) prettify-symbols-alist) ;
              (push '("<-" . 8592) prettify-symbols-alist) ;
              (push '("=>" . 8658) prettify-symbols-alist) ; ⇒
              ; (push '("map" . 8614) prettify-symbols-alist) ; ↦
              (push '("return" . 8592) prettify-symbols-alist))))

; http://stackoverflow.com/questions/1085170
(use-package hs-minor-mode
  :defer t
  :init
  (progn
    ;; Space to toggle folds.
    (define-key evil-motion-state-map (kbd "SPC") 'evil-toggle-fold)
    ;; TODO: Figure out how to only map " " when in hs-minor-mode
    ;; (evil-define-key 'motion hs-minor-mode-map " " 'evil-toggle-fold)

    ;; required for evil folding
    (setq hs-set-up-overlay 'dotemacs-fold-overlay)))

; This works and sets the mode correctly but the symbols do not show up
(use-package prettify-symbols-mode
  :defer t
  :init
  (progn
    ; (dolist (mode '(emacs-lisp js2 java python ruby))
    ;   (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
    ;             (lambda ()
    ;               (prettify-symbols-mode))))
    (setq prettify-symbol-categories '(lambda relational logical))))

; Instead set the mode globally
(global-prettify-symbols-mode)

(provide 'module-prog-head)
;;; module-prog-haed.el ends here
