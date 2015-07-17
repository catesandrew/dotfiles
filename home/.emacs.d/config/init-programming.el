(defun dotemacs-fold-overlay (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let ((col (save-excursion
                 (move-end-of-line 0)
                 (current-column)))
          (count (count-lines (overlay-start ov) (overlay-end ov))))
      (overlay-put ov 'after-string
                   (format "%s [ %d ] ... "
                           (make-string (- (window-width) col 32) (string-to-char "."))
                           count)))))

(defun dotemacs-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun dotemacs-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

;; To augment and/or counteract these defaults your own function
;; to dotemacs-prog-mode-hook, using:
;;
;; (add-hook 'dotemacs-prog-mode-hook 'dotemacs-prog-mode-defaults t)
;;
;; (the final optional t sets the *append* argument)

(defun dotemacs-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."

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
    (dotemacs-local-comment-auto-fill)
    (dotemacs-font-lock-comment-annotations)))

(provide 'init-programming)
