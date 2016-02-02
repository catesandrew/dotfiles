;;; Compile
(require 'module-global)
(require 'ansi-color)

(defun dotemacs-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer.

Taken from http://stackoverflow.com/a/3072831/355252."
  (interactive)
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(use-package compile                    ; Compile from Emacs
  :config
  (progn
    (setq compilation-ask-about-save nil
          compilation-always-kill t
          ;; Kill old compilation processes before starting new ones,
          ;; and automatically scroll up to the first error.
          compilation-scroll-output 'first-error)

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*compilation")
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (side            . bottom)
                   (reusable-frames . visible)
                   (window-height   . 0.4)))))

(use-package auto-compile
  :defer t
  :ensure t
  :diminish (auto-compile-mode . "")
  :init
  (progn
    (setq auto-compile-display-buffer nil
          ;; lets spaceline manage the mode-line
          auto-compile-use-mode-line nil
          auto-compile-mode-line-counter t)
    (add-hook 'emacs-lisp-mode-hook 'auto-compile-mode))
  :config
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'emacs-lisp-mode
      "cl" 'auto-compile-display-log)))

(provide 'module-compile)
;;; module-compile.el ends here
