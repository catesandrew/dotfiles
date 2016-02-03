;;; module-compile.el --- Compile Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-vars)
;; (require 'core-funcs)
;; (require 'core-keybindings)
;; (require 'core-display-init)
(require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)
(require 'ansi-color)

;;; Code:

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
    ;; From http://xugx2007.blogspot.ca/2007/06/benjamin-rutts-emacs-c-development-tips.html
    (setq compilation-finish-function
          (lambda (buf str)
            (if (or (string-match "exited abnormally" str)
                    (string-match "FAILED" (buffer-string)))

                ;; there were errors
                (message "There were errors. SPC-e-n to visit.")
              (unless (or (string-match "Grep finished" (buffer-string))
                          (string-match "Ag finished" (buffer-string))
                          (string-match "nosetests" (buffer-name)))

                ;; no errors
                (message "compilation ok.")))))

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
