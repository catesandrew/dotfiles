;;; module-stylus.el --- Stylus Module
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
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(defvar dotemacs-stylus-command-args nil
  "Additional list of arguments to pass into the stylus command.")

(defvar dotemacs-stylus-display-buffer-name "*Stylus Output*"
  "The name of the Stylus buffer with CSS output.")

(defvar dotemacs-stylus-last-command-mode nil)

(defun dotemacs-stylus-process-sentinel (process event)
  (when (equal event "finished\n")
    (if dotemacs-stylus-last-command-mode
        (display-buffer dotemacs-stylus-display-buffer-name)
      (with-current-buffer dotemacs-stylus-display-buffer-name
        (skewer-css-eval-buffer)))))

(defun dotemacs-stylus-setup-output-buffer (show)
  (let ((buffer (get-buffer-create dotemacs-stylus-display-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (when show
        (display-buffer buffer))
      (css-mode))))

(defun dotemacs-stylus-async (begin end show)
  (require 'skewer-css)
  (dotemacs-stylus-setup-output-buffer show)
  (setq dotemacs-stylus-last-command-mode show)
  (let ((process (apply 'start-process
                        "stylus" dotemacs-stylus-display-buffer-name "stylus" dotemacs-stylus-command-args)))
    (set-process-sentinel process 'dotemacs-stylus-process-sentinel)
    (process-send-region process begin end)
    (process-send-eof process)))

(defun dotemacs-stylus-compile-and-show-region (start end)
  (interactive "r")
  (dotemacs-stylus-async start end t))

(defun dotemacs-stylus-compile-and-show-buffer ()
  (interactive)
  (dotemacs-stylus-async (point-min) (point-max) t))

(defun dotemacs-stylus-compile-and-eval-buffer ()
  (interactive)
  (dotemacs-stylus-async (point-min) (point-max) nil))

(defun dotemacs-stylus-mode-defaults ()
  "Default stylus-mode coding hook."

  (unless (bound-and-true-p my-stylus-mh-ran)
    ;; add buffer-local indicator for whether prog-mode-hook has run.
    (set (make-local-variable 'my-stylus-mh-ran) t)

    ;; disable electric re-indenting...
    ; (electric-indent-local-mode -1)
    (unless (process-status "httpd")
      (httpd-start))))

(use-package stylus-mode
  :ensure t
  :defer t
  :mode ("\\.styl$" . stylus-mode)
  :init
  (progn
    (setq dotemacs-stylus-mode-hook #'dotemacs-stylus-mode-defaults)
    (add-hook 'stylus-mode-hook
              (lambda ()
                (run-hooks #'dotemacs-stylus-mode-hook)))))

(provide 'module-stylus)
;;; module-stylus.el ends here
