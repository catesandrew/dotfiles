;;; module-skewer.el --- Skewer Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
;; (require 'core-display-init)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

;; Bookmarklet to load skewer:
;;
;;     javascript:(function(){var d=document ;var s=d.createElement('script');s.src='http://localhost:8023/skewer';d.body.appendChild(s);})()
;;
;; (defun dotemacs-skewer-start ()
;;   (interactive)
;;   (let ((httpd-port 8023))
;;     (httpd-start)
;;     (message "Ready to skewer the browser. Now jack in with the bookmarklet.")))

;; This lets you send HTML, CSS, and Javascript fragments to Google Chrome. You
;; may need to start Chrome with `--allow-running-insecure-content`, if
;; you're using the user script with HTTPS sites.
(use-package skewer-mode
    :defer t
    :init
    (progn
      ;; Use local `phantomjs`, if available.
      (when-let (phantomjs (executable-find "phantomjs"))
        (setq phantomjs-program-name phantomjs))

      (dotemacs-register-repl 'skewer-mode 'dotemacs/skewer-start-repl "skewer")

      (add-hook 'css-mode-hook 'skewer-css-mode)
      (add-hook 'html-mode-hook 'skewer-html-mode)
      (add-hook 'js2-mode-hook 'skewer-mode))
    :config
    (progn
      (defun dotemacs/skewer-start-repl ()
        "Attach a browser to Emacs and start a skewer REPL."
        (interactive)
        (let ((httpd-port 8024))
          (run-skewer)
          (skewer-repl)))

      (defun dotemacs/skewer-load-buffer-and-focus ()
        "Execute whole buffer in browser and switch to REPL in insert state."
        (interactive)
        (skewer-load-buffer)
        (skewer-repl)
        (evil-insert-state))

      (defun dotemacs/skewer-eval-defun-and-focus ()
       "Execute function at point in browser and switch to REPL in insert state."
       (interactive)
       (skewer-eval-defun)
       (skewer-repl)
       (evil-insert-state))

      (defun dotemacs/skewer-eval-region (beg end)
        "Execute the region as JavaScript code in the attached browser."
        (interactive "r")
        (skewer-eval (buffer-substring beg end) #'skewer-post-minibuffer))

      (defun spacemacs/skewer-eval-region-and-focus (beg end)
        "Execute the region in browser and swith to REPL in insert state."
        (interactive "r")
        (dotemacs/skewer-eval-region beg end)
        (skewer-repl)
        (evil-insert-state))

      (dotemacs-set-leader-keys-for-major-mode 'js2-mode
        "'" 'dotemacs/skewer-start-repl
        "ee" 'skewer-eval-last-expression
        "eE" 'skewer-eval-print-last-expression
        "sb" 'skewer-load-buffer
        "sB" 'dotemacs/skewer-load-buffer-and-focus
        "si" 'dotemacs/skewer-start-repl
        "sf" 'skewer-eval-defun
        "sF" 'dotemacs/skewer-eval-defun-and-focus
        "sr" 'dotemacs/skewer-eval-region
        "sR" 'dotemacs/skewer-eval-region-and-focus
        "ss" 'skewer-repl)))

(provide 'module-skewer)
;;; module-skewer.el ends here
