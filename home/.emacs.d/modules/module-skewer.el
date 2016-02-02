;;; Skewer
(require 'module-global)

;; Bookmarklet to load skewer:
;;
;;     javascript:(function(){var d=document ;var s=d.createElement('script');s.src='http://localhost:8023/skewer';d.body.appendChild(s);})()
;;

(defun dotemacs-skewer-start ()
  (interactive)
  (let ((httpd-port 8023))
    (httpd-start)
    (message "Ready to skewer the browser. Now jack in with the bookmarklet.")))

(defun dotemacs-skewer-demo ()
  (interactive)
  (let ((httpd-port 8024))
    (run-skewer)
    (skewer-repl)))

(use-package init-skewer
  :load-path "config/"
  :defer t
  :commands(dotemacs-skewer-start
            dotemacs-skewer-demo))

; This lets you send HTML, CSS, and Javascript fragments to Google Chrome. You
; may need to start Chrome with `--allow-running-insecure-content`, if
; you're using the user script with HTTPS sites.
(use-package skewer-mode
  :ensure t
  :defer t
  :init (with-eval-after-load 'js2-mode
          (skewer-setup))
  :config
  (progn
    (require 'skewer-repl)
    (require 'skewer-html)
    (require 'skewer-css))
  :diminish skewer-mode)

(provide 'module-skewer)
;;; module-skewer.el ends here
