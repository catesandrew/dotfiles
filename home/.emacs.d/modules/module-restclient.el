;;; module-restclient.el --- REST Client Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'use-package)
(require 'core-vars)
(require 'core-funcs)
(require 'core-auto-completion)
(require 'core-keybindings)
(require 'core-auto-completion)
(require 'core-use-package-ext)
(require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(dotemacs-defvar-company-backends restclient-mode)

; (Emacs Rocks!)[http://emacsrocks.com/e15.html]
(use-package restclient                ; ReST REPL for Emacs
  :mode ("\\.http\\'" . restclient-mode)
  :ensure t
  :defer t
  :init
  (progn
    (defun restclient-http-send-current-raw-stay-in-window ()
      (interactive)
      (restclient-http-send-current t t))

    (dotemacs-set-leader-keys-for-major-mode 'restclient-mode
      "s" 'restclient-http-send-current-stay-in-window
      "S" 'restclient-http-send-current
      "r" 'restclient-http-send-current-raw-stay-in-window
      "R" 'restclient-http-send-current-raw
      "y" 'restclient-copy-curl-command
      )))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook restclient-mode))))

(use-package company-restclient    ; restclient backend for Company
  :if (eq dotemacs-completion-engine 'company)
  :ensure t
  :defer t
  :init
  (progn
    (push 'company-restclient company-backends-restclient-mode)))

(provide 'module-restclient)
;;; module-restclient.el ends here
