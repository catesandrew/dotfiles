;;; module-django.el --- Django Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'use-package)
(require 'evil-evilified-state)
(require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
;; (require 'core-display-init)
(require 'core-auto-completion)
(require 'core-use-package-ext)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)
(require 'module-python)

;;; Code:

(use-package pony-mode
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'python-mode
      ; d*j*ango f*a*bric
      "jaf" 'pony-fabric
      "jad" 'pony-fabric-deploy
      ; d*j*ango *f*iles
      "jfs" 'pony-goto-settings
      "jfc" 'pony-setting
      "jft" 'pony-goto-template
      "jfr" 'pony-resolve
      ; d*j*ango *i*nteractive
      "jid" 'pony-db-shell
      "jis" 'pony-shell
      ; d*j*ango *m*anage
      ; not including one-off management commands like "flush" and
      ; "startapp" even though they're implemented in pony-mode,
      ; because this is much handier
      "jm" 'pony-manage
      ; d*j*ango *r*unserver
      "jrd" 'pony-stopserver
      "jro" 'pony-browser
      "jrr" 'pony-restart-server
      "jru" 'pony-runserver
      "jrt" 'pony-temp-server
      ; d*j*ango *s*outh/*s*yncdb
      "jsc" 'pony-south-convert
      "jsh" 'pony-south-schemamigration
      "jsi" 'pony-south-initial
      "jsm" 'pony-south-migrate
      "jss" 'pony-syncdb
      ; d*j*ango *t*est
      "jtd" 'pony-test-down
      "jte" 'pony-test-goto-err
      "jto" 'pony-test-open
      "jtt" 'pony-test
      "jtu" 'pony-test-up)))

(provide 'module-djangjo)
;;; module-djangjo.el ends here
