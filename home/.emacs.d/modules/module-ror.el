;;; module-ror.el --- Ruby on Rails Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;;; Code:

(require 'use-package)
(require 'core-vars)
(require 'core-funcs)
(require 'core-fonts-support)
(require 'core-keybindings)
(require 'core-auto-completion)
(require 'core-use-package-ext)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)
(require 'module-ruby)

(use-package projectile-rails
  :ensure t
  :defer t
  :init (add-hook 'projectile-mode-hook 'projectile-rails-on)
  :config
  (progn
    (dotemacs-diminish projectile-rails-mode " ⇋" " RoR")

    ;; Find files
    (dolist (mode '(ruby-mode enh-ruby-mode))
      (dotemacs-set-leader-keys-for-major-mode mode
        "rfa" 'projectile-rails-find-locale
        "rfc" 'projectile-rails-find-controller
        "rfe" 'projectile-rails-find-environment
        "rff" 'projectile-rails-find-feature
        "rfh" 'projectile-rails-find-helper
        "rfi" 'projectile-rails-find-initializer
        "rfj" 'projectile-rails-find-javascript
        "rfl" 'projectile-rails-find-lib
        "rfm" 'projectile-rails-find-model
        "rfn" 'projectile-rails-find-migration
        "rfo" 'projectile-rails-find-log
        "rfp" 'projectile-rails-find-spec
        "rfr" 'projectile-rails-find-rake-task
        "rfs" 'projectile-rails-find-stylesheet
        "rft" 'projectile-rails-find-test
        "rfu" 'projectile-rails-find-fixture
        "rfv" 'projectile-rails-find-view
        "rfy" 'projectile-rails-find-layout
        "rf@" 'projectile-rails-find-mailer
        ;; Goto file
        "rgc" 'projectile-rails-find-current-controller
        "rgd" 'projectile-rails-goto-schema
        "rge" 'projectile-rails-goto-seeds
        "rgh" 'projectile-rails-find-current-helper
        "rgj" 'projectile-rails-find-current-javascript
        "rgg" 'projectile-rails-goto-gemfile
        "rgm" 'projectile-rails-find-current-model
        "rgn" 'projectile-rails-find-current-migration
        "rgp" 'projectile-rails-find-current-spec
        "rgr" 'projectile-rails-goto-routes
        "rgs" 'projectile-rails-find-current-stylesheet
        "rgt" 'projectile-rails-find-current-test
        "rgu" 'projectile-rails-find-current-fixture
        "rgv" 'projectile-rails-find-current-view
        "rgz" 'projectile-rails-goto-spec-helper
        "rg." 'projectile-rails-goto-file-at-point
        ;; Rails external commands
        "r:" 'projectile-rails-rake
        "rcc" 'projectile-rails-generate
        "ri" 'projectile-rails-console
        "rxs" 'projectile-rails-server
        ;; Refactoring 'projectile-rails-mode
        "rRx" 'projectile-rails-extract-region))
    ;; Ex-commands
    (evil-ex-define-cmd "A" 'projectile-toggle-between-implementation-and-test)))

(use-package feature-mode
  :ensure t
  :mode (("\\.feature\\'" . feature-mode)))

(provide 'module-ror)
;;; module-ror.el ends here
