(require 'init-programming)

(add-to-list 'auto-mode-alist '("\\.js\\'"    . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pac\\'"   . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(after 'javascript-mode
  (setq javascript-indent-level 2)) ; javascript-mode

(after 'js-mode
  (setq js-indent-level 2)) ; js-mode

(after 'js2-mode
  (defun my-js2-mode-defaults ()
    (js2-imenu-extras-mode +1)
    (setq mode-name "JS2")
    ; '(define-key js-mode-map "," 'self-insert-command)
    ; '(define-key js-mode-map ";" 'self-insert-command)
    ;; electric-layout-mode doesn't play nice with smartparens
    (setq-local electric-layout-rules '((?\; . after)))
    (message "My JS2 hook"))

  (setq my-js2-mode-hook 'my-js2-mode-defaults)
  (add-hook 'js2-mode-hook (lambda () (run-hooks 'my-js2-mode-hook)))

  (defun my-dotemacs-js-ctrl-c-ctrl-c ()
    (interactive)
    (require 'thingatpt)
    (let ((val (thing-at-point 'list)))
      ;; inside parameter list?
      (when (and (equal (substring val 0 1) "(")
                 (equal (substring val -1) ")"))
        (if (string-match-p "," val)
            (my-macro-ng-add-string-for-last-arg)
          (my-macro-ng-function-to-array-injected)))))

  (add-hook 'js2-mode-hook (lambda ()
    (local-set-key (kbd "C-c C-c") #'my-dotemacs-js-ctrl-c-ctrl-c)))

  (setq indent-tabs-mode nil
        tab-width 2
        js-indent-level 2)
  (setq js2-highlight-level 3)
  (setq js2-basic-offset 2)
  (setq js2-concat-multiline-strings (quote eol))
  (setq js2-include-node-externs t)
  (setq js2-indent-switch-body t)

  (setq js2-allow-rhino-new-expr-initializer nil)
  (setq js2-auto-indent-p nil)
  (setq js2-enter-indents-newline nil)
  (setq js2-global-externs '("setTimeout" "clearTimeout" "setInterval" "clearInterval" "__dirname" "console" "JSON" "_" "assert" "refute" "buster" "require" "global" "exports" "module" "describe" "it" "before" "after" "beforeEach" "afterEach" "chai" "expect" "sinon" "test" "asyncTest" "ok" "equal" "notEqual" "deepEqual" "expect"))
  (setq js2-idle-timer-delay 0.8)
  (setq js2-indent-on-enter-key nil)
  (setq js2-mirror-mode nil)
  (setq js2-strict-inconsistent-return-warning nil)
  (setq js2-include-rhino-externs nil)
  (setq js2-include-gears-externs nil)
  (setq js2-rebind-eol-bol-keys nil)

  ;; Let flycheck handle parse errors
  (setq js2-show-parse-errors nil)
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason
  (require 'js2-refactor)
  (js2r-add-keybindings-with-prefix "C-c C-m")

  (require 'js2-imenu-extras)
  (js2-imenu-extras-setup)

  ;; jshintrc
  (when (executable-find "tern")
    (require 'tern)
    (add-hook 'js2-mode-hook 'tern-mode)
    (after 'tern
      (after 'auto-complete
        (require 'tern-auto-complete)
        (tern-ac-setup))
      (after 'company-mode
        (require 'company-tern)))))

(after 'js3-mode

  (defun my-js3-mode-defaults ()
    (setq mode-name "JS3")
    ;; electric-layout-mode doesn't play nice with smartparens
    (setq-local electric-layout-rules '((?\; . after)))
  )

  (setq my-js3-mode-hook 'my-js3-mode-defaults)
  (add-hook 'js3-mode-hook (lambda () (run-hooks 'my-js3-mode-hook)))

  (setq js3-expr-indent-offset 2)
  (setq js3-paren-indent-offset 2)
  (setq js3-square-indent-offset 2)
  (setq js3-curly-indent-offset 2)
  (setq js3-auto-indent-p t)         ; it's nice for commas to right themselves.
  (setq js3-enter-indents-newline t) ; don't need to push tab before typing
  (setq js3-indent-on-enter-key t)   ; fix indenting before moving on

  (when (executable-find "tern")
    (require 'tern)
    (add-hook 'js3-mode-hook 'tern-mode)
    (after 'tern
      (after 'auto-complete
        (require 'tern-auto-complete)
        (tern-ac-setup))
      (after 'company-mode
        (require 'company-tern)))))

(provide 'init-js)
