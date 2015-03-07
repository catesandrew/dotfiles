(lazy-major-mode "\\.js$" js2-mode)

(after 'js2-mode

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

  (add-hook 'js2-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-c") #'my-dotemacs-js-ctrl-c-ctrl-c)))

  (require 'js2-refactor)
  (js2r-add-keybindings-with-prefix "C-c C-m")

  (setq js2-highlight-level 3)
  (setq-default js2-basic-offset 2)

  (when (executable-find "tern")
    (require 'tern)
    (add-hook 'js2-mode-hook 'tern-mode)
    (after 'tern
      (after 'auto-complete
        (require 'tern-auto-complete)
        (tern-ac-setup))
      (after 'company-mode
        (require 'company-tern)))))

(provide 'init-js)
