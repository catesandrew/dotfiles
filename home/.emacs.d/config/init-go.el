(lazy-major-mode "\\.go$" go-mode)

(with-eval-after-load 'go-mode
  (require 'go-eldoc)
  (add-hook 'go-mode-hook 'go-eldoc-setup)

  (with-eval-after-load "company-autoloads"
    (require 'company-go)
    (require 'company-go)
    (add-hook 'go-mode-hook (lambda ()
                              (set (make-local-variable 'company-backends) '(company-go))))))

(provide 'init-go)
