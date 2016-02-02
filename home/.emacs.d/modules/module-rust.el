;; http://doc.crates.io/guide.html
(defun dotemacs-rust-cargo-build ()
  (interactive)
  (compile "cargo build"))

(defun dotemacs-rust-cargo-run ()
  (interactive)
  (compile "cargo run"))

(defun dotemacs-rust-cargo-test ()
  (interactive)
  (compile "cargo test"))

(defun dotemacs-rust-cargo-doc ()
  (interactive)
  (compile "cargo doc"))

(dotemacs-defvar-company-backends rust-mode)

(defvar rust-enable-racer nil
  "If non-nil, load the racer package (this has an external dependency).")

(use-package rust-mode                  ; Rust
  :ensure t
  :defer t
  :config
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'rust-mode
      "cc" 'dotemacs-rust-cargo-build
      "ct" 'dotemacs-rust-cargo-test
      "cd" 'dotemacs-rust-cargo-doc
      "cx" 'dotemacs-rust-cargo-run)))

(use-package racer
  :if rust-enable-racer
  :defer t
  :ensure t
  :init (dotemacs/add-to-hook 'rust-mode-hook '(racer-mode eldoc-mode)))

(use-package flycheck-rust              ; Flycheck setup for Rust
  :ensure t
  :commands (flycheck-rust-setup)
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'rust-mode))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook rust-mode))))

(use-package company-racer
  :if (eq dotemacs-completion-engine 'company)
  :ensure t
  :defer t
  :init
  (progn
    (push 'company-tern company-backends-rust-mode)))

(use-package toml-mode                  ; Toml for Cargo files
  :ensure t
  :defer t
  :mode ("\\.toml$" . toml-mode)
  :config
    (add-hook 'toml-mode-hook
              (lambda ()
                (electric-indent-local-mode -1)
                (run-hooks 'prog-mode-hook))))

(provide 'module-rust)
;;; module-rust.el ends here
