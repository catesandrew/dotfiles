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

(provide 'init-rust)
