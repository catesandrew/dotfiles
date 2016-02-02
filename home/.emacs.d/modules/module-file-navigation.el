;;; File Navigation
(require 'module-global)

(use-package dired-open
  :ensure t
  :defer t
  :init
  (progn
    (defun dotemacs-dired-open-file ()
      "Hook dired to translate to projectile and neotree."
      (interactive)
      (let ((file (ignore-errors (dired-get-file-for-visit))))
        (when file
          (find-file (expand-file-name file (projectile-project-root)))
          (run-hooks 'projectile-find-file-hook)
          (message "Projectile root found: %s" (projectile-project-root))
          (when (fboundp 'neotree-dir)
            (if (neo-global--window-exists-p)
                (neotree-dir (projectile-project-root))
              (progn
                (neotree-dir (projectile-project-root))
                (neotree-hide)
                (let ((origin-buffer-file-name (buffer-file-name)))
                  (neotree-find (projectile-project-root))
                  (neotree-find origin-buffer-file-name))
                (neotree-hide)))))))
    (with-eval-after-load 'projectile
      (setq dired-open-functions 'dotemacs-dired-open-file))))

(dotemacs-use-package-add-hook dired
  :post-config
  (with-eval-after-load 'projectile
    (evilified-state-evilify dired-mode dired-mode-map
      (kbd "RET") 'dired-open-file)))

(provide 'module-file-navigation)
;;; module-file-navigation.el ends here
