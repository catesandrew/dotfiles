;;; Ranger
(require 'module-global)

(use-package ranger
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-set-leader-keys
      "ar" 'ranger
      "ad" 'deer)

    ;; set up image-dired to allow picture resize
    (setq image-dired-dir (concat dotemacs-cache-directory "image-dir"))
    (unless (file-directory-p image-dired-dir)
      (make-directory image-dired-dir))

    (setq ranger-show-literal nil
          ranger-preview-file t
          ranger-width-parents 0.15
          ranger-width-preview 0.65
          ranger-show-preview t
          ranger-parent-depth 1
          ranger-max-preview-size 10))
  :config
  (define-key ranger-mode-map (kbd "-") 'ranger-up-directory))

(provide 'module-ranger)
;;; module-ranger.el ends here
