(defun dotemacs-load-yasnippet ()
  (if (not (boundp 'yas-minor-mode))
      (progn
        (let* ((private-yas-dir (concat dotemacs-private-dir "private/"))
               (yas-dir (concat user-emacs-directory "yasnippet-snippets")))
          (setq yas-snippet-dirs
                (append (when (boundp 'yas-snippet-dirs)
                          yas-snippet-dirs)
                        (list  private-yas-dir yas-dir)))
          (setq yas-wrap-around-region t)
          (yas-minor-mode)))))

(defun dotemacs-force-yasnippet-off ()
  (yas-minor-mode -1)
  (setq yas-dont-activate t))

(provide 'init-yasnippet)
