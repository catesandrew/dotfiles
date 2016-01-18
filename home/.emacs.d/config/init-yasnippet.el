;;; Code:
(require 'yasnippet)

(defun dotemacs-load-yasnippet ()
  (unless yas-global-mode
    (progn
      (yas-global-mode 1)
      (let ((private-yas-dir (if dotemacs-ac-private-snippets-directory
                                 dotemacs-ac-private-snippets-directory
                               (concat dotemacs-private-dir "snippets/")))
            (dotemacs-snippets-dir
             (expand-file-name
              (concat user-emacs-directory "snippets/"))))
        (setq yas-snippet-dirs
              (append (list private-yas-dir)
                      (when (boundp 'yas-snippet-dirs)
                        yas-snippet-dirs)
                      dotemacs-snippets-dir))
        (yas-load-directory dotemacs-snippets-dir t)
        (yas-load-directory private-yas-dir t)
        (setq yas-wrap-around-region t))))
  (yas-minor-mode 1))

(defun dotemacs-force-yasnippet-off ()
  (yas-minor-mode -1)
  (setq yas-dont-activate t))

(defun dotemacs-auto-yasnippet-expand ()
  "Call `yas-expand' and switch to `insert state'"
  (interactive)
  (call-interactively 'aya-expand)
  (evil-insert-state))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
