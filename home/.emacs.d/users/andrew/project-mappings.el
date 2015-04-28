;; Emacs

(defun custom-persp/emacs ()
  (interactive)
  (custom-persp "emacs"
                (find-file "~/.emacs.d/init.el")))

(define-key persp-mode-map (kbd "C-x p e") 'custom-persp/emacs)
