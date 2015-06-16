(defun dotemacs-persp-switch-project ()
  (interactive)
  ; (evil-leader/set-key
  ;   "pp" 'helm-projectile-switch-project)
  (find-file "~/.dotfiles")
  (helm-projectile-switch-project)
  (persp-add-buffer "*dotfiles")
  (persp-kill "@dotfiles"))

;; Jump to last perspective
;; taken from Magnar Sveen
(defun dotemacs-custom-persp-last ()
  (interactive)
  (persp-switch (persp-name persp-last)))

(provide 'init-perspective)
