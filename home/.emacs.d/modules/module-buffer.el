;;; Buffer
(require 'module-global)

(use-package buffer-move
  :defer t
  :init
  (dotemacs-set-leader-keys
    "bmh" 'buf-move-left
    "bmj" 'buf-move-down
    "bmk" 'buf-move-up
    "bml" 'buf-move-right))

(use-package uniquify                   ; Make buffer names unique
  ;; When having windows with repeated filenames, uniquify them
  ;; by the folder they are in rather those annoying <2>,<3>,.. etc
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets
                uniquify-separator "/"
                uniquify-ignore-buffers-re "^\\*" ; leave special buffers alone
                uniquify-after-kill-buffer-p t))

(use-package helm-buffers
  :ensure helm
  :defer t
  :config (setq helm-buffers-fuzzy-matching t))

;; Don't kill the important buffers
(defconst dotemacs-do-not-kill-buffer-names '("*scratch*" "*Messages*" "*Require Times*")
  "Names of buffers that should not be killed.")

(defun dotemacs-do-not-kill-important-buffers ()
  "Inhibit killing of important buffers.

Add this to `kill-buffer-query-functions'."
  (if (not (member (buffer-name) dotemacs-do-not-kill-buffer-names))
      t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (bury-buffer)
    nil))

(defun dotemacs-force-save-some-buffers ()
  "Save all modified buffers, without prompts."
  (save-some-buffers 'dont-ask))

(add-hook 'kill-buffer-query-functions
          #'dotemacs-do-not-kill-important-buffers)

;; Autosave buffers when focus is lost, see
;; http://emacsredux.com/blog/2014/03/22/a-peek-at-emacs-24-dot-4-focus-hooks/
(add-hook 'focus-out-hook #'dotemacs-force-save-some-buffers)

(provide 'module-buffer)
;;; module-buffer.el ends here
