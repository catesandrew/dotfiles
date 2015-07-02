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

(defun dotemacs-ibuffer-get-major-modes-ibuff-rules-list (mm-list result-list)
  (if mm-list
      (let* ((cur-mm (car mm-list))
             (next-res-list-el `(,(symbol-name cur-mm) (mode . ,cur-mm))))
        (dotemacs-ibuffer-get-major-modes-ibuff-rules-list
         (cdr mm-list) (cons next-res-list-el result-list)))
    result-list))

(defun dotemacs-ibuffer-get-major-modes-list ()
  (mapcar
   (function (lambda (buffer)
               (buffer-local-value 'major-mode (get-buffer buffer))))
   (buffer-list (selected-frame))))

(defun dotemacs-ibuffer-create-buffs-group ()
  (interactive)
  (let* ((ignore-modes '(Buffer-menu-mode
                         compilation-mode
                         minibuffer-inactive-mode
                         ibuffer-mode
                         magit-process-mode
                         messages-buffer-mode
                         fundamental-mode
                         completion-list-mode
                         help-mode
                         Info-mode))
         (cur-bufs
          (list (cons "Home"
                      (dotemacs-ibuffer-get-major-modes-ibuff-rules-list
                       (cl-set-difference
                        (remove-duplicates
                         (dotemacs-ibuffer-get-major-modes-list))
                        ignore-modes) '())))))
    (setq ibuffer-saved-filter-groups cur-bufs)
    (ibuffer-switch-to-saved-filter-groups "Home")))

(defun dotemacs-ibuffer-group-by-projects ()
  "Group buffers by projects."
  (when (eq 'projects dotemacs-ibuffer-group-buffers-by)
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic))))

(defun dotemacs-ibuffer-group-by-modes ()
  "Group buffers by modes."
  (when (eq 'modes dotemacs-ibuffer-group-buffers-by)
    (dotemacs-ibuffer-create-buffs-group)))

(provide 'init-buffers)
