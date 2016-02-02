;;; Firestarter
(require 'module-global)
;; (require 'firestarter)

(defvar-local dotemacs-firestarter-last-status nil
  "The last state of firestarter.")

(defun dotemacs-firestarter-track-state (process)
  "Track the state of a firestarter PROCESS in the corresponding buffer."
  (let ((status (process-status process))
        (return-code (process-exit-status process))
        (buffer (get-buffer (process-get process 'buffer-name))))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (let ((status (pcase (list status return-code)
                        (`(exit 0) 'success)
                        (_ 'failure))))
          (setq dotemacs-firestarter-last-status status))
        (force-mode-line-update)))))

(add-hook 'firestarter-reporting-functions #'dotemacs-firestarter-track-state)

(advice-add 'firestarter :before
            (lambda ()
              (setq dotemacs-firestarter-last-status 'running)
              (force-mode-line-update))
            '((name . dotemacs-firestarter-track-running)))

(defun dotemacs-firestarter-mode-line ()
  "Get a mode line status string for firestarter."
  (let ((base "🔥")
        (status-string (pcase dotemacs-firestarter-last-status
                         ((guard (not firestarter)) " ")
                         (`running "⚫")
                         (`success "🔵")
                         (`failure "🔴")
                         (_ "⚪"))))
    (concat " " base status-string)))

(use-package firestarter                ; Run commands after save
  :ensure t
  :init (firestarter-mode)
  :config (progn (setq firestarter-default-type 'failure)
                 (dotemacs-load-private-file "firestarter-safe-values.el"
                                             'noerror))

  ;; Remove space from firestarter lighter
  :diminish firestarter-mode)

(provide 'module-firestarter)
;;; module-firestarter.el ends here
