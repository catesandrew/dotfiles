(defun dotemacs-current-git-branch ()
  (let ((branch (car (loop for match in (split-string (shell-command-to-string "git branch") "\n")
                           when (string-match "^\*" match)
                           collect match))))
    (if (not (eq branch nil))
        (concat " [" (substring branch 2) "]")
      "")))

(defun dotemacs-eshell-prompt ()
  (concat (propertize (abbreviate-file-name (eshell/pwd)) 'face 'eshell-prompt)
          (propertize (dotemacs-current-git-branch) 'face 'font-lock-function-name-face)
          (propertize " $ " 'face 'font-lock-constant-face)))

(setq dotemacs-eshell-buffer-count 0)
(defun dotemacs-new-eshell-split ()
  (interactive)
  (split-window)
  (setq dotemacs-eshell-buffer-count (+ 1 dotemacs-eshell-buffer-count))
  (eshell dotemacs-eshell-buffer-count))

(defun projectile-multi-term-in-root ()
  "Invoke `multi-term' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-project-root) (multi-term)))

(defun shell-comint-input-sender-hook ()
  "Check certain shell commands.
 Executes the appropriate behavior for certain commands."
  (setq comint-input-sender
        (lambda (proc command)
          (cond
           ;; Check for clear command and execute it.
           ((string-match "^[ \t]*clear[ \t]*$" command)
            (comint-send-string proc "\n")
            (erase-buffer))
           ;; Check for man command and execute it.
           ((string-match "^[ \t]*man[ \t]*" command)
            (comint-send-string proc "\n")
            (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
            (setq command (replace-regexp-in-string "[ \t]+$" "" command))
            (funcall 'man command))
           ;; Send other commands to the default handler.
           (t (comint-simple-send proc command))))))

(defun eshell/clear ()
  "Clear contents in eshell."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun dotemacs-term-kill-buffer-hook ()
  "Function that hook `kill-buffer-hook'."
  (when (eq major-mode 'term-mode)
    (when (term-check-proc (current-buffer))
      (term-quit-subjob))))

(defun dotemacs-default-pop-shell ()
  "Open the default shell in a popup."
  (interactive)
  (let ((shell (if (eq 'multi-term dotemacs-shell-default-shell)
                   'multiterm
                 dotemacs-shell-default-shell)))
    (call-interactively (intern (format "shell-pop-%S" shell)))))

(defun ansi-term-handle-close ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc))
                              (delete-window))))))

(defun term-send-tab ()
  "Send tab in term mode."
  (interactive)
  (term-send-raw-string "\t"))

(defun multiterm (_)
  "Wrapper to be able to call multi-term from shell-pop"
  (interactive)
  (multi-term))

(provide 'init-eshell)
