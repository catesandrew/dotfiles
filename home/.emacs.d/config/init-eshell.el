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

;; Defining a function like this makes it possible to type 'clear' in eshell and
;; have it work
(defun eshell/clear ()
  "Clear contents in eshell."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (eshell-send-input))

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

(defun dotemacs-eshell-auto-end ()
  "Move point to end of current prompt when switching to insert state."
  (when (and (eq major-mode 'eshell-mode)
             ;; Not on last line, we might want to edit within it.
             (not (eq (line-end-position) (point-max))))
    (end-of-buffer)))

(defun dotemacs-protect-eshell-prompt ()
  "Protect Eshell's prompt like Comint's prompts.

E.g. `evil-change-whole-line' won't wipe the prompt. This
is achieved by adding the relevant text properties."
  (let ((inhibit-field-text-motion t))
    (add-text-properties
     (point-at-bol)
     (point)
     '(rear-nonsticky t
       inhibit-line-move-field-capture t
       field output
       read-only t
       front-sticky (field inhibit-line-move-field-capture)))))

(defun dotemacs-init-eshell ()
  "Stuff to do when enabling eshell."
  (setq pcomplete-cycle-completions nil)
  (if (bound-and-true-p linum-mode) (linum-mode -1))
  (unless dotemacs-shell-enable-smart-eshell
    ;; we don't want auto-jump to prompt when smart eshell is enabled.
    ;; Idea: maybe we could make auto-jump smarter and jump only if the
    ;; point is not on a prompt line
    (add-hook 'evil-insert-state-entry-hook
              'dotemacs-eshell-auto-end nil t))
  ; (after "semantic"
  ;   (semantic-mode -1))

  ;; Caution! this will erase buffer's content at C-l
  (define-key eshell-mode-map (kbd "C-l") 'eshell/clear)
  (define-key eshell-mode-map (kbd "C-d") 'eshell-delchar-or-maybe-eof))

(provide 'init-eshell)
;;; init-eshell.el ends here
