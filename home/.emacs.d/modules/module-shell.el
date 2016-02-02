;;; Shell
(require 'module-global)

(use-package multi-term
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-register-repl 'multi-term 'multi-term)
    (dotemacs-set-leader-keys "ast" 'shell-pop-multi-term))
  :config
  (progn
    (add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab))
    ;; multi-term commands to create terminals and move through them.
    (dotemacs-set-leader-keys-for-major-mode 'term-mode "c" 'multi-term)
    (dotemacs-set-leader-keys-for-major-mode 'term-mode "p" 'multi-term-prev)
    (dotemacs-set-leader-keys-for-major-mode 'term-mode "n" 'multi-term-next)
    (dotemacs-set-leader-keys "p$t" 'projectile-multi-term-in-root)))

(use-package xterm-color
  :ensure t
  :init
  (progn
    ;; Comint and Shell
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
    (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
    (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region)
    (with-eval-after-load 'esh-mode
      (add-hook 'eshell-mode-hook (lambda () (setq xterm-color-preserve-properties t)))
      (add-hook 'eshell-preoutput-filter-functions 'xterm-color-filter)
      (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))))

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

(defun projectile-multi-term-in-root ()
  "Invoke `multi-term' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-project-root) (multi-term)))

(use-package shell                      ; Dump shell in Emacs
  :init
  (dotemacs-register-repl 'shell 'shell)
  (add-hook 'shell-mode-hook 'shell-comint-input-sender-hook))

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

(defun multiterm (_)
  "Wrapper to be able to call multi-term from shell-pop"
  (interactive)
  (multi-term))

(use-package shell-pop
  :defer t
  :ensure t
  :init
  (progn
    (setq shell-pop-window-position dotemacs-shell-default-position
          shell-pop-window-height   dotemacs-shell-default-height
          shell-pop-term-shell      dotemacs-shell-default-term-shell
          shell-pop-full-span t)
    (defmacro make-shell-pop-command (type &optional shell)
      (let* ((name (symbol-name type)))
        `(defun ,(intern (concat "shell-pop-" name)) (index)
           (interactive "P")
           (require 'shell-pop)
           (shell-pop--set-shell-type
            'shell-pop-shell-type
            (backquote (,name
                        ,(concat "*" name "*")
                        (lambda nil (funcall ',type ,shell)))))
           (shell-pop index))))
    (make-shell-pop-command eshell)
    (make-shell-pop-command shell)
    (make-shell-pop-command term shell-pop-term-shell)
    (make-shell-pop-command multiterm)
    (make-shell-pop-command ansi-term shell-pop-term-shell)

    (add-hook 'term-mode-hook 'ansi-term-handle-close)
    (add-hook 'term-mode-hook (lambda () (linum-mode -1)))

    (dotemacs-set-leader-keys
      "'"   'dotemacs-default-pop-shell
      "ase" 'shell-pop-eshell
      "asi" 'shell-pop-shell
      "asm" 'shell-pop-multiterm
      "ast" 'shell-pop-ansi-term
      "asT" 'shell-pop-term)))

(defun term-send-tab ()
  "Send tab in term mode."
  (interactive)
  (term-send-raw-string "\t"))

(use-package term
  :init
  (progn
    (dotemacs-register-repl 'term 'term)
    (dotemacs-register-repl 'term 'ansi-term)
    (defun term-send-tab ()
      "Send tab in term mode."
      (interactive)
      (term-send-raw-string "\t"))

    ;; hack to fix pasting issue, the paste micro-state won't
    ;; work in term
    (evil-define-key 'normal term-raw-map "p" 'term-paste)
    (evil-define-key 'insert term-raw-map (kbd "C-c C-d") 'term-send-eof)
    (evil-define-key 'insert term-raw-map (kbd "C-c C-z") 'term-stop-subjob)
    (evil-define-key 'insert term-raw-map (kbd "<tab>") 'term-send-tab)

    (evil-define-key 'insert term-raw-map
      (kbd "C-k") 'term-send-up
      (kbd "C-j") 'term-send-down)
    (evil-define-key 'normal term-raw-map
      (kbd "C-k") 'term-send-up
      (kbd "C-j") 'term-send-down)))

(dotemacs-use-package-add-hook smooth-scrolling
  :post-init
  (dotemacs/add-to-hooks 'dotemacs//unset-scroll-margin
                          '(eshell-mode-hook
                            comint-mode-hook
                            term-mode-hook)))

(dotemacs-use-package-add-hook magit
  :post-init
  (defalias 's 'magit-status))

(provide 'module-shell)
;;; module-shell.el ends here
