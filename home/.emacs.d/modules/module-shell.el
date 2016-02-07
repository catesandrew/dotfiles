;;; module-shell.el --- Shell Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;;; Code:
(require 'use-package)
(require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
(require 'module-utils)


;; built-in variables

;; move point to the end of buffer on new output
(setq comint-move-point-for-output t)

;; Add shell buffers to useful buffers list
(push "\\*\\(ansi-term\\|eshell\\|shell\\|terminal\.\+\\)\\*" dotemacs-useful-buffers-regexp)


;; variables
(defvar shell-default-shell
  (if (eq window-system 'w32)
      'eshell
    'ansi-term)
  "Default shell to use in emacs. Possible values are `eshell', `shell',
`term', `multi-term`,  and `ansi-term'.")

(defvar shell-default-position 'bottom
  "Position of the shell. Possible values are `top', `bottom' and `full'.")

(defvar shell-default-height 30
  "Height in percents for the shell window.")

(defvar shell-default-term-shell shell-file-name
  "Default shell to use in `term' and `ansi-term' shells.")


;; funcs
(defun dotemacs/projectile-shell-pop ()
  "Open a term buffer at projectile project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-interactively 'dotemacs/default-pop-shell)))


;; packages
(use-package comint
  :init
  (setq comint-prompt-read-only t))

(dotemacs-use-package-add-hook magit
  :post-init
  (defalias 's 'magit-status))

(use-package multi-term
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-register-repl 'multi-term 'multi-term)
    (dotemacs-set-leader-keys "ast" 'shell-pop-multi-term)
    (defun multiterm (_)
      "Wrapper to be able to call multi-term from shell-pop"
      (interactive)
      (multi-term)))
  :config
  (progn
    (defun term-send-tab ()
      "Send tab in term mode."
      (interactive)
      (term-send-raw-string "\t"))
    (add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab))
    ;; multi-term commands to create terminals and move through them.
    (dotemacs-set-leader-keys-for-major-mode 'term-mode "c" 'multi-term)
    (dotemacs-set-leader-keys-for-major-mode 'term-mode "p" 'multi-term-prev)
    (dotemacs-set-leader-keys-for-major-mode 'term-mode "n" 'multi-term-next)

    (defun projectile-multi-term-in-root ()
      "Invoke `multi-term' in the project's root."
      (interactive)
      (projectile-with-default-dir (projectile-project-root) (multi-term)))
    (dotemacs-set-leader-keys "p$t" 'projectile-multi-term-in-root)))

(dotemacs-use-package-add-hook projectile
  :post-init
  (dotemacs-set-leader-keys "p'" 'dotemacs/projectile-shell-pop))

(use-package shell                      ; Dump shell in Emacs
  :init
  (progn
    (dotemacs-register-repl 'shell 'shell)
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
                (setq command (replace-regexp-in-string
                               "^[ \t]*man[ \t]*" "" command))
                (setq command (replace-regexp-in-string
                               "[ \t]+$" "" command))
                (funcall 'man command))
               ;; Send other commands to the default handler.
               (t (comint-simple-send proc command))))))
    (add-hook 'shell-mode-hook 'shell-comint-input-sender-hook)))

(use-package shell-pop
  :defer t
  :ensure t
  :init
  (progn
    (setq shell-pop-window-position shell-default-position
          shell-pop-window-height   shell-default-height
          shell-pop-term-shell      shell-default-term-shell
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

    (defun ansi-term-handle-close ()
      "Close current term buffer when `exit' from term buffer."
      (when (ignore-errors (get-buffer-process (current-buffer)))
        (set-process-sentinel (get-buffer-process (current-buffer))
                              (lambda (proc change)
                                (when (string-match "\\(finished\\|exited\\)" change)
                                  (kill-buffer (process-buffer proc))
                                  (delete-window))))))
    (add-hook 'term-mode-hook 'ansi-term-handle-close)
    (add-hook 'term-mode-hook (lambda () (linum-mode -1)))

    (defun dotemacs/default-pop-shell ()
      "Open the default shell in a popup."
      (interactive)
      (let ((shell (if (eq 'multi-term shell-default-shell)
                       'multiterm
                     shell-default-shell)))
        (call-interactively (intern (format "shell-pop-%S" shell)))))

    (dotemacs-set-leader-keys
      "'"   'dotemacs/default-pop-shell
      "ase" 'shell-pop-eshell
      "asi" 'shell-pop-shell
      "asm" 'shell-pop-multiterm
      "ast" 'shell-pop-ansi-term
      "asT" 'shell-pop-term)))

(dotemacs-use-package-add-hook smooth-scrolling
  :post-init
  (dotemacs/add-to-hooks 'dotemacs//unset-scroll-margin
                         '(eshell-mode-hook
                           comint-mode-hook
                           term-mode-hook)))

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

(use-package xterm-color
  :ensure t
  :init
  (progn
    ;; Comint and Shell
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
    (setq comint-output-filter-functions
          (remove 'ansi-color-process-output comint-output-filter-functions))
    (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region)
    (with-eval-after-load 'esh-mode
      (add-hook 'eshell-mode-hook
                (lambda () (setq xterm-color-preserve-properties t)))
      (add-hook 'eshell-preoutput-filter-functions 'xterm-color-filter)
      (setq eshell-output-filter-functions
            (remove 'eshell-handle-ansi-color
                    eshell-output-filter-functions)))))

(use-package bracketed-paste
  :defer t
  :ensure t
  :init
  ;; Enable bracketed-paste for tty
  (add-hook 'tty-setup-hook 'bracketed-paste-enable))

(provide 'module-shell)
;;; module-shell.el ends here
