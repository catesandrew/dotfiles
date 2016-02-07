;;; module-file-search.el --- File Search Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'evil-evilified-state)
;; (require 'core-vars)
;; (require 'core-funcs)
(require 'core-keybindings)
;; (require 'core-display-init)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

;; --no-color, oddly enough, is required to allow emacs to colorize the output
(defvar git-grep-switches "--extended-regexp -I -n --ignore-case --no-color"
  "Switches to pass to `git grep'.")

(use-package grep
  :defer t
  :config
  (progn
    (defun dotemacs-rgrep-quit-window ()
      (interactive)
      (kill-buffer)
      (jump-to-register ?$))
    (define-key grep-mode-map "q" 'dotemacs-rgrep-quit-window)

    (defun dotemacs-rgrep-goto-file-and-close-rgrep ()
      (interactive)
      (compile-goto-error)
      (kill-buffer "*grep*")
      (delete-other-windows)
      (message "Type C-x r j $ to return to pre-rgrep windows."))
    (define-key grep-mode-map (kbd "C-<return>") 'dotemacs-rgrep-goto-file-and-close-rgrep)

    (when-let (gnu-find (and (eq system-type 'darwin)
                             (executable-find "gfind")))
      (setq find-program gnu-find))

    (when-let (gnu-xargs (and (eq system-type 'darwin)
                              (executable-find "gxargs")))
      (setq xargs-program gnu-xargs))))

(use-package locate                     ; Search files on the system
  :defer t
  :config
  ;; Use mdfind as locate substitute on OS X, to utilize the Spotlight database
  (when-let (mdfind (and (eq system-type 'darwin) (executable-find "mdfind")))
    (setq locate-command mdfind)))

(use-package fasd
  :ensure t
  :init
  (progn
    (global-fasd-mode 1)

    (defun fasd-find-file-only ()
      (interactive)
      (fasd-find-file -1))

    (defun fasd-find-directory-only ()
      (interactive)
      (fasd-find-file 1))

    (dotemacs-declare-prefix "fa" "fasd-find")
    (dotemacs-set-leader-keys "fad" 'fasd-find-directory-only)
    (dotemacs-set-leader-keys "faf" 'fasd-find-file-only)
    (dotemacs-set-leader-keys "fas" 'fasd-find-file)

    ;; we will fall back to using the default completing-read function, which is helm once helm is loaded.
    (setq fasd-completing-read-function 'nil)))

(use-package helm-ag
  :ensure t
  :defer t
  :init
  (progn
    (defun dotemacs-helm-do-ag-region-or-symbol (func &optional dir)
      "Search with `ag' with a default input."
      (require 'helm-ag)
      (cl-letf* (((symbol-value 'helm-ag-insert-at-point) 'symbol)
                 ;; make thing-at-point choosing the active region first
                 ((symbol-function 'this-fn) (symbol-function 'thing-at-point))
                 ((symbol-function 'thing-at-point)
                  (lambda (thing)
                    (let ((res (if (region-active-p)
                                   (buffer-substring-no-properties
                                    (region-beginning) (region-end))
                                 (this-fn thing))))
                      (when res (rxt-quote-pcre res))))))
        (funcall func dir)))

    (defun dotemacs-helm-do-search-find-tool (base tools default-inputp)
      "Create a cond form given a TOOLS string list and evaluate it."
      (eval
       `(cond
         ,@(mapcar
            (lambda (x)
              `((executable-find ,x)
                ',(let ((func
                         (intern
                          (format (if default-inputp
                                      "dotemacs-%s-%s-region-or-symbol"
                                    "dotemacs-%s-%s")
                                  base x))))
                    (if (fboundp func)
                        func
                      (intern (format "%s-%s"  base x))))))
            tools)
         (t 'helm-do-grep))))

    ;; Search in current file ----------------------------------------------

    (defun dotemacs-helm-file-do-ag (&optional _)
      "Wrapper to execute `helm-ag-this-file.'"
      (interactive)
      (helm-ag-this-file))

    (defun dotemacs-helm-file-do-ag-region-or-symbol ()
      "Search in current file with `ag' using a default input."
      (interactive)
      (dotemacs-helm-do-ag-region-or-symbol 'dotemacs-helm-file-do-ag))

    (defun dotemacs-helm-file-smart-do-search (&optional default-inputp)
      "Search in current file using `dotemacs-search-tools'.
Search for a search tool in the order provided by `dotemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
      (interactive)
      (call-interactively
       (dotemacs-helm-do-search-find-tool "helm-file-do"
                                          dotemacs-search-tools
                                          default-inputp)))

    (defun dotemacs-helm-file-smart-do-search-region-or-symbol ()
      "Search in current file using `dotemacs-search-tools' with
 default input.
Search for a search tool in the order provided by `dotemacs-search-tools'."
      (interactive)
      (dotemacs-helm-file-smart-do-search t))

    ;; Search in files -----------------------------------------------------

    (defun dotemacs-helm-files-do-ag (&optional dir)
      "Search in files with `ag' using a default input."
      (interactive)
      (let ((helm-ag-use-agignore t)
            (helm-ag-base-command "ag --nocolor --nogroup --smart-case --hidden"))
        (helm-do-ag dir)))

    (defun dotemacs-helm-files-do-ag-region-or-symbol ()
      "Search in files with `ag' using a default input."
      (interactive)
      (dotemacs-helm-do-ag-region-or-symbol 'dotemacs-helm-files-do-ag))

    (defun dotemacs-helm-files-do-ack (&optional dir)
      "Search in files with `ack'."
      (interactive)
      (let ((ackrc (concat user-home-directory ".ackrc")))
        (if (file-exists-p ackrc)
            (let ((helm-ag-base-command (format "ack --nocolor --nogroup --ackrc=%s" ackrc)))
              (helm-do-ag dir))
          (let ((helm-ag-base-command "ack --nocolor --nogroup"))
            (helm-do-ag dir)))))

    (defun dotemacs-helm-files-do-ack-region-or-symbol ()
      "Search in files with `ack' using a default input."
      (interactive)
      (dotemacs-helm-do-ag-region-or-symbol 'dotemacs-helm-files-do-ack))

    (defun dotemacs-helm-files-do-pt (&optional dir)
      "Search in files with `pt'."
      (interactive)
      (let ((ptignore (concat user-home-directory ".ptignore.toml")))
        (if (file-exists-p ptignore)
            (let ((helm-ag-base-command "pt -e --nocolor --nogroup --hidden --home-ptignore"))
              (helm-do-ag dir))
          (let ((helm-ag-base-command "pt -e --nocolor --nogroup --hidden --smart-case"))
            (helm-do-ag dir)))))

    (defun dotemacs-helm-files-do-pt-region-or-symbol ()
      "Search in files with `pt' using a default input."
      (interactive)
      (dotemacs-helm-do-ag-region-or-symbol 'dotemacs-helm-files-do-pt))

    (defun dotemacs-helm-files-smart-do-search (&optional default-inputp)
      "Search in opened buffers using `dotemacs-search-tools'.
Search for a search tool in the order provided by `dotemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
      (interactive)
      (call-interactively
       (dotemacs-helm-do-search-find-tool "helm-files-do"
                                          dotemacs-search-tools
                                          default-inputp)))

    (defun dotemacs-helm-files-smart-do-search-region-or-symbol ()
      "Search in opened buffers using `dotemacs-search-tools'.
with default input.
Search for a search tool in the order provided by `dotemacs-search-tools'."
      (interactive)
      (dotemacs-helm-files-smart-do-search t))

    ;; Search in buffers ---------------------------------------------------

    (defun dotemacs-helm-buffers-do-ag (&optional _)
      "Wrapper to execute `helm-ag-buffers.'"
      (interactive)
      (let ((helm-ag-use-agignore t)
            (helm-ag-base-command "ag --nocolor --nogroup --smart-case --hidden"))
        (helm-do-ag-buffers)))

    (defun dotemacs-helm-buffers-do-ag-region-or-symbol ()
      "Search in opened buffers with `ag' with a default input."
      (interactive)
      (dotemacs-helm-do-ag-region-or-symbol 'dotemacs-helm-buffers-do-ag))

    (defun dotemacs-helm-buffers-do-ack (&optional _)
      "Search in opened buffers with `ack'."
      (interactive)
      (let ((ackrc (concat user-home-directory ".ackrc")))
        (if (file-exists-p ackrc)
            (let ((helm-ag-base-command (format "ack --nocolor --nogroup --ackrc=%s" ackrc)))
              (helm-do-ag-buffers))
          (let ((helm-ag-base-command "ack --nocolor --nogroup"))
            (helm-do-ag-buffers)))))

    (defun dotemacs-helm-buffers-do-ack-region-or-symbol ()
      "Search in opened buffers with `ack' with a default input."
      (interactive)
      (dotemacs-helm-do-ag-region-or-symbol 'dotemacs-helm-buffers-do-ack))

    (defun dotemacs-helm-buffers-do-pt (&optional _)
      "Search in opened buffers with `pt'."
      (interactive)
      (let ((ptignore (concat user-home-directory ".ptignore.toml")))
        (if (file-exists-p ptignore)
            (let ((helm-ag-base-command "pt -e --nocolor --nogroup --hidden --home-ptignore"))
              (helm-do-ag-buffers))
          (let ((helm-ag-base-command "pt -e --nocolor --nogroup --hidden --smart-case"))
            (helm-do-ag-buffers)))))

    (defun dotemacs-helm-buffers-do-pt-region-or-symbol ()
      "Search in opened buffers with `pt' using a default input."
      (interactive)
      (dotemacs-helm-do-ag-region-or-symbol 'dotemacs-helm-buffers-do-pt))

    (defun dotemacs-helm-buffers-smart-do-search (&optional default-inputp)
      "Search in opened buffers using `dotemacs-search-tools'.
Search for a search tool in the order provided by `dotemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
      (interactive)
      (call-interactively
       (dotemacs-helm-do-search-find-tool "helm-buffers-do"
                                          dotemacs-search-tools
                                          default-inputp)))

    (defun dotemacs-helm-buffers-smart-do-search-region-or-symbol ()
      "Search in opened buffers using `dotemacs-search-tools' with
default input.
Search for a search tool in the order provided by `dotemacs-search-tools'."
      (interactive)
      (dotemacs-helm-buffers-smart-do-search t))

    ;; Search in project ---------------------------------------------------

    (defun dotemacs-helm-project-do-ag ()
      "Search in current project with `ag'."
      (interactive)
      (let ((dir (projectile-project-root)))
        (if dir
            (helm-do-ag dir)
          (message "error: Not in a project."))))

    (defun dotemacs-helm-project-do-ag-region-or-symbol ()
      "Search in current project with `ag' using a default input."
      (interactive)
      (let ((dir (projectile-project-root)))
        (if dir
            (dotemacs-helm-do-ag-region-or-symbol 'helm-do-ag dir)
          (message "error: Not in a project."))))

    (defun dotemacs-helm-project-do-ack ()
      "Search in current project with `ack'."
      (interactive)
      (let ((dir (projectile-project-root)))
        (if dir
            (dotemacs-helm-files-do-ack dir)
          (message "error: Not in a project."))))

    (defun dotemacs-helm-project-do-ack-region-or-symbol ()
      "Search in current project with `ack' using a default input."
      (interactive)
      (let ((dir (projectile-project-root)))
        (if dir
            (dotemacs-helm-do-ag-region-or-symbol 'dotemacs-helm-files-do-ack dir)
          (message "error: Not in a project."))))

    (defun dotemacs-helm-project-do-pt ()
      "Search in current project with `pt'."
      (interactive)
      (let ((dir (projectile-project-root)))
        (if dir
            (dotemacs-helm-files-do-pt dir)
          (message "error: Not in a project."))))

    (defun dotemacs-helm-project-do-pt-region-or-symbol ()
      "Search in current project with `pt' using a default input."
      (interactive)
      (let ((dir (projectile-project-root)))
        (if dir
            (dotemacs-helm-do-ag-region-or-symbol 'dotemacs-helm-files-do-pt dir)
          (message "error: Not in a project."))))

    (defun dotemacs-helm-project-smart-do-search (&optional default-inputp)
      "Search in current project using `dotemacs-search-tools'.
Search for a search tool in the order provided by `dotemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
      (interactive)
      (let ((projectile-require-project-root nil))
        (call-interactively
         (dotemacs-helm-do-search-find-tool "helm-project-do"
                                            dotemacs-search-tools
                                            default-inputp))))

    (defun dotemacs-helm-project-smart-do-search-region-or-symbol ()
      "Search in current project using `dotemacs-search-tools' with
 default input.
Search for a search tool in the order provided by `dotemacs-search-tools'."
      (interactive)
      (dotemacs-helm-project-smart-do-search t))

    ;; This overrides the default C-s action in helm-projectile-switch-project
    ;; to search using ag/pt/whatever instead of just grep
    (with-eval-after-load 'helm-projectile
      (defun dotemacs-helm-project-smart-do-search-in-dir (dir)
        (interactive)
        (let ((default-directory dir))
          (dotemacs-helm-project-smart-do-search)))
      (define-key helm-projectile-projects-map
        (kbd "C-s")
        (lambda ()
          (interactive)
          (helm-exit-and-execute-action
           'dotemacs-helm-project-smart-do-search-in-dir))))

    ;; evilify the helm-grep buffer
    (evilified-state-evilify helm-grep-mode helm-grep-mode-map
      (kbd "RET") 'helm-grep-mode-jump-other-window
      (kbd "q") 'quit-window)

    (dotemacs-set-leader-keys
      ;; helm-ag marks
      "s`"  'helm-ag-pop-stack
      ;; opened buffers scope
      "sb"  'dotemacs-helm-buffers-smart-do-search
      "sB"  'dotemacs-helm-buffers-smart-do-search-region-or-symbol
      "sab" 'helm-do-ag-buffers
      "saB" 'dotemacs-helm-buffers-do-ag-region-or-symbol
      "skb" 'dotemacs-helm-buffers-do-ack
      "skB" 'dotemacs-helm-buffers-do-ack-region-or-symbol
      "stb" 'dotemacs-helm-buffers-do-pt
      "stB" 'dotemacs-helm-buffers-do-pt-region-or-symbol
      ;; current file scope
      "ss"  'dotemacs-helm-file-smart-do-search
      "sS"  'dotemacs-helm-file-smart-do-search-region-or-symbol
      "saa" 'helm-ag-this-file
      "saA" 'dotemacs-helm-file-do-ag-region-or-symbol
      ;; files scope
      "sf"  'dotemacs-helm-files-smart-do-search
      "sF"  'dotemacs-helm-files-smart-do-search-region-or-symbol
      "saf" 'helm-do-ag
      "saF" 'dotemacs-helm-files-do-ag-region-or-symbol
      "skf" 'dotemacs-helm-files-do-ack
      "skF" 'dotemacs-helm-files-do-ack-region-or-symbol
      "stf" 'dotemacs-helm-files-do-pt
      "stF" 'dotemacs-helm-files-do-pt-region-or-symbol
      ;; current project scope
      ;; "/"   'dotemacs-helm-project-smart-do-search
      ;; "*"   'dotemacs-helm-project-smart-do-search-region-or-symbol
      "sp"  'dotemacs-helm-project-smart-do-search
      "sP"  'dotemacs-helm-project-smart-do-search-region-or-symbol
      "sap" 'dotemacs-helm-project-do-ag
      "saP" 'dotemacs-helm-project-do-ag-region-or-symbol
      "skp" 'dotemacs-helm-project-do-ack
      "skP" 'dotemacs-helm-project-do-ack-region-or-symbol
      "stp" 'dotemacs-helm-project-do-pt
      "stP" 'dotemacs-helm-project-do-pt-region-or-symbol))
  :config
  (progn
    ;; Use `grep-find-ignored-files' and `grep-find-ignored-directories' as
    ;; ignore pattern, but does not seem to be working, need to confirm
    ;; (setq helm-ag-use-grep-ignore-list nil)
    ;; example: (helm-ag-ignore-patterns '("*.md" "*.el"))
    ;; (setq helm-ag-ignore-patterns '(append grep-find-ignored-files
    ;;                                        grep-find-ignored-directories))

  (evil-define-key 'normal helm-ag-map "SPC" dotemacs-default-map)
  (evilified-state-evilify helm-ag-mode helm-ag-mode-map
    (kbd "RET") 'helm-ag-mode-jump-other-window
    (kbd "q") 'quit-window)))

(provide 'module-file-search)
;;; module-file-search.el ends here
