;;; module-file-search.el --- File Search Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-vars)
;; (require 'core-funcs)
;; (require 'core-keybindings)
;; (require 'core-display-init)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(defun dotemacs-rgrep-quit-window ()
  (interactive)
  (kill-buffer)
  (jump-to-register ?$))

(defun dotemacs-rgrep-goto-file-and-close-rgrep ()
  (interactive)
  (compile-goto-error)
  (kill-buffer "*grep*")
  (delete-other-windows)
  (message "Type C-x r j $ to return to pre-rgrep windows."))

;; --no-color, oddly enough, is required to allow emacs to colorize the output
(defvar git-grep-switches "--extended-regexp -I -n --ignore-case --no-color"
  "Switches to pass to `git grep'.")

(use-package helm-regex                 ; Helm regex tools
  :ensure helm
  :bind (([remap occur] . helm-occur)
         ("C-c e o"     . helm-multi-occur)))

(use-package grep
  :defer t
  :config
  (progn
    ;; Add custom keybindings
    (define-key grep-mode-map "q" #'dotemacs-rgrep-quit-window)
    (define-key grep-mode-map (kbd "C-<return>") #'dotemacs-rgrep-goto-file-and-close-rgrep)

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

(use-package ag                         ; Search code in files/projects
  :ensure t
  :config
  ; (add-hook 'ag-mode-hook (lambda () (toggle-truncate-lines t)))
  (setq ag-reuse-buffers t            ; Don't spam buffer list with ag buffers
        ag-highlight-search t         ; A little fanciness

        ;; Use Projectile to find the project root
        ag-project-root-function (lambda (d) (let ((default-directory d))
                                               (projectile-project-root)))))

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

(use-package wgrep                      ; Edit grep/occur/ag results in-place
  :ensure t
  :defer t
  :config
  (progn
    ;; Add custom keybindings
    (define-key grep-mode-map (kbd "C-x C-s") #'wgrep-save-all-buffers)
    ;; Use same keybinding as occur
    (setq wgrep-enable-key "e")))

(use-package wgrep-ag                   ; Wgrep for ag
  :ensure t
  :defer t)

(use-package helm-ag
  :ensure t
  :defer t
  :init
  (progn
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
          (helm-exit-and-execute-action 'dotemacs-helm-project-smart-do-search-in-dir))))

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
    (setq helm-ag-use-grep-ignore-list t)

    ;; example: (helm-ag-ignore-patterns '("*.md" "*.el"))
    ;; (setq helm-ag-ignore-patterns '(append grep-find-ignored-files
    ;;                                        grep-find-ignored-directories))

    (setq helm-ag-fuzzy-match t
          helm-ag-base-command "ag --nocolor --nogroup --hidden"
          helm-ag-insert-at-point 'symbol
          helm-ag-source-type 'file-line))

  (evil-define-key 'normal helm-ag-map (kbd dotemacs-leader-key) dotemacs-default-map)
    (evilified-state-evilify helm-ag-mode helm-ag-mode-map
      (kbd "RET") 'helm-ag-mode-jump-other-window
      (kbd "q") 'quit-window))

(provide 'module-file-search)
;;; module-file-search.el ends here
