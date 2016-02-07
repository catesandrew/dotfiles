;;; module-vinegar.el --- Vinegar Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; Tim Pope
;;
;; This layer is a port contribution layer for vim-vinegar for Emacs.
;;
;; A port of tpope's vinegar.vim
;; [vinegar][https://github.com/tpope/vim-vinegar], simplifying =dired=
;; with a limited number of details and exposing the ~-~ command in all
;; buffers to enter dired.
;;
;; ** Features
;;
;; -  navigation up folders with ~-~ key
;; -  simplify dired buffer to show only file names
;; -  better evil/vim bindings for navigation within dired buffer
;; -  keep only one active dired buffer
;; -  Use dired-k extension to show time / vcs related information in
;;    single bar
;; -  right mouse click moves up directory if in blank space or shows context menu
;;
(require 'use-package)
(require 'evil-evilified-state)
(require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
;; (require 'core-display-init)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)
(require 'dired-x)

;;; Code:

(defvar vinegar-reuse-dired-buffer nil
  "If non-nil, reuses one dired buffer for navigation.")

;; funcs

(defun vinegar/dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (message "h")
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
             (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(defun vinegar/back-to-top ()
  "Move to first file"
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 1))

(defun vinegar/jump-to-bottom ()
  "Move to last file"
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(defun vinegar/move-up ()
  "Move to previous file"
  (interactive)
  (dired-previous-line 1)
  (if (bobp)
      (dired-next-line 1)))

(defun vinegar/move-down ()
  "Move to next file"
  (interactive)
  (dired-next-line 1)
  (if (eobp)
      (dired-next-line -1)))

(defun vinegar/up-directory (&optional other-window)
  "Run Dired on parent directory of current directory."
  (interactive "P")
  (let* ((dir (dired-current-directory))
         (orig (current-buffer))
         (up (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
        ;; Only try dired-goto-subdir if buffer has more than one dir.
        (and (cdr dired-subdir-alist)
             (dired-goto-subdir up))
        (progn
          (kill-buffer orig)
          (dired up)
          (dired-goto-file dir)))))

(defun vinegar/dired-diff ()
  "Ediff marked files in dired or selected files in separate window"
  (interactive)
  (let* ((marked-files (dired-get-marked-files nil nil))
        (other-win (get-window-with-predicate
                    (lambda (window)
                      (with-current-buffer (window-buffer window)
                        (and (not (eq window (selected-window)))
                             (eq major-mode 'dired-mode))))))
        (other-marked-files (and other-win
                                 (with-current-buffer (window-buffer other-win)
                                   (dired-get-marked-files nil)))))
    (cond ((= (length marked-files) 2)
           (ediff-files (nth 0 marked-files)
                        (nth 1 marked-files)))
          ((= (length marked-files) 3)
           (ediff-files3 (nth 0 marked-files)
                         (nth 1 marked-files)
                         (nth 2 marked-files)
                         ))
          ((and (= (length marked-files) 1)
                (= (length other-marked-files) 1))
           (ediff-files (nth 0 marked-files)
                        (nth 0 other-marked-files)))
          ((= (length marked-files) 1)
           (dired-diff))
          (t (error "mark exactly 2 files, at least 1 locally")))))

(defun vinegar/dired-setup ()
  "Setup custom dired settings for vinegar"
  (setq dired-omit-verbose nil)
  (make-local-variable 'dired-hide-symlink-targets)
  (setq dired-hide-details-hide-symlink-targets nil)

  ;; hide details by default
  (dired-hide-details-mode t)
  ;; omit the .. in dired
  (dired-omit-mode t)

  ;; allow selection with mouse
  (make-local-variable 'mouse-1-click-follows-link)
  (setq mouse-1-click-follows-link nil)

  (local-set-key (kbd  "<mouse-1>") 'vinegar/dired-mouse-click)
  (local-set-key (kbd  "<mouse-3>") 'vinegar/up-directory)
  (local-set-key (kbd  "<down-mouse-3>") nil))

(defun vinegar/dired-mouse-click (event)
  "In Dired, visit the file or directory name you click on."
  (interactive "e")
  (let (window pos file)
    (save-excursion
      (setq window (posn-window (event-end event))
            pos (posn-point (event-end event)))
      (if (not (windowp window))
          (error "No file chosen"))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (setq file (dired-get-file-for-visit)))
    (find-alternate-file file)))

(defun vinegar/dired-mouse-click-3 (event)
  "In Dired, show context menu or go up a directory."
  (interactive "e")
  (let (window pos file)
    (save-excursion
      (setq window (posn-window (event-end event))
            pos (posn-point (event-end event)))
      (if (not (windowp window))
          (error "No file chosen"))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (condition-case ex
          (progn
            (setq file (dired-get-file-for-visit))
            (dired-find-file-other-window))
        ('error
         (vinegar/up-directory)
         ))
    )))

;; packages

(use-package dired-x                    ; Additional tools for Dired
  :init
  (add-hook 'dired-mode-hook 'vinegar/dired-setup)
  :config
  (define-key evil-normal-state-map (kbd "-") 'dired-jump))

(use-package dired+
  :defer t
  :ensure t
  :init
  (progn
    (setq diredp-hide-details-initially-flag t
          diredp-hide-details-propagate-flag t
          ;; use single buffer for all dired navigation
          ;; disable font themeing from dired+
          font-lock-maximum-decoration (quote ((dired-mode . 1) (t . t))))
    (toggle-diredp-find-file-reuse-dir 1)))

(dotemacs-use-package-add-hook diff-hl
  :post-init
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(use-package dired                      ; Edit directories
  :defer t
  :config
  (progn
    (setq dired-auto-revert-buffer t    ; Revert on re-visiting
          ;; Better dired flags: `-l' is mandatory, `-a' shows all files, `-h'
          ;; uses human-readable sizes, and `-F' appends file-type classifiers
          ;; to file names (for better highlighting)
          dired-listing-switches "-alhF"
          dired-ls-F-marks-symlinks t   ; -F marks links with @
          ;; Inhibit prompts for simple recursive operations
          dired-recursive-copies 'always)

    (evilified-state-evilify dired-mode dired-mode-map
      "j"         'vinegar/move-down
      "k"         'vinegar/move-up
      "-"         'vinegar/up-directory
      "0"         'dired-back-to-start-of-files
      "="         'vinegar/dired-diff
      (kbd "C-j") 'dired-next-subdir
      (kbd "C-k") 'dired-prev-subdir
      "I"         'vinegar/dotfiles-toggle
      (kbd "~")   '(lambda ()(interactive) (find-alternate-file "~/"))
      (kbd "RET") (if vinegar-reuse-dired-buffer
                      'dired-find-alternate-file
                    'dired-find-file)
      "f"         'helm-find-files
      "J"         'dired-goto-file
      (kbd "C-f") 'find-name-dired
      "H"         'diredp-dired-recent-dirs
      "T"         'dired-tree-down
      "K"         'dired-do-kill-lines
      "r"         'revert-buffer
      (kbd "C-r") 'dired-do-redisplay
      "gg"        'vinegar/back-to-top
      "G"         'vinegar/jump-to-bottom)

    ;; (evilified-state-evilify dired-mode dired-mode-map
    ;;   (kbd "RET") 'dired-open-file)

    (with-eval-after-load 'projectile
      (evilified-state-evilify dired-mode dired-mode-map
        (kbd "RET") 'dired-open-file))

    ))

(use-package dired-open
  :ensure t
  :defer t
  :init
  (progn
    (defun dotemacs-dired-open-file ()
      "Hook dired to translate to projectile and neotree."
      (interactive)
      (let ((file (ignore-errors (dired-get-file-for-visit))))
        (when file
          (find-file (expand-file-name file (projectile-project-root)))
          (run-hooks 'projectile-find-file-hook)
          (message "Projectile root found: %s" (projectile-project-root))
          (when (fboundp 'neotree-dir)
            (if (neo-global--window-exists-p)
                (neotree-dir (projectile-project-root))
              (progn
                (neotree-dir (projectile-project-root))
                (neotree-hide)
                (let ((origin-buffer-file-name (buffer-file-name)))
                  (neotree-find (projectile-project-root))
                  (neotree-find origin-buffer-file-name))
                (neotree-hide)))))))
    (with-eval-after-load 'projectile
      (setq dired-open-functions 'dotemacs-dired-open-file))))

(provide 'module-vinegar)
;;; module-vinegar.el ends here
