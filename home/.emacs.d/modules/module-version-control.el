;;; module-version-control.el --- Version Control Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;;; Code:
;;
(require 'use-package)
(require 'evil-evilified-state)
(require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
(require 'core-toggle)
(require 'core-transient-state)
(require 'core-fonts-support)
(require 'core-display-init)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;; config

(defvar version-control-global-margin t
  "If non-nil, will show diff margins globally.")

(defvar version-control-diff-tool 'git-gutter+
  "Options are `git-gutter', `git-gutter+', and `diff-hl' to show
version-control markers.")

;; funcs

;; unchanged face
(defface git-gutter+-unchanged
  '((t (:background "yellow")))
  "face for unchanged lines"
  :group 'git-gutter+)
(defface git-gutter:unchanged
  '((t (:background "yellow")))
  "face for unchanged lines"
  :group 'git-gutter+)

;; change face
(defface git-gutter+-modified
  '((t (:foreground "magenta" :weight bold)))
  "face for modified lines"
  :group 'git-gutter+)
(defface git-gutter:modified
  '((t (:foreground "magenta" :weight bold)))
  "face for modified lines"
  :group 'git-gutter+)
(defface diff-hl-change
  '((default :foreground "blue3")
    (((class color) (min-colors 88) (background light))
     :background "#ddddff")
    (((class color) (min-colors 88) (background dark))
     :background "#333355"))
  "Face used to highlight changed lines."
  :group 'diff-hl)

;; added face
(defface git-gutter+-added
  '((t (:foreground "green" :weight bold)))
  "face for added lines"
  :group 'git-gutter+)
(defface git-gutter:added
  '((t (:foreground "green" :weight bold)))
  "face for added lines"
  :group 'git-gutter+)
(defface diff-hl-insert
  '((default :inherit diff-added)
    (((class color)) :foreground "green4"))
  "Face used to highlight inserted lines."
  :group 'diff-hl)

;; deleted face
(defface git-gutter+-deleted
  '((t (:foreground "red" :weight bold)))
  "face for deleted lines"
  :group 'git-gutter+)
(defface git-gutter:deleted
  '((t (:foreground "red" :weight bold)))
  "face for deleted lines"
  :group 'git-gutter+)
(defface diff-hl-delete
  '((default :inherit diff-removed)
    (((class color)) :foreground "red3"))
  "Face used to highlight deleted lines."
  :group 'diff-hl)

;; funcs

(defun version-control/next-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-next-hunk)
       (git-gutter  'git-gutter:next-hunk)
       (git-gutter+ 'git-gutter+-next-hunk)))))

(defun version-control/previous-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-previous-hunk)
       (git-gutter  'git-gutter:previous-hunk)
       (git-gutter+ 'git-gutter+-previous-hunk)))))

(defun version-control/revert-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-revert-hunk)
       (git-gutter  'git-gutter:revert-hunk)
       (git-gutter+ 'git-gutter+-revert-hunks)))))

(defun version-control/stage-hunk ()
  (interactive)
  (if (eq 'diff-hl version-control-diff-tool)
      (message "Staging not available")
    (let ((current-prefix-arg t))
      (call-interactively
       (cl-case version-control-diff-tool
         (git-gutter  'git-gutter:stage-hunk)
         (git-gutter+ 'git-gutter+-stage-hunks))))))

(defun version-control/show-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-diff-goto-hunk)
       (git-gutter  'git-gutter:popup-hunk)
       (git-gutter+ 'git-gutter+-show-hunk-inline-at-point)))))

(defun version-control/enable-margin ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-mode)
       (git-gutter  'git-gutter-mode)
       (git-gutter+ 'git-gutter+-mode)))))

(defun version-control/disable-margin ()
  (interactive)
  (let ((current-prefix-arg nil))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-mode)
       (git-gutter  'git-gutter-mode)
       (git-gutter+ 'git-gutter+-mode)))))

(defun version-control/enable-margin-globally ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'global-diff-hl-mode)
       (git-gutter  'global-git-gutter-mode)
       (git-gutter+ 'global-git-gutter+-mode)))))

(defun version-control/disable-margin-globally ()
  (interactive)
  (let ((current-prefix-arg nil))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'global-diff-hl-mode)
       (git-gutter  'global-git-gutter-mode)
       (git-gutter+ 'global-git-gutter+-mode)))))

(defun version-control/show-help ()
  (interactive)
  (setq version-control--ms-doc-toggle
        (logxor version-control--ms-doc-toggle 1)))

(defun version-control/margin-p ()
  (interactive)
  (cl-case version-control-diff-tool
    (diff-hl     diff-hl-mode)
    (git-gutter  git-gutter-mode)
    (git-gutter+ git-gutter+-mode)))

(defun version-control/margin-global-p ()
  (interactive)
  (cl-case version-control-diff-tool
    (diff-hl     global-diff-hl-mode)
    (git-gutter  global-git-gutter-mode)
    (git-gutter+ global-git-gutter+-mode)))

(dotemacs-add-toggle version-control-margin
  :status (version-control/margin-p)
  :on (version-control/enable-margin)
  :off (version-control/disable-margin)
  :documentation "Enable diff margins."
  :evil-leader "Td")

(dotemacs-add-toggle version-control-margin-globally
  :status (version-control/margin-global-p)
  :on (version-control/enable-margin-globally)
  :off (version-control/disable-margin-globally)
  :documentation "Enable diff margins globally."
  :evil-leader "T C-d")

;; keybindings

(dotemacs-define-transient-state vcs
  :title "VCS Transient State"
  :doc "
 Hunk Commands^^^^^^                 Magit Commands
----------------------------^^^^^^  ------------------------------------------
 [_n_]^^^^      next hunk            [_w_/_u_]^^    stage/unstage in current file
 [_N_/_p_]^^    previous hunk        [_c_/_C_]^^    commit with popup/direct commit
 [_r_/_s_/_h_]  revert/stage/show    [_f_/_F_/_P_]  fetch/pull/push popup
 [_t_]^^^^      toggle diff signs    [_l_/_D_]^^    log/diff popup"
  :on-enter (version-control/enable-margin)
  :bindings
  ("C" magit-commit :exit t)
  ("d" magit-ediff-popup :exit t)
  ("D" magit-diff-unstaged :exit t)
  ("F" magit-pull-popup :exit t)
  ("P" magit-push-popup :exit t)
  ("c" magit-commit-popup :exit t)
  ("f" magit-fetch-popup :exit t)
  ("l" magit-log-popup :exit t)
  ("u" magit-unstage-file)
  ("w" magit-stage-file)
  ("n" version-control/next-hunk)
  ("N" version-control/previous-hunk)
  ("p" version-control/previous-hunk)
  ("r" version-control/revert-hunk)
  ("s" version-control/stage-hunk)
  ("h" version-control/show-hunk)
  ("t" dotemacs/toggle-version-control-margin)
  ("q" nil :exit t))
(dotemacs-set-leader-keys "g." 'dotemacs/vcs-transient-state/body)

;; packages

(use-package ediff
  :defer t
  :init
  (progn
    ;; first we set some sane defaults
    (setq-default
     ediff-window-setup-function 'ediff-setup-windows-plain
     ;; emacs is evil and decrees that vertical shall henceforth be horizontal
     ediff-split-window-function 'split-window-horizontally
     ediff-merge-split-window-function 'split-window-horizontally)
    ;; restore window layout when done
    (add-hook 'ediff-quit-hook #'winner-undo)))

(use-package diff-mode
  :defer t
  :ensure t
  :config
  (evilified-state-evilify diff-mode diff-mode-map
    "j" 'diff-hunk-next
    "k" 'diff-hunk-prev))

(use-package diff-hl                    ; Highlight hunks in fringe
  :ensure t
  :defer t
  :init
  (progn
    (setq diff-hl-side 'right)
    (when (eq version-control-diff-tool 'diff-hl)
      (when version-control-global-margin
        ;; Highlight changes to the current file in the fringe
        (global-diff-hl-mode))
      ;; Fall back to the display margin, if the fringe is unavailable
      (dotemacs|do-after-display-system-init
       (unless (display-graphic-p)
         (setq diff-hl-side 'left)
         (diff-hl-margin-mode))))))

(use-package git-gutter
  :ensure t
  :commands global-git-gutter-mode
  :init
  (progn
    ;; If you enable global minor mode
    (when (and (eq version-control-diff-tool 'git-gutter)
               version-control-global-margin)
      (global-git-gutter-mode t))
    ;; If you would like to use git-gutter.el and linum-mode
    (if dotemacs-line-numbers
        (git-gutter:linum-setup))
    (setq git-gutter:update-interval 2
          git-gutter:modified-sign " "
          git-gutter:added-sign "+"
          git-gutter:deleted-sign "-"
          git-gutter:diff-option "-w"
          git-gutter:hide-gutter t
          git-gutter:ask-p nil
          git-gutter:verbosity 0
          git-gutter:handled-backends '(git hg bzr svn)
          git-gutter:hide-gutter t))
  :config
  (dotemacs-hide-lighter git-gutter-mode))

(use-package git-gutter-fringe
  :ensure t
  :commands git-gutter-mode
  :init
  (progn
    (when (display-graphic-p)
      (with-eval-after-load 'git-gutter
        (require 'git-gutter-fringe)))
    (setq git-gutter-fr:side 'right-fringe))
  :config
  (progn
    ;; custom graphics that works nice with half-width fringes
    (fringe-helper-define 'git-gutter-fr:added nil
                          "..X...."
                          "..X...."
                          "XXXXX.."
                          "..X...."
                          "..X...."
                          )
    (fringe-helper-define 'git-gutter-fr:deleted nil
                          "......."
                          "......."
                          "XXXXX.."
                          "......."
                          "......."
                          )
    (fringe-helper-define 'git-gutter-fr:modified nil
                          "..X...."
                          ".XXX..."
                          "XX.XX.."
                          ".XXX..."
                          "..X...."
                          )))

(use-package git-gutter+
  :ensure t
  :commands global-git-gutter+-mode
  :init
  (progn
    ;; If you enable global minor mode
    (when (and (eq version-control-diff-tool 'git-gutter+)
               version-control-global-margin)
      (add-hook 'magit-pre-refresh-hook 'git-gutter+-refresh)
      (global-git-gutter+-mode t))
    (setq
     git-gutter+-modified-sign " "
     git-gutter+-added-sign "+"
     git-gutter+-deleted-sign "-"
     git-gutter+-diff-option "-w"
     git-gutter+-hide-gutter t))
  ;; identify magit changes
  :config
  (dotemacs-hide-lighter git-gutter+-mode)
  ;; (set-face-foreground 'git-gutter+-modified "black")
  ;; (set-face-foreground 'git-gutter+-added    "black")
  ;; (set-face-foreground 'git-gutter+-deleted  "black")
  ;; (set-face-background 'git-gutter+-modified "orange1")
  ;; (set-face-background 'git-gutter+-added    "green4")
  ;; (set-face-background 'git-gutter+-deleted  "red3")
  )

(use-package git-gutter-fringe+
  :ensure t
  :commands git-gutter+-mode
  :init
  (progn
    (dotemacs|do-after-display-system-init
     (when (display-graphic-p)
       (with-eval-after-load 'git-gutter+
         (require 'git-gutter-fringe+))))
    (setq git-gutter-fr+-side 'right-fringe))
  :config
  (progn
    ;; custom graphics that works nice with half-width fringes
    (fringe-helper-define 'git-gutter-fr+-added nil
                          "..X...."
                          "..X...."
                          "XXXXX.."
                          "..X...."
                          "..X...."
                          )
    (fringe-helper-define 'git-gutter-fr+-deleted nil
                          "......."
                          "......."
                          "XXXXX.."
                          "......."
                          "......."
                          )
    (fringe-helper-define 'git-gutter-fr+-modified nil
                          "..X...."
                          ".XXX..."
                          "XX.XX.."
                          ".XXX..."
                          "..X...."
                          )))

(provide 'module-version-control)
;;; module-version-control.el ends here
