;;; module-org.el --- ORG Module
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

(dotemacs-defvar-company-backends org-mode)

;; Insert key for org-mode and markdown a la C-h k
;; from SE endless http://emacs.stackexchange.com/questions/2206/
(defun dotemacs-insert-keybinding-org (key)
  "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
  (interactive "kType key sequence: ")
  (let* ((tag "@@html:<kbd>@@ %s @@html:</kbd>@@"))
    (if (null (equal key "\r"))
        (insert
         (format tag (help-key-description key nil)))
      (insert (format tag ""))
      (forward-char -8))))

(defun dotemacs-org-present-start ()
  "Initiate `org-present' mode."
  (org-present-big)
  (org-display-inline-images)
  (org-present-hide-cursor)
  (org-present-read-only)
  (evil-evilified-state))

(defun dotemacs-org-present-end ()
  "Terminate `org-present' mode."
  (org-present-small)
  (org-remove-inline-images)
  (org-present-show-cursor)
  (org-present-read-write)
  (evil-normal-state))

(use-package org-plus-contrib
  :mode ("\\.org$" . org-mode)
  :commands (org-clock-out org-occur-in-agenda-files)
  :ensure t
  :defer t
  :init
  (progn
    ;; (when (featurep 'org)
    ;;   (dotemacs-buffer/append
    ;;    (concat
    ;;     "Org features were loaded before the `org' layer initialized.\n") t))

    (setq org-replace-disputed-keys t ;; Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
          org-src-fontify-natively t ;; Fontify org-mode code blocks
          org-clock-persist-file (concat dotemacs-cache-directory "org-clock-save.el")
          org-id-locations-file
          (concat dotemacs-cache-directory ".org-id-locations")
          org-log-done t
          org-startup-with-inline-images t
          org-startup-indented t)

    (with-eval-after-load 'org-indent
      (dotemacs-hide-lighter org-indent-mode))

    (defmacro dotemacs-org-emphasize (fname char)
      "Make function for setting the emphasis in org mode"
      `(defun ,fname () (interactive)
              (org-emphasize ,char)))

    (dotemacs-set-leader-keys-for-major-mode 'org-mode
      "'" 'org-edit-special
      "c" 'org-capture
      "d" 'org-deadline
      "e" 'org-export-dispatch
      "f" 'org-set-effort
      "P" 'org-set-property

      "a" 'org-agenda
      "b" 'org-tree-to-indirect-buffer
      "A" 'org-archive-subtree
      "l" 'org-open-at-point
      "T" 'org-show-todo-tree

      "." 'org-time-stamp

      ;; headings
      "hi" 'org-insert-heading-after-current
      "hI" 'org-insert-heading

      ;; More cycling options (timestamps, headlines, items, properties)
      "L" 'org-shiftright
      "H" 'org-shiftleft
      "J" 'org-shiftdown
      "K" 'org-shiftup

      ;; Change between TODO sets
      "C-S-l" 'org-shiftcontrolright
      "C-S-h" 'org-shiftcontrolleft
      "C-S-j" 'org-shiftcontroldown
      "C-S-k" 'org-shiftcontrolup

      ;; Subtree editing
      "Sl" 'org-demote-subtree
      "Sh" 'org-promote-subtree
      "Sj" 'org-move-subtree-down
      "Sk" 'org-move-subtree-up

      ;; tables
      "ta" 'org-table-align
      "tb" 'org-table-blank-field
      "tc" 'org-table-convert
      "tdc" 'org-table-delete-column
      "tdr" 'org-table-kill-row
      "te" 'org-table-eval-formula
      "tE" 'org-table-export
      "th" 'org-table-previous-field
      "tH" 'org-table-move-column-left
      "tic" 'org-table-insert-column
      "tih" 'org-table-insert-hline
      "tiH" 'org-table-hline-and-move
      "tir" 'org-table-insert-row
      "tI" 'org-table-import
      "tj" 'org-table-next-row
      "tJ" 'org-table-move-row-down
      "tK" 'org-table-move-row-up
      "tl" 'org-table-next-field
      "tL" 'org-table-move-column-right
      "tn" 'org-table-create
      "tN" 'org-table-create-with-table.el
      "tr" 'org-table-recalculate
      "ts" 'org-table-sort-lines
      "ttf" 'org-table-toggle-formula-debugger
      "tto" 'org-table-toggle-coordinate-overlays
      "tw" 'org-table-wrap-region

      ;; Multi-purpose keys
      (or dotemacs-major-mode-leader-key ",") 'org-ctrl-c-ctrl-c
      "*" 'org-ctrl-c-star
      " RET" 'org-ctrl-c-ret
      "-" 'org-ctrl-c-minus
      "^" 'org-sort
      "/" 'org-sparse-tree

      "I" 'org-clock-in
      "n" 'org-narrow-to-subtree
      "N" 'widen
      "O" 'org-clock-out
      "q" 'org-clock-cancel
      "R" 'org-refile
      "s" 'org-schedule

      ;; insertion of common elements
      "il" 'org-insert-link
      "if" 'org-footnote-new
      "ik" 'dotemacs-insert-keybinding-org

      ;; images and other link types have no commands in org mode-line
      ;; could be inserted using yasnippet?
      ;; region manipulation
      "xb" (dotemacs-org-emphasize dotemacs-org-bold ?*)
      "xc" (dotemacs-org-emphasize dotemacs-org-code ?~)
      "xi" (dotemacs-org-emphasize dotemacs-org-italic ?/)
      "xr" (dotemacs-org-emphasize dotemacs-org-clear ? )
      "xs" (dotemacs-org-emphasize dotemacs-org-strike-through ?+)
      "xu" (dotemacs-org-emphasize dotemacs-org-underline ?_)
      "xv" (dotemacs-org-emphasize dotemacs-org-verbose ?=))

    (require 'ox-md)
    (require 'ox-ascii)
    (require 'ox-confluence)
    (require 'ox-html)
    (require 'org-bullets)

    (with-eval-after-load 'org-agenda
       ;; Since we could override SPC with <leader>, let's make RET do that
       ;; functionality
      (when (equal dotemacs-leader-key "SPC")
       (define-key org-agenda-mode-map
         (kbd "RET") 'org-agenda-show-and-scroll-up)
       (define-key org-agenda-mode-map
         (kbd "SPC") dotemacs-default-map))

      (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
      (define-key org-agenda-mode-map "k" 'org-agenda-previous-line))

    ;; Add global evil-leader mappings. Used to access org-agenda
    ;; functionalities – and a few others commands – from any other mode.
    (dotemacs-declare-prefix "ao" "org")
    (dotemacs-set-leader-keys
      ;; org-agenda
      "ao#" 'org-agenda-list-stuck-projects
      "ao/" 'org-occur-in-agenda-files
      "aoa" 'org-agenda-list
      "aoe" 'org-store-agenda-views
      "aom" 'org-tags-view
      "aoo" 'org-agenda
      "aos" 'org-search-view
      "aot" 'org-todo-list
      ;; other
      "aoO" 'org-clock-out
      "aoc" 'org-capture
      "aol" 'org-store-link))
  :config
  (progn
    (unless (file-exists-p org-directory)
      (make-directory org-directory))

    (setq my-inbox-org-file (concat org-directory "/inbox.org")
          org-indent-indentation-per-level 2
          org-use-fast-todo-selection t
          org-completion-use-ido t
          org-treat-S-cursor-todo-selection-as-state-change nil
          org-agenda-files `(,org-directory))

    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline my-inbox-org-file "INBOX")
             "* TODO %?\n%U\n%a\n")
            ("n" "Note" entry (file+headline my-inbox-org-file "NOTES")
             "* %? :NOTE:\n%U\n%a\n")
            ("m" "Meeting" entry (file my-inbox-org-file)
             "* MEETING %? :MEETING:\n%U")
            ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
             "* %?\n%U\n")))

    ;; org-mode colors
    (setq org-todo-keyword-faces
          '(
            ("INPR" . (:foreground "yellow" :weight bold))
            ("DONE" . (:foreground "green" :weight bold))
            ("IMPEDED" . (:foreground "red" :weight bold))
            ))

    (setq org-refile-targets '((nil :maxlevel . 9)
                  (org-agenda-files :maxlevel . 9)))

    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n@)" "|" "DONE(d)")
            (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")))

    (setq org-todo-state-tags-triggers
          ' (("CANCELLED" ("CANCELLED" . t))
             ("WAITING" ("WAITING" . t))
             ("TODO" ("WAITING") ("CANCELLED"))
             ("NEXT" ("WAITING") ("CANCELLED"))
             ("DONE" ("WAITING") ("CANCELLED"))))

    (font-lock-add-keywords
     'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
                  (1 font-lock-comment-face prepend)
                  (2 font-lock-function-name-face)
                  (3 font-lock-comment-face prepend))))

    (require 'org-indent)
    (define-key global-map "\C-cl" 'org-store-link)
    (define-key global-map "\C-ca" 'org-agenda)

    ;; Open links and files with RET in normal state
    (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)

    (dotemacs-set-leader-keys
      "Cc" 'org-capture)

    ;; Evilify the calendar tool on C-c .
    (unless (eq 'emacs dotemacs-editing-style)
      (define-key org-read-date-minibuffer-local-map (kbd "M-h")
        (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
      (define-key org-read-date-minibuffer-local-map (kbd "M-l")
        (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
      (define-key org-read-date-minibuffer-local-map (kbd "M-k")
        (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
      (define-key org-read-date-minibuffer-local-map (kbd "M-j")
        (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
      (define-key org-read-date-minibuffer-local-map (kbd "M-H")
        (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month 1))))
      (define-key org-read-date-minibuffer-local-map (kbd "M-L")
        (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-month 1))))
      (define-key org-read-date-minibuffer-local-map (kbd "M-K")
        (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-year 1))))
      (define-key org-read-date-minibuffer-local-map (kbd "M-J")
        (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-year 1)))))))

(use-package org-agenda
  :defer t
  :init
  (setq org-agenda-restore-windows-after-quit t)
  :config
  (evilified-state-evilify-map org-agenda-mode-map
    :mode org-agenda-mode
    :bindings
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line
    (kbd "M-j") 'org-agenda-next-item
    (kbd "M-k") 'org-agenda-previous-item
    (kbd "M-h") 'org-agenda-earlier
    (kbd "M-l") 'org-agenda-later
    (kbd "gd") 'org-agenda-toggle-time-grid
    (kbd "gr") 'org-agenda-redo))

(use-package org-bullets
  :ensure t
  :defer t
  :init
  (progn
    (setq org-bullets-bullet-list '("✿" "❀" "☢" "☯" "✸" ))
    (add-hook 'org-mode-hook 'org-bullets-mode)))

(use-package org-repo-todo
  :ensure t
  :defer t
  :init
  (progn
    (dotemacs-set-leader-keys
      "Ct"  'ort/capture-todo
      "CT"  'ort/capture-checkitem)
    (dotemacs-set-leader-keys-for-major-mode 'org-mode
      "gt" 'ort/goto-todos)))

(dotemacs-use-package-add-hook persp-mode
  :post-init
  (dotemacs-define-custom-layout "@Org"
                                  :binding "o"
                                  :body
                                  (find-file (first org-agenda-files))))

(use-package gnuplot
  :defer t
  :ensure t
  :init (dotemacs-set-leader-keys-for-major-mode 'org-mode
          "tp" 'org-plot/gnuplot))

(use-package evil-org
  :load-path "evil/"
  :commands evil-org-mode
  :init
  (add-hook 'org-mode-hook 'evil-org-mode)
  :config
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'org-mode
      "C" 'evil-org-recompute-clocks)
    (evil-define-key 'normal evil-org-mode-map
      "O" 'evil-open-above)
    (dotemacs-diminish evil-org-mode " ⓔ" " e")))

(dotemacs-use-package-add-hook evil-surround
  :post-init
  (defun dotemacs/add-org-surrounds ()
    (push '(?: . dotemacs//surround-drawer) evil-surround-pairs-alist))
  (add-hook 'org-mode-hook 'dotemacs/add-org-surrounds)
  (defun dotemacs//surround-drawer ()
    (let ((dname (read-from-minibuffer "" "")))
      (cons (format ":%s:" (or dname "")) ":END:"))))

(use-package org-mime
  :defer t
  :commands (org-mime-htmlize org-mime-org-buffer-htmlize)
  :init
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'message-mode
      "M" 'org-mime-htmlize)
    (dotemacs-set-leader-keys-for-major-mode 'org-mode
      "m" 'org-mime-org-buffer-htmlize)))

(use-package org-pomodoro
  :defer t
  :ensure t
  :init
  (progn
    (when (dotemacs/system-is-mac)
      (setq org-pomodoro-audio-player "/usr/bin/afplay"))
    (dotemacs-set-leader-keys-for-major-mode 'org-mode
      "p" 'org-pomodoro)))

(use-package org-present
  :defer t
  :ensure t
  :init
  (progn
    (evilified-state-evilify nil org-present-mode-keymap
      "h" 'org-present-prev
      "l" 'org-present-next
      "q" 'org-present-quit)
    (add-hook 'org-present-mode-hook 'dotemacs-org-present-start)
    (add-hook 'org-present-mode-quit-hook 'dotemacs-org-present-end)))

(use-package toc-org
  :ensure t
  :init
  (progn
    (setq toc-org-max-depth 10)
    (add-hook 'org-mode-hook 'toc-org-enable)))

(use-package htmlize
  :ensure t
  :defer t)

(use-package ox-pandoc
  :defer t
  :ensure t
  :init
  (with-eval-after-load 'org (require 'ox-pandoc)))

(dotemacs-use-package-add-hook emoji-cheat-sheet-plus
  :post-init
  (add-hook 'org-mode-hook 'dotemacs-delay-emoji-cheat-sheet-hook))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook org-mode)
      (push 'company-capf company-backends-org-mode)))
  (dotemacs-use-package-add-hook company-emoji
    :post-init
    (progn
      (push 'company-emoji company-backends-org-mode))))

(provide 'module-org)
;;; module-org.el ends here
