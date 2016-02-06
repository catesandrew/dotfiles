;;; module-editing.el --- Editing Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-vars)
;; (require 'core-funcs)
(require 'core-transient-state)
(require 'core-keybindings)
;; (require 'core-display-init)
;; (require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(defun dotemacs-smart-kill-whole-line (&optional arg)
  "Kill whole line and move back to indentation.

Kill the whole line with function `kill-whole-line' and then move
`back-to-indentation'."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))

(defun dotemacs-smart-backward-kill-line ()
  "Kill line backwards and re-indent."
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

(defun dotemacs-smart-open-line ()
  "Insert empty line after the current line."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun dotemacs-back-to-indentation-or-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun dotemacs-insert-current-date (iso)
  "Insert the current date at point.

When ISO is non-nil, insert the date in ISO 8601 format.
Otherwise insert the date as Mar 04, 2014."
  (interactive "P")
  (insert (format-time-string (if iso "%F" "%b %d, %Y"))))

(use-package clean-aindent-mode ; Keeps track of the last auto-indent operation and trims down white space
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'clean-aindent-mode))

(use-package move-text
  :ensure t
  :defer t
  :init
  (dotemacs-define-transient-state move-text
    :title "Move Text Transient State"
    :bindings
    ("J" move-text-down "move down")
    ("K" move-text-up "move up"))
  (dotemacs-set-leader-keys
   "xJ" 'dotemacs/move-text-transient-state/move-text-down
   "xK" 'dotemacs/move-text-transient-state/move-text-up))

(use-package delsel                     ; Delete the selection instead of insert
  :defer t
  :init (delete-selection-mode))

(use-package expand-region              ; Expand region by semantic units
  :ensure t
  :defer t
  :init (dotemacs-set-leader-keys "v" 'er/expand-region)
  :config
  (progn
    ;; add search capability to expand-region
    (defadvice er/prepare-for-more-expansions-internal
        (around helm-ag/prepare-for-more-expansions-internal activate)
      ad-do-it
      (let ((new-msg (concat (car ad-return-value)
                             ", / to search in project, "
                             "f to search in files, "
                             "b to search in opened buffers"))
            (new-bindings (cdr ad-return-value)))
        (cl-pushnew
         '("/" (lambda ()
                 (call-interactively
                  'dotemacs-helm-project-smart-do-search-region-or-symbol)))
         new-bindings)
        (cl-pushnew
         '("f" (lambda ()
                 (call-interactively
                  'dotemacs-helm-files-smart-do-search-region-or-symbol)))
         new-bindings)
        (cl-pushnew
         '("b" (lambda ()
                 (call-interactively
                  'dotemacs-helm-buffers-smart-do-search-region-or-symbol)))
         new-bindings)
        (setq ad-return-value (cons new-msg new-bindings))))
    (setq expand-region-contract-fast-key "V"
          expand-region-reset-fast-key "r")))

(use-package lorem-ipsum
  :ensure t
  :commands (lorem-ipsum-insert-list
             lorem-ipsum-insert-paragraphs
             lorem-ipsum-insert-sentences)
  :init
  (progn
    (dotemacs-declare-prefix "il" "lorem ipsum")
    (dotemacs-set-leader-keys
      "ill" 'lorem-ipsum-insert-list
      "ilp" 'lorem-ipsum-insert-paragraphs
      "ils" 'lorem-ipsum-insert-sentences)))

(use-package iedit
  :defer t
  :ensure t
  :init
  (progn
    (setq iedit-current-symbol-default t
          iedit-only-at-symbol-boundaries t
          iedit-toggle-key-default nil))
  :config
  (progn
    (defun iedit-toggle-selection ()
      "Override default iedit function to be able to add arbitrary overlays.

It will toggle the overlay under point or create an overlay of one character."
      (interactive)
      (iedit-barf-if-buffering)
      (let ((ov (iedit-find-current-occurrence-overlay)))
        (if ov
            (iedit-restrict-region (overlay-start ov) (overlay-end ov) t)
          (save-excursion
            (push (iedit-make-occurrence-overlay (point) (1+ (point)))
                  iedit-occurrences-overlays))
          (setq iedit-mode
                (propertize
                 (concat " Iedit:" (number-to-string
                                    (length iedit-occurrences-overlays)))
                 'face 'font-lock-warning-face))
          (force-mode-line-update))))))

(use-package subword                    ; Subword/superword editing
  :ensure t
  :if (unless (version< emacs-version "24.4"))
  :defer t
  :init
  (progn
    (unless (category-docstring ?U)
      (define-category ?U "Uppercase")
      (define-category ?u "Lowercase"))
    (modify-category-entry (cons ?A ?Z) ?U)
    (modify-category-entry (cons ?a ?z) ?u)
    (make-variable-buffer-local 'evil-cjk-word-separating-categories)
    (defun dotemacs-subword-enable-camel-case ()
      "Add support for camel case to subword."
      (if subword-mode
          (push '(?u . ?U) evil-cjk-word-separating-categories)
        (setq evil-cjk-word-separating-categories
              (default-value 'evil-cjk-word-separating-categories))))
    (add-hook 'subword-mode-hook 'dotemacs-subword-enable-camel-case)
    (dotemacs-add-toggle camel-case-motion
      :status subword-mode
      :on (subword-mode +1)
      :off (subword-mode -1)
      :documentation "Toggle camelCase motions"
      :evil-leader "tc")
    (dotemacs-add-toggle camel-case-motion-globally
      :status subword-mode
      :on (global-subword-mode +1)
      :off (global-subword-mode -1)
      :documentation "Globally toggle camelCase motions"
      :evil-leader "t C-c"))
  :config
  (dotemacs-diminish subword-mode " ⓒ" " c"))

(use-package visual-fill-column
  :ensure t
  :disabled t
  :init (add-hook 'visual-line-mode-hook 'visual-fill-column-mode))

(use-package undo-tree                  ; Branching undo
  :ensure t
  :init
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  :config
  (dotemacs-hide-lighter undo-tree-mode))

(use-package copyright                  ; Deal with copyright notices
  ;; Update copyright when visiting files
  :init (add-hook 'find-file-hook 'copyright-update)
  ;; Use ranges to denote consecutive years
  :config (setq copyright-year-ranges t
                copyright-names-regexp (regexp-quote user-full-name)))

(setq editorconfig-packages '(editorconfig))
(use-package editorconfig
  :defer t
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.editorconfig" . conf-unix-mode)))

(provide 'module-editing)
;;; module-editing.el ends here
