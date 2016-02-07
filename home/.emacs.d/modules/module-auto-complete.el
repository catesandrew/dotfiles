;;; module-auto-completions.el --- Auto-completion functions File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;; Commentary:
;;
(require 'use-package)
(require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
(require 'core-display-init)
(require 'core-toggle)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

;; config

(defvar auto-completion-return-key-behavior 'complete
  "What the RET key should do when auto-completion menu is active.
Possible values are `complete' or `nil'.")

(defvar auto-completion-tab-key-behavior 'cycle
  "What the TAB key should do when auto-completion menu is active.
Possible values are `complete', `cycle' or `nil'.")

(defvar auto-completion-complete-with-key-sequence nil
  "Provide a key sequence (string) to complete the current
selection.")

(defvar auto-completion-complete-with-key-sequence-delay 0.1
  "Timeout (seconds) when waiting for the second key of
`auto-completion-complete-with-key-sequence'.")

(defvar auto-completion-enable-snippets-in-popup t
  "If non nil show snippets in the auto-completion popup.")

(defvar auto-completion-enable-sort-by-usage t
  "If non nil suggestions are sorted by how often they are used.")

(defvar auto-completion-enable-help-tooltip t
  "If non nil the docstring appears in a tooltip.")

(defvar company-mode-completion-cancel-keywords
  '("do"
    "then"
    "begin"
    "case")
  "Keywords on which to cancel completion so that you can use RET
to complet without blocking common line endings.")

;; toggle

(dotemacs-add-toggle auto-completion
  :status
  (if (boundp 'auto-completion-front-end)
      (if (eq 'company auto-completion-front-end)
          company-mode
        auto-complete-mode)
    ;; default completion hardcoded to be company for now
    (setq auto-completion-front-end 'company)
    nil)
  :on
  (progn
    (if (eq 'company auto-completion-front-end)
        (company-mode)
      (auto-complete-mode))
    (message "Enabled auto-completion (using %S)."
             auto-completion-front-end))
  :off
  (progn
    (if (eq 'company auto-completion-front-end)
        (company-mode -1)
      (auto-complete-mode -1))
    (message "Disabled auto-completion."))
  :documentation "Enable auto-completion."
  :evil-leader "ta")

;; auto-completion key bindings functions

(defun dotemacs//auto-completion-set-RET-key-behavior (package)
  "Bind RET key appropriately for the given PACKAGE and value of
`auto-completion-return-key-behavior'."
  (cond
   ((eq 'company package)
    (let ((map company-active-map))
      (cond
       ((eq 'complete auto-completion-return-key-behavior)
        (define-key map [return] 'company-complete-selection)
        (define-key map (kbd "RET") 'company-complete-selection))
       (t
        (define-key map [return] 'nil)
        (define-key map (kbd "RET") 'nil)))))
   (t (message "Not yet implemented for package %S" package))))

(defun dotemacs//auto-completion-set-TAB-key-behavior (package)
  "Bind TAB key appropriately for the given PACKAGE and value of
`auto-completion-tab-key-behavior'."
  (cond
   ((eq 'company package)
    (let ((map company-active-map))
      (cond
       ((eq 'complete auto-completion-tab-key-behavior)
        (define-key map (kbd "TAB") 'company-complete-selection)
        (define-key map (kbd "<tab>") 'company-complete-selection))
       ((eq 'cycle auto-completion-tab-key-behavior)
        (define-key map (kbd "TAB") 'company-complete-common-or-cycle)
        (define-key map (kbd "<tab>") 'company-complete-common-or-cycle)
        (define-key map (kbd "<S-tab>")
          'dotemacs//company-complete-common-or-cycle-backward)
        (define-key map (kbd "<backtab>")
          'dotemacs//company-complete-common-or-cycle-backward))
       (t
        (define-key map (kbd "TAB") nil)
        (define-key map (kbd "<tab>") nil)))))
   (t (message "Not yet implemented for package %S" package))))

(defun dotemacs//auto-completion-setup-key-sequence (package)
  "Setup the key sequence to complete current selection."
  (when auto-completion-complete-with-key-sequence
    (let ((first-key (elt auto-completion-complete-with-key-sequence 0)))
      (cond ((eq 'company package)
             (define-key company-active-map (kbd (char-to-string first-key))
               'dotemacs//auto-completion-key-sequence-start))
            (t (message "Not yet implemented for package %S" package))))))

;; key sequence to complete selection

(defvar dotemacs--auto-completion-time nil)
(defvar dotemacs--auto-completion-complete-last-candidate nil)
(defvar dotemacs--auto-completion-shadowed-insert-binding nil)
(defvar dotemacs--auto-completion-shadowed-emacs-binding nil)

(defun dotemacs//auto-completion-key-sequence-start ()
  "Initiate auto-completion sequence."
  (interactive)
  (self-insert-command 1)
  (setq dotemacs--auto-completion-complete-last-candidate
        (cond
         ((bound-and-true-p company-mode)
          (nth company-selection company-candidates))))
  ;; enable second key of the sequence
  (let ((second-key (kbd (char-to-string
                          (elt auto-completion-complete-with-key-sequence 1)))))
    (setq dotemacs--auto-completion-shadowed-insert-binding
          (lookup-key evil-insert-state-map second-key))
    (setq dotemacs--auto-completion-shadowed-emacs-binding
          (lookup-key evil-emacs-state-map second-key))
    (define-key
      evil-insert-state-map
      second-key
      'dotemacs//auto-completion-key-sequence-end)
    (define-key
      evil-emacs-state-map
      second-key
      'dotemacs//auto-completion-key-sequence-end))
  ;; set a timer to restore the old bindings
  (run-at-time auto-completion-complete-with-key-sequence-delay
               nil
               'dotemacs//auto-completion-key-sequence-restore)
  (when dotemacs--auto-completion-complete-last-candidate
    (setq dotemacs--auto-completion-time (current-time))))

(defun dotemacs//auto-completion-key-sequence-end ()
  "Check if the auto-completion key sequence has been entered."
  (interactive)
  (if (or (null dotemacs--auto-completion-time)
          (< auto-completion-complete-with-key-sequence-delay
             (float-time (time-since spacemacs--auto-completion-time))))
      (self-insert-command 1)
    (cond
     ((bound-and-true-p company-mode)
      (unless company-candidates
        ;; if the auto-completion menu is still active then we don't need to
        ;; delete the last inserted first key of the sequence
        (delete-char -1))
      (let ((company-idle-delay))
        (company-auto-begin)
        (company-finish dotemacs--auto-completion-complete-last-candidate)))))
  (dotemacs//auto-completion-key-sequence-restore)
  (setq dotemacs--auto-completion-time nil))

(defun dotemacs//auto-completion-key-sequence-restore ()
  "Restore the shadowed key bindings used to auto-complete."
  (let ((second-key (kbd (char-to-string
                          (elt auto-completion-complete-with-key-sequence 1)))))
    (define-key
      evil-insert-state-map
      second-key
      dotemacs--auto-completion-shadowed-insert-binding)
    (define-key
      evil-emacs-state-map
      second-key
      dotemacs--auto-completion-shadowed-emacs-binding)))

;; packages

(use-package ac-ispell
  :ensure t
  :defer t
  :if (eq dotemacs-completion-engine 'auto-complete)
  :init
  (progn
    (setq ac-ispell-requires 4)
    (with-eval-after-load 'auto-complete
      (ac-ispell-setup))))

(use-package auto-complete
  :ensure t
  :if (eq dotemacs-completion-engine 'auto-complete)
  :defer t
  :init
  (setq ac-auto-start 0
        ac-delay 0.2
        ac-quick-help-delay 1.
        ac-use-fuzzy t
        ; ac-auto-show-menu t
        ; ac-quick-help-height 30
        ; ac-show-menu-immediately-on-auto-complete t
        ; completion-ignored-extensions '(".xpt" ".a" ".so" ".o" ".d" ".elc" ".class" "~" ".ckp" ".bak" ".imp" ".lpt" ".bin" ".otl" ".err" ".lib" ".aux" ".elf" )
        ac-fuzzy-enable t
        ac-comphist-file (concat dotemacs-cache-directory "ac-comphist.dat")
        standard-indent 2
        ;; use 'complete when auto-complete is disabled
        tab-always-indent 'complete
        ac-dwim t)
  :config
  (progn
    (require 'auto-complete-config)
    (setq-default ac-sources '(ac-source-abbrev
                               ac-source-dictionary
                               ac-source-words-in-same-mode-buffers
                               ac-source-yasnippet))

    (add-to-list 'completion-styles 'initials t)
    (define-key ac-completing-map (kbd "C-j") 'ac-next)
    (define-key ac-completing-map (kbd "C-k") 'ac-previous)
    (define-key ac-completing-map (kbd "<S-tab>") 'ac-previous)
    (dotemacs-diminish auto-complete-mode " ⓐ" " a")))

(use-package company                    ; Graphical (auto-)completion
  :ensure t
  :defer t
  :if (eq dotemacs-completion-engine 'company)
  :init
  (progn
    (setq company-idle-delay 0.2
          company-tooltip-align-annotations t
          ; company-tooltip-limit 10
          company-minimum-prefix-length 2
          ;; invert the navigation direction if the the completion popup-isearch-match
          ;; is displayed on top (happens near the bottom of windows)
          ; company-tooltip-flip-when-above t
          company-require-match nil
          ; company-dabbrev-code-ignore-case t
          ; company-dabbrev-code-everywhere t
          company-show-numbers t ;; Easy navigation to candidates with M-<n>
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil
          company-frontends '(company-pseudo-tooltip-frontend))
          (defvar-local company-fci-mode-on-p nil)

    (defun company-turn-off-fci (&rest ignore)
      (when (boundp 'fci-mode)
        (setq company-fci-mode-on-p fci-mode)
        (when fci-mode (fci-mode -1))))

    (defun company-maybe-turn-on-fci (&rest ignore)
      (when company-fci-mode-on-p (fci-mode 1)))

    (add-hook 'company-completion-started-hook 'company-turn-off-fci)
    (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
    (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))
  :config
  (progn
    (dotemacs-diminish company-mode " ⓐ" " a")

    ;; key bindings
    (defun dotemacs//company-complete-common-or-cycle-backward ()
      "Complete common prefix or cycle backward."
      (interactive)
      (company-complete-common-or-cycle -1))

    (dotemacs//auto-completion-set-RET-key-behavior 'company)
    (dotemacs//auto-completion-set-TAB-key-behavior 'company)
    (dotemacs//auto-completion-setup-key-sequence 'company)
    (let ((map company-active-map))
      (define-key map (kbd "C-/") 'company-search-candidates)
      (define-key map (kbd "C-M-/") 'company-filter-candidates)
      (define-key map (kbd "C-d") 'company-show-doc-buffer)
      (define-key map (kbd "C-j") 'company-select-next)
      (define-key map (kbd "C-k") 'company-select-previous)
      (define-key map (kbd "C-l") 'company-complete-selection))

    ;; Nicer looking faces
    (custom-set-faces
     '(company-tooltip-common
       ((t (:inherit company-tooltip :weight bold :underline nil))))
     '(company-tooltip-common-selection
       ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

    ;; Transformers
    (defun dotemacs//company-transformer-cancel (candidates)
      "Cancel completion if prefix is in the list
`company-mode-completion-cancel-keywords'"
      (unless (member company-prefix company-mode-completion-cancel-keywords)
        candidates))
    (setq company-transformers '(dotemacs//company-transformer-cancel
                                   company-sort-by-occurrence))))

(use-package company-statistics
  :ensure t
  :if (and auto-completion-enable-sort-by-usage
           (eq dotemacs-completion-engine 'company))
  :defer t
  :init
  (progn
    (setq company-statistics-file (concat dotemacs-cache-directory
                                          "company-statistics-cache.el"))
    (add-hook 'company-mode-hook 'company-statistics-mode)))

(unless (version< emacs-version "24.4")
  (use-package company-quickhelp
    :ensure t
    :if (and auto-completion-enable-help-tooltip (display-graphic-p))
    :defer t
    :init (add-hook 'company-mode-hook 'company-quickhelp-mode)))

(use-package helm-c-yasnippet
  :ensure t
  :defer t
  :init
  (progn
    (defun dotemacs-helm-yas ()
      "Properly lazy load helm-c-yasnipper."
      (interactive)
      (dotemacs-load-yasnippet)
      (require 'helm-c-yasnippet)
      (call-interactively 'helm-yas-complete))
    (dotemacs-set-leader-keys "is" 'dotemacs-helm-yas)
    (setq helm-c-yas-space-match-any-greedy t)))

(use-package helm-company
  :ensure t
  :if (eq dotemacs-completion-engine 'company)
  :defer t
  :init
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-/") 'helm-company)))

(provide 'module-auto-complete)
;;; module-auto-complete.el ends here
