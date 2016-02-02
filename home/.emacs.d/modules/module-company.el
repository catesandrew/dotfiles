;;; Company
(require 'module-global)

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
    (defun dotemacs-company-complete-common-or-cycle-backward ()
      "Complete common prefix or cycle backward."
      (interactive)
      (company-complete-common-or-cycle -1))

    (dotemacs-auto-completion-set-RET-key-behavior 'company)
    (dotemacs-auto-completion-set-TAB-key-behavior 'company)
    (dotemacs-auto-completion-setup-key-sequence 'company)
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
    (defun dotemacs-company-transformer-cancel (candidates)
      "Cancel completion if prefix is in the list
`company-mode-completion-cancel-keywords'"
      (unless (member company-prefix company-mode-completion-cancel-keywords)
        candidates))
    (setq company-transformers '(dotemacs-company-transformer-cancel
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

(use-package company-quickhelp
  :defer t
  :disabled t
  :init
  (progn
    (dotemacs|do-after-display-system-init
    (when (and auto-completion-enable-help-tooltip
               (not (version< emacs-version "24.4"))  ;; company-quickhelp from MELPA
                                                      ;; is not compatible with 24.3 anymore
               (eq dotemacs-completion-engine 'company)
               (display-graphic-p))
      (add-hook 'company-mode-hook 'company-quickhelp-mode)))))

(use-package helm-company
  :ensure t
  :if (eq dotemacs-completion-engine 'company)
  :defer t
  :init
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-/") 'helm-company)))

(provide 'module-company)
;;; module-company.el ends here
