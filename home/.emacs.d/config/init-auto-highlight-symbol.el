(defun dotemacs-goto-last-searched-ahs-symbol ()
  "Go to the last known occurrence of the last symbol searched with
`auto-highlight-symbol'."
  (interactive)
  (if dotemacs-last-ahs-highlight-p
      (progn (goto-char (nth 1 dotemacs-last-ahs-highlight-p))
             (dotemacs-ahs-highlight-now-wrapper)
             (dotemacs-symbol-highlight-micro-state))
    (message "No symbol has been searched for now.")))

(defun dotemacs-integrate-evil-search (forward)
  ;; isearch-string is last searched item.  Next time
  ;; "n" is hit we will use this.
  (setq isearch-string
        (concat "\\<" (evil-find-thing forward 'symbol) "\\>")
        isearch-regexp
        (concat "\\<" (evil-find-thing forward 'symbol) "\\>"))
  ;; Next time "n" is hit, go the correct direction.
  (setq isearch-forward forward)
  ;; ahs does a case sensitive search.  We could set
  ;; this, but it would break the user's current
  ;; sensitivity settings.  We could save the setting,
  ;; then next time the user starts a search we could
  ;; restore the setting.
  ;;(setq case-fold-search nil)
  ;; Place the search term into the search rings.
  (isearch-update-ring isearch-string t)
  (evil-push-search-history isearch-string forward)
  ;; Use this search term for empty pattern "%s//replacement/"
  ;; Append case sensitivity
  (setq evil-ex-last-was-search nil
        evil-ex-substitute-pattern `(,(concat isearch-string "\\C")
                                     nil (0 0))))

(defun dotemacs-ensure-ahs-enabled-locally ()
  "Ensures ahs is enabled for the local buffer."
  (unless
    (bound-and-true-p ahs-mode-line)
    (auto-highlight-symbol-mode)))

(defun dotemacs-ahs-highlight-now-wrapper ()
  "Safe wrapper for ahs-highlight-now"
  (eval '(progn
           (dotemacs-ensure-ahs-enabled-locally)
           (ahs-highlight-now)) nil))

(defun dotemacs-enter-ahs-forward ()
  "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'"
  (interactive)
  (setq dotemacs--ahs-searching-forward t)
  (dotemacs-quick-ahs-forward))

(defun dotemacs-enter-ahs-backward ()
  "Go to the previous occurrence of symbol under point with
`auto-highlight-symbol'"
  (interactive)
  (setq dotemacs--ahs-searching-forward nil)
  (dotemacs-quick-ahs-forward))

(defun dotemacs-quick-ahs-forward ()
  "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'"
  (interactive)
  (dotemacs-quick-ahs-move t))

(defun dotemacs-quick-ahs-backward ()
  "Go to the previous occurrence of symbol under point with
`auto-highlight-symbol'"
  (interactive)
  (dotemacs-quick-ahs-move nil))

(defun dotemacs-quick-ahs-move (forward)
  "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'"
  (if (eq forward dotemacs--ahs-searching-forward)
      (progn
        (dotemacs-integrate-evil-search t)
        (dotemacs-ahs-highlight-now-wrapper)
        (evil-set-jump)
        (dotemacs-symbol-highlight-micro-state)
        (ahs-forward))
    (progn
      (dotemacs-integrate-evil-search nil)
      (dotemacs-ahs-highlight-now-wrapper)
      (evil-set-jump)
      (dotemacs-symbol-highlight-micro-state)
      (ahs-backward))))

(defun dotemacs-symbol-highlight ()
  "Highlight the symbol under point with `auto-highlight-symbol'."
  (interactive)
  (dotemacs-ahs-highlight-now-wrapper)
  (setq dotemacs-last-ahs-highlight-p (ahs-highlight-p))
  (dotemacs-symbol-highlight-micro-state)
  (dotemacs-integrate-evil-search nil))

(defun dotemacs-ahs-ms-on-exit ()
  ;; Restore user search direction state as ahs has exitted in a state
  ;; good for <C-s>, but not for 'n' and 'N'"
  (setq isearch-forward dotemacs--ahs-searching-forward))

(defun dotemacs-symbol-highlight-reset-range ()
  "Reset the range for `auto-highlight-symbol'."
  (interactive)
  (ahs-change-range ahs-default-range))

(provide 'init-auto-highlight-symbol)
