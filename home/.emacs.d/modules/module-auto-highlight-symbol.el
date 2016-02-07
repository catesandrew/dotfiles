;;; module-auto-highlight-symbol.el --- Auto Highlight Symbol
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-vars)
;; (require 'core-funcs)
(require 'core-keybindings)
(require 'core-transient-state)
(require 'core-toggle)
(require 'core-fonts-support)
;; (require 'core-display-init)
;; (require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)
(require 'use-package)

;;; Code:

(use-package auto-highlight-symbol
  :defer t
  :ensure t
  :init
  (progn
    (setq ahs-case-fold-search nil
          ahs-default-range 'ahs-range-whole-buffer
          ;; by default disable auto-highlight of symbol
          ;; current symbol can always be highlighted with <leader> s h
          ahs-idle-timer 0
          ahs-idle-interval 0.25
          ahs-inhibit-face-list nil)

    (dotemacs-add-toggle automatic-symbol-highlight
      :status (timerp ahs-idle-timer)
      :on (progn
            (auto-highlight-symbol-mode)
            (setq ahs-idle-timer
                  (run-with-idle-timer ahs-idle-interval t
                                       'ahs-idle-function)))
      :off (when (timerp ahs-idle-timer)
             (auto-highlight-symbol-mode)
             (cancel-timer ahs-idle-timer)
             (setq ahs-idle-timer 0))
      :documentation "Automatic highlight of current symbol."
      :evil-leader "tha")

    (dotemacs/add-to-hooks 'auto-highlight-symbol-mode '(prog-mode-hook
                                                         markdown-mode-hook)))
  :config
  (progn
    (dotemacs-hide-lighter auto-highlight-symbol-mode)
    (defvar-local dotemacs-last-ahs-highlight-p nil
      "Info on the last searched highlighted symbol.")
    (defvar-local dotemacs--ahs-searching-forward t)

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
            (dotemacs/symbol-highlight-transient-state/body)
            (ahs-forward))
        (progn
          (dotemacs-integrate-evil-search nil)
          (dotemacs-ahs-highlight-now-wrapper)
          (evil-set-jump)
          (dotemacs/symbol-highlight-transient-state/body)
          (ahs-backward))))

    (with-eval-after-load 'evil
      (define-key evil-motion-state-map (kbd "*")
        'dotemacs-enter-ahs-forward)
      (define-key evil-motion-state-map (kbd "#")
        'dotemacs-enter-ahs-backward))

    (defun dotemacs-symbol-highlight ()
      "Highlight the symbol under point with `auto-highlight-symbol'."
      (interactive)
      (dotemacs-ahs-highlight-now-wrapper)
      (setq dotemacs-last-ahs-highlight-p (ahs-highlight-p))
      (dotemacs/symbol-highlight-transient-state/body)
      (dotemacs-integrate-evil-search nil))

    (defun dotemacs-ahs-ms-on-exit ()
      ;; Restore user search direction state as ahs has exitted in a state
      ;; good for <C-s>, but not for 'n' and 'N'"
      (setq isearch-forward dotemacs--ahs-searching-forward))

    (defun dotemacs-symbol-highlight-reset-range ()
      "Reset the range for `auto-highlight-symbol'."
      (interactive)
      (ahs-change-range ahs-default-range))

    (dotemacs-set-leader-keys
      "sh" 'dotemacs-symbol-highlight
      "sH" 'dotemacs-goto-last-searched-ahs-symbol)

    ;; micro-state to easily jump from a highlighted symbol to the others
    (dolist (sym '(ahs-forward
                   ahs-forward-definition
                   ahs-backward
                   ahs-backward-definition
                   ahs-back-to-start
                   ahs-change-range))
      (let* ((advice (intern (format "dotemacs-%s" (symbol-name sym)))))
        (eval `(defadvice ,sym (around ,advice activate)
                 (dotemacs-ahs-highlight-now-wrapper)
                 ad-do-it
                 (dotemacs-ahs-highlight-now-wrapper)
                 (setq dotemacs-last-ahs-highlight-p (ahs-highlight-p))))))

    (defun symbol-highlight-doc ()
      (let* ((i 0)
             (overlay-count (length ahs-overlay-list))
             (overlay (format "%s" (nth i ahs-overlay-list)))
             (current-overlay (format "%s" ahs-current-overlay))
             (st (ahs-stat))
             (plighter (ahs-current-plugin-prop 'lighter))
             (plugin (format " <%s> " (cond ((string= plighter "HS") "D")
                                            ((string= plighter "HSA") "B")
                                            ((string= plighter "HSD") "F"))))
             (propplugin (propertize plugin 'face
                                     `(:foreground "#ffffff"
                                                   :background ,(face-attribute
                                                                 'ahs-plugin-defalt-face :foreground)))))
        (while (not (string= overlay current-overlay))
          (setq i (1+ i))
          (setq overlay (format "%s" (nth i ahs-overlay-list))))
        (let* ((x/y (format "(%s/%s)" (- overlay-count i) overlay-count))
               (propx/y (propertize x/y 'face ahs-plugin-whole-buffer-face))
               (hidden (if (< 0 (- overlay-count (nth 4 st))) "*" ""))
               (prophidden (propertize hidden 'face '(:weight bold))))
          (format "%s %s%s" propplugin propx/y prophidden))))

    (defun ahs-to-iedit ()
      (interactive)
      (evil-iedit-state/iedit-mode)
      (ahs-edit-mode t))

    (dotemacs-define-transient-state symbol-highlight
      :title "Symbol Highlight Transient State"
      :doc "
%s(symbol-highlight-doc)  [_n_/_N_/_p_] next/prev/prev   [_R_] restart      [_e_] iedit       [_b_] search buffers
%s(make-string (length (symbol-highlight-doc)) 32)  [_d_/_D_]^^   next/prev def'n  [_r_] change range [_/_] search proj [_f_] search files"
      :before-exit (dotemacs-ahs-ms-on-exit)
      :bindings
      ("d" ahs-forward-definition)
      ("D" ahs-backward-definition)
      ("e" ahs-to-iedit :exit t)
      ("n" dotemacs-quick-ahs-forward)
      ("N" dotemacs-quick-ahs-backward)
      ("p" dotemacs/quick-ahs-backward)
      ("R" ahs-back-to-start)
      ("r" ahs-change-range)
      ("/" dotemacs-helm-project-smart-do-search-region-or-symbol :exit t)
      ("b" dotemacs-helm-buffers-smart-do-search-region-or-symbol :exit t)
      ("f" dotemacs-helm-files-smart-do-search-region-or-symbol :exit t)
      ("q" nil :exit t))

    (defun dotemacs/symbol-highlight ()
      "Highlight the symbol under point with `auto-highlight-symbol'."
      (interactive)
      (dotemacs-ahs-highlight-now-wrapper)
      (setq dotemacs-last-ahs-highlight-p (ahs-highlight-p))
      (dotemacs/symbol-highlight-transient-state/body)
      (dotemacs-integrate-evil-search nil))))

(provide 'module-auto-highlight-symbol)
;;; module-auto-highlight-symbol.el ends here
