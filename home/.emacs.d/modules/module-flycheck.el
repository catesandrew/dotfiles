;;; module-flycheck.el --- Flycheck Module
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

;; (defun current-buffer-remote-p ()
;;   (--any? (and it (file-remote-p it))
;;           (list
;;            (buffer-file-name)
;;            list-buffers-directory
;;            default-directory)))
;;
;; (defun flycheck-turn-on-maybe ()
;;   (unless (or buffer-read-only
;;               (hardhat-buffer-included-p (current-buffer))
;;               (current-buffer-remote-p))
;;     (flycheck-mode)))

(defun dotemacs/add-flycheck-hook (mode &optional target)
  "Enable flycheck for the given MODE, if
`syntax-checking-enable-by-default' is true."
  (when syntax-checking-enable-by-default
    (let ((mode-hook (intern (format "%S-hook" mode))))
      ;; (add-hook mode-hook 'flycheck-turn-on-maybe)
      (add-hook mode-hook 'flycheck-mode))))

(defun dotemacs-discard-undesired-html-tidy-error (err)
  "Discard ERR if it is undesired.

Tidy is very verbose, so we prevent Flycheck from highlighting
most errors from HTML Tidy."
  ;; A non-nil result means to inhibit further processing (i.e. highlighting)
  ;; of the error
  (and (eq (flycheck-error-checker err) 'html-tidy)
       ;; Only allow warnings about missing tags, or unexpected end tags being
       ;; discarded
       (not (string-match-p (rx (or "missing" "discarding"))
                            (flycheck-error-message err)))))

(defun dotemacs-flycheck-mode-line-status ()
  "Create a mode line status text for Flycheck."
  (let* ((menu (mouse-menu-non-singleton flycheck-mode-menu-map))
         (map (make-mode-line-mouse-map 'mouse-1
                                        (lambda ()
                                          (interactive)
                                          (popup-menu menu))))
         (text-and-face
          (pcase flycheck-last-status-change
            (`not-checked nil)
            (`no-checker '("⭕" . warning))
            (`running '( "🔨" . success))
            (`errored '( "❗️" . error))
            (`finished
             (let* ((error-counts (flycheck-count-errors
                                   flycheck-current-errors))
                    (no-errors (cdr (assq 'error error-counts)))
                    (no-warnings (cdr (assq 'warning error-counts)))
                    (face (cond (no-errors 'error)
                                (no-warnings 'warning)
                                (t 'success))))
               (cons (format "%s|%s" (or no-errors 0) (or no-warnings 0))
                     face)))
            (`interrupted (cons "❌" nil))
            (`suspicious '("⁉" . warning)))))
    (when text-and-face
      (propertize (car text-and-face) 'face (cdr text-and-face)
                  'mouse-face 'mode-line-highlight
                  'local-map map))))

(defun dotemacs-mode-line-flycheck-info-toggle ()
  "Toggle display of flycheck info."
  (interactive)
  (if flycheck-mode
      (flycheck-mode -1)
    (flycheck-mode)))

(defun dotemacs-adjust-flycheck-automatic-syntax-eagerness ()
  "Adjust how often we check for errors based on if there are any.

This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
  (setq flycheck-idle-change-delay
        (if flycheck-current-errors 0.5 30.0)))

(defun flycheck-handle-idle-change ()
  "Handle an expired idle time since the last change.

This is an overwritten version of the original
flycheck-handle-idle-change, which removes the forced deferred.
Timers should only trigger in between commands in a single
threaded system and the forced deferred makes errors never show
up before you execute another command."
  (flycheck-clear-idle-change-timer)
  (flycheck-buffer-automatically 'idle-change))

(defvar dotemacs//flycheck-executables-searched nil)
(defvar dotemacs//flycheck-executable-eslint nil)
(defvar dotemacs//flycheck-executable-jscs nil)
(defvar dotemacs//flycheck-executable-jshint nil)
(defvar dotemacs//flycheck-executable-tidy5 nil)

(defun dotemacs-eslint-set-local-eslint-from-projectile ()
  "Use local eslint CLI from `./node_modules` if available."
  (when-let (eslint (executable-find "eslint"))
            (setq flycheck-javascript-eslint-executable eslint)
            (setq dotemacs//flycheck-executable-eslint eslint)))

(defun dotemacs-flycheck-executables-search ()
  "Lazy locate javascript executables."
  (unless dotemacs//flycheck-executables-searched
    (when-let (eslint (executable-find "eslint"))
              (setq flycheck-javascript-eslint-executable eslint)
              (setq dotemacs//flycheck-executable-eslint eslint))
    (when-let (jscs (executable-find "jscs"))
              (setq flycheck-javascript-jscs-executable jscs)
              (setq dotemacs//flycheck-executable-jscs jscs))
    (when-let (jshint (executable-find "jshint"))
              (setq flycheck-javascript-jshint-executable jshint)
              (setq dotemacs//flycheck-executable-jshint jshint))
    (when-let (tidy5 (executable-find "tidy5"))
              (setq flycheck-html-tidy-executable tidy5)
              (setq dotemacs//flycheck-executable-tidy5 tidy5))
    (setq dotemacs//flycheck-executables-searched t)))

(defun dotemacs//flycheck-disable (checker)
  (interactive)
  (add-to-list 'flycheck-disabled-checkers checker))

(defun dotemacs//flycheck-enable (checker)
  (interactive)
  (setq flycheck-disabled-checkers (remove checker flycheck-disabled-checkers)))

(defun dotemacs-flycheck-executables-updated ()
  (when (bound-and-true-p dotemacs//flycheck-executables-searched)
    (when dotemacs//flycheck-executable-eslint
      (dotemacs-set-leader-keys
        "tee" 'flycheck-eslint-enable
        "teE" 'flycheck-eslint-disable))

    (when dotemacs//flycheck-executable-jscs
      (dotemacs-set-leader-keys
        "tec" 'flycheck-jscs-enable
        "teC" 'flycheck-jscs-disable))

    (when dotemacs//flycheck-executable-jshint
      (dotemacs-set-leader-keys
        "teh" 'flycheck-jshint-enable
        "teH" 'flycheck-jshint-disable))

    ;; (when (equal major-mode 'js2-mode)
    ;;   (dotemacs-flycheck-init-javascript))
    ))

(defun dotemacs-flycheck-init-react ()
  "Init flycheck settings for react-mode."
  (when (bound-and-true-p dotemacs//flycheck-executables-searched)
    (if dotemacs//flycheck-executable-eslint
        (progn
          (dotemacs//flycheck-enable 'javascript-eslint)
          ;; disable jshint since we prefer eslint checking
          (when dotemacs//flycheck-executable-jshint
            (dotemacs//flycheck-disable 'javascript-jshint)))
      (progn
        ;; otherwise enable jshint if eslint is not found
        (when dotemacs//flycheck-executable-jshint
          (dotemacs//flycheck-enable 'javascript-jshint))))

    ;; disable html-tidy
    (when dotemacs//flycheck-executable-tidy5
      (dotemacs//flycheck-disable 'html-tidy))

    ;; disable jscs
    (when dotemacs//flycheck-executable-jscs
      (dotemacs//flycheck-disable 'javascript-jscs))

    ;; disable json-jsonlist checking for json files
    (dotemacs//flycheck-disable 'json-jsonlist)))

(defun dotemacs-flycheck-init-javascript ()
  "Use flycheck settings for a js2-mode."
  (if dotemacs//flycheck-executable-eslint
      (progn
        (dotemacs//flycheck-enable 'javascript-eslint)
        ;; disable jshint since we prefer eslint checking
        (when dotemacs//flycheck-executable-jshint
          (dotemacs//flycheck-disable 'javascript-jshint)))
    (progn
      ;; otherwise enable jshint if eslint is not found
      (when dotemacs//flycheck-executable-jshint
        (dotemacs//flycheck-enable 'javascript-jshint))))

  ;; disable jscs
  (when dotemacs//flycheck-executable-jscs
    (dotemacs//flycheck-disable 'javascript-jscs)))

(use-package flycheck                   ; On-the-fly syntax checking
  :ensure t
  :defer t
  :init
  (progn
    (setq flycheck-standard-error-navigation nil
          flycheck-completion-system 'ido)

    ;; Each buffer gets its own idle-change-delay because of the
    ;; buffer-sensitive adjustment above.
    (make-variable-buffer-local 'flycheck-idle-change-delay)

    (add-hook 'flycheck-after-syntax-check-hook
              #'dotemacs-adjust-flycheck-automatic-syntax-eagerness)

    (dotemacs-add-toggle syntax-checking
      :status flycheck-mode
      :on (flycheck-mode)
      :off (flycheck-mode -1)
      :documentation "Enable error and syntax checking."
      :evil-leader "ts"))
  :config
  (progn
    ;; Make flycheck recognize packages in loadpath
    ;; i.e (require 'company) will not give an error now
    (setq flycheck-emacs-lisp-load-path 'inherit)

    (dotemacs-diminish flycheck-mode " ⓢ" " s")

    (evilified-state-evilify-map flycheck-error-list-mode-map
      :mode flycheck-error-list-mode
      :bindings
      "RET" 'flycheck-error-list-goto-error
      "j" 'flycheck-error-list-next-error
      "k" 'flycheck-error-list-previous-error)

    (dotemacs-set-leader-keys
      "ec" 'flycheck-clear
      "eh" 'flycheck-describe-checker
      "el" 'dotemacs-toggle-flycheck-error-list
      ; ; https://github.com/flycheck/flycheck/pull/494
      ; "el" 'dotemacs-flycheck-pop-to-error-list
      ; "eL" 'dotemacs-flycheck-hide-list-errors
      "es" 'flycheck-select-checker
      "eS" 'flycheck-set-checker-executable
      "ev" 'flycheck-verify-setup)

    (dotemacs|do-after-display-system-init
      (unless (display-graphic-p)
        (setq flycheck-display-errors-function
              #'flycheck-display-error-messages-unless-error-list
              flycheck-mode-line
              '(:eval (dotemacs-flycheck-mode-line-status)))))

    ;; Don't highlight undesired errors from html tidy
    (add-hook 'flycheck-process-error-functions
              #'dotemacs-discard-undesired-html-tidy-error)

    ;; Use italic face for checker name
    (set-face-attribute 'flycheck-error-list-checker-name nil
                        :inherit 'italic)

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Flycheck errors*" eos)
                    (display-buffer-reuse-window
                      display-buffer-in-side-window)
                    (side            . bottom)
                    (reusable-frames . visible)
                    (window-height   . 0.4)))

    ;; toggle flycheck window
    (defun dotemacs-toggle-flycheck-error-list ()
      "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
      (interactive)
      (-if-let (window (flycheck-get-error-list-window))
          (quit-window nil window)
        (flycheck-list-errors)))

    (defun dotemacs-flycheck-pop-to-error-list ()
      (interactive)
      (flycheck-list-errors)
      (pop-to-buffer flycheck-error-list-buffer))

    (defun dotemacs-flycheck-hide-list-errors ()
      "Hide the error list for the current buffer."
      (interactive)
      (let ((buffer (get-buffer flycheck-error-list-buffer)))
        (when buffer
          (let ((window (get-buffer-window buffer)))
            (when window
              (unless (flycheck-overlays-at (point))
                (quit-window nil window)))))))

    ;; Custom fringe indicator
    (when (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'my-flycheck-fringe-indicator
        (vector #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00011100
                #b00111110
                #b00111110
                #b00111110
                #b00011100
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000)))

    (flycheck-define-error-level 'error
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-error)
    (flycheck-define-error-level 'warning
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-warning)
    (flycheck-define-error-level 'info
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-info)))

(use-package flycheck-pos-tip
  :ensure t
  :if dotemacs-s-syntax-checking-enable-tooltips
  :defer t
  :init
  (with-eval-after-load 'flycheck
    (dotemacs|do-after-display-system-init
     (flycheck-pos-tip-mode))))

(use-package helm-flycheck
  :ensure t
  :commands helm-flycheck
  :init (dotemacs-set-leader-keys "ef" 'helm-flycheck))

(provide 'module-flycheck)
;;; module-flycheck.el ends here
