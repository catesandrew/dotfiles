;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'flycheck)

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
      (evil-leader/set-key
        "tee" 'flycheck-eslint-enable
        "teE" 'flycheck-eslint-disable))

    (when dotemacs//flycheck-executable-jscs
      (evil-leader/set-key
        "tec" 'flycheck-jscs-enable
        "teC" 'flycheck-jscs-disable))

    (when dotemacs//flycheck-executable-jshint
      (evil-leader/set-key
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

(provide 'init-syntax-checking)
;;; init-syntax-checking.el ends here
