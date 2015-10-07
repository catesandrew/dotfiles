(require 'flycheck)

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

;; color mode line faces
(defun dotemacs-defface-flycheck-mode-line-color (state)
  "Define a face for the given Flycheck STATE."
  (let* ((fname (intern (format "dotemacs-mode-line-flycheck-%s-face"
                                (symbol-name state))))
        (foreground (face-foreground
                     (intern (format "flycheck-fringe-%s" state)))))
    (eval `(defface ,fname '((t ()))
             ,(format "Color for Flycheck %s feedback in mode line."
                      (symbol-name state))
             :group 'spacemacs))
    (set-face-attribute fname nil
                        :foreground foreground
                        :box (face-attribute 'mode-line :box))))

(defun dotemacs-set-flycheck-mode-line-faces ()
  "Define or set the flycheck info mode-line faces."
  (mapcar 'dotemacs-defface-flycheck-mode-line-color
          '(error warning info)))

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

(defun dotemacs//eslint-set-local-eslint-from-projectile ()
  "use local eslint CLI from `./node_modules` if available."
  (when-let (eslint (executable-find "eslint"))
            (setq flycheck-javascript-eslint-executable eslint)
            (setq dotemacs//flycheck-executable-eslint eslint)))

(defun dotemacs//flycheck-executables-search ()
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

(provide 'init-syntax-checking)
