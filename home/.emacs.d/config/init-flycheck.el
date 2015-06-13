; TODO Go through the following commented code

(require 'flycheck)

; (after "flycheck"
;   ;; Remove newline checks, since they would trigger an immediate check
;   ;; when we want the idle-change-delay to be in effect while editing.
;   (setq flycheck-check-syntax-automatically '(save
;                                               idle-change
;                                               mode-enabled))
;   (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
;   (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
;   (setq flycheck-standard-error-navigation nil))
;
; (global-flycheck-mode t)
;
; ;; flycheck errors on a tooltip (doesnt work on console)
; (when (display-graphic-p (selected-frame))
;   (eval-after-load 'flycheck
;     '(custom-set-variables
;       '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))
;
; (defun my/adjust-flycheck-automatic-syntax-eagerness ()
;   "Adjust how often we check for errors based on if there are any.
;
; This lets us fix any errors as quickly as possible, but in a
; clean buffer we're an order of magnitude laxer about checking."
;   (setq flycheck-idle-change-delay
;         (if flycheck-current-errors 0.5 30.0)))
;
; ;; Each buffer gets its own idle-change-delay because of the
; ;; buffer-sensitive adjustment above.
; (make-variable-buffer-local 'flycheck-idle-change-delay)
;
; (add-hook 'flycheck-after-syntax-check-hook
;           'my/adjust-flycheck-automatic-syntax-eagerness)
;
; (defun flycheck-handle-idle-change ()
;   "Handle an expired idle time since the last change.
;
; This is an overwritten version of the original
; flycheck-handle-idle-change, which removes the forced deferred.
; Timers should only trigger inbetween commands in a single
; threaded system and the forced deferred makes errors never show
; up before you execute another command."
;   (flycheck-clear-idle-change-timer)
;   (flycheck-buffer-automatically 'idle-change))


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

(provide 'init-flycheck)
