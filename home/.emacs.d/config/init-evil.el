(require 'evil)

(defun dotemacs-state-color-face (state)
  "Return the symbol of the face for the given STATE."
  (intern (format "dotemacs-%s-face" (symbol-name state))))

(defun dotemacs-state-color (state)
  "Return the color string associated to STATE."
  (face-background (dotemacs-state-color-face state)))

(defun dotemacs-current-state-color ()
  "Return the color string associated to the current state."
  (face-background (dotemacs-state-color-face evil-state)))

(defun dotemacs-state-face (state)
  "Return the face associated to the STATE."
  (dotemacs-state-color-face state))

(defun dotemacs-current-state-face ()
  "Return the face associated to the current state."
  (let ((state (if (eq evil-state 'operator)
                   evil-previous-state
                 evil-state)))
    (dotemacs-state-color-face state)))

(defun evil-insert-state-cursor-hide ()
  (setq evil-insert-state-cursor '((hbar . 0))))



(defun dotemacs-evil-smart-doc-lookup ()
  "Version of `evil-lookup' that attempts to use
  the mode specific goto-definition binding,
  i.e. `<leader> m h h`, to lookup the source of the definition,
  while falling back to `evil-lookup'"
  (interactive)
  (condition-case nil
      (execute-kbd-macro (kbd (concat dotemacs-leader-key " mhh")))
    (error (evil-lookup))))

(defun dotemacs-evil-smart-goto-definition ()
  "Version of `evil-goto-definition' that attempts to use
  the mode specific goto-definition binding,
  i.e. `<leader> m g g`, to lookup the source of the definition,
  while falling back to `evil-goto-definition'"
  (interactive)
  (condition-case nil
      (execute-kbd-macro (kbd (concat dotemacs-leader-key " mgg")))
    (error (evil-goto-definition))))

;; scrolling micro state
(defun dotemacs-scroll-half-page-up ()
  "Scroll half a page up while keeping cursor in middle of page."
  (interactive)
  (evil-window-top)
  (let ((recenter-redisplay nil))
    (recenter nil)))

(defun dotemacs-scroll-half-page-down ()
  "Scroll half a page down while keeping cursor in middle of page."
  (interactive)
  (evil-window-bottom)
  ;; required to make repeated presses idempotent
  (evil-next-visual-line)
  (let ((recenter-redisplay nil))
    (recenter nil)))

(defun dotemacs-paste-ms-doc ()
  "The documentation for the paste micro-state."
  (format (concat "[%s/%s] Type [p] or [P] to paste the previous or "
                  "next copied text, [.] to paste the same text")
          (length kill-ring-yank-pointer) (length kill-ring)))

(provide 'init-evil)
;;; init-evil.el ends here
