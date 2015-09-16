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

; support for auto-indentation inhibition on universal argument
(dotemacs-advise-commands
  "handle-indent" (evil-paste-before evil-paste-after) around
  "Handle the universal prefix argument for auto-indentation."
  (let ((prefix (ad-get-arg 0)))
    (ad-set-arg 0 (unless (equal '(4) prefix) prefix))
    ad-do-it
    (ad-set-arg 0 prefix)))

; pasting micro-state
(dotemacs-advise-commands
  "paste-micro-state"
  (evil-paste-before evil-paste-after evil-visual-paste) after
  "Initate the paste micro-state."
  (unless (or (evil-ex-p)
                    (eq 'evil-paste-from-register this-command))
          (dotemacs-paste-micro-state)))

(defun dotemacs-paste-ms-doc ()
  "The documentation for the paste micro-state."
  (format (concat "[%s/%s] Type [p] or [P] to paste the previous or "
                  "next copied text, [.] to paste the same text")
          (length kill-ring-yank-pointer) (length kill-ring)))

(dotemacs-define-micro-state paste
  :doc (dotemacs-paste-ms-doc)
  :use-minibuffer t
  :bindings
  ("p" evil-paste-pop)
  ("P" evil-paste-pop-next))

(defun dotemacs-standard-text-objects ()
  ;; between dollars sign:
  (dotemacs-define-text-object "$" "dollar" "$" "$")
  ;; define stars
  (dotemacs-define-text-object "*" "star" "*" "*")
  ;; define block star text object
  (dotemacs-define-text-object "8" "block-star" "/*" "*/")
  ;; between pipe characters:
  (dotemacs-define-text-object "|" "bar" "|" "|")
  ;; between percent signs:
  (dotemacs-define-text-object "%" "percent" "%" "%"))

(defun dotemacs-turn-on-search-highlight-persist ()
  "Enable search-highlight-persist in the current buffer."
  (interactive)
  (evil-search-highlight-persist 1))

(defun dotemacs-turn-off-search-highlight-persist ()
  "Disable evil-search-highlight-persist in the current buffer."
  (interactive)
  (evil-search-highlight-persist -1))

; (when (>= emacs-major-version 25)
;   (defadvice elisp--preceding-sexp (around evil activate)
;     "In normal-state or motion-state, last sexp ends at point."
;     (if (or (evil-normal-state-p) (evil-motion-state-p))
;         (save-excursion
;           (unless (or (eobp) (eolp)) (forward-char))
;           ad-do-it)
;       ad-do-it)))

(provide 'init-evil)
