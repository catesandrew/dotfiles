;; hack for speeding up the use of ace-jump-line as a motion
;; https://bitbucket.org/lyro/evil/issue/472/evil-half-cursor-makes-evil-ace-jump-mode
(defun evil-half-cursor ()
  "Change cursor to a half-height box. (This is really just a thick horizontal bar.)"
    (let ((height (/ (window-pixel-height) (* (window-height) 2))))
      (setq cursor-type (cons 'hbar height))))

(defun dotemacs-state-color-face (state)
  "Return the symbol of the face for the given STATE."
  (intern (format "dotemacs-%s-face" (symbol-name state))))

(defun dotemacs-defface-state-color (state color)
  "Define a face for the given STATE and background COLOR."
  (eval `(defface ,(dotemacs-state-color-face state) '((t ()))
           ,(format "%s state face." (symbol-name state))
           :group 'dotemacs))
  (set-face-attribute (dotemacs-state-color-face state) nil
                      :background color
                      :foreground (face-background 'mode-line)
                      :box (face-attribute 'mode-line :box)
                      :inherit 'mode-line))

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

(defun dotemacs-set-state-faces ()
  "Define or set the state faces."
  (mapcar (lambda (x) (dotemacs-defface-state-color (car x) (cdr x)))
          dotemacs-evil-cursor-colors))

(defun set-default-evil-emacs-state-cursor ()
  (let ((c (dotemacs-state-color 'emacs)))
    (setq evil-emacs-state-cursor `(,c box))))

(defun set-default-evil-evilified-state-cursor ()
  (let ((c (dotemacs-state-color 'evilified)))
    (setq evil-evilified-state-cursor `(,c box))))

(defun set-default-evil-normal-state-cursor ()
  (let ((c (dotemacs-state-color 'normal)))
    (setq evil-normal-state-cursor `(,c box))))

(defun set-default-evil-insert-state-cursor ()
  (let ((c (dotemacs-state-color 'insert)))
    (setq evil-insert-state-cursor `(,c (bar . 2)))))

(defun set-default-evil-visual-state-cursor ()
  (let ((c (dotemacs-state-color 'visual)))
  (setq evil-visual-state-cursor `(,c (hbar . 2)))))

(defun set-default-evil-replace-state-cursor ()
  (let ((c (dotemacs-state-color 'visual)))
  (setq evil-replace-state-cursor`(,c (bar . 2))))) ;; red/bar

(defun set-default-evil-operator-state-cursor ()
  (let ((c (dotemacs-state-color 'visual)))
  (setq evil-operator-state-cursor `(,c (hollow . 2))))) ;; red/hollow

(defun set-default-evil-motion-state-cursor ()
  (let ((c (dotemacs-state-color 'motion)))
    (setq evil-motion-state-cursor `(,c box))))

(defun set-default-evil-lisp-state-cursor ()
  (let ((c (dotemacs-state-color 'lisp)))
    (setq evil-lisp-state-cursor `(,c box))))

(defun set-default-evil-iedit-state-cursor ()
  (let ((c (dotemacs-state-color 'iedit)))
    (setq evil-iedit-state-cursor `(,c box))))

(defun set-default-evil-iedit-insert-state-cursor ()
  (let ((c (dotemacs-state-color 'iedit-insert)))
    (setq evil-iedit-insert-state-cursor `(,c (bar . 2)))))

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
 (unless (evil-ex-p) (dotemacs-paste-micro-state)))

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

(defun dotemacs-major-mode-evil-state-adjust ()
  (if (apply 'derived-mode-p dotemacs-evil-evil-state-modes)
      (turn-on-evil-mode))
  (when (apply 'derived-mode-p dotemacs-evil-emacs-state-modes)
    (turn-off-evil-mode)))

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
