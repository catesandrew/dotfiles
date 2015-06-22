(defun dotemacs-helm-find-files (arg)
  "Custom implementation for calling helm-find-files-1.

Removes the automatic guessing of the initial value based on thing at point. "
  (interactive "P")
  (let* ((hist          (and arg helm-ff-history (helm-find-files-history)))
          (default-input hist )
          (input         (cond ((and (eq major-mode 'dired-mode) default-input)
                              (file-name-directory default-input))
                              ((and (not (string= default-input ""))
                                      default-input))
                              (t (expand-file-name (helm-current-directory))))))
      (set-text-properties 0 (length input) nil input)
      (helm-find-files-1 input )))

(defun dotemacs-helm-find-files-navigate-back (orig-fun &rest args)
  )

(defun dotemacs-helm-do-grep-region-or-symbol (&optional targs use-region-or-symbol-p)
  "Version of `helm-do-grep' with a default input."
  (interactive)
  (require 'helm)
  (cl-letf*
      (((symbol-function 'this-fn) (symbol-function 'helm-do-grep-1))
       ((symbol-function 'helm-do-grep-1)
        (lambda (targets &optional recurse zgrep exts default-input region-or-symbol-p)
          (let* ((new-input (when region-or-symbol-p
                             (if (region-active-p)
                                 (buffer-substring-no-properties
                                  (region-beginning) (region-end))
                               (thing-at-point 'symbol t))))
                (quoted-input (when new-input (rxt-quote-pcre new-input))))
            (this-fn targets recurse zgrep exts default-input quoted-input))))
       (preselection (or (dired-get-filename nil t)
                         (buffer-file-name (current-buffer))))
       (targets   (if targs
                      targs
                    (helm-read-file-name
                    "Search in file(s): "
                    :marked-candidates t
                    :preselect (and helm-do-grep-preselect-candidate
                                    (if helm-ff-transformer-show-only-basename
                                        (helm-basename preselection)
                                      preselection))))))
    (helm-do-grep-1 targets nil nil nil nil use-region-or-symbol-p)))


(defun dotemacs-helm-file-do-grep ()
  "Search in current file with `grep' using a default input."
  (interactive)
  (dotemacs-helm-do-grep-region-or-symbol
   (list (buffer-file-name (current-buffer))) nil))

(defun dotemacs-helm-file-do-grep-region-or-symbol ()
  "Search in current file with `grep' using a default input."
  (interactive)
  (dotemacs-helm-do-grep-region-or-symbol
   (list (buffer-file-name (current-buffer))) t))

(defun dotemacs-helm-files-do-grep ()
  "Search in files with `grep'."
  (interactive)
  (dotemacs-helm-do-grep-region-or-symbol nil nil))

(defun dotemacs-helm-files-do-grep-region-or-symbol ()
  "Search in files with `grep' using a default input."
  (interactive)
  (dotemacs-helm-do-grep-region-or-symbol nil t))

(defun dotemacs-helm-buffers-do-grep ()
  "Search in opened buffers with `grep'."
  (interactive)
  (let ((buffers (cl-loop for buffer in (buffer-list)
                          when (buffer-file-name buffer)
                          collect (buffer-file-name buffer))))
    (dotemacs-helm-do-grep-region-or-symbol buffers nil)))

(defun dotemacs-helm-buffers-do-grep-region-or-symbol ()
  "Search in opened buffers with `grep' with a default input."
  (interactive)
  (let ((buffers (cl-loop for buffer in (buffer-list)
                          when (buffer-file-name buffer)
                          collect (buffer-file-name buffer))))
    (dotemacs-helm-do-grep-region-or-symbol buffers t)))

(defun dotemacs-last-search-buffer ()
  "open last helm-ag or hgrep buffer."
  (interactive)
  (if (get-buffer "*helm ag results*")
      (switch-to-buffer-other-window "*helm ag results*")
      (if (get-buffer "*hgrep*")
          (switch-to-buffer-other-window "*hgrep*")
          (message "No previous search buffer found"))))

(defvar dotemacs-helm-display-help-buffer-regexp '("*.*Helm.*Help.**"))
(defvar dotemacs-helm-display-buffer-regexp `("*.*helm.**"
                                               (display-buffer-in-side-window)
                                               (inhibit-same-window . t)
                                               (window-height . 0.4)))
(defvar dotemacs-display-buffer-alist nil)

(defun dotemacs-display-helm-at-bottom ()
  "Display the helm buffer at the bottom of the frame."
  ;; avoid Helm buffer being diplaye twice when user
  ;; sets this variable to some function that pop buffer to
  ;; a window. See https://github.com/syl20bnr/spacemacs/issues/1396
  (let ((display-buffer-base-action '(nil)))
    ;; backup old display-buffer-base-action
    (setq dotemacs-display-buffer-alist display-buffer-alist)
    ;; the only buffer to display is Helm, nothing else we must set this
    ;; otherwise Helm cannot reuse its own windows for copyinng/deleting
    ;; etc... because of existing popwin buffers in the alist
    (setq display-buffer-alist nil)
    (add-to-list 'display-buffer-alist dotemacs-helm-display-buffer-regexp)
    ;; this or any specialized case of Helm buffer must be added AFTER
    ;; `dotemacs-helm-display-buffer-regexp'. Otherwise,
    ;; `dotemacs-helm-display-buffer-regexp' will be used before
    ;; `dotemacs-helm-display-help-buffer-regexp' and display
    ;; configuration for normal Helm buffer is applied for helm help
    ;; buffer, making the help buffer unable to be displayed.
    (add-to-list 'display-buffer-alist dotemacs-helm-display-help-buffer-regexp)
    (popwin-mode -1)))

(defun dotemacs-restore-previous-display-config ()
  (popwin-mode 1)
  ;; we must enable popwin-mode first then restore `display-buffer-alist'
  ;; Otherwise, popwin keeps adding up its own buffers to `display-buffer-alist'
  ;; and could slow down Emacs as the list grows
  (setq display-buffer-alist dotemacs-display-buffer-alist))


(defun dotemacs-helm-cleanup ()
  "Cleanup some helm related states when quitting."
  ;; deactivate any running transient map (micro-state)
  (setq overriding-terminal-local-map nil))


(defun dotemacs-set-dotted-directory ()
  "Set the face of diretories for `.' and `..'"
  (set-face-attribute 'helm-ff-dotted-directory
                      nil
                      :foreground nil
                      :background nil
                      :inherit 'helm-ff-directory))

;; alter helm-bookmark key bindings to be simpler
(defun simpler-helm-bookmark-keybindings ()
  (define-key helm-bookmark-map (kbd "C-d") 'helm-bookmark-run-delete)
  (define-key helm-bookmark-map (kbd "C-e") 'helm-bookmark-run-edit)
  (define-key helm-bookmark-map (kbd "C-f") 'helm-bookmark-toggle-filename)
  (define-key helm-bookmark-map (kbd "C-o") 'helm-bookmark-run-jump-other-window)
  (define-key helm-bookmark-map (kbd "C-/") 'helm-bookmark-help))

;; helm navigation on hjkl
(defun dotemacs-helm-hjkl-navigation (&optional arg)
  "Set navigation in helm on `jklh'.
ARG non nil means that the editing style is `vim'."
  (cond
   (arg
    (define-key helm-map (kbd "C-j") 'helm-next-line)
    (define-key helm-map (kbd "C-k") 'helm-previous-line)
    (define-key helm-map (kbd "C-h") 'helm-next-source)
    (define-key helm-map (kbd "C-l") 'helm-previous-source))
   (t
    (define-key helm-map (kbd "C-j") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-k") 'helm-delete-minibuffer-contents)
    (define-key helm-map (kbd "C-h") nil)
    (define-key helm-map (kbd "C-l") 'helm-recenter-top-bottom-other-window))))

(defun dotemacs-helm-edit ()
  "Switch in edit mode depending on the current helm buffer."
  (interactive)
  (cond
   ((string-equal "*helm-ag*" helm-buffer)
    (helm-ag-edit))))

(defun dotemacs-helm-navigation-ms-on-enter ()
  "Initialization of helm micro-state."
  ;; faces
  (dotemacs-helm-navigation-ms-set-face)
  (setq spacemacs--helm-navigation-ms-face-cookie-minibuffer
        (face-remap-add-relative
         'minibuffer-prompt
         'dotemacs-helm-navigation-ms-face))
  ;; bind actions on numbers starting from 1 which executes action 0
  (dotimes (n 10)
    (define-key helm-map (number-to-string n)
      `(lambda () (interactive) (helm-select-nth-action
                                 ,(% (+ n 9) 10))))))

(defun dotemacs-helm-navigation-ms-set-face ()
  "Set the face for helm header in helm navigation micro-state"
  (with-helm-window
    (setq spacemacs--helm-navigation-ms-face-cookie-header
          (face-remap-add-relative
           'helm-header
           'dotemacs-helm-navigation-ms-face))))

(defun dotemacs-helm-navigation-ms-on-exit ()
  "Action to perform when exiting helm micro-state."
  ;; restore helm key map
  (dotimes (n 10) (define-key helm-map (number-to-string n) nil))
  ;; restore faces
  (with-helm-window
    (face-remap-remove-relative
     spacemacs--helm-navigation-ms-face-cookie-header))
  (face-remap-remove-relative
   spacemacs--helm-navigation-ms-face-cookie-minibuffer))

(defun dotemacs-helm-navigation-ms-full-doc ()
  "Full documentation for helm navigation micro-state."
    "
  [?]          display this help
  [a]          toggle action selection page
  [e]          edit occurrences if supported
  [j] [k]      next/previous candidate
  [h] [l]      previous/next source
  [t]          toggle visible mark
  [T]          toggle all mark
  [v]          persistent action
  [q]          quit")


(provide 'init-helm)
