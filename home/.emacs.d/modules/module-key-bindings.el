;;; module-keybindings.el --- Key Bindings Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'core-vars)
;; (require 'core-funcs)
;; (require 'core-keybindings)
;; (require 'core-display-init)
(require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(declare-function dotemacs-home "module-utils")

(defun dotemacs/sort-lines ()
  "Sort lines in region or current buffer."
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (sort-lines nil beg end)))

(defun dotemacs/uniquify-lines ()
  "Remove duplicate adjacent lines in region or current buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (let ((beg (if (region-active-p) (region-beginning) (point-min)))
            (end (if (region-active-p) (region-end) (point-max))))
        (goto-char beg)
        (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
          (replace-match "\\1"))))))

(defun dotemacs/switch-to-scratch-buffer ()
  "Switch to the `*scratch*' buffer. Create it first if needed."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(defun dotemacs-copy-file ()
  "Write the file under new name."
  (interactive)
  (call-interactively 'write-file))

(defun dotemacs/unix2dos ()
  "Converts the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun dotemacs/dos2unix ()
  "Converts the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun dotemacs-write-file ()
  "Write the file if visiting a file.
   Otherwise ask for new filename."
  (interactive)
  (if (buffer-file-name)
      (call-interactively 'evil-write)
    (call-interactively 'write-file)))

;; modified function from http://emacswiki.org/emacs/AlignCommands
(defun dotemacs/align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let ((complete-regexp (if after
                             (concat regexp "\\([ \t]*\\)")
                           (concat "\\([ \t]*\\)" regexp)))
        (group (if justify-right -1 1)))
    (align-regexp start end complete-regexp group 1 t)))

;; Modified answer from http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun dotemacs/align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)"
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(defun dotemacs-smart-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun dotemacs-backward-kill-word-or-region (&optional arg)
  "Calls `kill-region' when a region is active and
`backward-kill-word' otherwise. ARG is passed to
`backward-kill-word' if no region is active."
  (interactive "p")
  (if (region-active-p)
      ;; call interactively so kill-region handles rectangular selection
      ;; correctly (see https://github.com/syl20bnr/spacemacs/issues/3278)
      (call-interactively #'kill-region)
    (backward-kill-word arg)))

(defmacro dotemacs/create-align-repeat-x (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "dotemacs/align-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
         (dotemacs/align-repeat start end ,regexp ,justify-right after)))))

(dotemacs/create-align-repeat-x "comma" "," nil t)
(dotemacs/create-align-repeat-x "semicolon" ";" nil t)
(dotemacs/create-align-repeat-x "colon" ":" nil t)
(dotemacs/create-align-repeat-x "equal" "=")
(dotemacs/create-align-repeat-x "math-oper" "[+\\-*/]")
(dotemacs/create-align-repeat-x "ampersand" "&")
(dotemacs/create-align-repeat-x "bar" "|")
(dotemacs/create-align-repeat-x "left-paren" "(")
(dotemacs/create-align-repeat-x "right-paren" ")" t)

(defun dotemacs/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

;; http://stackoverflow.com/a/10216338/4869
(defun dotemacs/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun dotemacs/close-compilation-window ()
  "Close the window containing the '*compilation*' buffer."
  (interactive)
  (delete-windows-on "*compilation*"))

(defun dotemacs/switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun dotemacs-open-in-external-app ()
  "Open current file in external application."
  (interactive)
  (let ((file-path (if (eq major-mode 'dired-mode)
                       (dired-get-file-for-visit)
                     (buffer-file-name))))
    (if file-path
        (cond
         ((dotemacs/system-is-mswindows) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
         ((dotemacs/system-is-mac) (shell-command (format "open \"%s\"" file-path)))
         ((dotemacs/system-is-linux) (let ((process-connection-type nil))
                                       (start-process "" nil "xdg-open" file-path))))
      (message "No file associated to this buffer."))))

(defun dotemacs-safe-erase-buffer ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s ? " (current-buffer)))
      (erase-buffer)))

(defun dotemacs-safe-revert-buffer ()
  "Prompt before reverting the file."
  (interactive)
  (revert-buffer nil nil))

(defun dotemacs-toggle-frame-fullscreen ()
  "Respect the `dotemacs-fullscreen-use-non-native' variable when
toggling fullscreen."
  (interactive)
  (if dotemacs-fullscreen-use-non-native
      (dotemacs/toggle-frame-fullscreen-non-native)
    (toggle-frame-fullscreen)))

(defun dotemacs/toggle-frame-fullscreen-non-native ()
  "Toggle full screen non-natively. Uses the `fullboth' frame paramerter
   rather than `fullscreen'. Useful to fullscreen on OSX w/o animations."
  (interactive)
  (modify-frame-parameters
   nil
   `((maximized
      . ,(unless (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
           (frame-parameter nil 'fullscreen)))
     (fullscreen
      . ,(if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
             (if (eq (frame-parameter nil 'maximized) 'maximized)
                 'maximized)
           'fullboth)))))

(defun dotemacs-frame-killer ()
  "Kill server buffer and hide the main Emacs window"
  (interactive)
  (condition-case nil
      (delete-frame nil 1)
    (error
     (make-frame-invisible nil 1))))

(defun dotemacs-prompt-kill-emacs ()
  "Prompt to save changed buffers and exit dotfiles"
  (interactive)
  (setq dotemacs-really-kill-emacs t)
  (save-some-buffers)
  (kill-emacs))

(defun dotemacs-kill-emacs ()
  "Lose all changes and exit dotfiles"
  (interactive)
  (setq dotemacs-really-kill-emacs t)
  (kill-emacs))

(defun dotemacs-save-buffers-kill-emacs ()
  "Save all changed buffers and exit dotfiles"
  (interactive)
  (setq dotemacs-really-kill-emacs t)
  (save-buffers-kill-emacs))

(defun dotemacs-insert-line-below-no-indent (count)
  "Insert a new line below with no identation."
  (interactive "p")
  (save-excursion
    (evil-move-end-of-line)
    (while (> count 0)
      (insert "\n")
      (setq count (1- count)))))

(defun dotemacs-insert-line-above-no-indent (count)
  (interactive "p")
  (let ((p (+ (point) count)))
    (save-excursion
      (if (eq (line-number-at-pos) 1)
          (evil-move-beginning-of-line)
        (progn
          (evil-previous-line)
          (evil-move-end-of-line)))
      (while (> count 0)
        (insert "\n")
        (setq count (1- count))))
    (goto-char p)))

;; from https://github.com/gempesaw/dotemacs/blob/emacs/dg-defun.el
(defun dotemacs/kill-matching-buffers-rudely (regexp &optional internal-too)
  "Kill buffers whose name matches the specified REGEXP. This
function, unlike the built-in `kill-matching-buffers` does so
WITHOUT ASKING. The optional second argument indicates whether to
kill internal buffers too."
  (interactive "sKill buffers matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (kill-buffer buffer)))))

;; advise to prevent server from closing
(defun dotemacs/persistent-server-running-p ()
  "Requires dotemacs-really-kill-emacs to be toggled and
dotemacs-persistent-server to be t"
  (and (fboundp 'server-running-p)
       (server-running-p)
       dotemacs-persistent-server))

(defun dotemacs/layout-double-columns ()
  " Set the layout to double columns. "
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun dotemacs/layout-triple-columns ()
  " Set the layout to triple columns. "
  (interactive)
  (delete-other-windows)
  (dotimes (i 2) (split-window-right))
  (balance-windows))

(defun dotemacs-split-and-new-line ()
  "Split a quoted string or s-expression and insert a new line with
auto-indent."
  (interactive)
  (sp-split-sexp 1)
  (sp-newline))

(defun dotemacs-push-mark-and-goto-beginning-of-line ()
  "Push a mark at current location and go to the beginnign of the line."
  (interactive)
  (push-mark (point))
  (evil-beginning-of-line))

(defun dotemacs-push-mark-and-goto-end-of-line ()
  "Push a mark at current location and go to the end of the line."
  (interactive)
  (push-mark (point))
  (evil-end-of-line))

;; insert one or several line below without changing current evil state
(defun evil-insert-line-below (count)
  "Insert one of several lines below the current point's line without changing
the current state and point position."
  (interactive "p")
  (save-excursion
    (evil-save-state (evil-open-below count))))

;; insert one or several line above without changing current evil state
(defun dotemacs/evil-insert-line-above (count)
  "Insert one of several lines above the current point's line without changing
the current state and point position."
  (interactive "p")
  (save-excursion
    (evil-save-state (evil-open-above count))))

(defun dotemacs/evil-goto-next-line-and-indent (&optional count)
  (interactive "p")
  (let ((counter (or count 1)))
    (while (> counter 0)
      (join-line 1)
      (newline-and-indent)
      (setq counter (1- counter)))))

;; from https://gist.github.com/3402786
(defun dotemacs/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc'_ register-alist))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

;; A small minor mode to use a big fringe
;; from http://bzg.fr/emacs-strip-tease.html
(defvar bzg-big-fringe-mode nil)
(define-minor-mode bzg-big-fringe-mode
  "Minor mode to use big fringe in the current buffer."
  :init-value nil
  :global t
  :variable bzg-big-fringe-mode
  :group 'editing-basics
  (if (not bzg-big-fringe-mode)
      (set-fringe-style nil)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
           (* 100 (frame-char-width)))
        2))))

(defun dotemacs/toggle-maximize-centered-buffer ()
  "Maximize buffer and center it on the screen"
  (interactive)
  (if (= 1 (length (window-list)))
      (progn  (bzg-big-fringe-mode 0)
              (jump-to-register '_))
    (progn
      (set-register '_ (list (current-window-configuration)))
      (delete-other-windows)
      (bzg-big-fringe-mode 1))))

;; from magnars modified by ffevotte for dedicated windows support
(defun dotemacs/rotate-windows (count)
  "Rotate your windows.
Dedicated windows are left untouched. Giving a negative prefix
argument takes the kindows rotate backwards."
  (interactive "p")
  (let* ((non-dedicated-windows (remove-if 'window-dedicated-p (window-list)))
         (num-windows (length non-dedicated-windows))
         (i 0)
         (step (+ num-windows count)))
    (cond ((not (> num-windows 1))
           (message "You can't rotate a single window!"))
          (t
           (dotimes (counter (- num-windows 1))
             (let* ((next-i (% (+ step i) num-windows))

                    (w1 (elt non-dedicated-windows i))
                    (w2 (elt non-dedicated-windows next-i))

                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))

                    (s1 (window-start w1))
                    (s2 (window-start w2)))
               (set-window-buffer w1 b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i next-i)))))))

(defun dotemacs/rotate-windows-backward (count)
  "Rotate your windows backward."
  (interactive "p")
  (dotemacs/rotate-windows (* -1 count)))

;; from magnars
(defun dotemacs/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

;; from magnars
(defun dotemacs/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun dotemacs-select-current-block ()
  "Select the current block of text between blank lines."
  (interactive)
  (let (p1 p2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "move")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq p1 (point)))
        (setq p1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "move")
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq p2 (point)))
        (setq p2 (point))))
    (set-mark p1)))

(defcustom dotemacs-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'dotemacs)

(defun dotemacs-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode dotemacs-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (evil-indent (point-min) (point-max))
          (message "Indented buffer.")))
      (whitespace-cleanup))))

;; linum gutter helpers
(defvar dotemacs-linum-mdown-line nil
  "Define persistent variable for linum selection")

(defun dotemacs-line-at-click ()
  "Determine the visual line at click"
  (save-excursion
    (let ((click-y (cddr (mouse-position)))
          (debug-on-error t)
          (line-move-visual t))
      (goto-char (window-start))
      (next-line (1- click-y))
      (1+ (line-number-at-pos))
      )))

(defun dotemacs-md-select-linum (event)
  "Set point as dotemacs-linum-mdown-line"
  (interactive "e")
  (mouse-select-window event)
  (goto-line (dotemacs-line-at-click))
  (set-mark (point))
  (setq dotemacs-linum-mdown-line
        (line-number-at-pos)))

(defun dotemacs-mu-select-linum ()
  "Select code block between point and dotemacs-linum-mdown-line"
  (interactive)
  (when dotemacs-linum-mdown-line
    (let (mu-line)
      (setq mu-line (dotemacs-line-at-click))
      (goto-line (max dotemacs-linum-mdown-line mu-line))
      (set-mark (line-end-position))
      (goto-line (min dotemacs-linum-mdown-line mu-line))
      (setq dotemacs-linum-mdown-line nil))))

;; from magnars
(defun dotemacs/sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; found at http://emacswiki.org/emacs/KillingBuffers
(defun dotemacs/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (let (name (buffer-name))
    (when (yes-or-no-p (format "Killing all buffers except \"%s\"? " (buffer-name)))
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
      (message "Buffers deleted!"))))

;; from http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun dotemacs/toggle-current-window-dedication ()
  "Toggle dedication state of a window."
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun dotemacs/show-and-copy-buffer-filename ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

;; adapted from bozhidar
;; http://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
(defun dotemacs/find-user-init-file ()
  "Edit the `user-init-file', in the current window."
  (interactive)
  (find-file-existing user-init-file))

(defun dotemacs/set-google-translate-languages (source target)
  "Set source language for google translate.
For instance pass En as source for English."
  (interactive
   "sEnter source language (ie. en): \nsEnter target language (ie. en): "
   source target)
  (message
   (format "Set google translate source language to %s and target to %s"
           source target))
  (setq google-translate-default-source-language (downcase source))
  (setq google-translate-default-target-language (downcase target)))

;; from http://www.emacswiki.org/emacs/WordCount
(defun dotemacs/count-words-analysis (start end)
  "Count how many times each word is used in the region.
 Punctuation is ignored."
  (interactive "r")
  (let (words alist_words_compare (formated ""))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\w+" end t)
        (let* ((word (intern (match-string 0)))
               (cell (assq word words)))
          (if cell
              (setcdr cell (1+ (cdr cell)))
            (setq words (cons (cons word 1) words))))))
    (defun alist_words_compare (a b)
      "Compare elements from an associative list of words count.
Compare them on count first,and in case of tie sort them alphabetically."
      (let ((a_key (car a))
            (a_val (cdr a))
            (b_key (car b))
            (b_val (cdr b)))
        (if (eq a_val b_val)
            (string-lessp a_key b_key)
          (> a_val b_val))))
    (setq words (cl-sort words 'alist_words_compare))
    (while words
      (let* ((word (pop words))
             (name (car word))
             (count (cdr word)))
        (setq formated (concat formated (format "[%s: %d], " name count)))))
    (when (interactive-p)
      (if (> (length formated) 2)
          (message (substring formated 0 -2))
        (message "No words.")))
    words))

(defadvice save-buffers-kill-emacs (around dotemacs-really-exit activate)
  "Only kill emacs if a prefix is set"
  (if (or dotemacs-really-kill-emacs (not dotemacs-persistent-server))
      ad-do-it
    (dotemacs/frame-killer)))

(defadvice kill-emacs (around dotemacs-really-exit activate)
  "Only kill emacs if a prefix is set"
  (if (and (not dotemacs-really-kill-emacs)
           (dotemacs/persistent-server-running-p))
      (dotemacs/frame-killer)
    ad-do-it))

;; have no use for these default bindings
(global-unset-key (kbd "C-x m"))  ;; send email

;; improve delete-other-windows
(define-key global-map (kbd "C-x 1") 'dotemacs/toggle-maximize-buffer)

;; alternate binding to search next occurrence with isearch without
;; exiting isearch
(define-key isearch-mode-map (kbd "S-<return>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-S-<return>") 'isearch-repeat-backward)
;; Escape from isearch-mode("/" and "?" in evil-mode) like vim
(define-key isearch-mode-map (kbd "<escape>") 'isearch-cancel)

;; Make <escape> quit as much as possible
(define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key evil-visual-state-map (kbd "<escape>") 'keyboard-quit)
(define-key minibuffer-local-ns-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map (kbd "<escape>") 'keyboard-escape-quit)

;; linum margin bindings-------------------------------------------------------
(global-set-key (kbd "<left-margin> <down-mouse-1>") 'dotemacs-md-select-linum)
(global-set-key (kbd "<left-margin> <mouse-1>") 'dotemacs-mu-select-linum)
(global-set-key (kbd "<left-margin> <double-mouse-1>") 'dotemacs-select-current-block)
(global-set-key (kbd "<left-margin> <drag-mouse-1>") 'dotemacs-mu-select-linum)

;; let TAB jump between links in help buffers
(evil-define-key 'motion help-mode-map (kbd "TAB") 'forward-button)
(evil-define-key 'motion help-mode-map (kbd "S-TAB") 'backward-button)

(global-set-key (kbd "C-a") 'dotemacs-smart-move-beginning-of-line)
(global-set-key (kbd "C-w") 'dotemacs-backward-kill-word-or-region)

;; ---------------------------------------------------------------------------
;; evil-leader key bindings
;; ---------------------------------------------------------------------------

;; Universal argument ---------------------------------------------------------
(dotemacs-set-leader-keys "u" 'universal-argument)
; (when (memq dotemacs-editing-style '(vim hybrid))
;   (define-key universal-argument-map
;     (kbd (concat dotemacs-leader-key " u"))
;     'universal-argument-more))
;; shell command  -------------------------------------------------------------
(dotemacs-set-leader-keys "!" 'shell-command)
;; applications ---------------------------------------------------------------
(dotemacs-set-leader-keys
  "ac"  'calc-dispatch
  "ad"  'dired
  "ap"  'proced
  "au"  'undo-tree-visualize)
;; buffers --------------------------------------------------------------------
(dotemacs-set-leader-keys
  "bd"  'kill-this-buffer
  "TAB" 'dotemacs-alternate-buffer
  "bh"  'dotemacs-home
  "be"  'dotemacs-safe-erase-buffer
  "bK"  'dotemacs/kill-other-buffers
  "bk"  'ido-kill-buffer
  "b C-k" 'dotemacs/kill-matching-buffers-rudely
  "bP"  'dotemacs/copy-clipboard-to-whole-buffer
  "bn"  'dotemacs-next-useful-buffer
  "bp"  'dotemacs-previous-useful-buffer
  "bR"  'dotemacs-safe-revert-buffer
  "bs"  'dotemacs/switch-to-scratch-buffer
  "bY"  'dotemacs/copy-whole-buffer-to-clipboard
  "bw"  'read-only-mode)
;; Cycling settings -----------------------------------------------------------
(dotemacs-set-leader-keys "Tn" 'dotemacs-cycle-dotemacs-theme)
;; describe functions ---------------------------------------------------------
(defmacro dotemacs-set-helm-key (keys func)
  "Define a key bindings for FUNC using KEYS.
Ensure that helm is required before calling FUNC."
  (let ((func-name (intern (format "dotemacs-%s" (symbol-name func)))))
    `(progn
       (defun ,func-name ()
         ,(format "Wrapper to ensure that `helm' is loaded before calling %s."
                  (symbol-name func))
         (interactive)
         (require 'helm)
         (call-interactively ',func))
       (dotemacs-set-leader-keys ,keys ',func-name))))
(dotemacs-set-helm-key "hdb" describe-bindings)
(dotemacs-set-helm-key "hdc" describe-char)
(dotemacs-set-helm-key "hdf" describe-function)
(dotemacs-set-helm-key "hdk" describe-key)
(dotemacs-set-helm-key "hdm" describe-mode)
(dotemacs-set-helm-key "hdp" describe-package)
(dotemacs-set-helm-key "hdt" describe-theme)
(dotemacs-set-helm-key "hdv" describe-variable)
(dotemacs-set-helm-key "hL" helm-locate-library)
;; search functions -----------------------------------------------------------
(dotemacs-set-helm-key "sww" helm-wikipedia-suggest)
(dotemacs-set-helm-key "swg" helm-google-suggest)
;; errors ---------------------------------------------------------------------
(dotemacs-set-leader-keys
  "ej" 'dotemacs-next-error
  "ek" 'dotemacs-previous-error
  "en" 'dotemacs-next-error
  "ep" 'dotemacs-previous-error)
;; file -----------------------------------------------------------------------
(dotemacs-set-leader-keys
  "fc" 'dotemacs-copy-file
  "fD" 'dotemacs/delete-current-buffer-file
  "fei" 'dotemacs/find-user-init-file
  "fed" 'dotemacs/find-dotfile
  "feD" 'ediff-dotfile-and-template
  "fev" 'dotemacs-display-and-copy-version
  "fCd" 'dotemacs/unix2dos
  "fCu" 'dotemacs/dos2unix
  "fg" 'rgrep
  "fj" 'dired-jump
  "fl" 'find-file-literally
  "fo" 'dotemacs-open-in-external-app
  "fE" 'dotemacs/sudo-edit
  "fR" 'dotemacs/rename-current-buffer-file
  "fS" 'evil-write-all
  "fs" 'dotemacs-write-file
  "fvd" 'add-dir-local-variable
  "fvf" 'add-file-local-variable
  "fvp" 'add-file-local-variable-prop-line
  "fy" 'dotemacs/show-and-copy-buffer-filename)
;; insert stuff ---------------------------------------------------------------
(dotemacs-set-leader-keys
  "iJ" 'dotemacs-insert-line-below-no-indent
  "iK" 'dotemacs-insert-line-above-no-indent
  "ik" 'dotemacs/evil-insert-line-above
  "ij" 'evil-insert-line-below)
;; format ---------------------------------------------------------------------
;; , j k key binding for a frequent action: go and indent line below the point
;; , J split the current line at point and indent it
(dotemacs-set-leader-keys
  "J"  'sp-split-sexp
  "jj" 'sp-newline
  "jo" 'open-line
  "j=" 'dotemacs-indent-region-or-buffer
  "jJ" 'dotemacs-split-and-new-line
  "jk" 'dotemacs/evil-goto-next-line-and-indent)

;; navigation -----------------------------------------------------------------
(dotemacs-set-leader-keys
  "jh" 'dotemacs-push-mark-and-goto-beginning-of-line
  "jl" 'dotemacs-push-mark-and-goto-end-of-line
  "jb" 'bookmark-jump
  "jd" 'dired-jump
  "jD" 'dired-jump-other-window
  "jf" 'find-function-at-point
  "ji" 'dotemacs/jump-in-buffer
  "jv" 'find-variable-at-point)

;; Compilation ----------------------------------------------------------------
(dotemacs-set-leader-keys
  "cC" 'compile
  "cr" 'recompile
  "cq" 'dotemacs/close-compilation-window)

;; narrow & widen -------------------------------------------------------------
(dotemacs-set-leader-keys
  "nr" 'narrow-to-region
  "np" 'narrow-to-page
  "nf" 'narrow-to-defun
  "nw" 'widen)
;; spell check  ---------------------------------------------------------------
(dotemacs-set-leader-keys
  "Sd" 'ispell-change-dictionary
  "Sn" 'flyspell-goto-next-error)
;; toggle ---------------------------------------------------------------------
(dotemacs-add-toggle highlight-current-line-globally
  :status global-hl-line-mode
  :on (global-hl-line-mode)
  :off (global-hl-line-mode -1)
  :documentation "Globally highlight the current line."
  :evil-leader "thh")
(dotemacs-add-toggle truncate-lines
  :status nil
  :on (toggle-truncate-lines)
  :documentation "Truncate long lines (no wrap)."
  :evil-leader "tt")
(dotemacs-add-toggle visual-line-navigation
  :status visual-line-mode
  :on (progn
        (visual-line-mode)
        (define-key evil-motion-state-map "j" 'evil-next-visual-line)
        (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
        (when (bound-and-true-p evil-escape-mode)
          (evil-escape-mode -1)
          (setq evil-escape-motion-state-shadowed-func nil)
          (define-key evil-motion-state-map "j" 'evil-next-visual-line)
          (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
          (evil-escape-mode)))
  :off (progn
         (visual-line-mode -1)
         (define-key evil-motion-state-map "j" 'evil-next-line)
         (define-key evil-motion-state-map "k" 'evil-previous-line)
         (when (bound-and-true-p evil-escape-mode)
           (evil-escape-mode -1)
           (setq evil-escape-motion-state-shadowed-func nil)
           (define-key evil-motion-state-map "j" 'evil-next-line)
           (define-key evil-motion-state-map "k" 'evil-previous-line)
           (evil-escape-mode)))
  :documentation "Move point according to visual lines."
  :evil-leader "tv")
(dotemacs-add-toggle line-numbers
  :status linum-mode
  :on (linum-mode)
  :off (linum-mode -1)
  :documentation "Show the line numbers."
  :evil-leader "tl")
(dotemacs-add-toggle auto-fill-mode
  :status auto-fill-function
  :on (auto-fill-mode)
  :off (auto-fill-mode -1)
  :documentation "Break line beyond `current-fill-column` while editing."
  :evil-leader "tF")
(dotemacs-add-toggle debug-on-error
  :status debug-on-error
  :on (setq debug-on-error t)
  :off (setq debug-on-error nil)
  :documentation "Toggle display of backtrace when an error happens."
  :evil-leader "tD")
(dotemacs-add-toggle fringe
  :status (not (equal fringe-mode 0))
  :on (call-interactively 'fringe-mode)
  :off (fringe-mode 0)
  :documentation "Display the fringe in GUI mode."
  :evil-leader "Tf")
(dotemacs-add-toggle fullscreen-frame
  :status nil
  :on (dotemacs-toggle-frame-fullscreen)
  :documentation "Display the current frame in full screen."
  :evil-leader "TF")
(dotemacs-add-toggle maximize-frame
  :if (version< "24.3.50" emacs-version)
  :status nil
  :on (toggle-frame-maximized)
  :documentation "Maximize the current frame."
  :evil-leader "TM")
(dotemacs-add-toggle mode-line
  :status (not hidden-mode-line-mode)
  :on (hidden-mode-line-mode -1)
  :off (hidden-mode-line-mode)
  :documentation "Toggle the visibility of modeline."
  :evil-leader "tmt")
(dotemacs-add-toggle transparent-frame
  :status nil
  :on (dotemacs-toggle-transparency)
  :documentation "Make the current frame non-opaque."
  :evil-leader "TT")
(dotemacs-add-toggle tool-bar
  :if window-system
  :status tool-bar-mode
  :on (tool-bar-mode)
  :off (tool-bar-mode -1)
  :documentation "Display the tool bar in GUI mode."
  :evil-leader "Tt")
(dotemacs-add-toggle menu-bar
  :if (or window-system (version<= "24.3.1" emacs-version))
  :status menu-bar-mode
  :on (menu-bar-mode)
  :off (menu-bar-mode -1)
  :documentation "Display the menu bar."
  :evil-leader "Tm")
(dotemacs-add-toggle semantic-stickyfunc
  :status semantic-stickyfunc-mode
  :on (semantic-stickyfunc-mode)
  :off (semantic-stickyfunc-mode -1)
  :documentation "Enable semantic-stickyfunc."
  :evil-leader "Ts")
(dotemacs-add-toggle semantic-stickyfunc-globally
  :status global-semantic-stickyfunc-mode
  :on (global-semantic-stickyfunc-mode)
  :off (global-semantic-stickyfunc-mode -1)
  :documentation "Enable semantic-stickyfunc globally."
  :evil-leader "T C-s")
;; quit -----------------------------------------------------------------------
(dotemacs-set-leader-keys
  "qs" 'dotemacs-save-buffers-kill-emacs
  "qq" 'dotemacs-prompt-kill-emacs
  "qQ" 'dotemacs-kill-emacs
  "qz" 'dotemacs-frame-killer)
;; window ---------------------------------------------------------------------
(defun split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right))

(dotemacs-set-leader-keys
  "w2"  'dotemacs/layout-double-columns
  "w3"  'dotemacs/layout-triple-columns
  "wb"  'dotemacs/switch-to-minibuffer-window
  "wc"  'delete-window
  "wd"  'dotemacs/toggle-current-window-dedication
  "wH"  'evil-window-move-far-left
  "w <S-left>"  'evil-window-move-far-left
  "wh"  'evil-window-left
  "w <left>"  'evil-window-left
  "wJ"  'evil-window-move-very-bottom
  "w <S-down>"  'evil-window-move-very-bottom
  "wj"  'evil-window-down
  "w <down>"  'evil-window-down
  "wK"  'evil-window-move-very-top
  "w <S-up>"  'evil-window-move-very-top
  "wk"  'evil-window-up
  "w <up>"  'evil-window-up
  "wL"  'evil-window-move-far-right
  "w <S-right>"  'evil-window-move-far-right
  "wl"  'evil-window-right
  "w <right>"  'evil-window-right
  "wm"  'dotemacs/toggle-maximize-buffer
  "wM"  'dotemacs/toggle-maximize-centered-buffer
  "wo"  'other-frame
  "wR"  'dotemacs/rotate-windows
  "ws"  'split-window-below
  "wS"  'split-window-below-and-focus
  "w-"  'split-window-below
  "wU"  'winner-redo
  "wu"  'winner-undo
  "wv"  'split-window-right
  "wV"  'split-window-right-and-focus
  "ww"  'other-window
  "w/"  'split-window-right
  "w="  'balance-windows)
;; text -----------------------------------------------------------------------
(defalias 'count-region 'count-words-region)

(dotemacs-set-leader-keys
  "xaa" 'align
  "xar" 'dotemacs/align-repeat
  "xam" 'dotemacs/align-repeat-math-oper
  "xa." 'dotemacs/align-repeat-decimal
  "xa," 'dotemacs/align-repeat-comma
  "xa;" 'dotemacs/align-repeat-semicolon
  "xa:" 'dotemacs/align-repeat-colon
  "xa=" 'dotemacs/align-repeat-equal
  "xa&" 'dotemacs/align-repeat-ampersand
  "xa|" 'dotemacs/align-repeat-bar
  "xa(" 'dotemacs/align-repeat-left-paren
  "xa)" 'dotemacs/align-repeat-right-paren
  "xc"  'count-region
  "xdw" 'delete-trailing-whitespace
  "xls" 'dotemacs/sort-lines
  "xlu" 'dotemacs/uniquify-lines
  "xtc" 'transpose-chars
  "xtl" 'transpose-lines
  "xtw" 'transpose-words
  "xU"  'upcase-region
  "xu"  'downcase-region
  "xwC" 'dotemacs/count-words-analysis)
;; google translate -----------------------------------------------------------
(dotemacs-set-leader-keys
  "xgl" 'dotemacs/set-google-translate-languages)
;; shell ----------------------------------------------------------------------
(with-eval-after-load 'shell
  (progn
    (evil-define-key 'insert comint-mode-map [up] 'comint-previous-input)
    (evil-define-key 'insert comint-mode-map [down] 'comint-next-input)))

;; ---------------------------------------------------------------------------
;; Micro-states
;; ---------------------------------------------------------------------------

;; Buffer micro state

(dotemacs-define-micro-state buffer
  :doc "[n]ext [p]revious [K]ill [q]uit"
  :disable-evil-leader t
  :evil-leader "b."
  :bindings
  ("K" kill-this-buffer)
  ("n" dotemacs-next-useful-buffer)
  ("N" dotemacs-previous-useful-buffer)
  ("p" dotemacs-previous-useful-buffer)
  ("q" nil :exit t))

;; end of Buffer micro state

;; Window Manipulation Micro State

(defun dotemacs-shrink-window-horizontally (delta)
  "Wrap `dotemacs-shrink-window-horizontally'."
  (interactive "p")
  (shrink-window delta t))

(defun dotemacs-shrink-window (delta)
  "Wrap `dotemacs-shrink-window'."
  (interactive "p")
  (shrink-window delta))

(defun dotemacs-enlarge-window (delta)
  "Wrap `dotemacs-enlarge-window'."
  (interactive "p")
  (enlarge-window delta))

(defun dotemacs-enlarge-window-horizontally (delta)
  "Wrap `dotemacs-enlarge-window-horizontally'."
  (interactive "p")
  (enlarge-window delta t))

(defun dotemacs-window-manipulation-full-doc ()
  "Full documentation for window manipulation micro-state."
  "
  [?]                       display this help
  [0,9]                     go to numbered window
  [-] [/] [s] [v] [S] [V]   split windows below|right and focus
  [c] [C]                   close current|other windows
  [g]                       toggle golden-ratio
  [h] [j] [k] [l]           go to left|bottom|top|right
  [H] [J] [K] [L]           move windows to far/very left|bottom|top|right
  [[] []] [{] [}]           shrink/enlarge horizontally and vertically respectively
  [o] [w]                   other frame|window
  [R]                       rotate windows
  [u] [U]                   restore previous|next window layout")

(defun dotemacs-window-manipulation-move-doc ()
  "Help string for moving between windows"
  (concat "[h] [j] [k] [l] to move focus, "
          "[H] [J] [K] [L] to move window, "
          "[R]otate windows, other [f]rame, other [w]indow"))

(defun dotemacs-window-manipulation-resize-doc ()
  "Dynamic help string when resizing windows."
  (format
   (concat "[%sx%s] Resize window: [[] []] shrink/enlarge horizontally, "
           "[{] [}] shrink/enlarge vertically.")
   (window-total-width) (window-total-height)))

(defun dotemacs-window-manipulation-split-doc ()
  "Help string for moving between windows"
  (concat "[-], [s] to split horizontally,  [/], [v] to split vertically, "
          "[S], [V] to split and focus"))

(defun dotemacs-window-manipulation-number-doc ()
  "Help string for selecting window with number."
  (format "(selected window #%s) press [0,9] to select the corresponding numbered window."
          (window-numbering-get-number-string)))

(defun dotemacs-window-manipulation-layout-doc ()
  "Help string for layout manipulation"
  (concat "[c]lose window, [C]lose other windows, "
          "[u]ndo window layout, [U] redo window layout."))

(defun dotemacs-window-manipulation-gratio-doc ()
  "Help string for golden ratio"
  (format "(golden-ration %s) toggle with [g]"
          (if (symbol-value golden-ratio-mode) "enabled" "disabled")))

(dotemacs-define-micro-state window-manipulation
  :doc "[?] for help"
  :evil-leader "w."
  :use-minibuffer t
  :bindings
  ("?" nil                                   :doc (dotemacs-window-manipulation-full-doc))
  ("0" select-window-0                       :doc (dotemacs-window-manipulation-number-doc))
  ("1" select-window-1                       :doc (dotemacs-window-manipulation-number-doc))
  ("2" select-window-2                       :doc (dotemacs-window-manipulation-number-doc))
  ("3" select-window-3                       :doc (dotemacs-window-manipulation-number-doc))
  ("4" select-window-4                       :doc (dotemacs-window-manipulation-number-doc))
  ("5" select-window-5                       :doc (dotemacs-window-manipulation-number-doc))
  ("6" select-window-6                       :doc (dotemacs-window-manipulation-number-doc))
  ("7" select-window-7                       :doc (dotemacs-window-manipulation-number-doc))
  ("8" select-window-8                       :doc (dotemacs-window-manipulation-number-doc))
  ("9" select-window-9                       :doc (dotemacs-window-manipulation-number-doc))
  ("-" split-window-below-and-focus          :doc (dotemacs-window-manipulation-split-doc))
  ("/" split-window-right-and-focus          :doc (dotemacs-window-manipulation-split-doc))
  ("[" dotemacs-shrink-window-horizontally   :doc (dotemacs-window-manipulation-resize-doc))
  ("]" dotemacs-enlarge-window-horizontally  :doc (dotemacs-window-manipulation-resize-doc))
  ("{" dotemacs-shrink-window                :doc (dotemacs-window-manipulation-resize-doc))
  ("}" dotemacs-enlarge-window               :doc (dotemacs-window-manipulation-resize-doc))
  ("c" delete-window                         :doc (dotemacs-window-manipulation-layout-doc))
  ("C" delete-other-windows                  :doc (dotemacs-window-manipulation-layout-doc))
  ; ("g" dotemacs-toggle-golden-ratio          :doc (dotemacs-window-manipulation-gratio-doc))
  ("h" evil-window-left                      :doc (dotemacs-window-manipulation-move-doc))
  ("<left>" evil-window-left                 :doc (dotemacs-window-manipulation-move-doc))
  ("j" evil-window-down                      :doc (dotemacs-window-manipulation-move-doc))
  ("<down>" evil-window-down                 :doc (dotemacs-window-manipulation-move-doc))
  ("k" evil-window-up                        :doc (dotemacs-window-manipulation-move-doc))
  ("<up>" evil-window-up                     :doc (dotemacs-window-manipulation-move-doc))
  ("l" evil-window-right                     :doc (dotemacs-window-manipulation-move-doc))
  ("<right>" evil-window-right               :doc (dotemacs-window-manipulation-move-doc))
  ("H" evil-window-move-far-left             :doc (dotemacs-window-manipulation-move-doc))
  ("<S-left>" evil-window-move-far-left      :doc (dotemacs-window-manipulation-move-doc))
  ("J" evil-window-move-very-bottom          :doc (dotemacs-window-manipulation-move-doc))
  ("<S-down>" evil-window-move-very-bottom   :doc (dotemacs-window-manipulation-move-doc))
  ("K" evil-window-move-very-top             :doc (dotemacs-window-manipulation-move-doc))
  ("<S-up>" evil-window-move-very-top        :doc (dotemacs-window-manipulation-move-doc))
  ("L" evil-window-move-far-right            :doc (dotemacs-window-manipulation-move-doc))
  ("<S-right>" evil-window-move-far-right    :doc (dotemacs-window-manipulation-move-doc))
  ("o" other-frame                           :doc (dotemacs-window-manipulation-move-doc))
  ("R" dotemacs/rotate-windows               :doc (dotemacs-window-manipulation-move-doc))
  ("s" split-window-below                    :doc (dotemacs-window-manipulation-split-doc))
  ("S" split-window-below-and-focus          :doc (dotemacs-window-manipulation-split-doc))
  ("u" winner-undo                           :doc (dotemacs-window-manipulation-layout-doc))
  ("U" winner-redo                           :doc (dotemacs-window-manipulation-layout-doc))
  ("v" split-window-right                    :doc (dotemacs-window-manipulation-split-doc))
  ("V" split-window-right-and-focus          :doc (dotemacs-window-manipulation-split-doc))
  ("w" other-window                          :doc (dotemacs-window-manipulation-move-doc)))

;; end of Window Manipulation Micro State

;; text Manipulation Micro State

(defun dotemacs-scale-up-or-down-font-size (direction)
  "Scale the font. If DIRECTION is positive or zero the font is scaled up,
otherwise it is scaled down."
  (interactive)
  (let ((scale 0.5))
    (if (eq direction 0)
        (text-scale-set 0)
      (if (< direction 0)
          (text-scale-decrease scale)
        (text-scale-increase scale)))))

(defun dotemacs-scale-up-font ()
  "Scale up the font."
  (interactive)
  (dotemacs-scale-up-or-down-font-size 1))

(defun dotemacs-scale-down-font ()
  "Scale up the font."
  (interactive)
  (dotemacs-scale-up-or-down-font-size -1))

(defun dotemacs-reset-font-size ()
  "Reset the font size."
  (interactive)
  (dotemacs-scale-up-or-down-font-size 0))

(dotemacs-define-micro-state scale-font
  :doc "[+/=] scale up [-] scale down [0] reset font [q]uit"
  :evil-leader "zx"
  :bindings
  ("+" dotemacs-scale-up-font)
  ("=" dotemacs-scale-up-font)
  ("-" dotemacs-scale-down-font)
  ("0" dotemacs-reset-font-size)
  ("q" nil :exit t))

;; end of Text Manipulation Micro State

;; Transparency micro-state

(defun dotemacs-toggle-transparency-core ()
  "Toggle between transparent or opaque display."
  (interactive)
  ;; Define alpha if it's nil
  (if (eq (frame-parameter (selected-frame) 'alpha) nil)
      (set-frame-parameter (selected-frame) 'alpha '(100 100)))
  ;; Do the actual toggle
  (if (/= (cadr (frame-parameter (selected-frame) 'alpha)) 100)
      (set-frame-parameter (selected-frame) 'alpha '(100 100))
    (set-frame-parameter (selected-frame) 'alpha
                         (list dotemacs-active-transparency
                               dotemacs-inactive-transparency))))

(defun dotemacs-toggle-transparency ()
  "Toggle between transparent or opaque display, then enter the micro-state."
  (interactive)
  (dotemacs-toggle-transparency-core)
  ;; Immediately enter the micro-state
  (dotemacs-scale-transparency-micro-state))

(defun dotemacs-increase-transparency ()
  "Increase transparency of current frame."
  (interactive)
  (let* ((current-alpha (car (frame-parameter (selected-frame) 'alpha)))
         (increased-alpha (- current-alpha 5)))
    (when (>= increased-alpha frame-alpha-lower-limit)
      (set-frame-parameter (selected-frame) 'alpha (list increased-alpha increased-alpha)))))

(defun dotemacs-decrease-transparency ()
  "Decrease transparency of current frame."
  (interactive)
  (let* ((current-alpha (car (frame-parameter (selected-frame) 'alpha)))
         (decreased-alpha (+ current-alpha 5)))
    (when (<= decreased-alpha 100)
      (set-frame-parameter (selected-frame) 'alpha (list decreased-alpha decreased-alpha)))))

(dotemacs-define-micro-state scale-transparency
  :doc "[+] increase [-] decrease [T] toggle transparency [q] quit"
  :bindings
  ("+" dotemacs-increase-transparency)
  ("-" dotemacs-decrease-transparency)
  ("T" dotemacs-toggle-transparency)
  ("q" nil :exit t))

(provide 'module-key-bindings)
;;; module-key-bindings.el ends here
