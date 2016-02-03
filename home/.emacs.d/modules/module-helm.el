;;; module-helm.el --- Heml Module
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
(require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(defvar helm-resize nil
  "If non nil, `helm' will try to miminimize the space it uses.")

(defvar helm-no-header t
  "if non nil, the helm header is hidden when there is only one source.")

(defvar dotemacs-helm-position 'bottom
  "Position in which to show the `helm' mini-buffer.")

(defvar dotemacs-search-tools '("ag" "pt" "ack" "grep")
  "List of search tool executable names.  Dotemacs uses the first installed
tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.")

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
    (helm-do-grep-1 targets t nil nil nil use-region-or-symbol-p)))

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
  (cond ((get-buffer "*helm ag results*")
         (switch-to-buffer-other-window "*helm ag results*"))
        ((get-buffer "*helm-ag*")
         (helm-resume "*helm-ag*"))
        ((get-buffer "*hgrep*")
         (switch-to-buffer-other-window "*hgrep*"))
        (t
         (message "No previous search buffer found"))))

(defvar dotemacs-helm-display-help-buffer-regexp '("*.*Helm.*Help.**"))
(defvar dotemacs-helm-display-buffer-regexp `("*.*helm.**"
                                               (display-buffer-in-side-window)
                                               (inhibit-same-window . t)
                                               (side . ,dotemacs-helm-position)
                                               (window-width . 0.6)
                                               (window-height . 0.4)))
(defvar dotemacs-display-buffer-alist nil)
(defun dotemacs-helm-prepare-display ()
  "Prepare necessary settings to make Helm display properly."
  ;; avoid Helm buffer being diplaye twice when user
  ;; sets this variable to some function that pop buffer to
  ;; a window. See https://github.com/syl20bnr/spacemacs/issues/1396
  (let ((display-buffer-base-action '(nil)))
    (setq dotemacs-display-buffer-alist display-buffer-alist)
    ;; the only buffer to display is Helm, nothing else we must set this
    ;; otherwise Helm cannot reuse its own windows for copyinng/deleting
    ;; etc... because of existing popwin buffers in the alist
    (setq display-buffer-alist nil)
    (popwin-mode -1)
    ;; workaround for a helm-evil incompatibility
    ;; see https://github.com/syl20bnr/spacemacs/issues/3700
    (when helm-prevent-escaping-from-minibuffer
      (define-key evil-motion-state-map [down-mouse-1] nil))))

(defun dotemacs-display-helm-window (buffer)
  (let ((display-buffer-alist (list dotemacs-helm-display-help-buffer-regexp
                                    ;; this or any specialized case of Helm buffer must be added AFTER
                                    ;; `dotemacs-helm-display-buffer-regexp'. Otherwise,
                                    ;; `dotemacs-helm-display-buffer-regexp' will be used before
                                    ;; `dotemacs-helm-display-help-buffer-regexp' and display
                                    ;; configuration for normal Helm buffer is applied for helm help
                                    ;; buffer, making the help buffer unable to be displayed.
                                    dotemacs-helm-display-buffer-regexp)))
    (helm-default-display-buffer buffer)))

(defun dotemacs-restore-previous-display-config ()
  ;; workaround for a helm-evil incompatibility
  ;; see https://github.com/syl20bnr/spacemacs/issues/3700
  (when helm-prevent-escaping-from-minibuffer
    (define-key evil-motion-state-map [down-mouse-1] 'evil-mouse-drag-region))
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
    ;; better navigation on homerow
    ;; rebind `describe-key' for convenience
    (define-key helm-map (kbd "C-j") 'helm-next-line)
    (define-key helm-map (kbd "C-k") 'helm-previous-line)
    (define-key helm-map (kbd "C-h") 'helm-next-source)
    (define-key helm-map (kbd "C-S-h") 'describe-key)
    (define-key helm-map (kbd "C-l") (kbd "RET"))
    (dolist (keymap (list helm-find-files-map helm-read-file-map))
      (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
      (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
      (define-key keymap (kbd "C-S-h") 'describe-key)))
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
  (setq dotemacs--helm-navigation-ms-face-cookie-minibuffer
        (face-remap-add-relative
         'minibuffer-prompt
         'dotemacs-helm-navigation-ms-face)))

(defun dotemacs-helm-navigation-ms-set-face ()
  "Set the face for helm header in helm navigation micro-state"
  (with-helm-window
    (setq dotemacs--helm-navigation-ms-face-cookie-header
          (face-remap-add-relative
           'helm-header
           'dotemacs-helm-navigation-ms-face))))

(defun dotemacs-helm-navigation-ms-on-exit ()
  "Action to perform when exiting helm micro-state."
  (with-helm-window
    (face-remap-remove-relative
     dotemacs--helm-navigation-ms-face-cookie-header))
  (face-remap-remove-relative
   dotemacs--helm-navigation-ms-face-cookie-minibuffer))

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

;; helm-ag

(defun dotemacs-helm-do-ag-region-or-symbol (func &optional dir)
  "Search with `ag' with a default input."
  (require 'helm-ag)
  (cl-letf* (((symbol-value 'helm-ag-insert-at-point) 'symbol)
             ;; make thing-at-point choosing the active region first
             ((symbol-function 'this-fn) (symbol-function 'thing-at-point))
             ((symbol-function 'thing-at-point)
              (lambda (thing)
                (let ((res (if (region-active-p)
                    (buffer-substring-no-properties
                     (region-beginning) (region-end))
                    (this-fn thing))))
                  (when res (rxt-quote-pcre res))))))
    (funcall func dir)))

(defun dotemacs-helm-do-search-find-tool (base tools default-inputp)
  "Create a cond form given a TOOLS string list and evaluate it."
  (eval
   `(cond
     ,@(mapcar
        (lambda (x)
          `((executable-find ,x)
            ',(let ((func
                     (intern
                      (format (if default-inputp
                                  "dotemacs-%s-%s-region-or-symbol"
                                "dotemacs-%s-%s")
                              base x))))
                (if (fboundp func)
                    func
                  (intern (format "%s-%s"  base x))))))
               tools)
     (t 'helm-do-grep))))

;; Search in current file ----------------------------------------------

(defun dotemacs-helm-file-do-ag (&optional _)
  "Wrapper to execute `helm-ag-this-file.'"
  (interactive)
  (helm-ag-this-file))

(defun dotemacs-helm-file-do-ag-region-or-symbol ()
  "Search in current file with `ag' using a default input."
  (interactive)
  (dotemacs-helm-do-ag-region-or-symbol 'dotemacs-helm-file-do-ag))

(defun dotemacs-helm-file-smart-do-search (&optional default-inputp)
  "Search in current file using `dotemacs-search-tools'.
Search for a search tool in the order provided by `dotemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
  (interactive)
  (call-interactively
   (dotemacs-helm-do-search-find-tool "helm-file-do"
                                      dotemacs-search-tools
                                      default-inputp)))

(defun dotemacs-helm-file-smart-do-search-region-or-symbol ()
  "Search in current file using `dotemacs-search-tools' with
 default input.
Search for a search tool in the order provided by `dotemacs-search-tools'."
  (interactive)
  (dotemacs-helm-file-smart-do-search t))

;; Search in files -----------------------------------------------------

(defun dotemacs-helm-files-do-ag (&optional dir)
  "Search in files with `ag' using a default input."
  (interactive)
  (helm-do-ag dir))

(defun dotemacs-helm-files-do-ag-region-or-symbol ()
  "Search in files with `ag' using a default input."
  (interactive)
  (dotemacs-helm-do-ag-region-or-symbol 'dotemacs-helm-files-do-ag))

(defun dotemacs-helm-files-do-ack (&optional dir)
  "Search in files with `ack'."
  (interactive)
  (let ((helm-ag-base-command "ack --nocolor --nogroup"))
    (helm-do-ag dir)))

(defun dotemacs-helm-files-do-ack-region-or-symbol ()
  "Search in files with `ack' using a default input."
  (interactive)
  (dotemacs-helm-do-ag-region-or-symbol 'dotemacs-helm-files-do-ack))

(defun dotemacs-helm-files-do-pt (&optional dir)
  "Search in files with `pt'."
  (interactive)
  (let ((helm-ag-base-command "pt -e --nocolor --nogroup"))
    (helm-do-ag dir)))

(defun dotemacs-helm-files-do-pt-region-or-symbol ()
  "Search in files with `pt' using a default input."
  (interactive)
  (dotemacs-helm-do-ag-region-or-symbol 'dotemacs-helm-files-do-pt))

(defun dotemacs-helm-files-smart-do-search (&optional default-inputp)
  "Search in opened buffers using `dotemacs-search-tools'.
Search for a search tool in the order provided by `dotemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
  (interactive)
  (call-interactively
   (dotemacs-helm-do-search-find-tool "helm-files-do"
                                      dotemacs-search-tools
                                      default-inputp)))

(defun dotemacs-helm-files-smart-do-search-region-or-symbol ()
  "Search in opened buffers using `dotemacs-search-tools'.
with default input.
Search for a search tool in the order provided by `dotemacs-search-tools'."
  (interactive)
  (dotemacs-helm-files-smart-do-search t))

;; Search in buffers ---------------------------------------------------

(defun dotemacs-helm-buffers-do-ag (&optional _)
  "Wrapper to execute `helm-ag-buffers.'"
  (interactive)
  (helm-do-ag-buffers))

(defun dotemacs-helm-buffers-do-ag-region-or-symbol ()
  "Search in opened buffers with `ag' with a default input."
  (interactive)
  (dotemacs-helm-do-ag-region-or-symbol 'dotemacs-helm-buffers-do-ag))

(defun dotemacs-helm-buffers-do-ack (&optional _)
  "Search in opened buffers with `ack'."
  (interactive)
  (let ((helm-ag-base-command "ack --nocolor --nogroup"))
    (helm-do-ag-buffers)))

(defun dotemacs-helm-buffers-do-ack-region-or-symbol ()
  "Search in opened buffers with `ack' with a default input."
  (interactive)
  (dotemacs-helm-do-ag-region-or-symbol 'dotemacs-helm-buffers-do-ack))

(defun dotemacs-helm-buffers-do-pt (&optional _)
  "Search in opened buffers with `pt'."
  (interactive)
  (let ((helm-ag-base-command "pt -e --nocolor --nogroup"))
    (helm-do-ag-buffers)))

(defun dotemacs-helm-buffers-do-pt-region-or-symbol ()
  "Search in opened buffers with `pt' using a default input."
  (interactive)
  (dotemacs-helm-do-ag-region-or-symbol 'dotemacs-helm-buffers-do-pt))

(defun dotemacs-helm-buffers-smart-do-search (&optional default-inputp)
  "Search in opened buffers using `dotemacs-search-tools'.
Search for a search tool in the order provided by `dotemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
  (interactive)
  (call-interactively
   (dotemacs-helm-do-search-find-tool "helm-buffers-do"
                                      dotemacs-search-tools
                                      default-inputp)))

(defun dotemacs-helm-buffers-smart-do-search-region-or-symbol ()
  "Search in opened buffers using `dotemacs-search-tools' with
default input.
Search for a search tool in the order provided by `dotemacs-search-tools'."
  (interactive)
  (dotemacs-helm-buffers-smart-do-search t))

;; Search in project ---------------------------------------------------

(defun dotemacs-helm-project-do-ag ()
  "Search in current project with `ag'."
  (interactive)
  (let ((dir (projectile-project-root)))
    (if dir
        (helm-do-ag dir)
      (message "error: Not in a project."))))

(defun dotemacs-helm-project-do-ag-region-or-symbol ()
  "Search in current project with `ag' using a default input."
  (interactive)
  (let ((dir (projectile-project-root)))
    (if dir
        (dotemacs-helm-do-ag-region-or-symbol 'helm-do-ag dir)
      (message "error: Not in a project."))))

(defun dotemacs-helm-project-do-ack ()
  "Search in current project with `ack'."
  (interactive)
  (let ((dir (projectile-project-root)))
    (if dir
        (dotemacs-helm-files-do-ack dir)
      (message "error: Not in a project."))))

(defun dotemacs-helm-project-do-ack-region-or-symbol ()
  "Search in current project with `ack' using a default input."
  (interactive)
  (let ((dir (projectile-project-root)))
    (if dir
        (dotemacs-helm-do-ag-region-or-symbol 'dotemacs-helm-files-do-ack dir)
      (message "error: Not in a project."))))

(defun dotemacs-helm-project-do-pt ()
  "Search in current project with `pt'."
  (interactive)
  (let ((dir (projectile-project-root)))
    (if dir
        (dotemacs-helm-files-do-pt dir)
      (message "error: Not in a project."))))

(defun dotemacs-helm-project-do-pt-region-or-symbol ()
  "Search in current project with `pt' using a default input."
  (interactive)
  (let ((dir (projectile-project-root)))
    (if dir
        (dotemacs-helm-do-ag-region-or-symbol 'dotemacs-helm-files-do-pt dir)
      (message "error: Not in a project."))))

(defun dotemacs-helm-project-smart-do-search (&optional default-inputp)
  "Search in current project using `dotemacs-search-tools'.
Search for a search tool in the order provided by `dotemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
  (interactive)
  (let ((projectile-require-project-root nil))
   (call-interactively
    (dotemacs-helm-do-search-find-tool "helm-project-do"
                                       dotemacs-search-tools
                                       default-inputp))))

(defun dotemacs-helm-project-smart-do-search-region-or-symbol ()
  "Search in current project using `dotemacs-search-tools' with
 default input.
Search for a search tool in the order provided by `dotemacs-search-tools'."
  (interactive)
  (dotemacs-helm-project-smart-do-search t))

;;; Debugging
;;
;;
(defun helm-debug-toggle ()
  (interactive)
  (setq helm-debug (not helm-debug))
  (message "Helm Debug is now %s"
           (if helm-debug "Enabled" "Disabled")))

(defun helm-ff-candidates-lisp-p (candidate)
  (cl-loop for cand in (helm-marked-candidates)
           always (string-match "\.el$" cand)))

;; hide minibuffer in Helm session, since we use the header line already
(defun helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(defun helm-toggle-header-line ()
 "Hide the `helm' header is there is only one source."
 (when helm-no-header
   (if (> (length helm-sources) 1)
       (set-face-attribute 'helm-source-header
                           nil
                           :foreground helm-source-header-default-foreground
                           :background helm-source-header-default-background
                           :box helm-source-header-default-box
                           :height helm-source-header-default-height)
     (set-face-attribute 'helm-source-header
                         nil
                         :foreground (face-attribute 'helm-selection :background)
                         :background (face-attribute 'helm-selection :background)
                         :box nil
                         :height 0.1))))

(defun dotemacs-helm-multi-files ()
  "Runs `helm-multi-files`, but first primes the `helm-ls-git` file lists."
  (interactive)
  (dotemacs-helm-ls-git-ls)
  (helm-multi-files))

(defun dotemacs-helm-ls-git-ls ()
  (interactive)
  (when (not (helm-ls-git-not-inside-git-repo))
    (unless (and helm-source-ls-git
                 helm-source-ls-git-buffers)
      (setq helm-source-ls-git (helm-make-source "Git files" 'helm-ls-git-source
                                                 :fuzzy-match helm-ls-git-fuzzy-match)
            helm-source-ls-git-buffers (helm-make-source "Buffers in project" 'helm-source-buffers
                                                         :header-name #'helm-ls-git-header-name
                                                         :buffer-list (lambda () (helm-browse-project-get-buffers
                                                                                   (helm-ls-git-root-dir))))))))

(defun dotemacs//hide-cursor-in-helm-buffer ()
  "Hide the cursor in helm buffers."
  (with-helm-buffer
    (setq cursor-in-non-selected-windows nil)))

(defun dotemacs/helm-faces ()
  "Describe face."
  (interactive)
  (require 'helm-elisp)
  (let ((default (or (face-at-point) (thing-at-point 'symbol))))
    (helm :sources (helm-def-source--emacs-faces
                    (format "%s" (or default "default")))
          :buffer "*helm faces*")))

(defun dotemacs-jump-in-buffer ()
  (interactive)
  (cond
   ((eq major-mode 'org-mode)
    (call-interactively 'helm-org-in-buffer-headings))
   (t
    (call-interactively 'helm-semantic-or-imenu))))

(use-package helm-flx
  :ensure t
  :defer t)

(dotemacs-use-package-add-hook helm
  :pre-config
  (progn
    ;; Disable for helm-find-files until performance issues are sorted
    ;; https://github.com/PythonNut/helm-flx/issues/9
    (setq helm-flx-for-helm-find-files nil)
    (helm-flx-mode)))

;; Helm: Unite/CtrlP style fuzzy file/buffer/anything searcher on steroids
;;
;; Helm does the same thing as Unite/CtrlP on Vim and does it really well. You
;; can also enable Helm to manage the command buffer, which is pretty awesome
;; with: (helm-mode 1)
(use-package helm
  :ensure t
  :defer 1
  :commands dotemacs-helm-find-files
  :init
  (progn
    (with-eval-after-load 'helm-config
                          (warn "`helm-config' loaded! Get rid of it ASAP!"))

    ;; NOTE: Apple OS X users also need a version of grep that accepts --exclude-dir
    ;; brew tap homebrew/dupes
    ;; brew install homebrew/dupes/grep
    (when-let (gnu-grep (and (eq system-type 'darwin)
                             (executable-find "ggrep")))
              (setq helm-grep-default gnu-grep)
              (setq helm-grep-default-command (concat gnu-grep " --color=never -a -d skip %e -n%cH -e %p %f"))
              (setq helm-grep-default-recurse-command (concat gnu-grep " --color=never -a -d recurse %e -n%cH -e %p %f")))

    ;; https://github.com/syl20bnr/spacemacs/issues/1544
    ;; Vim users are used to CtrlP plugin.
    (setq helm-for-files-preferred-list '(helm-source-buffers-list
                                          helm-source-buffer-not-found
                                          helm-source-ls-git
                                          helm-source-ls-git-buffers
                                          helm-source-projectile-projects
                                          helm-source-projectile-files-list
                                          helm-source-recentf
                                          helm-source-bookmarks
                                          helm-source-file-cache
                                          helm-source-files-in-current-dir))
    ;; (define-key evil-normal-state-map (kbd "C-p") #'dotemacs-helm-multi-files)

    (setq helm-prevent-escaping-from-minibuffer t
          helm-bookmark-show-location t
          helm-display-header-line nil
          helm-split-window-in-side-p t
          helm-always-two-windows t
          helm-echo-input-in-header-line t
          helm-imenu-execute-action-at-once-if-one nil
          helm-org-format-outline-path t)

    ;; hide minibuffer in Helm session, since we use the header line already
    (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

    ;; fuzzy matching setting
    (setq helm-M-x-fuzzy-match t
          helm-apropos-fuzzy-match t
          helm-file-cache-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-lisp-fuzzy-completion t
          helm-recentf-fuzzy-match t
          helm-semantic-fuzzy-match t
          helm-buffers-fuzzy-matching t)

    ;; Use helm to provide :ls, unless ibuffer is used
    ;; (unless 'ibuffer
    ;;   (evil-ex-define-cmd "buffers" 'helm-buffers-list))

    ;; use helm by default for M-x
    (global-set-key (kbd "M-x") 'helm-M-x)

    (dotemacs-set-leader-keys
      "<f1>" 'helm-apropos
      "bb"   'helm-mini
      "Cl"   'helm-colors
      "ff"   'dotemacs-helm-find-files
      "fF"   'helm-find-files
      "fL"   'helm-locate
      "fm"   'dotemacs-helm-multi-files
      "fr"   'helm-recentf
      "hb"   'helm-filtered-bookmarks
      "hdF"  'dotemacs/helm-faces
      "hi"   'helm-info-at-point
      "hl"   'helm-resume
      "hm"   'helm-man-woman
      "iu"   'helm-ucs
      "ry"   'helm-show-kill-ring
      "rr"   'helm-register
      "rm"   'helm-all-mark-rings
      "sL"   'dotemacs-last-search-buffer
      "sl"   'dotemacs-jump-in-buffer)

    ;; search with grep
    (dotemacs-set-leader-keys
      "sgb"  'dotemacs-helm-buffers-do-grep
      "sgB"  'dotemacs-helm-buffers-do-grep-region-or-symbol
      "sgf"  'dotemacs-helm-files-do-grep
      "sgF"  'dotemacs-helm-files-do-grep-region-or-symbol
      "sgg"  'dotemacs-helm-file-do-grep
      "sgG"  'dotemacs-helm-file-do-grep-region-or-symbol)

    ;; define the key binding at the very end in order to allow the user
    ;; to overwrite any key binding
    (add-hook 'emacs-startup-hook
              (lambda ()
                (dotemacs-set-leader-keys dotemacs-command-key 'helm-M-x)))

    ;; Hide the cursor in helm buffers.
    (add-hook 'helm-after-initialize-hook 'dotemacs//hide-cursor-in-helm-buffer)

    ;; this or any specialized case of Helm buffer must be added AFTER
    ;; `dotemacs-helm-display-buffer-regexp'. Otherwise,
    ;; `dotemacs-helm-display-buffer-regexp' will be used before
    ;; `dotemacs-helm-display-help-buffer-regexp' and display
    ;; configuration for normal Helm buffer is applied for helm help
    ;; buffer, making the help buffer unable to be displayed.
    (setq helm-display-function 'dotemacs-display-helm-window)

    ;; Prepare necessary settings to make Helm display properly.
    (add-hook 'helm-after-initialize-hook 'dotemacs-helm-prepare-display)

    ;;  Restore popwin-mode after a Helm session finishes.
    (add-hook 'helm-cleanup-hook 'dotemacs-restore-previous-display-config)

    ;; Add minibuffer history with `helm-minibuffer-history'
    (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

    ;; Cleanup some helm related states when quitting.
    (add-hook 'helm-cleanup-hook 'dotemacs-helm-cleanup)

    (defface dotemacs-helm-navigation-ms-face
      `((t :background ,(face-attribute 'error :foreground) :foreground "black"))
      "Face for helm heder when helm micro-state is activated."
      :group 'dotemacs))
  :config
  (progn
    (helm-mode +1)

    (when (and helm-resize
               (or (eq dotemacs-helm-position 'bottom)
                   (eq dotemacs-helm-position 'top)))
      (setq helm-autoresize-min-height 10)
      (helm-autoresize-mode 1))

    ;; from https://www.reddit.com/r/emacs/comments/2z7nbv/lean_helm_window/
    (defvar helm-source-header-default-background
      (face-attribute 'helm-source-header :background))
    (defvar helm-source-header-default-foreground
      (face-attribute 'helm-source-header :foreground))
    (defvar helm-source-header-default-box
      (face-attribute 'helm-source-header :box))
    (defvar helm-source-header-default-height
      (face-attribute 'helm-source-header :height) )

    ;; Hide the `helm' header is there is only one source.
    (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)

    ;; helm-locate uses es (from everything on windows, which doesnt like fuzzy)
    (helm-locate-set-command)
    (setq helm-locate-fuzzy-match (string-match "locate" helm-locate-command))

    ;; Set the face of diretories for `.' and `..'
    (add-hook 'helm-find-files-before-init-hook 'dotemacs-set-dotted-directory)

    ;; alter helm-bookmark key bindings to be simpler
    (add-hook 'helm-mode-hook 'simpler-helm-bookmark-keybindings)

    ;; helm navigation on hjkl
    (dotemacs-helm-hjkl-navigation (member dotemacs-editing-style '(vim hybrid)))

    ;; Define functions to pick actions
    (dotimes (n 10)
      (let ((func (intern (format "dotemacs/helm-action-%d" n)))
            (doc (format "Select helm action #%d" n)))
        (eval `(defun ,func ()
                 ,doc
                 (intern)
                 (helm-select-nth-action ,(1- n))))))

    (dotemacs-define-micro-state helm-navigation
      :persistent t
      :disable-evil-leader t
      :define-key (helm-map . "M-SPC") (helm-map . "s-M-SPC")
      :on-enter (dotemacs-helm-navigation-ms-on-enter)
      :on-exit  (dotemacs-helm-navigation-ms-on-exit)
      :bindings
      ("1" dotemacs/helm-action-1 :exit t)
      ("2" dotemacs/helm-action-2 :exit t)
      ("3" dotemacs/helm-action-3 :exit t)
      ("4" dotemacs/helm-action-4 :exit t)
      ("5" dotemacs/helm-action-5 :exit t)
      ("6" dotemacs/helm-action-6 :exit t)
      ("7" dotemacs/helm-action-7 :exit t)
      ("8" dotemacs/helm-action-8 :exit t)
      ("9" dotemacs/helm-action-9 :exit t)
      ("0" dotemacs/helm-action-10 :exit t)
      ("<tab>" helm-select-action :exit t)
      ("C-i" helm-select-action :exit t)
      ("<RET>" helm-maybe-exit-minibuffer :exit t)
      ("?" nil :doc (dotemacs-helm-navigation-ms-full-doc))
      ("a" helm-select-action :post (dotemacs-helm-navigation-ms-set-face))
      ("e" dotemacs-helm-edit)
      ("g" helm-beginning-of-buffer)
      ("G" helm-end-of-buffer)
      ("h" helm-previous-source)
      ("j" helm-next-line)
      ("k" helm-previous-line)
      ("l" helm-next-source)
      ("q" nil :exit t)
      ("t" helm-toggle-visible-mark)
      ("T" helm-toggle-all-marks)
      ("v" helm-execute-persistent-action))

    ;; Swap default TAB and C-z commands.
    ;; For GUI.
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-find-files-map
      (kbd "S-<tab>") 'helm-find-files-up-one-level)
    (define-key helm-find-files-map
      (kbd "<backtab>") 'helm-find-files-up-one-level)
    ;; For terminal.
    (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
    (define-key helm-find-files-map
      (kbd "S-TAB") 'helm-find-files-up-one-level)
    (define-key helm-map (kbd "C-z") 'helm-select-action)

    (with-eval-after-load 'helm-mode ; required
      (dotemacs-hide-lighter helm-mode))))

(use-package helm-ls-git
  :defer t
  :ensure t
  :init
  (progn
    (require 'helm-ls-git))
  :config
  (progn
    (setq helm-ls-git-show-abs-or-relative 'relative) ))

(use-package helm-swoop
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'helm
    (setq helm-swoop-split-with-multiple-windows t
          helm-swoop-use-line-number-face t
          helm-swoop-split-direction 'split-window-vertically
          helm-swoop-speed-or-color t
          helm-swoop-split-window-function 'helm-default-display-buffer
          helm-swoop-pre-input-function (lambda () ""))

    (defun dotemacs-helm-swoop-region-or-symbol ()
      "Call `helm-swoop' with default input."
      (interactive)
      (let ((helm-swoop-pre-input-function
             (lambda ()
               (if (region-active-p)
                   (buffer-substring-no-properties (region-beginning)
                                                   (region-end))
                 (let ((thing (thing-at-point 'symbol t)))
                   (if thing thing ""))))))
        (call-interactively 'helm-swoop)))

    (dotemacs-set-leader-keys
      "ss"    'helm-swoop
      "sS"    'dotemacs-helm-swoop-region-or-symbol
      "s C-s" 'helm-multi-swoop-all)

    (defadvice helm-swoop (before add-evil-jump activate)
      (evil-set-jump))))

(use-package helm-misc                  ; Misc helm commands
  :ensure helm
  :bind (([remap switch-to-buffer] . helm-mini)))

(use-package helm-themes
  :ensure helm
  :defer t
  :init
  (dotemacs-set-leader-keys
    "Th" 'helm-themes))

(use-package helm-command               ; M-x in Helm
  :ensure helm
  :bind (([remap execute-extended-command] . helm-M-x)))

(use-package helm-eval                  ; Evaluate expressions with Helm
  :ensure helm
  :bind (("C-c h M-:" . helm-eval-expression-with-eldoc)
         ("C-c h *"   . helm-calcul-expression)))

(use-package helm-color                 ; Input colors with Helm
  :ensure helm
  :bind (("C-c h c" . helm-colors)))

(use-package helm-unicode               ; Unicode input with Helm
  :ensure t
  :bind ("C-c h 8" . helm-unicode))

(use-package helm-mode-manager
  :ensure t
  :defer t
  :init
  (dotemacs-set-leader-keys
    "hM"    'helm-switch-major-mode
    ;; "hm"    'helm-disable-minor-mode
    "h C-m" 'helm-enable-minor-mode))

(provide 'module-helm)
;;; module-helm.el ends here
