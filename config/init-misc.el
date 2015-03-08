(require 'undo-tree)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist
      `(("." . ,(concat dotemacs-cache-directory "undo"))))
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)
(global-undo-tree-mode)


(after 'evil
  (add-hook 'multiple-cursors-mode-enabled-hook #'evil-emacs-state)
  (add-hook 'multiple-cursors-mode-disabled-hook #'evil-normal-state))


(when (executable-find "ag")
  (setq ag-highlight-search t)
  (add-hook 'ag-mode-hook (lambda () (toggle-truncate-lines t))))


(after 'project-explorer
  (setq pe/cache-directory (concat dotemacs-cache-directory "project-explorer"))
  (setq pe/omit-regex (concat pe/omit-regex "\\|^node_modules$")))


(require 'editorconfig)


(require 'aggressive-indent)
(add-to-list 'aggressive-indent-excluded-modes #'stylus-mode)
(add-to-list 'aggressive-indent-excluded-modes #'org-mode)
(add-to-list 'aggressive-indent-excluded-modes #'vimrc-mode)
(global-aggressive-indent-mode)


(setq etags-select-go-if-unambiguous t)


(require 'windsize)
(setq windsize-cols 16)
(setq windsize-rows 8)
(windsize-default-keybindings)


(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


(require 'framemove)
(setq framemove-hook-into-windmove t)


(setq paradox-execute-asynchronously nil)


(when (eq system-type 'darwin)
  (require 'vkill))


;; fiplr ignore
(setq fiplr-root-markers '("Rakefile" "Makefile" "Jakefile.js" "bower.json" "package.json" "build.xml" ".git" ".svn"))
(setq fiplr-ignored-globs
      '((directories
          ;; Version control
          (".git"
           ".svn"
           ".hg"
           ".bzr"
           ;; intellij
           ".idea"
           ;; sass
           ".sass-cache"
           ;; NPM
           "node_modules"
           ;; Bower
           "bower_components"
           "components"
           ;; Maven
           "target"
           ;; Ruby
           "vendor"
           "vendor/rails"
           "vendor/gems"
           "vendor/plugins"
           ;; Other
           "assets"
           "build"
           "tmp"
           "log"
           ;; Python
           "__pycache__"))
        (files
          ;; Emacs
          (".#*"
           ;; Vim
           "*~"
           ;; Objects
           "*.so"
           "*.o"
           "*.obj"
           ;; Media
           "*.jpg"
           "*.jpeg"
           "*.bmp"
           "*.png"
           "*.gif"
           "*.pdf"
           ;; Other
           ".DS_Store"
           "*.elc"
           "*.pyc"
           "*.swp"
           "*.psd"
           "*.ai"
           "*.mov"
           "*.aep"
           ;; Archives
           "*.dmg"
           "*.gz"
           "*.zip"))))

;; make sure $PATH is set correctly
(if (eq system-type 'windows-nt)
    (dolist (path (split-string (getenv "PATH") ";"))
      (add-to-list 'exec-path (replace-regexp-in-string "\\\\" "/" path)))
  (progn
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)))


;; http://emacs.stackexchange.com/questions/7308/define-key-to-toggle-between-javascript-implementation-and-test-file
(defun js-jump-to (current from to format-name)
  (find-file
   (cl-loop with parts = (reverse current)
            with fname = (file-name-sans-extension (cl-first parts))
            for (name . rest) on (cl-rest parts)
            until (string-equal name from)
            collect name into names
            finally (cl-return
                     (mapconcat 'identity
                                (nconc (reverse rest)
                                       (list to)
                                       (reverse names)
                                       (list (funcall format-name fname) )) "/" )))))

(defun js-format-impl-name (fname)
  (format "%s.js" (replace-regexp-in-string "Spec" "" fname)))

(defun js-format-test-name (fname)
  (format "%sSpec.js" fname))

(defun js-jump-to-implementation-or-test ()
  (interactive)
  (let ((current (split-string (buffer-file-name) "/")))
    (cond
     ((member "test" current) (js-jump-to current "test" "lib" 'js-format-impl-name))
     ((member "lib" current)  (js-jump-to current "lib" "test" 'js-format-test-name))
     (t (error "not within a test or lib directory"))
     )))


; Sessions (:mksession in Vim)

; Emacs have the commands M-x desktop-save and desktop-read. To have it
; automatically saved/restored put into the .emacs: (desktop-save-mode 1). If
; you want to start emacs without auto loading the session (if you configured
; it), the command is emacs --no-desktop. But Emacs sessions doesn't know about
; elscreens (which evil-tabs use for creating Vim-like tabs) so if you want to
; save and restore full sessions use these functions:

;; Save session including tabs
;; http://stackoverflow.com/questions/22445670/save-and-restore-elscreen-tabs-and-split-frames
(defun session-save ()
    "Store the elscreen tab configuration."
    (interactive)
    (if (desktop-save user-emacs-directory)
        (with-temp-file (concat user-emacs-directory ".elscreen")
            (insert (prin1-to-string (elscreen-get-screen-to-name-alist))))))

;; Load session including tabs
(defun session-load ()
    "Restore the elscreen tab configuration."
    (interactive)
    (if (desktop-read)
        (let ((screens (reverse
                        (read
                         (with-temp-buffer
                          (insert-file-contents (concat user-emacs-directory ".elscreen"))
                          (buffer-string))))))
            (while screens
                (setq screen (car (car screens)))
                (setq buffers (split-string (cdr (car screens)) ":"))
                (if (eq screen 0)
                    (switch-to-buffer (car buffers))
                    (elscreen-find-and-goto-by-buffer (car buffers) t t))
                (while (cdr buffers)
                    (switch-to-buffer-other-window (car (cdr buffers)))
                    (setq buffers (cdr buffers)))
                (setq screens (cdr screens))))))

; Here, I define a "helm-my-buffers" function that when called will show Helm
; interface but searching (fuzzy, real time as you write, unordered) in open
; buffers, recent files, project files (see below for more on that), tags
; inside the files, tabs and results from the Linux command `locate` that
; searches quickly from a database of all the files in the file system.

; But this is only the tip of the iceberg of Helm power. There are sources for
; searching the symbols (functions, classes, globals, etc) in the current
; buffer (helm-imenu), bookmarks (including Chrome/Firefox bookmarks), HTML
; colors (showing the color, name, and hexadecimal code), apt packages and
; more.

; If you check the sources of the helm-my-buffers function above you can see
; that I'm also using helm-c-source-projectile-files-list. This will use
; another installable third party package called `Projectile` that will search
; for a git/hg/svn file in the current directory and its parents and extract
; the current project files. Linking it will Helm makes it super easy to open
; any file in your current project (providing you've it under version control)
; without having the browse the filesystem, even for files that you have never
; opened (and thus are not in Emacs' recent files list).

(defun helm-my-buffers ()
  (interactive)
  (require 'helm-files)
  (let ((helm-ff-transformer-show-only-basename nil))
  (helm-other-buffer '(helm-c-source-buffers-list
                       helm-c-source-elscreen
                       helm-c-source-occur
;;                        helm-c-source-projectile-files-list
                       helm-c-source-ctags
                       helm-c-source-recentf
                       helm-c-source-locate)
                     "*helm-my-buffers*")))

(provide 'init-misc)
