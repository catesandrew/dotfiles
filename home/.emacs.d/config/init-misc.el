;; TODO to be deleted
;; Fix minibuffer behaviour

;; When changing focus to the minibuffer, stop allowing point to move
;; over the prompt. Code taken from ergoemacs.

(setq minibuffer-prompt-properties (add-to-list 'minibuffer-prompt-properties 'minibuffer-avoid-prompt))
(setq minibuffer-prompt-properties (add-to-list 'minibuffer-prompt-properties 'point-entered))

;; On OS X, make sure M-3 is remapped to hash:

(when (eq system-type 'darwin)
  (fset 'insert-pound "#")
  (define-key global-map "\M-3" #'insert-pound))


;; Tooltips

;; Emacs convention is to show help and other inline documentation in
;; the message area. Show help there instead of using an OS tooltip:

(when (display-graphic-p)
  (tooltip-mode -1))

;; Dialogue boxes and windows

;; Just don’t show them. Use native Emacs controls:

(when (display-graphic-p)
  (setq use-dialog-box nil))

;; Cursor

;; On modern operating systems, a vertical bar is used as a cursor:

(when (display-graphic-p)
  (setq-default cursor-type 'bar))

;; sRGB display fixes

;; As of Emacs 24.4, Emacs natively supports proper sRGB values on OS X:

;; If you’re not using Emacs 24.4 this variable setting will have no
;; effect. See Homebrew’s Emacs recipe for details of how to get this
;; behaviour in earlier Emacs versions.

(setq ns-use-srgb-colorspace t)

;; Terminal integration

;; Using this configuration, Emacs runs best in iTerm2.

;; On the desktop, Emacs integrates with the OS X clipboard, so kill
;; etc. copy to the clipboard, and yank copies from the clipboard.

;; Obviously this doesn’t work in the terminal, so we need to use the
;; interprogram-(cut|paste)-function variables to copy/paste. Most of
;; this code gotten from this blog comment.

(when (and (not (display-graphic-p)) (eq system-type 'darwin))
  (defun my/copy-from-osx ()
    "Copies the current clipboard content using the `pbcopy` command"
    (shell-command-to-string "pbpaste"))

  (defun my/paste-to-osx (text &optional push)
    "Copies the top of the kill ring stack to the OSX clipboard"
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'my/paste-to-osx)
  (setq interprogram-paste-function 'my/copy-from-osx))

;; Get keychain password

;; If I’m on OS X, I can fetch passwords etc. from my Keychain. This
;; is much more secure than storing them in configuration on disk:

(defun my/chomp (str)
  "Chomp leading and tailing whitespace from `str'."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str))) str)

(defun my/get-keychain-password (account-name)
  "Get `account-name' keychain password from OS X Keychain"
  (interactive "sAccount name: ")
  (when (executable-find "security")
    (my/chomp
     (shell-command-to-string
      (concat
       "security find-generic-password -wa "
       account-name)))))

;; VC-mode integration

;; Since I use Magit I don’t need to use Emacs’s native vc-mode:

(delete 'Git vc-handled-backends)


(with-eval-after-load 'evil
  (add-hook 'multiple-cursors-mode-enabled-hook #'evil-emacs-state)
  (add-hook 'multiple-cursors-mode-disabled-hook #'evil-normal-state))


(when (executable-find "ag")
  (setq ag-highlight-search t)
  (add-hook 'ag-mode-hook (lambda () (toggle-truncate-lines t))))


(with-eval-after-load 'project-explorer
  (setq pe/cache-directory (concat dotemacs-cache-directory "project-explorer"))
  (setq pe/omit-regex (concat pe/omit-regex "\\|^node_modules$")))


(require 'editorconfig)


(setq etags-select-go-if-unambiguous t)


(require 'windsize)
(setq windsize-cols 16)
(setq windsize-rows 8)
(windsize-default-keybindings)


(require 'framemove)
(setq framemove-hook-into-windmove t)


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


;; http://www.shallowsky.com/dotfiles/.emacs
;; For composing in emacs then pasting into a word processor,
;; this un-fills all the paragraphs (i.e. turns each paragraph
;; into one very long line) and removes any blank lines that
;; previously separated paragraphs.
;;
(defun wp-munge () "un-fill paragraphs and remove blank lines" (interactive)
  (let ((save-fill-column fill-column))
    (set-fill-column 1000000)
    (mark-whole-buffer)
    (fill-individual-paragraphs (point-min) (point-max))
    ;(delete-matching-lines "^$")
    (set-fill-column save-fill-column)
    ))

(defun wp-unmunge () "fill paragraphs and separate them with blank lines"
  (interactive)
  (mark-whole-buffer)
  (replace-regexp "\(.\)$" "\1\n")
  (fill-individual-paragraphs (point-min) (point-max))
  ;;(delete-matching-lines "^$")
  ;;(replace-regexp "^$" "\n")
  )

(defun unfill() "un-fill paragraphs" (interactive)
  (let ((save-fill-column fill-column))
    (set-fill-column 1000000)
    (mark-whole-buffer)
    (fill-individual-paragraphs (point-min) (point-max))
    (set-fill-column save-fill-column)
    ))

(provide 'init-misc)
