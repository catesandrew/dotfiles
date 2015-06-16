;; TODO to be deleted

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
