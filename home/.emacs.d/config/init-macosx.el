(require 'dash)

;; Treat option as meta and command as super
; (setq mac-option-key-is-meta t)
; (setq mac-command-key-is-meta nil)
; (setq mac-command-modifier 'super)
; (setq mac-option-modifier 'meta)

;; Treat command as meta and option as super
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; Keybindings
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-w") 'kill-this-buffer)
(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-Z") 'undo-tree-redo)

;; keybinding to toggle full screen mode
(global-set-key (quote [M-f10]) (quote ns-toggle-fullscreen))

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Don't open files from the workspace in a new frame
(setq ns-pop-up-frames nil)

;; Use aspell for spell checking: brew install aspell --lang=en
(setq ispell-program-name "/usr/local/bin/aspell")

;; Open files
(defun mac-open-current-file ()
  (interactive)
  (shell-command (concat "open " (buffer-file-name))))

(global-set-key (kbd "C-c C-S-o") 'mac-open-current-file)

(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)

(provide 'mac-osx)
