; dtrt-indent

; Automatically determine the indentation settings used on the file that you're
; currently editting and adapt Emacs's settings to them. It's great when you're
; editing external files not created by you or that for some reason follow
; different indentation rules that the ones you've in your config file.

(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; To start the scratch buffer in text mode you will want to initial-major-mode variable
(setq initial-major-mode 'text-mode)

;; For setting of auto-fill when you start a specific major-mode you'll want to add an event to the mode hook
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; make side by side buffers function the same as the main window
(setq truncate-partial-width-windows nil)

;; disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files


;; pcomplete
(setq pcomplete-ignore-case t)


;; imenu
(setq-default imenu-auto-rescan t)


;; comint
(with-eval-after-load 'comint
  (defun my-toggle-comint-scroll-to-bottom-on-output ()
    (interactive)
    (if comint-scroll-to-bottom-on-output
        (setq comint-scroll-to-bottom-on-output nil)
      (setq comint-scroll-to-bottom-on-output t))))

; Vim's Marks => Evil's Marks + Emacs' Bookmarks

; Evil has marks just like Vim: m to jump to a mark, m-letter to set a mark,
; m-uppercase_letter to set a mark that works between buffers. But while marks
; are pretty useful for example to quickly jump between two or three positions
; inside some files when you're coding, Emacs also has the concept of
; "bookmarks" that are like inter-file marks that you can set with a name
; (instead of a letter) and that with the elisp bit below in your config file
; can be saved between sessions. I'm using helm-bookmarks to see and set them.
; To delete bookmarks, press TAB inside the helm sub-window to see the list of
; actions and choose "Delete Bookmark(s)".

;; fringe
(when (display-graphic-p)
  (fringe-mode 16))


;; re-builder
(setq reb-re-syntax 'string) ;; fix backslash madness


;; clean up old buffers periodically
(require 'midnight)
(midnight-delay-set 'midnight-delay 0)


; spaces instead of tabs
(setq-default c-basic-offset 2 c-default-style "bsd")

;; some more interface-related settings
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq read-file-name-completion-ignore-case t)
(setq mouse-yank-at-point t)


(xterm-mouse-mode t)
(which-function-mode t)
(random t) ;; seed


(setenv "EDITOR" "emacsclient")


(defun my-find-file-check-large-file ()
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (when (fboundp #'undo-tree-mode)
      (undo-tree-mode -1))
    (fundamental-mode)))


(add-hook 'find-file-hook (lambda ()
                            (unless (eq major-mode 'org-mode)
                              (setq show-trailing-whitespace t))))
(add-hook 'find-file-hook #'visual-line-mode)
(add-hook 'find-file-hook #'my-find-file-check-large-file)


(defun my-wrap-with (s)
  "Create a wrapper function for smartparens using S."
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))


(provide 'init-core)
