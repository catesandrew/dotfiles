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


;; make sure $PATH is set correctly
(if (eq system-type 'windows-nt)
    (dolist (path (split-string (getenv "PATH") ";"))
      (add-to-list 'exec-path (replace-regexp-in-string "\\\\" "/" path)))
  (progn
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)))




;; Save session including tabs
;; http://stackoverflow.com/questions/22445670/save-and-restore-elscreen-tabs-and-split-frames
(defun session-save ()
    "Store the elscreen tab configuration."
    (interactive)
    (if (desktop-save emacs-configuration-directory)
        (with-temp-file elscreen-tab-configuration-store-filename
            (insert (prin1-to-string (elscreen-get-screen-to-name-alist))))))

;; Load session including tabs
(defun session-load ()
    "Restore the elscreen tab configuration."
    (interactive)
    (if (desktop-read)
        (let ((screens (reverse
                        (read
                         (with-temp-buffer
                          (insert-file-contents elscreen-tab-configuration-store-filename)
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

(provide 'init-misc)
