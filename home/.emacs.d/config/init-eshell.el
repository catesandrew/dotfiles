(defun dotemacs-current-git-branch ()
  (let ((branch (car (loop for match in (split-string (shell-command-to-string "git branch") "\n")
                           when (string-match "^\*" match)
                           collect match))))
    (if (not (eq branch nil))
        (concat " [" (substring branch 2) "]")
      "")))

(defun dotemacs-eshell-prompt ()
  (concat (propertize (abbreviate-file-name (eshell/pwd)) 'face 'eshell-prompt)
          (propertize (dotemacs-current-git-branch) 'face 'font-lock-function-name-face)
          (propertize " $ " 'face 'font-lock-constant-face)))

(defun eshell/j ()
  "Quickly jump to previous directories."
  (eshell/cd (ido-completing-read "Jump to directory: "
                                  (delete-dups (ring-elements eshell-last-dir-ring)))))

(setq dotemacs-eshell-buffer-count 0)
(defun dotemacs-new-eshell-split ()
  (interactive)
  (split-window)
  (setq dotemacs-eshell-buffer-count (+ 1 dotemacs-eshell-buffer-count))
  (eshell dotemacs-eshell-buffer-count))

(provide 'init-eshell)
