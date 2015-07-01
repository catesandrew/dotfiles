;; Insert key for org-mode and markdown a la C-h k
;; from SE endless http://emacs.stackexchange.com/questions/2206/i-want-to-have-the-kbd-tags-for-my-blog-written-in-org-mode/2208#2208
(defun dotemacs-insert-keybinding-markdown (key)
  "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
  (interactive "kType key sequence: ")
  (let* ((tag "~%s~"))
    (if (null (equal key "\r"))
        (insert
         (format tag (help-key-description key nil)))
      (insert (format tag ""))
      (forward-char -6))))

(provide 'init-markdown)
