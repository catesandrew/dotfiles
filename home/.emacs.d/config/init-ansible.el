(setq ansible/ansible-filename-re
      "\\(site\.yml\\|roles/.+\.yml\\|group_vars/.+\\|host_vars/.+\\)")

(defun ansible/ansible-should-enable? ()
  (and (stringp buffer-file-name)
       (string-match ansible/ansible-filename-re buffer-file-name)))

(defun ansible/ansible-maybe-enable ()
  (when (ansible/ansible-should-enable?)
    (ansible 1)))

(defun ansible/ansible-doc-maybe-enable ()
  (when (ansible/ansible-should-enable?)
    (ansible-doc-mode 1)
    (evil-leader/set-key-for-mode 'yaml-mode
      "ma?" 'ansible-doc)))

(provide 'init-ansible)
