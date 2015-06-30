(defun dotemacs-web-mode-ms-doc ()
  (if (equal 0 dotemacs--web-mode-ms-doc-toggle)
      "[?] for help"
    "
  [?] display this help
  [k] previous [j] next   [K] previous sibling [J] next sibling
  [h] parent   [l] child  [c] clone [d] delete [D] kill [r] rename
  [w] wrap     [p] xpath
  [q] quit"))

(defun dotemacs-web-mode-ms-toggle-doc ()
  (interactive)
  (setq dotemacs--web-mode-ms-doc-toggle
        (logxor dotemacs--web-mode-ms-doc-toggle 1)))

(provide 'init-web)
