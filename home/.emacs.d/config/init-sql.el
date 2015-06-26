(defvar sql-pre-extensions
  '(
    ;; pre extension sqls go here
    )
  "List of all extensions to load before the packages.")

(defvar sql-post-extensions
  '(
    ;; post extension sqls go here
    )
  "List of all extensions to load after the packages.")

(defvar sql-packages '(sql sql-indent))

(defvar sql-excluded-packages '())

(defun dotemacs-sql-source (products)
  "return a source for helm selection"
  `((name . "SQL Products")
    (candidates . ,(mapcar (lambda (product)
                             (cons (sql-get-product-feature (car product) :name)
                                   (car product)))
                           products))
    (action . (lambda (candidate) (helm-marked-candidates)))))

(defun dotemacs-sql-highlight ()
  "set SQL dialect-specific highlighting"
  (interactive)
  (let ((product (car (helm
                       :sources (list (dotemacs-sql-source dotemacs-sql-highlightable))))))
    (sql-set-product product)))

(defun dotemacs-sql-start ()
  "set SQL dialect-specific highlighting and start inferior SQLi process"
  (interactive)
  (let ((product (car (helm
                       :sources (list (dotemacs-sql-source dotemacs-sql-startable))))))
    (sql-set-product product)
    (sql-product-interactive product)))

(defun dotemacs-sql-send-string-and-focus ()
  "Send a string to SQLi and switch to SQLi in `insert state'."
  (interactive)
  (let ((sql-pop-to-buffer-after-send-region t))
    (call-interactively 'sql-send-string)
    (evil-insert-state)))

(defun dotemacs-sql-send-buffer-and-focus ()
  "Send the buffer to SQLi and switch to SQLi in `insert state'."
  (interactive)
  (let ((sql-pop-to-buffer-after-send-region t))
    (sql-send-buffer)
    (evil-insert-state)))

(defun dotemacs-sql-send-paragraph-and-focus ()
  "Send the paragraph to SQLi and switch to SQLi in `insert state'."
  (interactive)
  (let ((sql-pop-to-buffer-after-send-region t))
    (sql-send-paragraph)
    (evil-insert-state)))

(defun dotemacs-sql-send-region-and-focus (start end)
  "Send region to SQLi and switch to SQLi in `insert state'."
  (interactive "r")
  (let ((sql-pop-to-buffer-after-send-region t))
    (sql-send-region start end)
    (evil-insert-state)))

(provide 'init-sql)
