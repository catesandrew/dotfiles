;;; init-smartparens.el

;;; Commentary:

;; Personal smartparens configuration.
;;
;; Extends the default `smartparens-config' with key-bindings and
;; additional pairs.

;;; Code:

;; enable smartparens-mode in `eval-expression'
(defun dotemacs-conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (smartparens-mode)))

(defun dotemacs-smartparens-pair-newline (id action context)
  (save-excursion
    (newline)
    (indent-according-to-mode)))

(defun dotemacs-smartparens-pair-newline-and-indent (id action context)
  (dotemacs-smartparens-pair-newline id action context)
  (indent-according-to-mode))

(defun dotemacs-sp-kill-surrounding-sexp ()
  "Kill from beginning to end of sexp."
  (interactive)
  (kill-region (progn (sp-beginning-of-sexp)
                 (1- (point)))
    (progn (sp-end-of-sexp)
      (1+  (point)))))

(defun dotemacs-gfm-skip-asterisk (ms mb me)
  (save-excursion
    (goto-char mb)
    (save-match-data (looking-at "^\\* "))))

(defun dotemacs-org-skip-asterisk (ms mb me)
  (or (and (= (line-beginning-position) mb)
           (eq 32 (char-after (1+ mb))))
      (and (= (1+ (line-beginning-position)) me)
           (eq 32 (char-after me)))))

(defun dotemacs-after-symbol-p (_id action _context)
  (when (eq action 'insert)
    (save-excursion
      (backward-char 1)
      (looking-back "\\sw\\|\\s_\\|\\s'"))))

(defun dotemacs-php-handle-docstring (&rest _ignored)
  (-when-let (line (save-excursion
                     (forward-line)
                     (thing-at-point 'line)))
    (cond
     ((string-match-p "function" line)
      (save-excursion
        (insert "\n")
        (let ((args (save-excursion
                      (forward-line)
                      (my-php-get-function-args))))
          (--each args
            (insert (format "* @param %s\n" it)))))
      (insert "* "))
     ((string-match-p ".*class\\|interface" line)
      (save-excursion (insert "\n*\n* @author\n"))
      (insert "* ")))
    (let ((o (sp--get-active-overlay)))
      (indent-region (overlay-start o) (overlay-end o)))))

(provide 'init-smartparens)
