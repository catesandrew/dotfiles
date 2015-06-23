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

(provide 'init-smartparens)
