(defun dotemacs-js-ctrl-c-ctrl-c ()
  (interactive)
  (require 'thingatpt)
  (let ((val (thing-at-point 'list)))
    ;; inside parameter list?
    (when (and (equal (substring val 0 1) "(")
               (equal (substring val -1) ")"))
      (if (string-match-p "," val)
          (my-macro-ng-add-string-for-last-arg)
        (my-macro-ng-function-to-array-injected)))))

(defun dotemacs-hide-test-functions ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ignore-errors
      (while (re-search-forward "\"[^\"]+\": function (")
        (js2-mode-hide-element)))))

;; js2-mode steals TAB, let's steal it back for yasnippet
(defun dotemacs-tab-properly ()
  (interactive)
  (let ((yas-fallback-behavior 'return-nil))
    (unless (yas-expand)
      (indent-for-tab-command)
      (if (looking-back "^\s*")
          (back-to-indentation)))))

(defun my-aget (key map)
  (cdr (assoc key map)))

(defun dotemacs-fetch-autolint-externs (file)
  (let* ((settings (with-temp-buffer
                     (insert-file-literally file)
                     (javascript-mode)
                     (let (kill-ring kill-ring-yank-pointer) (kill-comment 1000))
                     (->> (buffer-substring (point-min) (point-max))
                       (s-trim)
                       (s-chop-prefix "module.exports = ")
                       (s-chop-suffix ";")
                       (json-read-from-string))))
         (predef (->> settings
                   (my-aget 'linterOptions)
                   (my-aget 'predef))))
    (--each (append predef nil)
      (add-to-list 'js2-additional-externs it))))

(provide 'init-js)
