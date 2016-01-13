;;; package --- Summary
;;; Commentary:
;;; Code:
;; (require 'flyspell)

(defun dotemacs/add-flyspell-hook (mode &optional target)
  "Enable flyspell for the given MODE, if
`spell-checking-enable-by-default' is true."
  (when spell-checking-enable-by-default
    (let ((mode-hook (intern (format "%S-hook" mode))))
      (add-hook mode-hook 'flyspell-mode))))

(provide 'init-spell-checking)
;;; init-spell-checking.el ends here
