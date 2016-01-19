;;; package --- Summary
;;; Commentary:
;;; Code:
;; (require 'flyspell)

(defun spell-checking/add-flyspell-hook (mode &optional target)
  "Enable flyspell for the given MODE, if
`spell-checking-enable-by-default' is true."
  (when spell-checking-enable-by-default
    (let ((mode-hook (intern (format "%S-hook" mode))))
      (add-hook mode-hook 'flyspell-mode))))

(defun spell-checking/change-dictionary ()
  "Change the dictionary. Use the ispell version if
auto-dictionary is not used, use the adict version otherwise."
  (interactive)
  (if (fboundp 'addict-change-dictionary)
      (adict-change-dictionary)
    (call-interactively 'ispell-change-dictionary)))

(provide 'init-spell-checking)
;;; init-spell-checking.el ends here
