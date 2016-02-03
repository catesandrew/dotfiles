;;; module-diminish.el --- Diminish Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'core-display-init)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(use-package diminish
  :ensure t
  :init
  (progn
    ;; Minor modes abbrev --------------------------------------------------------
    (dotemacs|do-after-display-system-init
     (when (display-graphic-p)
       (with-eval-after-load 'eproject
         (diminish 'eproject-mode " eⓅ"))
       (with-eval-after-load 'flymake
         (diminish 'flymake-mode " Ⓕ2"))))
    ;; Minor Mode (hidden) ------------------------------------------------------
    (with-eval-after-load 'elisp-slime-nav
      (diminish 'elisp-slime-nav-mode))
    (with-eval-after-load 'hi-lock
      (diminish 'hi-lock-mode))
    (with-eval-after-load 'abbrev
      (diminish 'abbrev-mode))
    (with-eval-after-load 'subword
      (when (eval-when-compile (version< "24.3.1" emacs-version))
        (diminish 'subword-mode)))))

(provide 'module-diminish)
;;; module-diminish.el ends here
