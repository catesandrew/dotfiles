;;; module-common.el --- Common functions and utilities

;; This file is NOT part of GNU Emacs.

;;; License:

;;; Commentary:
;;
;; This can never require `module-utils` or `module-core`

(require 'module-vars)

;;; Code:

(defun current//buffer-remote-p ()
  (--any? (and it (file-remote-p it))
          (list
           (buffer-file-name)
           list-buffers-directory
           default-directory)))

(defun flycheck//turn-on-maybe ()
  (unless (or buffer-read-only
              (hardhat-buffer-included-p (current-buffer))
              (current//buffer-remote-p))
    (flycheck-mode)))

(defun dotemacs/add-flycheck-hook (mode &optional target)
  "Enable flycheck for the given MODE, if
`syntax-checking-enable-by-default' is true."
  (when syntax-checking-enable-by-default
    (let ((mode-hook (intern (format "%S-hook" mode))))
      ;; (add-hook mode-hook 'flycheck//turn-on-maybe)
      (add-hook mode-hook 'flycheck-mode))))

(defun spell-checking/add-flyspell-hook (mode &optional target)
  "Enable flyspell for the given MODE, if
`spell-checking-enable-by-default' is true."
  (when spell-checking-enable-by-default
    (let ((mode-hook (intern (format "%S-hook" mode))))
      (add-hook mode-hook 'flyspell-mode))))

(defun dotemacs-load-yasnippet ()
  (unless yas-global-mode
    (progn
      (yas-global-mode 1)
      (let ((private-yas-dir (if dotemacs-ac-private-snippets-directory
                                 dotemacs-ac-private-snippets-directory
                               (concat dotemacs-private-dir "/snippets/")))
            (dotemacs-snippets-dir
             (expand-file-name
              (concat user-emacs-directory "snippets/"))))
        (setq yas-snippet-dirs
              (append (list private-yas-dir)
                      (when (boundp 'yas-snippet-dirs)
                        yas-snippet-dirs)
                      dotemacs-snippets-dir))
        (yas-load-directory dotemacs-snippets-dir t)
        (yas-load-directory private-yas-dir t)
        (setq yas-wrap-around-region t))))
  (yas-minor-mode 1))


(provide 'module-common)
;;; module-common.el ends here
