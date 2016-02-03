;;; module-ignoramus.el --- Ignoramus Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-vars)
;; (require 'core-funcs)
;; (require 'core-keybindings)
;; (require 'core-display-init)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(use-package ignoramus                  ; Ignore uninteresting files everywhere
  :ensure t
  :config
  (progn
    (defun dotemacs//ignoramus-setup (fcn)
      (funcall fcn)
      ;; neotree
      (setq neo-hidden-regexp-list (list ignoramus-boring-file-regexp))

      ;; helm-grep
      (with-eval-after-load 'helm-grep
        (setq helm-grep-ignored-files (cons ".#*" (delq nil (mapcar #'(lambda (pat)
                                                                        (concat "*" pat)) ignoramus-file-basename-endings))))
        (setq helm-grep-ignored-directories ignoramus-file-basename-exact-names))

      ;; according to what projectile expects
      ;; (setq projectile-globally-ignored-file-extensions (mapcar #'(lambda (ext)
      ;;                                                               (replace-regexp-in-string "\\`\\." "" ext))
      ;;                                                           ignoramus-file-basename-endings))
      (setq projectile-ignored-file-extensions (mapcar #'(lambda (ext)
                                                           (replace-regexp-in-string "\\`\\." "" ext))
                                                       ignoramus-file-basename-endings))
      ;; (setq projectile-globally-ignored-files ignoramus-file-basename-exact-names)
      (setq projectile-ignored-files ignoramus-file-basename-exact-names)
      ;; (setq projectile-globally-ignored-directories ignoramus-file-basename-exact-names)
      (setq projectile-ignored-directories ignoramus-file-basename-exact-names))

    (advice-add 'ignoramus-setup
                :around 'dotemacs//ignoramus-setup)

    (dolist (name '(
                    "pids"
                    ".grunt"
                    ".cache"
                    ".lock-wscript"
                    ".cask"
                    ".vagrant"
                    ".DS_Store"
                    "lib-cov"
                    "coverage"
                    ".builds"
                    ".bzr"
                    ".cdv"
                    ".classpath"
                    ".coverage"
                    ".git"
                    ".hg"
                    ".idea"
                    ".ido.last"
                    ".netrwhist"
                    ".pc"
                    ".project"
                    ".projectile"
                    ".puppet-bak"
                    ".rspec"
                    ".sass-cache"
                    ".scala_dependencies"
                    ".svn"
                    "_darcs"
                    "auto-save-list"
                    "bower_components"
                    "node_modules"
                    ".cache"
                    ".sx"
                    "elpa"
                    ".tox"
                    "virtualenv"))

      ;; Ignore some additional directories
      (add-to-list 'ignoramus-file-basename-exact-names name))

    (dolist (name '(
                    ".tern-port"
                    ".png"
                    ".jpg"
                    ".jpeg"
                    ".gif"
                    "-autoloads.el"
                    ".class"
                    ".elc"
                    ".min.js"
                    "-min.js"
                    ".min.css"
                    "-min.css"
                    ".pyc"
                    ".pyd"
                    ".pyo"
                    ".rbc"
                    ".sassc"
                    ".suo"
                    ".swo"
                    ".venv"
                    ".swp"
                    ".psd"
                    ".ai"
                    ".pdf"
                    ".mov"
                    ".aep"
                    ".dmg"
                    ".zip"
                    ".gz"
                    ".bmp"
                    ".7z"
                    ".jar"
                    ".rar"
                    ".zip"
                    ".gz"
                    ".bzip"
                    ".bz2"
                    ".xz"
                    ".lzma"
                    ".cab"
                    ".iso"
                    ".tar"
                    ".dmg"
                    ".xpi"
                    ".gem"
                    ".egg"
                    ".deb"
                    ".rpm"
                    ".msi"
                    ".msm"
                    ".msp"
                    ".pid"
                    ".seed"))

      ;; Ignore some additional filename endings
      (add-to-list 'ignoramus-file-basename-endings name))

    (dolist (name '(
                    "\\`\\.flycheck.*\\'"
                    "\\`.*_flymake\\..*'"
                    ))

      ;; Ignore some additional filename endings
      (add-to-list 'ignoramus-file-basename-regexps name))

    (ignoramus-setup)))

(provide 'module-ignoramus)
;;; module-ignoramus.el ends here
