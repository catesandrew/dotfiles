;;; module-projectile.el --- Projectile Module
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

(use-package projectile
  :ensure t
  :defer 1.4
  :commands (projectile-ack
             projectile-ag
             projectile-compile-project
             projectile-dired
             projectile-grep
             projectile-find-dir
             projectile-find-file
             projectile-find-tag
             projectile-find-test-file
             projectile-invalidate-cache
             projectile-kill-buffers
             projectile-multi-occur
             projectile-project-root
             projectile-recentf
             projectile-regenerate-tags
             projectile-replace
             projectile-run-async-shell-command-in-root
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-vc)
  :init
  (progn
    (defun projectile-find-file-ignored ()
      "Projectile find file without ignore."
      (interactive)
      (let ((projectile-git-command "git ls-files -zco"))
        (call-interactively 'projectile-find-file)))

    (defun projectile-add-magit-repo-dirs-to-known-projects ()
      "Add `magit-repo-dirs' to `projectile-known-projects'."
      (interactive)
      (--each (mapcar 'cdr (magit-list-repos magit-repo-dirs))
        (projectile-add-known-project (file-name-as-directory
                                       (file-truename it)))))

    ;; note for Windows: GNU find or Cygwin find must be in path to enable
    ;; fast indexing
    (when (and (dotemacs/system-is-mswindows) (executable-find "find"))
      (setq projectile-indexing-method 'alien
            projectile-generic-command "find . -type f"))
    (setq projectile-sort-order 'recentf
          projectile-use-git-grep t
          projectile-switch-project-action 'projectile-dired
          projectile-cache-file (concat dotemacs-cache-directory
                                        "projectile.cache")
          projectile-known-projects-file (concat dotemacs-cache-directory
                                                 "projectile-bookmarks.eld")
          projectile-find-dir-includes-top-level t
          projectile-require-project-root t
          projectile-verbose nil)

    (setq projectile-enable-caching nil)
    (defadvice projectile-mode (before maybe-use-cache activate)
      (when
        (--any? (and it (file-remote-p it))
                (list
                  (buffer-file-name)
                   list-buffers-directory
                   default-directory))
        (setq-local projectile-enable-caching t)))

    (setq projectile-project-root-files '(
            ; "rebar.config"       ; Rebar project file
            "project.clj"        ; Leiningen project file
            ; "SConstruct"         ; Scons project file
            "pom.xml"            ; Maven project file
            ; "build.sbt"          ; SBT project file
            ; "build.gradle"       ; Gradle project file
            "Gemfile"            ; Bundler file
            ; "requirements.txt"   ; Pip file
            ; "setup.py"           ; Setuptools file
            ; "tox.ini"            ; Tox file
            "package.json"       ; npm package file
            "gulpfile.js"        ; Gulp build file
            "Gruntfile.js"       ; Grunt project file
            "bower.json"         ; Bower project file
            "composer.json"      ; Composer project file
            "Cargo.toml"         ; Cargo project file
            ; "mix.exs"            ; Elixir mix project file
            ))

    (setq projectile-project-root-files-bottom-up '(
            ".projectile" ; projectile project marker
            ".git"        ; Git VCS root dir
            ".hg"         ; Mercurial VCS root dir
            ; ".fslckout"   ; Fossil VCS root dir
            ; ".bzr"        ; Bazaar VCS root dir
            ; "_darcs"      ; Darcs VCS root dir
            ))

    (setq projectile-globally-ignored-file-suffixes
          '("class"
            "elc"))

    (unless (boundp 'dotemacs-use-helm-projectile)
      (dotemacs-set-leader-keys
        "pb" 'projectile-switch-to-buffer
        "pd" 'projectile-find-dir
        "pf" 'projectile-find-file
        "ph" 'helm-projectile
        "pr" 'projectile-recentf
        "ps" 'projectile-switch-project))
    (dotemacs-set-leader-keys
      "p!" 'projectile-run-shell-command-in-root
      "p&" 'projectile-run-async-shell-command-in-root
      "pa" 'projectile-toggle-between-implementation-and-test
      "pc" 'projectile-compile-project
      "pD" 'projectile-dired
      "pG" 'projectile-regenerate-tags
      "pI" 'projectile-invalidate-cache
      "pk" 'projectile-kill-buffers
      "po" 'projectile-multi-occur
      "pR" 'projectile-replace
      "pT" 'projectile-find-test-file
      "py" 'projectile-find-tag))
  :config
  (progn
    ; (add-hook 'projectile-after-switch-project-hook 'pyenv-mode-set-local-version)
    (defun dotemacs//projectile-switch-project-by-name (fcn project-to-switch &optional args)
      (apply fcn project-to-switch args)
      (when (projectile-project-p)
        (message "Switching to project: %s" (projectile-project-root))
        (when (fboundp 'neotree-dir)
          (if (neo-global--window-exists-p)
              (neotree-dir (projectile-project-root))
            (progn
              (neotree-dir (projectile-project-root))
              (neotree-hide)
              (let ((origin-buffer-file-name (buffer-file-name)))
                (neotree-find (projectile-project-root))
                (neotree-find origin-buffer-file-name))
              (neotree-hide))))))
    (advice-add 'projectile-switch-project-by-name
                :around 'dotemacs//projectile-switch-project-by-name)

    ;; Remove dead projects when Emacs is idle
    (run-with-idle-timer 10 nil 'projectile-cleanup-known-projects)
    (projectile-global-mode)
    (dotemacs-hide-lighter projectile-mode)))

(use-package help-fns+
  :commands (describe-keymap)
  :ensure t
  :init
  (dotemacs-set-leader-keys "hdK" 'describe-keymap))

(use-package helm-projectile
  :ensure t
  :defer t
  :commands (helm-projectile-switch-to-buffer
             helm-projectile-find-dir
             helm-projectile-dired-find-dir
             helm-projectile-recentf
             helm-projectile-find-file
             helm-projectile-grep
             helm-projectile
             helm-projectile-switch-project)
  :init
  (progn
    (setq projectile-switch-project-action 'helm-projectile)

    (defconst dotemacs-use-helm-projectile t
      "This variable is only defined if helm-projectile is used.")

    ;; needed for smart search if user's default tool is grep
    (defalias 'dotemacs-helm-project-do-grep 'helm-projectile-grep)
    (defalias 'dotemacs-helm-project-do-grep-region-or-symbol 'helm-projectile-grep)

    (dotemacs-set-leader-keys
      "pb"  'helm-projectile-switch-to-buffer
      "pd"  'helm-projectile-find-dir
      "pf"  'helm-projectile-find-file
      "ph"  'helm-projectile
      "pp"  'helm-projectile-switch-project
      "pr"  'helm-projectile-recentf
      "pv"  'projectile-vc
      "sgp" 'helm-projectile-grep))
  :config
  (with-eval-after-load 'projectile
    (add-to-list 'helm-projectile-sources-list 'helm-source-projectile-recentf-list)))

(provide 'module-projectile)
;;; module-projectile.el ends here
