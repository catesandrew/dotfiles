;;; module-github.el --- GitHub Module
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

;; Github
(setq github/key-binding-prefixes '(("gh" . "github")
                                    ("gg" . "github gist")))
(mapc (lambda (x) (dotemacs-declare-prefix (car x) (cdr x)))
      github/key-binding-prefixes)

(use-package gist
  :defer t
  :ensure t
  :init
  (progn
    (evilified-state-evilify gist-list-mode gist-list-menu-mode-map
      "f" 'gist-fetch-current
      "K" 'gist-kill-current
      "o" 'gist-browse-current-url)

    (dotemacs-set-leader-keys
      "ggb" 'gist-buffer
      "ggB" 'gist-buffer-private
      "ggl" 'gist-list
      "ggr" 'gist-region
      "ggR" 'gist-region-private)))

(use-package github-browse-file
  :defer t
  :ensure t
  :init
  (dotemacs-set-leader-keys
    "gho" 'github-browse-file))

(use-package github-clone
  :defer t
  :ensure t
  :init
  (dotemacs-set-leader-keys
   "gh C-c" 'github-clone))

(use-package git-link
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-set-leader-keys
      "ghl" 'git-link
      "ghL" 'dotemacs-git-link-copy-url-only
      "ghc" 'git-link-commit
      "ghC" 'dotemacs-git-link-commit-copy-url-only)

    ;; default is to open the generated link
    (setq git-link-open-in-browser t))
  :config
  (progn
    (defun git-link-ibaset (hostname dirname filename branch commit start end)
      (format "https://git.ibaset.com/%s/blob/%s/%s#%s"
        dirname
        (or branch commit)
        filename
        (if (and start end)
            (format "L%s-L%s" start end)
          (format "L%s" start))))

    (defun git-link-commit-ibaset (hostname dirname commit)
      (format "https://git.ibaset.com/%s/commit/%s" dirname commit))

    (add-to-list 'git-link-remote-alist
      '("git.ibaset.com" git-link-github))
    (add-to-list 'git-link-commit-remote-alist
      '("git.ibaset.com" git-link-commit-github))
    (add-to-list 'git-link-remote-alist
      '("andrew.git.ibaset.com" git-link-ibaset))
    (add-to-list 'git-link-commit-remote-alist
      '("andrew.git.ibaset.com" git-link-commit-ibaset))))

;; magit-gh-pulls has to be loaded via a pre-config hook because the source
;; code makes assumptions about the status of the magit-mode keymaps that are
;; incompatible with the dotemacs' evilification feature. To avoid errors,
;; magit-gh-pulls must be loaded after magit, but before magit is configured by
;; dotemacs.

(dotemacs-use-package-add-hook magit
  :pre-config
  (progn
    (use-package magit-gh-pulls
      :ensure t
      :init
      (progn
        (define-key magit-mode-map "#" 'dotemacs-load-gh-pulls-mode))
      :config
      (dotemacs-diminish magit-gh-pulls-mode "Github-PR"))))

(provide 'module-github)
;;; module-github.el ends here
