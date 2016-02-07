;;; module-eyebrowse.el --- Eyebrowse Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-funcs)
(require 'core-keybindings)
(require 'core-transient-state)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
(require 'module-utils)

;;; Code:

(defvar eyebrowse-display-help t
  "If non-nil additional help is displayed when selecting a workspace.")

(dotemacs-declare-prefix "W" "workspaces")

(use-package eyebrowse
  :ensure t
  :defer t
  :diminish eyebrowse-mode
  :init
  (progn
    (setq eyebrowse-new-workspace #'dotemacs-home-delete-other-windows
          eyebrowse-wrap-around t)
    (eyebrowse-mode)

    ;; vim-style tab switching
    (define-key evil-motion-state-map "gt" 'eyebrowse-next-window-config)
    (define-key evil-motion-state-map "gT" 'eyebrowse-prev-window-config)

    (defun dotemacs/workspaces-ms-rename ()
      "Rename a workspace and get back to micro-state."
      (interactive)
      (eyebrowse-rename-window-config (eyebrowse--get 'current-slot))
      (dotemacs-workspaces-micro-state))

    (defun dotemacs//workspaces-ms-get-slot-name (window-config)
      "Return the name for the given window-config"
      (let ((slot (car window-config))
            (caption (eyebrowse-format-slot window-config)))
        (if (= slot current-slot)
            (format "[%s]" caption)
          caption)))

    (defun dotemacs//workspaces-ms-get-window-configs ()
      "Return the list of window configs."
      (--sort (if (eq (car other) 0)
                  t
                (< (car it) (car other)))
              (eyebrowse--get 'window-configs)))

    (dotemacs-define-transient-state workspaces
        :title "Workspaces Transient State"
        :doc "
[_0_.._9_] switch to workspace  [_n_/_p_] next/prev  [_[tab]_] last  [_c_] close  [_r_] rename"
        :bindings
        ("0" eyebrowse-switch-to-window-config-0)
        ("1" eyebrowse-switch-to-window-config-1)
        ("2" eyebrowse-switch-to-window-config-2)
        ("3" eyebrowse-switch-to-window-config-3)
        ("4" eyebrowse-switch-to-window-config-4)
        ("5" eyebrowse-switch-to-window-config-5)
        ("6" eyebrowse-switch-to-window-config-6)
        ("7" eyebrowse-switch-to-window-config-7)
        ("8" eyebrowse-switch-to-window-config-8)
        ("9" eyebrowse-switch-to-window-config-9)
        ("<tab>" eyebrowse-last-window-config)
        ("C-i" eyebrowse-last-window-config)
        ("c" eyebrowse-close-window-config)
        ("h" eyebrowse-prev-window-config)
        ("l" eyebrowse-next-window-config)
        ("n" eyebrowse-next-window-config)
        ("N" eyebrowse-prev-window-config)
        ("p" eyebrowse-prev-window-config)
        ("r" dotemacs/workspaces-ms-rename :exit t)
        ("w" eyebrowse-switch-to-window-config :exit t))

    (defun dotemacs-eyebrowse-switch ()
      "Hook eyebrowse to projectile and neotree."
      (interactive)
      (when (projectile-project-p)
        (message "eyebrowse switching to: %s" (projectile-project-root))
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

    (with-eval-after-load 'projectile
      (add-hook 'eyebrowse-post-window-switch-hook 'dotemacs-eyebrowse-switch))))

(provide 'module-eyebrowse)
;;; module-eyebrowse.el ends here
