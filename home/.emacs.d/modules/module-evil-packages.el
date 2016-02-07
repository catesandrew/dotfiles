;;; module-evil-packages.el --- Evil Packages Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'use-package)
(require 'core-vars)
;; (require 'core-funcs)
(require 'core-keybindings)
(require 'core-display-init)
(require 'core-toggle)
(require 'core-fonts-support)
(require 'core-transient-state)
(require 'core-use-package-ext)
(require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)
(require 'evil)

;;; Code:

(defvar evil-snipe-enable-alternate-f-and-t-behaviors nil
  "if non nil f/F/t/T behaviors are replaced by evil-snipe behavior.")

(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :ensure t
  :init
  (setq evil-snipe-scope 'whole-buffer
        evil-snipe-enable-highlight t
        evil-snipe-enable-incremental-highlight t
        evil-snipe-auto-disable-substitute t
        evil-snipe-show-prompt nil
        evil-snipe-smart-case t)
  :config
  (progn
    (if evil-snipe-enable-alternate-f-and-t-behaviors
        (progn
          (setq evil-snipe-repeat-scope 'whole-buffer)
          (evil-snipe-override-mode 1))
      (evil-snipe-mode 1))))

(dotemacs-use-package-add-hook magit
  :post-init
  (if evil-snipe-enable-alternate-f-and-t-behaviors
      (progn
        (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
        (add-hook 'git-rebase-mode-hook 'turn-off-evil-snipe-override-mode))
    (add-hook 'magit-mode-hook 'turn-off-evil-snipe-mode)
    (add-hook 'git-rebase-mode-hook 'turn-off-evil-snipe-mode)))

(use-package evil-args
  :ensure t
  :init
  (progn
    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)))

(use-package evil-escape
  :disabled t
  :init (evil-escape-mode)
  :config
  (dotemacs-hide-lighter evil-escape-mode))

(use-package evil-exchange
  :ensure t
  :init (evil-exchange-install))

(use-package evil-iedit-state
  :ensure t
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init (dotemacs-set-leader-keys "se" 'evil-iedit-state/iedit-mode)
  :config
  ;; activate leader in iedit and iedit-insert states
  (define-key evil-iedit-state-map
    (kbd dotemacs-leader-key) dotemacs-default-map))

(use-package evil-jumper
  :ensure t
  :init
  (progn
    (setq evil-jumper-post-jump-hook 'recenter
          evil-jumper-auto-save-interval 600)
    (evil-jumper-mode t)))

(use-package evil-lisp-state
  :ensure t
  :init (setq evil-lisp-state-global t)
  :config (dotemacs-set-leader-keys "k" evil-lisp-state-map))

(use-package evil-mc
  :ensure t
  :defer t)

; TODO: Add switch between evil-commentary and evil-nerd-commenter
(use-package evil-commentary
  :diminish evil-commentary-mode
  :ensure t
  :init
  (progn
    (evil-commentary-mode)
    (dotemacs-set-leader-keys ";" 'evil-commentary)))

(use-package evil-nerd-commenter
  :disabled t
  :ensure t
  :commands (evilnc-comment-operator)
  :init
  (progn
    ;; double all the commenting functions so that the inverse operations
    ;; can be called without setting a flag
    (defun dotemacs/comment-or-uncomment-lines-inverse (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line t))
        (evilnc-comment-or-uncomment-lines arg)))

    (defun dotemacs/comment-or-uncomment-lines (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line nil))
        (evilnc-comment-or-uncomment-lines arg)))

    (defun dotemacs/copy-and-comment-lines-inverse (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line t))
        (evilnc-copy-and-comment-lines arg)))

    (defun dotemacs/copy-and-comment-lines (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line nil))
        (evilnc-copy-and-comment-lines arg)))

    (defun dotemacs/quick-comment-or-uncomment-to-the-line-inverse
        (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line t))
        (evilnc-comment-or-uncomment-to-the-line arg)))

    (defun dotemacs/quick-comment-or-uncomment-to-the-line (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line nil))
        (evilnc-comment-or-uncomment-to-the-line arg)))

    (defun dotemacs/comment-or-uncomment-paragraphs-inverse (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line t))
        (evilnc-comment-or-uncomment-paragraphs arg)))

    (defun dotemacs/comment-or-uncomment-paragraphs (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line nil))
        (evilnc-comment-or-uncomment-paragraphs arg)))

    (define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
    (define-key evil-normal-state-map "gy" 'dotemacs/copy-and-comment-lines)

    (dotemacs-set-leader-keys
      ";"  'evilnc-comment-operator
      "cl" 'dotemacs/comment-or-uncomment-lines
      "cL" 'dotemacs/comment-or-uncomment-lines-inverse
      "cp" 'dotemacs/comment-or-uncomment-paragraphs
      "cP" 'dotemacs/comment-or-uncomment-paragraphs-inverse
      "ct" 'dotemacs/quick-comment-or-uncomment-to-the-line
      "cT" 'dotemacs/quick-comment-or-uncomment-to-the-line-inverse
      "cy" 'dotemacs/copy-and-comment-lines
      "cY" 'dotemacs/copy-and-comment-lines-inverse)))

;; TODO: These modes have to enable evil-matchit from their moduels
;; (dolist (hook '(LaTeX-mode-hook mustache-mode-hook handlebars-mode-hook ruby-mode-hook))
;;   (add-hook hook 'turn-on-evil-matchit-mode))
(use-package evil-matchit
  :ensure t
  :defer t)

(use-package evil-indent-plus
  :ensure t
  :init
  (evil-indent-plus-default-bindings))

(use-package evil-numbers
  :ensure t
  :config
  (progn
    (dotemacs-define-transient-state evil-numbers
      :title "Evil Numbers Transient State"
      :doc "\n[_+_/_=_] increase number [_-_] decrease"
      :bindings
      ("+" evil-numbers/inc-at-pt)
      ("=" evil-numbers/inc-at-pt)
      ("-" evil-numbers/dec-at-pt))
    (dotemacs-set-leader-keys
     "n+" 'dotemacs/evil-numbers-transient-state/evil-numbers/inc-at-pt
     "n=" 'dotemacs/evil-numbers-transient-state/evil-numbers/inc-at-pt
     "n-" 'dotemacs/evil-numbers-transient-state/evil-numbers/dec-at-pt)))

(use-package evil-search-highlight-persist
  :ensure t
  :init
  (progn
    (global-evil-search-highlight-persist)
    ;; (set-face-attribute )
    (dotemacs-set-leader-keys "/" 'evil-search-highlight-persist-remove-all)
    (dotemacs-set-leader-keys "sc" 'evil-search-highlight-persist-remove-all)
    (define-key evil-search-highlight-persist-map (kbd "C-x SPC") 'rectangle-mark-mode)
    (evil-ex-define-cmd "nohlsearch"
                        'evil-search-highlight-persist-remove-all)

    (defun dotemacs/adaptive-evil-highlight-persist-face ()
      (set-face-attribute 'evil-search-highlight-persist-highlight-face nil
                          :inherit 'region
                          :background nil
                          :foreground nil))
    (dotemacs/adaptive-evil-highlight-persist-face)))

(use-package evil-surround
  :ensure t
  :init
  (progn
    (global-evil-surround-mode 1)
    ;; `s' for surround instead of `substitute'
    ;; see motivation for this change in the documentation
    (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
    (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)))

(use-package evil-terminal-cursor-changer
  :ensure t
  :init
  (progn
    (dotemacs|do-after-display-system-init
      (unless (display-graphic-p)
        (setq evil-visual-state-cursor 'box ; █
              evil-insert-state-cursor 'bar ; ⎸
              evil-emacs-state-cursor 'hbar ; _
        )))))

(use-package evil-tutor
  :disabled t
  :ensure t
  :defer t
  :commands (evil-tutor-start
             evil-tutor-resume)
  :init
  (progn
    (setq evil-tutor-working-directory
          (concat dotemacs-cache-directory ".tutor/"))
    (dotemacs-set-leader-keys "hT" 'evil-tutor-start)))

(use-package evil-visualstar
  :ensure t
  :commands (evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (progn
    (define-key evil-visual-state-map (kbd "*")
      'evil-visualstar/begin-search-forward)
    (define-key evil-visual-state-map (kbd "#")
      'evil-visualstar/begin-search-backward)))

(use-package evil-anzu                  ; Position/matches count for isearch
  :ensure t
  :init
  (global-anzu-mode t)
  :config
  (progn
    (dotemacs-hide-lighter anzu-mode)
    (setq anzu-search-threshold 1000
          anzu-cons-mode-line-p nil)
    ;; powerline integration
    (defun dotemacs/anzu-update-mode-line (here total)
      "Custom update function which does not propertize the status."
      (when anzu--state
        (let ((status (cl-case anzu--state
                        (search (format "(%s/%d%s)"
                                        (anzu--format-here-position here total)
                                        total (if anzu--overflow-p "+" "")))
                        (replace-query (format "(%d replace)" total))
                        (replace (format "(%d/%d)" here total)))))
          status)))
    (setq anzu-mode-line-update-function 'dotemacs/anzu-update-mode-line)))

(use-package evil-cleverparens
  :defer t
  :ensure t
  :diminish evil-cleverparens-mode
  :init
  (progn
    (setq evil-cleverparens-use-regular-insert t)
    (dotemacs-add-toggle evil-cleverparens
      :status evil-cleverparens-mode
      :on  (evil-cleverparens-mode)
      :off (evil-cleverparens-mode -1)
      :documentation "Enable evil-cleverparens.")))

(use-package evil-visual-mark-mode
  :defer t
  :ensure t
  :diminish evil-visual-mark-mode
  :init
  (progn
    (dotemacs-add-toggle evil-visual-mark-mode
      :status evil-visual-mark-mode
      :on (evil-visual-mark-mode)
      :off (evil-visual-mark-mode -1)
      :documentation "Show evil marks")))

(use-package evil-ediff
  :after (ediff)
  :ensure t)

(provide 'module-evil-packages)
;;; module-evil-packages.el ends here
