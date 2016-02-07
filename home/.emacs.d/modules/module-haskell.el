;;; module-haskell.el --- Haskell Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; This Haskell setup needs:
;;
;; cabal install hasktags haskell-docs hoogle hindent
;; cabal install stylish-haskell hlint ghc-mod hasktags
;;
;; Additionally, to be installed from source:
;;
;; - https://github.com/chrisdone/ghci-ng
;;
(require 'use-package)
;; (require 'core-vars)
;; (require 'core-funcs)
(require 'core-keybindings)
;; (require 'core-display-init)
(require 'core-use-package-ext)
(require 'core-auto-completion)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

;; config

(dotemacs-defvar-company-backends haskell-mode)
(dotemacs-defvar-company-backends haskell-cabal-mode)

(defvar haskell-enable-ghci-ng-support nil
  "If non-nil ghci-ng support is enabled")

(defvar haskell-enable-shm-support nil
  "If non-nil structured-haskell-mode support is enabled")

(defvar haskell-enable-hindent-style 'fundamental
  "Style to use for formatting with hindent; available are: fundamental johan-tibell chris-done gibiansky. If nil hindent is disabled.")

(defvar haskell-enable-ghc-mod-support t
  "If non-nil ghc-mod support is enabled")

(use-package cmm-mode
  :ensure t
  :defer t)

(use-package helm-hoogle
  :ensure t
  :defer t
  :init
  (dotemacs-set-leader-keys-for-major-mode 'haskell-mode "hf" 'helm-hoogle))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'haskell-mode))

(use-package flycheck-haskell           ; Setup Flycheck from Cabal projects
  :ensure t
  :commands flycheck-haskell-configure
  :init (add-hook 'flycheck-mode-hook 'flycheck-haskell-configure))

(use-package ghc
  :ensure t
  :if haskell-enable-ghc-mod-support
  :defer t
  :init (add-hook 'haskell-mode-hook 'ghc-init)
  :config
  (progn
    (dotemacs-declare-prefix-for-mode 'haskell-mode "mm" "haskell/ghc-mod")
    (dotemacs-set-leader-keys-for-major-mode 'haskell-mode
      "mt" 'ghc-insert-template-or-signature
      "mu" 'ghc-initial-code-from-signature
      "ma" 'ghc-auto
      "mf" 'ghc-refine
      "me" 'ghc-expand-th
      "mn" 'ghc-goto-next-hole
      "mp" 'ghc-goto-prev-hole
      "m>"  'ghc-make-indent-deeper
      "m<"  'ghc-make-indent-shallower)))

(dotemacs-use-package-add-hook flycheck
  :post-config
  (progn
    (with-eval-after-load 'ghc
      ;; remove overlays from ghc-check.el if flycheck is enabled
      (set-face-attribute 'ghc-face-error nil :underline nil)
      (set-face-attribute 'ghc-face-warn nil :underline nil))))

(use-package haskell-mode
  :ensure t
  :defer t
  :init
  (progn
    (defun dotemacs//force-haskell-mode-loading ()
      "Force `haskell-mode' loading when visiting cabal file."
      (require 'haskell-mode))
    (add-hook 'haskell-cabal-mode-hook
              'dotemacs//force-haskell-mode-loading)

    ;; settings
    (setq
      ;; Use notify.el (if you have it installed) at the end of running
      ;; Cabal commands or generally things worth notifying.
      haskell-notify-p t
      ;; To enable tags generation on save.
      haskell-tags-on-save t
      ;; Remove annoying error popups
      haskell-interactive-popup-errors nil
      ;; Better import handling
      haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      haskell-process-use-presentation-mode t ; Don't clutter the echo area
      haskell-process-show-debug-tips nil     ; Disable tips
      haskell-process-log t                   ; Log debugging information
      ;; Suggest imports automatically with Hayoo.  Hayoo is slower because
      ;; it's networked, but it covers all of hackage, which is really an
      ;; advantage.
      haskell-process-suggest-hoogle-imports nil
      haskell-process-suggest-hayoo-imports t
      ;; Disable haskell-stylish on save, it breaks flycheck highlighting
      haskell-stylish-on-save nil))
  :config
  (progn
    ;; Haskell main editing mode key bindings.
    (defun dotemacs-init-haskell-mode ()
      ;; use only internal indentation system from haskell
      (if (fboundp 'electric-indent-local-mode)
          (electric-indent-local-mode -1))
      (when haskell-enable-shm-support
        ;; in structured-haskell-mode line highlighting creates noise
        (setq-local global-hl-line-mode nil)))

    (defun dotemacs/haskell-interactive-bring ()
      "Bring up the interactive mode for this session without
         switching to it."
      (interactive)
      (let* ((session (haskell-session))
             (buffer (haskell-session-interactive-buffer session)))
        (display-buffer buffer)))

    ;; hooks
    (add-hook 'haskell-mode-hook 'dotemacs-init-haskell-mode)
    (unless haskell-enable-ghc-mod-support
      (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

    ;; prefixes
    (dotemacs-declare-prefix-for-mode 'haskell-mode "mg" "haskell/navigation")
    (dotemacs-declare-prefix-for-mode 'haskell-mode "ms" "haskell/repl")
    (dotemacs-declare-prefix-for-mode 'haskell-mode "mc" "haskell/cabal")
    (dotemacs-declare-prefix-for-mode 'haskell-mode "mh" "haskell/documentation")
    (dotemacs-declare-prefix-for-mode 'haskell-mode "md" "haskell/debug")
    (dotemacs-declare-prefix-for-mode 'haskell-interactive-mode "ms" "haskell/repl")
    (dotemacs-declare-prefix-for-mode 'haskell-cabal-mode "ms" "haskell/repl")

    ;; key bindings
    (defun dotemacs-haskell-process-do-type-on-prev-line ()
      (interactive)
      (if haskell-enable-ghci-ng-support
          (haskell-mode-show-type-at 1)
        (haskell-process-do-type 1)))

    (dotemacs-set-leader-keys-for-major-mode 'haskell-mode
      "gg"  'haskell-mode-jump-to-def-or-tag
      "gi"  'haskell-navigate-imports
      "f"   'haskell-mode-stylish-buffer

      "sb"  'haskell-process-load-or-reload
      "sc"  'haskell-interactive-mode-clear
      "ss"  'dotemacs/haskell-interactive-bring
      "sS"  'haskell-interactive-switch

      "ca"  'haskell-process-cabal
      "cb"  'haskell-process-cabal-build
      "cc"  'haskell-compile
      "cv"  'haskell-cabal-visit-file

      "hd"  'inferior-haskell-find-haddock
      "hh"  'hoogle
      "hH"  'hoogle-lookup-from-local
      "hi"  (lookup-key haskell-mode-map (kbd "C-c C-i"))
      "ht"  (lookup-key haskell-mode-map (kbd "C-c C-t"))
      "hT"  'dotemacs-haskell-process-do-type-on-prev-line
      "hy"  'hayoo

      "dd"  'haskell-debug
      "db"  'haskell-debug/break-on-function
      "dn"  'haskell-debug/next
      "dN"  'haskell-debug/previous
      "dB"  'haskell-debug/delete
      "dc"  'haskell-debug/continue
      "da"  'haskell-debug/abandon
      "dr"  'haskell-debug/refresh)

    ;; configure C-c C-l so it doesn't throw any errors
    (bind-key "C-c C-l" 'haskell-process-load-or-reload haskell-mode-map)

    ;; Switch back to editor from REPL
    (dotemacs-set-leader-keys-for-major-mode 'haskell-interactive-mode
      "sS"  'haskell-interactive-switch-back)

    ;; Compile
    (dotemacs-set-leader-keys-for-major-mode 'haskell-cabal
      "C"  'haskell-compile)

    ;; Cabal-file bindings
    (dotemacs-set-leader-keys-for-major-mode 'haskell-cabal-mode
      ;; "="   'haskell-cabal-subsection-arrange-lines ;; Does a bad job, 'gg=G' works better
      "d"   'haskell-cabal-add-dependency
      "b"   'haskell-cabal-goto-benchmark-section
      "e"   'haskell-cabal-goto-executable-section
      "t"   'haskell-cabal-goto-test-suite-section
      "m"   'haskell-cabal-goto-exposed-modules
      "l"   'haskell-cabal-goto-library-section
      "n"   'haskell-cabal-next-subsection
      "p"   'haskell-cabal-previous-subsection
      "sc"  'haskell-interactive-mode-clear
      "ss"  'dotemacs/haskell-interactive-bring
      "sS"  'haskell-interactive-switch
      "N"   'haskell-cabal-next-section
      "P"   'haskell-cabal-previous-section
      "f"   'haskell-cabal-find-or-create-source-file)

    ;; Make "RET" behaviour in REPL saner
    (evil-define-key 'insert haskell-interactive-mode-map
      (kbd "RET") 'haskell-interactive-mode-return)
    (evil-define-key 'normal haskell-interactive-mode-map
      (kbd "RET") 'haskell-interactive-mode-return)

    ;;GHCi-ng
    (when haskell-enable-ghci-ng-support
      (when-let (ghci-ng (executable-find "ghci-ng"))
        ;; Use GHCI NG from https://github.com/chrisdone/ghci-ng
        (setq haskell-process-path-ghci ghci-ng)
        ;; haskell-process-type is set to auto, so setup ghci-ng for either case
        ;; if haskell-process-type == cabal-repl
        (add-to-list 'haskell-process-args-cabal-repl
                     '("--ghc-option=-ferror-spans" (concat "--with-ghc=" ghci-ng))))

      ;; fixes ghci-ng for stack projects
      (setq haskell-process-wrapper-function
            (lambda (args)
              (append args (list "--with-ghc" "ghci-ng"))))

      (dotemacs-set-leader-keys-for-major-mode 'haskell-mode
        ;; function suggested in
        ;; https://github.com/chrisdone/ghci-ng#using-with-haskell-mode
        "u"   'haskell-mode-find-uses
        "ht"  'haskell-mode-show-type-at
        "gg"  'haskell-mode-goto-loc))

    ;; Useful to have these keybindings for .cabal files, too.
    (with-eval-after-load 'haskell-cabal-mode-map
      (define-key haskell-cabal-mode-map
        [?\C-c ?\C-z] 'haskell-interactive-switch))

    ;; align rules for Haskell
    (with-eval-after-load 'align
      (add-to-list 'align-rules-list
                   '(haskell-types
                     (regexp . "\\(\\s-+\\)\\(::\\|?\\)\\s-+")
                     (modes . '(haskell-mode literate-haskell-mode))))
      (add-to-list 'align-rules-list
                   '(haskell-assignment
                     (regexp . "\\(\\s-+\\)=\\s-+")
                     (modes . '(haskell-mode literate-haskell-mode))))
      (add-to-list 'align-rules-list
                   '(haskell-arrows
                     (regexp . "\\(\\s-+\\)\\(->\\|?\\)\\s-+")
                     (modes . '(haskell-mode literate-haskell-mode))))
      (add-to-list 'align-rules-list
                   '(haskell-left-arrows
                     (regexp . "\\(\\s-+\\)\\(<-\\|?\\)\\s-+")
                     (modes . '(haskell-mode literate-haskell-mode)))))))

(use-package haskell-snippets
  :ensure t
  :defer t
  :init
  (progn
    ;; manually load the package since the current implementation is not lazy
    ;; loading friendly (funny coming from the haskell mode :-))
    (setq haskell-snippets-dir (dotemacs-get-package-directory
                                'haskell-snippets))

    (defun haskell-snippets-initialize ()
      (let ((snip-dir (expand-file-name "snippets" haskell-snippets-dir)))
        (add-to-list 'yas-snippet-dirs snip-dir t)
        (yas-load-directory snip-dir)))

    (with-eval-after-load 'yasnippet (haskell-snippets-initialize))))

(use-package hindent                    ; Automated Haskell indentation
  :defer t
  :ensure t
  :if (stringp haskell-enable-hindent-style)
  :init
  (add-hook 'haskell-mode-hook #'hindent-mode)
  :config
  (progn
    (setq hindent-style haskell-enable-hindent-style)
    (dotemacs-set-leader-keys-for-major-mode 'haskell-mode
      "F" 'hindent/reformat-decl)))

(use-package shm
  :defer t
  :ensure t
  :if haskell-enable-shm-support
  :init
  (add-hook 'haskell-mode-hook 'structured-haskell-mode)
  :config
  (progn
    (when (require 'shm-case-split nil 'noerror)
      ;;TODO: Find some better bindings for case-splits
      (define-key shm-map (kbd "C-c S") 'shm/case-split)
      (define-key shm-map (kbd "C-c C-s") 'shm/do-case-split))

    (evil-define-key 'normal shm-map
      (kbd "RET") nil
      (kbd "C-k") nil
      (kbd "C-j") nil
      (kbd "D") 'shm/kill-line
      (kbd "R") 'shm/raise
      (kbd "P") 'shm/yank
      (kbd "RET") 'shm/newline-indent
      (kbd "RET") 'shm/newline-indent
      (kbd "M-RET") 'evil-ret)

    (evil-define-key 'operator shm-map
      (kbd ")") 'shm/forward-node
      (kbd "(") 'shm/backward-node)

    (evil-define-key 'motion shm-map
      (kbd ")") 'shm/forward-node
      (kbd "(") 'shm/backward-node)

    (define-key shm-map (kbd "C-j") nil)
    (define-key shm-map (kbd "C-k") nil)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook haskell-mode)
      (dotemacs-add-company-hook haskell-cabal-mode)))

  (use-package company-ghc
    :ensure t
    :defer t
    :init
    (push (if haskell-enable-ghc-mod-support
              '(company-ghc company-dabbrev-code company-yasnippet)
            '(company-dabbrev-code company-yasnippet))
          company-backends-haskell-mode))

  (use-package company-cabal
    :ensure t
    :defer t
    :init
    (progn
      (push '(company-cabal) company-backends-haskell-cabal-mode))))

(provide 'module-haskell)
;;; module-haskell.el ends here
