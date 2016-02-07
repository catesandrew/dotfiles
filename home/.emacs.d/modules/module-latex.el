;;; module-latex.el --- LaTeX Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'use-package)
(require 'core-vars)
(require 'core-keybindings)
(require 'core-auto-completion)
(require 'core-use-package-ext)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

;; config

(dotemacs-defvar-company-backends LaTeX-mode)

(defvar latex-build-command (if (executable-find "latexmk") "LatexMk" "LaTeX")
  "The default command to use with `, m b'.")

(defvar latex-enable-auto-fill t
  "Whether to use `auto-fill-mode` or not in tex files.")

(defvar latex-enable-folding t
  "Whether to use `TeX-fold-mode' or not in tex/latex buffers.")

(defvar latex-nofill-env '("equation"
                           "equation*"
                           "align"
                           "align*"
                           "tabular"
                           "tikzpicture")
  "List of environment names in which `auto-fill-mode' will be inhibited.")

;; funcs

(defun latex/build ()
  (interactive)
  (progn
    (let ((TeX-save-query nil))
      (TeX-save-document (TeX-master-file)))
    (TeX-command latex-build-command 'TeX-master-file -1)))
    ;; (setq build-proc (TeX-command latex-build-command 'TeX-master-file -1))
    ;; ;; Sometimes, TeX-command returns nil causing an error in set-process-sentinel
    ;; (when build-proc
    ;;   (set-process-sentinel build-proc 'latex//build-sentinel))))

(defun latex//build-sentinel (process event)
  (if (string= event "finished\n")
      (TeX-view)
    (message "Errors! Check with C-`")))

(defun latex//autofill ()
  "Check whether the pointer is ucrrently inside on the
environments described in `latex-nofill-env' and if so, inhibits
the automatic filling of the current paragraph."
  (let ((do-auto-fill t)
        (current-environment "")
        (level 0))
    (while (and do-auto-fill (not (string= current-environment "document")))
      (setq level (1+ level)
            current-environment (LaTeX-current-environment level)
            do-auto-fill (not (member current-environment latex-nofill-env))))
    (when do-auto-fill
      (do-auto-fill))))

(defun latex/auto-fill-mode ()
  "Toggle uato-fill-mode using the custom auto-fill function."
  (interactive)
  (auto-fill-mode)
  (setq auto-fill-function 'latex//autofill))

;; Rebindings for TeX-font
(defun latex/font-bold () (interactive) (TeX-font nil ?\C-b))
(defun latex/font-medium () (interactive) (TeX-font nil ?\C-m))
(defun latex/font-code () (interactive) (TeX-font nil ?\C-t))
(defun latex/font-emphasis () (interactive) (TeX-font nil ?\C-e))
(defun latex/font-italic () (interactive) (TeX-font nil ?\C-i))
(defun latex/font-clear () (interactive) (TeX-font nil ?\C-d))
(defun latex/font-calligraphic () (interactive) (TeX-font nil ?\C-a))
(defun latex/font-small-caps () (interactive) (TeX-font nil ?\C-c))
(defun latex/font-sans-serif () (interactive) (TeX-font nil ?\C-f))
(defun latex/font-normal () (interactive) (TeX-font nil ?\C-n))
(defun latex/font-serif () (interactive) (TeX-font nil ?\C-r))
(defun latex/font-oblique () (interactive) (TeX-font nil ?\C-s))
(defun latex/font-upright () (interactive) (TeX-font nil ?\C-u))

;; packages

(use-package auctex-latexmk             ; latexmk command for AUCTeX
  :ensure t
  :if (string= latex-build-command "LatexMk")
  :defer t
  :init
  (progn
    (setq auctex-latexmk-inherit-TeX-PDF-mode t)
    (dotemacs-use-package-add-hook tex
      :post-config
      (auctex-latexmk-setup))))

(use-package tex                        ; TeX editing/processing
  :ensure auctex
  :defer t
  :init
  (progn
    (setq TeX-command-default latex-build-command
          TeX-auto-save t               ; Automatically save style information
          TeX-parse-self t              ; Parse documents to provide completion
                                        ; for packages, etc.
          TeX-electric-sub-and-superscript t ; Automatically insert braces after
                                        ; sub- and superscripts in math mode
          TeX-electric-math '("\\(" "\\)")
          ;; Don't insert magic quotes right away.
          TeX-quote-after-quote t
          ;; Don't ask for confirmation when cleaning
          TeX-clean-confirm nil
          TeX-syntactic-comment t
          ;; Provide forward and inverse search with Sync TeX
          TeX-source-correlate-mode t
          TeX-source-correlate-start-server nil
          ;; Setup reftex style (RefTeX is supported through extension)
          TeX-source-correlate-method 'synctex
          ;; Don't insert line-break at inline math
          LaTeX-fill-break-at-separators nil)

    (setq-default TeX-master nil        ; Ask for the master file
                  TeX-engine 'luatex    ; Use a modern engine
                  ;; Redundant in 11.88, but keep for older AUCTeX
                  TeX-PDF-mode t)

    (when latex-enable-auto-fill
      (add-hook 'LaTeX-mode-hook 'latex/auto-fill-mode))
    (when latex-enable-folding
      (add-hook 'LaTeX-mode-hook 'TeX-fold-mode))
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode))
  :config
  (progn
    ;; Key bindings for plain TeX
    (dolist (mode '(tex-mode latex-mode))
      (dotemacs-set-leader-keys-for-major-mode mode
        "\\"  'TeX-insert-macro           ;; C-c C-m
        "-"   'TeX-recenter-output-buffer ;; C-c C-l
        "b"   'latex/build
        "k"   'TeX-kill-job               ;; C-c C-k
        "l"   'TeX-recenter-output-buffer ;; C-c C-l
        "m"   'TeX-insert-macro           ;; C-c C-m
        "v"   'TeX-view                   ;; C-c C-v
        ;; TeX-doc is a very slow function
        "hd"  'TeX-doc
        "xb"  'latex/font-bold
        "xc"  'latex/font-code
        "xe"  'latex/font-emphasis
        "xi"  'latex/font-italic
        "xr"  'latex/font-clear
        "xo"  'latex/font-oblique
        "xfc" 'latex/font-small-caps
        "xff" 'latex/font-sans-serif
        "xfr" 'latex/font-serif)

      (when dotemacs-major-mode-emacs-leader-key
        (dotemacs-set-leader-keys-for-major-mode mode
          dotemacs-major-mode-emacs-leader-key 'TeX-command-master))
      (when dotemacs-major-mode-leader-key
        (dotemacs-set-leader-keys-for-major-mode mode
          dotemacs-major-mode-leader-key 'TeX-command-master))
      (when latex-enable-folding
        (dotemacs-set-leader-keys-for-major-mode mode
          "z=" 'TeX-fold-math
          "zb" 'TeX-fold-buffer
          "ze" 'TeX-fold-env
          "zm" 'TeX-fold-macro
          "zr" 'TeX-fold-region))
      (dotemacs-declare-prefix-for-mode mode "mx"  "text/fonts")
      (dotemacs-declare-prefix-for-mode mode "mz"  "fold"))

    ;; Key bindings specific to LaTeX
    (dotemacs-set-leader-keys-for-major-mode 'latex-mode
      "*"   'LaTeX-mark-section      ;; C-c *
      "."   'LaTeX-mark-environment  ;; C-c .
      "c"   'LaTeX-close-environment ;; C-c ]
      "e"   'LaTeX-environment       ;; C-c C-e
      "i"   'LaTeX-insert-item       ;; C-c C-j
      "s"   'LaTeX-section           ;; C-c C-s
      "pb"  'preview-buffer
      "pc"  'preview-clearout
      "pd"  'preview-document
      "pe"  'preview-environment
      "pf"  'preview-cache-preamble
      "pp"  'preview-at-point
      "pr"  'preview-region
      "ps"  'preview-section
      "xB"  'latex/font-medium
      "xr"  'latex/font-clear
      "xfa" 'latex/font-calligraphic
      "xfn" 'latex/font-normal
      "xfu" 'latex/font-upright)))

(use-package tex-site                   ; AUCTeX initialization
  :ensure auctex)

(use-package tex-buf                    ; TeX buffer management
  :ensure auctex
  :defer t
  ;; Don't ask for confirmation when saving before processing
  :config (setq TeX-save-query nil))

(use-package tex-style                  ; TeX style
  :ensure auctex
  :defer t
  :config
  ;; Enable support for csquotes
  (setq LaTeX-csquotes-close-quote "}"
        LaTeX-csquotes-open-quote "\\enquote{"))

(use-package tex-fold                   ; TeX folding
  :ensure auctex
  :defer t
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(use-package tex-mode                   ; TeX mode
  :ensure auctex
  :defer t
  :config
  (font-lock-add-keywords 'latex-mode
                          `((,(rx "\\"
                                  symbol-start
                                  "fx" (1+ (or (syntax word) (syntax symbol)))
                                  symbol-end)
                             . font-lock-warning-face))))

(use-package latex                      ; LaTeX editing
  :ensure auctex
  :defer t
  :config
  (progn
    ;; Teach TeX folding about KOMA script sections
    (setq TeX-outline-extra `((,(rx (0+ space) "\\section*{") 2)
                              (,(rx (0+ space) "\\subsection*{") 3)
                              (,(rx (0+ space) "\\subsubsection*{") 4)
                              (,(rx (0+ space) "\\minisec{") 5))
          ;; No language-specific hyphens please
          LaTeX-babel-hyphen nil)
    (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)))    ; Easy math input

(use-package bibtex                     ; BibTeX editing
  :defer t
  :config
  (progn
    ;; Run prog mode hooks for bibtex
    (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

    ;; Use a modern BibTeX dialect
    (bibtex-set-dialect 'biblatex)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook LaTeX-mode)))

  (use-package company-auctex
    :defer t
    :init
    (progn
      (push 'company-auctex-labels company-backends-LaTeX-mode)
      (push 'company-auctex-bibs company-backends-LaTeX-mode)
      (push '(company-auctex-macros
              company-auctex-symbols
              company-auctex-environments) company-backends-LaTeX-mode)))

  (use-package company-math
    :ensure t
    :defer t
    :init (push 'company-math company-backends-LaTeX-mode)))

(dotemacs-use-package-add-hook evil-matchit
  :post-init
  (add-hook 'LaTeX-mode-hook `turn-on-evil-matchit-mode))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'LaTeX-mode))

(dotemacs-use-package-add-hook flyspell
  :post-init
  (spell-checking/add-flyspell-hook 'LaTeX-mode-hook))

(dotemacs-use-package-add-hook smartparens
  :post-init
  (add-hook 'LaTeX-mode-hook 'smartparens-mode))

(dotemacs-use-package-add-hook typo
  :post-init
  (defun dotemacs//disable-typo-mode ()
    (typo-mode -1))
  (add-hook 'LaTeX-mode-hook 'dotemacs//disable-typo-mode))

(dotemacs-use-package-add-hook yasnippet
  :post-init
  (add-hook 'LaTeX-mode-hook 'dotemacs-load-yasnippet))

(dotemacs-use-package-add-hook which-key
  :post-init
  (push '("\\`latex/font-\\(.+\\)\\'" . "\\1") which-key-description-replacement-alist))

(provide 'module-latex)
;;; module-latex.el ends here
