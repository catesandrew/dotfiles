;;; LaTeX with AUCTeX
(require 'module-global)

(dotemacs-defvar-company-backends LaTeX-mode)

;; latex settings
(defgroup dotemacs-latex nil
  "Configuration options for latex."
  :group 'dotemacs
  :prefix 'dotemacs-latex)

(defcustom dotemacs-latex-build-command (if (executable-find "latexmk") "LatexMk" "LaTeX")
  "The default command to use with `, m b'"
  :group 'dotemacs-latex)

(defcustom dotemacs-latex-enable-auto-fill t
  "Whether to use auto-fill-mode or not in tex files."
  :group 'dotemacs-latex)

(defcustom dotemacs-latex-nofill-env '("equation"
                                       "equation*"
                                       "align"
                                       "align*"
                                       "tabular"
                                       "tikzpicture")
  "List of environment names in which `auto-fill-mode' will be inhibited."
  :group 'dotemacs-latex)

(defun latex/build ()
  (interactive)
  (progn
    (let ((TeX-save-query nil))
      (TeX-save-document (TeX-master-file)))
    (TeX-command dotemacs-latex-build-command 'TeX-master-file -1)))
    ;; (setq build-proc (TeX-command dotemacs-latex-build-command 'TeX-master-file -1))
    ;; ;; Sometimes, TeX-command returns nil causing an error in set-process-sentinel
    ;; (when build-proc
    ;;   (set-process-sentinel build-proc 'latex//build-sentinel))))

(defun latex//build-sentinel (process event)
  (if (string= event "finished\n")
      (TeX-view)
    (message "Errors! Check with C-`")))

(defun latex//autofill ()
  "Check whether the pointer is ucrrently inside on the
environments described in `dotemacs-latex-nofill-env' and if so, inhibits
the automatic filling of the current paragraph."
  (let ((do-auto-fill t)
        (current-environment "")
        (level 0))
    (while (and do-auto-fill (not (string= current-environment "document")))
      (setq level (1+ level)
            current-environment (LaTeX-current-environment level)
            do-auto-fill (not (member current-environment dotemacs-latex-nofill-env))))
    (when do-auto-fill
      (do-auto-fill))))

(defun latex/auto-fill-mode ()
  "Toggle uato-fill-mode using the custom auto-fill function."
  (interactive)
  (auto-fill-mode)
  (setq auto-fill-function 'latex//autofill))

(defun dotemacs-reftex-find-ams-environment-caption (environment)
  "Find the caption of an AMS ENVIRONMENT."
  (let ((re (rx-to-string `(and "\\begin{" ,environment "}"))))
    ;; Go to the beginning of the label first
    (re-search-backward re)
    (goto-char (match-end 0)))
  (if (not (looking-at (rx (zero-or-more space) "[")))
      (error "Environment %s has no title" environment)
    (let ((beg (match-end 0)))
      ;; Move point onto the title start bracket and move over to the end,
      ;; skipping any other brackets in between, and eventually extract the text
      ;; between the brackets
      (goto-char (1- beg))
      (forward-list)
      (buffer-substring-no-properties beg (1- (point))))))

(use-package tex-site                   ; AUCTeX initialization
  :ensure auctex)

(use-package tex                        ; TeX editing/processing
  :ensure auctex
  :defer t
  :init
  (progn
    (setq TeX-command-default dotemacs-latex-build-command
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
          ;; Provide forward and inverse search with SyncTeX
          TeX-source-correlate-mode t
          TeX-source-correlate-start-server nil
          ;; Setup reftex style (RefTeX is supported through extension)
          reftex-use-fonts t
          TeX-source-correlate-method 'synctex
          ;; Don't insert line-break at inline math
          LaTeX-fill-break-at-separators nil)

    (setq-default TeX-master nil        ; Ask for the master file
                  TeX-engine 'luatex    ; Use a modern engine
                  ;; Redundant in 11.88, but keep for older AUCTeX
                  TeX-PDF-mode t)

    (when dotemacs-latex-enable-auto-fill
      (add-hook 'LaTeX-mode-hook 'latex/auto-fill-mode))
    (add-hook 'LaTeX-mode-hook 'latex-math-mode))
  :config
  (progn
    ;; Move to chktex
    (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s")

    ;; Key bindings for plain TeX
    (dotemacs-set-leader-keys-for-major-mode 'tex-mode
      "\\" 'TeX-insert-macro
      "b" 'latex/build
      "C" 'TeX-command-master
      ;; Find a way to rebind tex-fonts
      "f" 'TeX-font
      "v" 'TeX-view)

    ;; Key bindings for LaTeX
    (dotemacs-set-leader-keys-for-major-mode 'latex-mode
      "\\" 'TeX-insert-macro
      "b" 'latex/build
      "c" 'LaTeX-close-environment
      "C" 'TeX-command-master
      "e" 'LaTeX-environment
      ;; Find a way to rebind tex-fonts
      "f" 'TeX-font
      "hd" 'TeX-doc
      "i" 'LaTeX-insert-item
      ;; TeX-doc is a very slow function
      "pb" 'preview-buffer
      "pc" 'preview-clearout
      "pd" 'preview-document
      "pe" 'preview-environment
      "pf" 'preview-cache-preamble
      "pp" 'preview-at-point
      "pr" 'preview-region
      "ps" 'preview-section
      "v" 'TeX-view)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook LaTeX-mode))))

(use-package company-math
  :if (eq dotemacs-completion-engine 'company)
  :ensure t
  :defer t
  :init (push 'company-math company-backends-LaTeX-mode))

(use-package company-auctex
  :if (eq dotemacs-completion-engine 'company)
  :defer t
  :init
  (progn
    (push 'company-auctex-labels company-backends-LaTeX-mode)
    (push 'company-auctex-bibs company-backends-LaTeX-mode)
    (push '(company-auctex-macros
            company-auctex-symbols
            company-auctex-environments) company-backends-LaTeX-mode)))

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

(use-package auctex-latexmk             ; latexmk command for AUCTeX
  :ensure t
  :if (string= dotemacs-latex-build-command "LatexMk")
  :defer t
  :init
  (progn
    (setq auctex-latexmk-inherit-TeX-PDF-mode t)
    (dotemacs-use-package-add-hook tex
      :post-config
      (auctex-latexmk-setup))))

(use-package bibtex                     ; BibTeX editing
  :defer t
  :config
  (progn
    ;; Run prog mode hooks for bibtex
    (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

    ;; Use a modern BibTeX dialect
    (bibtex-set-dialect 'biblatex)))

(use-package reftex                     ; TeX/BibTeX cross-reference management
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
  :config
  (progn
    (setq reftex-plug-into-AUCTeX '(nil nil t t t))

    (dotemacs-set-leader-keys-for-major-mode 'latex-mode
      "rc"    'reftex-citation
      "rg"    'reftex-grep-document
      "ri"    'reftex-index-selection-or-word
      "rI"    'reftex-display-index
      "r C-i" 'reftex-index
      "rl"    'reftex-label
      "rp"    'reftex-index-phrase-selection-or-word
      "rP"    'reftex-index-visit-phrases-buffer
      "rr"    'reftex-reference
      "rs"    'reftex-search-document
      "rt"    'reftex-toc
      "rT"    'reftex-toc-recenter
      "rv"    'reftex-view-crossref)

    ;; Plug into AUCTeX
    (setq reftex-plug-into-AUCTeX t
          ;; Automatically derive labels, and prompt for confirmation
          reftex-insert-label-flags '(t t)
          reftex-label-alist
          '(
            ;; Additional label definitions for RefTeX.
            ("definition" ?d "def:" "~\\ref{%s}"
             dotemacs-reftex-find-ams-environment-caption
             ("definition" "def.") -3)
            ("theorem" ?h "thm:" "~\\ref{%s}"
             dotemacs-reftex-find-ams-environment-caption
             ("theorem" "th.") -3)
            ("example" ?x "ex:" "~\\ref{%s}"
             dotemacs-reftex-find-ams-environment-caption
             ("example" "ex") -3)
            ;; Algorithms package
            ("algorithm" ?a "alg:" "~\\ref{%s}"
             "\\\\caption[[{]" ("algorithm" "alg") -3)))

    ;; Provide basic RefTeX support for biblatex
    (unless (assq 'biblatex reftex-cite-format-builtin)
      (add-to-list 'reftex-cite-format-builtin
                   '(biblatex "The biblatex package"
                              ((?\C-m . "\\cite[]{%l}")
                               (?t . "\\textcite{%l}")
                               (?a . "\\autocite[]{%l}")
                               (?p . "\\parencite{%l}")
                               (?f . "\\footcite[][]{%l}")
                               (?F . "\\fullcite[]{%l}")
                               (?x . "[]{%l}")
                               (?X . "{%l}"))))
      (setq reftex-cite-format 'biblatex)))
  :diminish reftex-mode)

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'LaTeX-mode))

(dotemacs-use-package-add-hook flyspell
  :post-init
  (spell-checking/add-flyspell-hook 'LaTeX-mode))

(dotemacs-use-package-add-hook yasnippet
  :post-init
  (add-hook 'LaTeX-mode-hook 'dotemacs-load-yasnippet))

(provide 'module-latex)
;;; module-latex.el ends here
