;;; SmartParens

;;; Commentary:

;; Personal smartparens configuration.
;;
;; Extends the default `smartparens-config' with key-bindings and
;; additional pairs.

;;; Code:

(require 'module-global)

;; enable smartparens-mode in `eval-expression'
(defun dotemacs-conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (smartparens-mode)))

(defun dotemacs-smartparens-pair-newline (id action context)
  (save-excursion
    (newline)
    (indent-according-to-mode)))

(defun dotemacs-smartparens-pair-newline-and-indent (id action context)
  (dotemacs-smartparens-pair-newline id action context)
  (indent-according-to-mode))

(defun dotemacs-sp-kill-surrounding-sexp ()
  "Kill from beginning to end of sexp."
  (interactive)
  (kill-region (progn (sp-beginning-of-sexp)
                 (1- (point)))
    (progn (sp-end-of-sexp)
      (1+  (point)))))

(defun dotemacs-gfm-skip-asterisk (ms mb me)
  (save-excursion
    (goto-char mb)
    (save-match-data (looking-at "^\\* "))))

(defun dotemacs-org-skip-asterisk (ms mb me)
  (or (and (= (line-beginning-position) mb)
           (eq 32 (char-after (1+ mb))))
      (and (= (1+ (line-beginning-position)) me)
           (eq 32 (char-after me)))))

(defun dotemacs-after-symbol-p (_id action _context)
  (when (eq action 'insert)
    (save-excursion
      (backward-char 1)
      (looking-back "\\sw\\|\\s_\\|\\s'"))))

(defun dotemacs-php-handle-docstring (&rest _ignored)
  (-when-let (line (save-excursion
                     (forward-line)
                     (thing-at-point 'line)))
    (cond
     ((string-match-p "function" line)
      (save-excursion
        (insert "\n")
        (let ((args (save-excursion
                      (forward-line)
                      (my-php-get-function-args))))
          (--each args
            (insert (format "* @param %s\n" it)))))
      (insert "* "))
     ((string-match-p ".*class\\|interface" line)
      (save-excursion (insert "\n*\n* @author\n"))
      (insert "* ")))
    (let ((o (sp--get-active-overlay)))
      (indent-region (overlay-start o) (overlay-end o)))))

;; Use SmartParens instead of Paredit and Electric Pair
(use-package smartparens                ; Parenthesis editing and balancing
  :ensure t
  :defer t
  :commands (sp-split-sexp sp-newline)
  :init
  (progn
    (dotemacs/add-to-hooks (if dotemacs-smartparens-strict-mode
                                'smartparens-strict-mode
                              'smartparens-mode)
                            '(prog-mode-hook))

    (add-hook 'minibuffer-setup-hook 'dotemacs-conditionally-enable-smartparens-mode)

    ;; TODO move these hooks into their layers
    ;; (dolist (hook '(LaTeX-mode-hook web-moode-hook inferior-python-mode-hook))
    ;;   (add-hook hook #'smartparens-mode))

    (dotemacs-add-toggle smartparens
      :status smartparens-mode
      :on (smartparens-mode)
      :off (smartparens-mode -1)
      :documentation "Enable smartparens."
      :evil-leader "tp")

    (dotemacs-add-toggle smartparens-globally
      :status smartparens-mode
      :on (smartparens-global-mode)
      :off (smartparens-global-mode -1)
      :documentation "Enable smartparens globally."
      :evil-leader "t C-p")

    (setq sp-show-pair-delay 0.2
          sp-autoskip-closing-pair 'always ; https://github.com/Fuco1/smartparens/issues/142
          ; fix paren highlighting in normal mode
          sp-show-pair-from-inside t
          sp-cancel-autoskip-on-backward-movement nil)

    (dotemacs-set-leader-keys
     "J"  'sp-split-sexp
     "jj" 'sp-newline))
  :config
  (progn
    (require 'smartparens-config)
    (dotemacs-diminish smartparens-mode " ⓟ" " p")

    (show-smartparens-global-mode +1)

    ;;; Additional pairs for various modes
    (sp-with-modes '(php-mode)
      (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
                                                 (dotemacs-php-handle-docstring "RET")))
      (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
      (sp-local-pair "(" nil :prefix "\\(\\sw\\|\\s_\\)*"))

    (sp-with-modes '(scala-mode)
      (sp-local-pair "'" nil :actions nil))

    (sp-with-modes '(text-mode)
      (sp-local-pair "`" "'" :actions '(insert wrap)))

    (sp-with-modes '(racket-mode)
      (sp-local-pair "'" nil :actions nil)
      (sp-local-pair "`" nil :actions nil))

    ;; TODO research sp-local-tag or evil-surround with modes
    (sp-with-modes '(tex-mode
                     plain-tex-mode
                     latex-mode)
      ; (sp-local-tag "i" "\"<" "\">")
      (sp-local-pair "$" " $")
      (sp-local-pair "\\[" " \\]")
      (sp-local-pair "\\(" " \\)")
      (sp-local-pair "\\{" " \\}")
      (sp-local-pair "\\left(" " \\right)")
      (sp-local-pair "\\left\\{" " \\right\\}"))

    (sp-with-modes 'org-mode
      (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'dotemacs-org-skip-asterisk)
      (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
      (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "«" "»"))

    (sp-with-modes '(markdown-mode
                     gfm-mode
                     rst-mode)
      ; (sp-local-tag "2" "**" "**")
      ; (sp-local-tag "s" "```scheme" "```")
      ; (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags)
      (sp-local-pair "*" "*" :wrap "C-*" :skip-match 'dotemacs-gfm-skip-asterisk)
      (sp-local-pair "_" "_" :wrap "C-_"))

    (sp-with-modes '(rust-mode)
      ;; Don't pair lifetime specifiers
      (sp-local-pair "'" nil :actions nil))

    (sp-with-modes '(malabar-mode c++-mode)
      (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))

    (sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
                                                        ("* ||\n[i]" "RET")))
    (sp-with-modes '(haskell-mode)
      (sp-local-pair "'" nil :unless '(dotemacs-after-symbol-p))
      (sp-local-pair "\\(" nil :actions nil))

    ;; Emacs Lisp
    (sp-with-modes '(emacs-lisp-mode
                     inferior-emacs-lisp-mode
                     lisp-interaction-mode
                     lisp-mode)
      (sp-local-pair "'" nil :actions nil)
      (sp-local-pair "(" nil :bind "M-(")
      (sp-local-pair "`" "'" :when '(sp-in-string-p) :actions '(insert wrap)))

    ;; don't create a pair with single quote in minibuffer
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

    (sp-pair "{" nil :post-handlers
             '(:add (dotemacs-smartparens-pair-newline-and-indent "RET")))
    (sp-pair "[" nil :post-handlers
             '(:add (dotemacs-smartparens-pair-newline-and-indent "RET")))))

(provide 'module-smartparens)
;;; module-smartparens.el ends here
