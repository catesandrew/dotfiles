(require 'smartparens-config)

; TODO go through the commented code
; (defgroup dotemacs-smartparens nil
;   "Configuration options for smartparens."
;   :group 'dotemacs
;   :prefix 'dotemacs-smartparens)
;
; (defcustom dotemacs-smartparens/autoinsert nil
;   "When non-nil, turn on smartparens auto pairing instead of the default Emacs electric-pair-mode."
;   :group 'dotemacs-smartparens)
;
; (defcustom dotemacs-smartparens/show-paren nil
;   "When non-nil, turn on smartparens paren matching instead of the default Emacs show-paren-mode."
;   :group 'dotemacs-smartparens)
;
; (setq sp-autoescape-string-quote nil)
; (setq sp-autoinsert-quote-if-followed-by-closing-pair nil)
;
; (if dotemacs-smartparens/autoinsert
;     (progn
;       (setq sp-autoinsert-pair t)
;       (electric-pair-mode -1))
;   (setq sp-autoinsert-pair nil))
;
; (sp-use-smartparens-bindings)
;
; (when dotemacs-smartparens/show-paren
;   (setq sp-show-pair-delay 0)
;   (setq sp-show-pair-from-inside t)
;   (show-paren-mode -1)
;   (show-smartparens-global-mode t))
;
; (defun my-open-block-c-mode (id action context)
;   (when (eq action 'insert)
;     (newline)
;     (indent-according-to-mode)
;     (forward-line -1)
;     (indent-according-to-mode)))
;
; (sp-pair "{" nil :post-handlers '(:add (my-open-block-c-mode "RET")))
; (sp-pair "[" nil :post-handlers '(:add (my-open-block-c-mode "RET")))
;
; ;; fix conflict where smartparens clobbers yas' key bindings
; (with-eval-after-load 'yasnippet
;   (defadvice yas-expand (before advice-for-yas-expand activate)
;     (sp-remove-active-pair-overlay)))



;;; Additional pairs for various modes

;; Emacs Lisp
(sp-local-pair '(emacs-lisp-mode lisp-interaction-mode inferior-emacs-lisp-mode)
               "(" nil :bind "M-(")


;;; My key bindings

(let ((map smartparens-mode-map))
  ;; Movement and navigation
  (define-key map (kbd "C-M-f") #'sp-forward-sexp)
  (define-key map (kbd "C-M-b") #'sp-backward-sexp)
  (define-key map (kbd "C-M-u") #'sp-backward-up-sexp)
  (define-key map (kbd "C-M-d") #'sp-down-sexp)
  (define-key map (kbd "C-M-p") #'sp-backward-down-sexp)
  (define-key map (kbd "C-M-n") #'sp-up-sexp)
  ;; Deleting and killing
  (define-key map (kbd "C-M-k") #'sp-kill-sexp)
  (define-key map (kbd "C-M-w") #'sp-copy-sexp)
  ;; Depth changing
  (define-key map (kbd "M-S-<up>") #'sp-splice-sexp)
  (define-key map (kbd "M-<up>") #'sp-splice-sexp-killing-backward)
  (define-key map (kbd "M-<down>") #'sp-splice-sexp-killing-forward)
  (define-key map (kbd "M-C-<up>") #'sp-splice-sexp-killing-around)
  (define-key map (kbd "M-?") #'sp-convolute-sexp)
  ;; Barfage & Slurpage
  (define-key map (kbd "C-)") #'sp-forward-slurp-sexp)
  (define-key map (kbd "C-<right>") #'sp-forward-slurp-sexp)
  (define-key map (kbd "C-}") #'sp-forward-barf-sexp)
  (define-key map (kbd "C-<left>") #'sp-forward-barf-sexp)
  (define-key map (kbd "C-(") #'sp-backward-slurp-sexp)
  (define-key map (kbd "C-M-<left>") #'sp-backward-slurp-sexp)
  (define-key map (kbd "C-{") #'sp-backward-barf-sexp)
  (define-key map (kbd "C-M-<right>") #'sp-backward-barf-sexp)
  ;; Miscellaneous commands
  (define-key map (kbd "M-S") #'sp-split-sexp)
  (define-key map (kbd "M-J") #'sp-join-sexp)
  (define-key map (kbd "C-M-t") #'sp-transpose-sexp))

(let ((map smartparens-strict-mode-map))
  (define-key map (kbd "M-q") #'sp-indent-defun))

(provide 'init-smartparens)
