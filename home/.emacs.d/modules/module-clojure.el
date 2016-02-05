;;; module-clojure.el --- Clojure Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-vars)
;; (require 'core-funcs)
(require 'evil-evilified-state)
(require 'core-auto-completion)
(require 'core-keybindings)
;; (require 'core-display-init)
(require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(dotemacs-defvar-company-backends cider-mode)
(dotemacs-defvar-company-backends cider-repl-mode)

(defvar clojure-enable-fancify-symbols t
  "If non nil the `fancify-symbols' function is enabled.")

(setq clojure/key-binding-prefixes '(("md" . "debug")
                                     ("me" . "evaluation")
                                     ("mg" . "goto")
                                     ("mh" . "documentation")
                                     ("mr" . "refactor")
                                     ("mra" . "add")
                                     ("mrc" . "cycle/clean")
                                     ("mrd" . "destructure")
                                     ("mre" . "extract/expand")
                                     ("mrf" . "find/function")
                                     ("mrh" . "hotload")
                                     ("mri" . "introduce/inline")
                                     ("mrm" . "move")
                                     ("mrp" . "project/promote")
                                     ("mrr" . "remove/rename/replace")
                                     ("mrs" . "show/sort/stop")
                                     ("mrt" . "thread")
                                     ("mru" . "unwind/update")
                                     ("ms" . "repl")
                                     ("mt" . "test")
                                     ("mT" . "toggle")
                                     ("mf" . "format")))
(dolist (mode '(clojure-mode
                clojurec-mode
                clojurescript-mode
                clojurex-mode
                cider-repl-mode))
  (mapc (lambda (x) (dotemacs-declare-prefix-for-mode
                     mode (car x) (cdr x)))
        clojure/key-binding-prefixes))

(defun clojure/fancify-symbols (mode)
  "Pretty symbols for Clojure's anonymous functions and sets,
   like (λ [a] (+ a 5)), ƒ(+ % 5), and ∈{2 4 6}."
  (font-lock-add-keywords mode
    `(("(\\(fn\\)[\[[:space:]]"
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1) "λ"))))
      ("(\\(partial\\)[\[[:space:]]"
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1) "Ƥ"))))
      ("(\\(comp\\)[\[[:space:]]"
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1) "∘"))))
      ("\\(#\\)("
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1) "ƒ"))))
      ("\\(#\\){"
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1) "∈")))))))

(defun dotemacs//cider-eval-in-repl-no-focus (form)
  "Insert FORM in the REPL buffer and eval it."
  (while (string-match "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" form)
    (setq form (replace-match "" t t form)))
  (with-current-buffer (cider-current-repl-buffer)
    (let ((pt-max (point-max)))
      (goto-char pt-max)
      (insert form)
      (indent-region pt-max (point))
      (cider-repl-return))))

(defun dotemacs/cider-send-last-sexp-to-repl ()
  "Send last sexp to REPL and evaluate it without changing the focus."
  (interactive)
  (dotemacs//cider-eval-in-repl-no-focus (cider-last-sexp)))

(defun dotemacs/cider-send-last-sexp-to-repl-focus ()
  "Send last sexp to REPL and evaluate it and switch to the REPL in `insert state'."
  (interactive)
  (cider-insert-last-sexp-in-repl t)
  (evil-insert-state))

(defun dotemacs/cider-send-region-to-repl (start end)
  "Send region to REPL and evaluate it without changing the focus."
  (interactive "r")
  (dotemacs//cider-eval-in-repl-no-focus
   (buffer-substring-no-properties start end)))

(defun dotemacs/cider-send-region-to-repl-focus (start end)
  "Send region to REPL and evaluate it and switch to the REPL in `insert state'."
  (interactive "r")
  (cider-insert-in-repl
   (buffer-substring-no-properties start end) t)
  (evil-insert-state))

(defun dotemacs/cider-send-function-to-repl ()
  "Send current function to REPL and evaluate it without changing the focus."
  (interactive)
  (dotemacs//cider-eval-in-repl-no-focus (cider-defun-at-point)))

(defun dotemacs/cider-send-function-to-repl-focus ()
  "Send current function to REPL and evaluate it and switch to the REPL in `insert state'."
  (interactive)
  (cider-insert-defun-in-repl t)
  (evil-insert-state))

(defun dotemacs/cider-send-ns-form-to-repl ()
  "Send buffer's ns form to REPL and evaluate it without changing the focus."
  (interactive)
  (dotemacs//cider-eval-in-repl-no-focus (cider-ns-form)))

(defun dotemacs/cider-send-ns-form-to-repl-focus ()
  "Send ns form to REPL and evaluate it and switch to the REPL in `insert state'."
  (interactive)
  (cider-insert-ns-form-in-repl t)
  (evil-insert-state))

(defun dotemacs/cider-send-buffer-in-repl-and-focus ()
  "Send the current buffer in the REPL and switch to the REPL in `insert state'."
  (interactive)
  (cider-load-buffer)
  (cider-switch-to-repl-buffer)
  (evil-insert-state))

(defun dotemacs/cider-test-run-focused-test ()
  (interactive)
  (cider-load-buffer)
  (dotemacs//cider-eval-in-repl-no-focus (cider-test-run-test)))

(defun dotemacs/cider-test-run-all-tests ()
  (interactive)
  (cider-load-buffer)
  (dotemacs//cider-eval-in-repl-no-focus (cider-test-run-tests nil)))

(defun dotemacs/cider-test-rerun-tests ()
  (interactive)
  (cider-load-buffer)
  (dotemacs//cider-eval-in-repl-no-focus (cider-test-rerun-tests)))

(defun dotemacs/cider-display-error-buffer (&optional arg)
  "Displays the *cider-error* buffer in the current window. If called with a prefix argument, uses the other-window instead."
  (interactive "P")
  (let ((buffer (get-buffer cider-error-buffer)))
    (when buffer
      (funcall (if (equal arg '(4))
                   'switch-to-buffer-other-window
                 'switch-to-buffer)
               buffer))))

(defun dotemacs/cider-toggle-repl-pretty-printing ()
  (interactive)
  (setq cider-repl-use-pretty-printing
        (if cider-repl-use-pretty-printing nil t))
  (message "Cider REPL pretty printing: %s"
           (if cider-repl-use-pretty-printing "ON" "OFF")))

(defun dotemacs/cider-toggle-repl-font-locking ()
  (interactive)
  (setq cider-repl-use-clojure-font-lock
        (if cider-repl-use-pretty-printing nil t))
  (message "Cider REPL clojure-mode font-lock: %s"
           (if cider-repl-use-clojure-font-lock "ON" "OFF")))

(use-package align-cljlet
  :defer t
  :ensure t
  :init
  (add-hook 'clojure-mode-hook (lambda () (require 'align-cljlet)))
  :config
  (dotemacs-set-leader-keys-for-major-mode 'clojure-mode
    "fl" 'align-cljlet))

(use-package cider
  :defer t
  :init
  (progn
    (dotemacs-register-repl 'cider 'cider-jack-in "cider")
    (setq cider-stacktrace-default-filters '(tooling dup)
          cider-repl-pop-to-buffer-on-connect nil
          cider-prompt-save-file-on-load nil
          cider-repl-use-clojure-font-lock t)
    (push "\\*cider-repl\.\+\\*" dotemacs-useful-buffers-regexp)
    (add-hook 'clojure-mode-hook 'cider-mode)
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (if dotemacs-smartparens-strict-mode
        (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)))
  :config
  (progn
    ;; add support for golden-ratio
    ;; (with-eval-after-load 'golden-ratio
    ;;   (push 'cider-popup-buffer-quit-function golden-ratio-extra-commands))
    ;; add support for evil
    (evil-set-initial-state 'cider-stacktrace-mode 'motion)
    (evil-set-initial-state 'cider-popup-buffer-mode 'motion)

    (defun dotemacs/cider-debug-setup ()
      (evil-make-overriding-map cider--debug-mode-map 'normal)
      (evil-normalize-keymaps))

    (add-hook 'cider--debug-mode-hook 'dotemacs/cider-debug-setup)

    (evilified-state-evilify cider-stacktrace-mode cider-stacktrace-mode-map
      (kbd "C-j") 'cider-stacktrace-next-cause
      (kbd "C-k") 'cider-stacktrace-previous-cause
      (kbd "TAB") 'cider-stacktrace-cycle-current-cause
      (kbd "0")   'cider-stacktrace-cycle-all-causes
      (kbd "1")   'cider-stacktrace-cycle-cause-1
      (kbd "2")   'cider-stacktrace-cycle-cause-2
      (kbd "3")   'cider-stacktrace-cycle-cause-3
      (kbd "4")   'cider-stacktrace-cycle-cause-4
      (kbd "5")   'cider-stacktrace-cycle-cause-5
      (kbd "a")   'cider-stacktrace-toggle-all
      (kbd "c")   'cider-stacktrace-toggle-clj
      (kbd "d")   'cider-stacktrace-toggle-duplicates
      (kbd "J")   'cider-stacktrace-toggle-java
      (kbd "r")   'cider-stacktrace-toggle-repl
      (kbd "T")   'cider-stacktrace-toggle-tooling)

    ;; open cider-doc directly and close it with q
    (setq cider-prompt-for-symbol nil)

    (evilified-state-evilify cider-docview-mode cider-docview-mode-map
      (kbd "q") 'cider-popup-buffer-quit)

    (evilified-state-evilify cider-inspector-mode cider-inspector-mode-map
      (kbd "L") 'cider-inspector-pop
      (kbd "n") 'cider-inspector-next-page
      (kbd "N") 'cider-inspector-previous-page
      (kbd "r") 'cider-inspector-refresh)

    (evilified-state-evilify cider-test-report-mode cider-test-report-mode-map
      (kbd "C-j") 'cider-test-next-result
      (kbd "C-k") 'cider-test-previous-result
      (kbd "RET") 'cider-test-jump
      (kbd "d")   'cider-test-ediff
      (kbd "e")   'cider-test-stacktrace
      (kbd "q")   'cider-popup-buffer-quit
      (kbd "r")   'cider-test-rerun-tests
      (kbd "t")   'cider-test-run-test
      (kbd "T")   'cider-test-run-tests)

    ;; TODO: having this work for cider-macroexpansion-mode would be nice,
    ;;       but the problem is that it uses clojure-mode as its major-mode

    (dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
      (dotemacs-set-leader-keys-for-major-mode m
        "hh" 'cider-doc
        "hg" 'cider-grimoire
        "hj" 'cider-javadoc

        "eb" 'cider-eval-buffer
        "ee" 'cider-eval-last-sexp
        "ef" 'cider-eval-defun-at-point
        "er" 'cider-eval-region
        "ew" 'cider-eval-last-sexp-and-replace

        "fb" 'cider-format-buffer

        "gb" 'cider-jump-back
        "ge" 'cider-jump-to-compilation-error
        "gg" 'cider-find-var
        "gr" 'cider-jump-to-resource

        "'"  'cider-jack-in
        "sb" 'cider-load-buffer
        "sB" 'dotemacs/cider-send-buffer-in-repl-and-focus
        "sc" 'cider-connect
        "se" 'dotemacs/cider-send-last-sexp-to-repl
        "sE" 'dotemacs/cider-send-last-sexp-to-repl-focus
        "sf" 'dotemacs/cider-send-function-to-repl
        "sF" 'dotemacs/cider-send-function-to-repl-focus
        "si" 'cider-jack-in
        "sI" 'cider-jack-in-clojurescript
        "sn" 'dotemacs/cider-send-ns-form-to-repl
        "sN" 'dotemacs/cider-send-ns-form-to-repl-focus
        "sq" 'cider-quit
        "sr" 'dotemacs/cider-send-region-to-repl
        "sR" 'dotemacs/cider-send-region-to-repl-focus
        "ss" 'cider-switch-to-repl-buffer
        "sx" 'cider-refresh

        "Tf" 'dotemacs/cider-toggle-repl-font-locking
        "Tp" 'dotemacs/cider-toggle-repl-pretty-printing

        "ta" 'dotemacs/cider-test-run-all-tests
        "tr" 'dotemacs/cider-test-rerun-tests
        "tt" 'dotemacs/cider-test-run-focused-test

        "db" 'cider-debug-defun-at-point
        "de" 'dotemacs/cider-display-error-buffer
        "di" 'cider-inspect))

    (dotemacs-set-leader-keys-for-major-mode 'cider-repl-mode
      "hh" 'cider-doc
      "hg" 'cider-grimoire
      "hj" 'cider-javadoc

      "ee" 'cider-eval-last-sexp
      "ef" 'cider-eval-defun-at-point
      "er" 'cider-eval-region
      "ew" 'cider-eval-last-sexp-and-replace

      "gb" 'cider-jump-back
      "ge" 'cider-jump-to-compilation-error
      "gg" 'cider-find-var
      "gr" 'cider-jump-to-resource

      "sc" 'cider-repl-clear-buffer
      "sn" 'cider-repl-set-ns
      "sq" 'cider-quit
      "ss" 'cider-switch-to-last-clojure-buffer
      "sx" 'cider-refresh

      "Tf" 'dotemacs/cider-toggle-repl-font-locking
      "Tp" 'dotemacs/cider-toggle-repl-pretty-printing

      "de" 'dotemacs/cider-display-error-buffer
      "di" 'cider-inspect)

    (evil-define-key 'normal cider-repl-mode-map
      "C-j" 'cider-repl-next-input
      "C-k" 'cider-repl-previous-input)

    (when clojure-enable-fancify-symbols
      (clojure/fancify-symbols 'cider-repl-mode)))

  ;; evil-jumper
  (defadvice cider-jump-to-var (before add-evil-jump activate)
    (evil-set-jump)))

(with-eval-after-load 'eval-sexp-fu
  (require 'cider-eval-sexp-fu))

(use-package clj-refactor
  :defer t
  :init
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  :config
  (progn
    (cljr-add-keybindings-with-prefix "C-c C-f")

    (dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
      (dolist (r cljr--all-helpers)
        (let* ((binding (car r))
               (func (car (cdr r))))
          (when (not (string-prefix-p "hydra" (symbol-name func)))
            (dotemacs-set-leader-keys-for-major-mode m (concat "r" binding) func)))))

    (dotemacs-set-leader-keys-for-major-mode 'cider-repl-mode
      "r?"  'cljr-describe-refactoring
      "rap" 'cljr-add-project-dependency
      "ras" 'cljr-add-stubs
      "rcc" 'cljr-cycle-coll
      "rci" 'cljr-cycle-if
      "rcp" 'cljr-cycle-privacy
      "rdk" 'cljr-destructure-keys
      "rel" 'cljr-expand-let
      "rfu" 'cljr-find-usages
      "rhd" 'cljr-hotload-dependency
      "ril" 'cljr-introduce-let
      "rml" 'cljr-move-to-let
      "rpc" 'cljr-project-clean
      "rrl" 'cljr-remove-let
      "rsp" 'cljr-sort-project-dependencies
      "rsc" 'cljr-show-changelog
      "rtf" 'cljr-thread-first-all
      "rth" 'cljr-thread
      "rtl" 'cljr-thread-last-all
      "rua" 'cljr-unwind-all
      "rup" 'cljr-update-project-dependencies
      "ruw" 'cljr-unwind)))

(use-package clojure-mode
  :defer t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
    (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode)))
  :config
  (progn

    (defun dotemacs/clojure-mode-toggle-default-indent-style ()
      (interactive)
      (setq clojure-defun-style-default-indent
            (if clojure-defun-style-default-indent nil t))
      (message "Clojure-mode default indent style: %s"
               (if clojure-defun-style-default-indent "ON" "OFF")))

    (dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
      (dotemacs-set-leader-keys-for-major-mode m
        "Ti" 'dotemacs/clojure-mode-toggle-default-indent-style
        "fl" 'clojure-align))

    (when clojure-enable-fancify-symbols
      (dolist (m '(clojure-mode clojurescript-mode clojurec-mode clojurex-mode))
        (clojure/fancify-symbols m)))))

(dotemacs-use-package-add-hook popwin
  :post-config
  (push '("*cider-error*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
        popwin:special-display-config)
  (push '("*cider-doc*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
        popwin:special-display-config))

(dotemacs-use-package-add-hook rainbow-delimiters
  :post-init
  (add-hook 'cider-mode-hook 'rainbow-delimiters-mode))

(unless (version< emacs-version "24.4")
  (add-hook 'cider-mode-hook 'subword-mode))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (push 'company-capf company-backends-cider-mode)
      (dotemacs-add-company-hook cider-mode)

      (push 'company-capf company-backends-cider-repl-mode)
      (dotemacs-add-company-hook cider-repl-mode))))

(provide 'module-clojure)
;;; module-clojure.el ends here
