;;; module-scala.el --- Scala Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'use-package)
(require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
(require 'module-utils)

;;; Code:

(defvar scala-enable-eldoc nil
  "If non nil then eldoc-mode is enabled in the scala layer.")

(defvar scala-auto-insert-asterisk-in-comments nil
  "If non-nil automatically insert leading asterisk in multi-line comments.")

(defvar scala-use-unicode-arrows t
  "If non-nil then `->`, `=>` and `<-` are replaced with unicode arrows.")

(autoload 'ensime-config-find-file "ensime-config")
(autoload 'ensime-config-find "ensime-config")
(autoload 'projectile-project-p "projectile")

(defun scala/configure-ensime ()
  "Ensure the file exists before starting `ensime-mode'."
  (cond
   ((and (buffer-file-name) (file-exists-p (buffer-file-name)))
    (ensime-mode +1))
   ((buffer-file-name)
    (add-hook 'after-save-hook (lambda () (ensime-mode +1)) nil t))))

(defun scala/maybe-start-ensime ()
  (when (buffer-file-name)
    (let ((ensime-buffer (scala/ensime-buffer-for-file (buffer-file-name)))
          (file (ensime-config-find-file (buffer-file-name)))
          (is-source-file (s-matches? (rx (or "/src/" "/test/")) (buffer-file-name))))

      (when (and is-source-file (null ensime-buffer))
        (noflet ((ensime-config-find (&rest _) file))
          (save-window-excursion
            (ensime)))))))

(defun scala/ensime-buffer-for-file (file)
  "Find the Ensime server buffer corresponding to FILE."
  (let ((default-directory (file-name-directory file)))
    (-when-let (project-name (projectile-project-p))
      (--first (-when-let (bufname (buffer-name it))
                 (and (s-contains? "inferior-ensime-server" bufname)
                      (s-contains? (file-name-nondirectory project-name) bufname)))
               (buffer-list)))))

(defun scala/enable-eldoc ()
  (setq-local eldoc-documentation-function
              (lambda ()
                (when (ensime-connected-p)
                  (ensime-print-type-at-point))))
  (eldoc-mode +1))

(defun dotemacs/ensime-refactor-accept ()
  (interactive)
  (funcall continue-refactor)
  (ensime-popup-buffer-quit-function))

(defun dotemacs/ensime-refactor-cancel ()
  (interactive)
  (funcall cancel-refactor)
  (ensime-popup-buffer-quit-function))

;;; Interactive commands

(defun dotemacs/scala-join-line ()
  "Adapt `scala-indent:join-line' to behave more like evil's line join.

`scala-indent:join-line' acts like the vanilla `join-line',
joining the current line with the previous one. The vimmy way is
to join the current line with the next.

Try to move to the subsequent line and then join. Then manually move
point to the position of the join."
  (interactive)
  (let (join-pos)
    (save-excursion
      (goto-char (line-end-position))
      (unless (eobp)
        (forward-line)
        (call-interactively 'scala-indent:join-line)
        (setq join-pos (point))))

    (when join-pos
      (goto-char join-pos))))

(defun scala/completing-dot ()
  "Insert a period and show company completions."
  (interactive "*")
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t))

  (cond (company-backend
         (company-complete-selection)
         (scala/completing-dot))

        (t
         (insert ".")
         (company-complete))))

;; flyspell

(defun scala/flyspell-verify ()
  "Prevent common flyspell false positives in scala-mode."
  (and (flyspell-generic-progmode-verify)
       (not (s-matches? (rx bol (* space) "package") (current-line)))))

(defun scala/configure-flyspell ()
  (setq-local flyspell-generic-check-word-predicate 'scala/flyspell-verify))

;; packages

(use-package ensime                     ; Scala interaction mode
  :ensure t
  :commands (ensime-mode)
  :init
  (progn
    (dotemacs-register-repl 'ensime 'ensime-inf-switch "ensime")
    (when scala-enable-eldoc
      (add-hook 'ensime-mode-hook 'scala/enable-eldoc))
    (add-hook 'scala-mode-hook 'scala/configure-flyspell)
    (add-hook 'scala-mode-hook 'scala/configure-ensime)
    (add-hook 'scala-mode-hook 'scala/maybe-start-ensime))
  :config
  (progn
    (setq user-emacs-ensime-directory ".cache/ensime")

    (evil-define-key 'insert ensime-mode-map
      (kbd ".") 'scala/completing-dot
      (kbd "M-.") 'ensime-edit-definition
      (kbd "M-,") 'ensime-pop-find-definition-stack)

    (evil-define-key 'normal ensime-mode-map
      (kbd "M-.") 'ensime-edit-definition
      (kbd "M-,") 'ensime-pop-find-definition-stack)

    (evil-define-key 'normal ensime-popup-buffer-map
      (kbd "q") 'ensime-popup-buffer-quit-function)

    (evil-define-key 'normal ensime-inspector-mode-map
      (kbd "q") 'ensime-popup-buffer-quit-function)

    (evil-define-key 'normal ensime-refactor-info-map
      (kbd "q") 'dotemacs/ensime-refactor-cancel
      (kbd "c") 'dotemacs/ensime-refactor-accept
      (kbd "RET") 'dotemacs/ensime-refactor-accept)

    (evil-define-key 'normal ensime-compile-result-map
      (kbd "g") 'ensime-show-all-errors-and-warnings
      (kbd "TAB") 'forward-button
      (kbd "<backtab>") 'backward-button
      (kbd "M-n") 'forward-button
      (kbd "M-p") 'backward-button
      (kbd "n") 'forward-button
      (kbd "N") 'backward-button)

    (defun ensime-gen-and-restart()
      "Regenerate `.ensime' file and restart the ensime server."
      (interactive)
      (progn
        (sbt-command "gen-ensime")
        (ensime-shutdown)
        (ensime)))

    (defun ensime-inf-eval-buffer-switch ()
      "Send buffer content to shell and switch to it in insert mode."
      (interactive)
      (ensime-inf-eval-buffer)
      (ensime-inf-switch)
      (evil-insert-state))

    (defun ensime-inf-eval-region-switch (start end)
      "Send region content to shell and switch to it in insert mode."
      (interactive "r")
      (ensime-inf-switch)
      (ensime-inf-eval-region start end)
      (evil-insert-state))

    (dolist (prefix '(("mb" . "scala/build")
                      ("mc" . "scala/check")
                      ("md" . "scala/debug")
                      ("me" . "scala/errors")
                      ("mg" . "scala/goto")
                      ("mh" . "scala/docs")
                      ("mi" . "scala/inspect")
                      ("mn" . "scala/ensime")
                      ("mr" . "scala/refactor")
                      ("mt" . "scala/test")
                      ("ms" . "scala/repl")))
      (dotemacs-declare-prefix-for-mode 'scala-mode (car prefix) (cdr prefix)))

    (dotemacs-set-leader-keys-for-major-mode 'scala-mode
        "/"      'ensime-search
        "'"      'ensime-inf-switch

        "bc"     'ensime-sbt-do-compile
        "bC"     'ensime-sbt-do-clean
        "bi"     'ensime-sbt-switch
        "bp"     'ensime-sbt-do-package
        "br"     'ensime-sbt-do-run

        "ct"     'ensime-typecheck-current-file
        "cT"     'ensime-typecheck-all

        "dA"     'ensime-db-attach
        "db"     'ensime-db-set-break
        "dB"     'ensime-db-clear-break
        "dC"     'ensime-db-clear-all-breaks
        "dc"     'ensime-db-continue
        "dd"     'ensime-db-start
        "di"     'ensime-db-inspect-value-at-point
        "dl"     'ensime-db-list-locals
        "dn"     'ensime-db-next
        "do"     'ensime-db-step-out
        "dq"     'ensime-db-quit
        "dr"     'ensime-db-run
        "ds"     'ensime-db-step
        "dt"     'ensime-db-backtrace

        "ee"     'ensime-print-errors-at-point
        "el"     'ensime-show-all-errors-and-warnings
        "es"     'ensime-stacktrace-switch

        "gg"     'ensime-edit-definition
        "gp"     'ensime-pop-find-definition-stack
        "gi"     'ensime-goto-impl
        "gt"     'ensime-goto-test

        "hh"     'ensime-show-doc-for-symbol-at-point
        "hu"     'ensime-show-uses-of-symbol-at-point
        "ht"     'ensime-print-type-at-point

        "ii"     'ensime-inspect-type-at-point
        "iI"     'ensime-inspect-type-at-point-other-frame
        "ip"     'ensime-inspect-project-package

        "nF"     'ensime-reload-open-files
        "ns"     'ensime
        "nS"     'ensime-gen-and-restart

        "rd"     'ensime-refactor-inline-local
        "rD"     'ensime-undo-peek
        "rf"     'ensime-format-source
        "ri"     'ensime-refactor-organize-imports
        "rm"     'ensime-refactor-extract-method
        "rr"     'ensime-refactor-rename
        "rt"     'ensime-import-type-at-point
        "rv"     'ensime-refactor-extract-local

        "ta"     'ensime-sbt-do-test-dwim
        "tr"     'ensime-sbt-do-test-quick-dwim
        "tt"     'ensime-sbt-do-test-only-dwim

        "sa"     'ensime-inf-load-file
        "sb"     'ensime-inf-eval-buffer
        "sB"     'ensime-inf-eval-buffer-switch
        "si"     'ensime-inf-switch
        "sr"     'ensime-inf-eval-region
        "sR"     'ensime-inf-eval-region-switch

        "z"      'ensime-expand-selection-command)

    ;; Don't use scala checker if ensime mode is active, since it provides
    ;; better error checking.
    (with-eval-after-load 'flycheck
      (defun scala/disable-flycheck-scala ()
        (push 'scala flycheck-disabled-checkers))

      (add-hook 'ensime-mode-hook 'scala/disable-flycheck-scala))

    ;; Enable Expand Region integration from Ensime.  Ignore load errors to
    ;; handle older Ensime versions gracefully.
    (require 'ensime-expand-region nil 'noerror)))

(use-package noflet :ensure t)

(use-package sbt-mode                   ; Scala build tool
  :ensure t
  :defer t
  :config
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'scala-mode
      "bb" 'sbt-command)))

(use-package scala-mode2                ; Scala editing
  :ensure t
  :defer t
  :init
  (dolist (ext '(".cfe" ".cfs" ".si" ".gen" ".lock"))
    (add-to-list 'completion-ignored-extensions ext))
  :config
  (progn
    ;; Automatically insert asterisk in a comment when enabled
    (defun scala/newline-and-indent-with-asterisk ()
      (interactive)
      (newline-and-indent)
      (when scala-auto-insert-asterisk-in-comments
        (scala-indent:insert-asterisk-on-multiline-comment)))

    (evil-define-key 'insert scala-mode-map
      (kbd "RET") 'scala/newline-and-indent-with-asterisk)

    ;; Automatically replace arrows with unicode ones when enabled
    (defconst scala-unicode-arrows-alist
      '(("=>" . "⇒")
        ("->" . "→")
        ("<-" . "←")))

    (defun scala/replace-arrow-at-point ()
      "Replace the arrow before the point (if any) with unicode ones.
An undo boundary is inserted before doing the replacement so that
it can be undone."
      (let* ((end (point))
             (start (max (- end 2) (point-min)))
             (x (buffer-substring start end))
             (arrow (assoc x scala-unicode-arrows-alist)))
        (when arrow
          (undo-boundary)
          (backward-delete-char 2)
          (insert (cdr arrow)))))

    (defun scala/gt ()
      "Insert a `>' to the buffer. If it's part of a right arrow (`->' or `=>'),
replace it with the corresponding unicode arrow."
      (interactive)
      (insert ">")
      (scala/replace-arrow-at-point))

    (defun scala/hyphen ()
      "Insert a `-' to the buffer. If it's part of a left arrow (`<-'),
replace it with the unicode arrow."
      (interactive)
      (insert "-")
      (scala/replace-arrow-at-point))

    (when scala-use-unicode-arrows
      (define-key scala-mode-map
        (kbd ">") 'scala/gt)
      (define-key scala-mode-map
        (kbd "-") 'scala/hyphen))

    (evil-define-key 'normal scala-mode-map "J" 'dotemacs/scala-join-line)

    ;; Compatibility with `aggressive-indent'
    (setq scala-indent:align-forms t
          scala-indent:align-parameters t
          scala-indent:default-run-on-strategy scala-indent:operator-strategy)))

(provide 'module-scala)
;;; module-scala.el ends here
