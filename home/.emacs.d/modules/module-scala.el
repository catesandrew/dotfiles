;;; Scala
(require 'module-global)
;; (require 'flycheck)
;; (require 'ensime)

(autoload 'ensime-config-find-file "ensime-config")
(autoload 'ensime-config-find "ensime-config")
(autoload 'projectile-project-p "projectile")

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

(defun dotemacs-ensime-refactor-accept ()
  (interactive)
  (funcall continue-refactor)
  (ensime-popup-buffer-quit-function))

(defun dotemacs-ensime-refactor-cancel ()
  (interactive)
  (funcall cancel-refactor)
  (ensime-popup-buffer-quit-function))

;;; Interactive commands

(defun dotemacs-scala-join-line ()
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

(defun current-line ()
  "Return the line at point as a string."
  (buffer-substring (line-beginning-position) (line-end-position)))

;;; Flyspell
(defun scala/flyspell-verify ()
  "Prevent common flyspell false positives in scala-mode."
  (and (flyspell-generic-progmode-verify)
       (not (s-matches? (rx bol (* space) "package") (current-line)))))

(defun scala/configure-flyspell ()
  (setq-local flyspell-generic-check-word-predicate 'scala/flyspell-verify))

(use-package noflet
  :ensure t
  :defer t)

(use-package scala-mode2                ; Scala editing
  :ensure t
  :defer t
  :init
  (dolist (ext '(".cfe" ".cfs" ".si" ".gen" ".lock"))
    (add-to-list 'completion-ignored-extensions ext))
  :config
  (progn
    (evil-define-key 'normal scala-mode-map "J" 'dotemacs-scala-join-line)

    ;; Compatibility with `aggressive-indent'
    (setq scala-indent:align-forms t
          scala-indent:align-parameters t
          scala-indent:default-run-on-strategy scala-indent:operator-strategy)

    (require 'noflet)

    (defadvice scala-indent:indent-code-line (around retain-trailing-ws activate)
      "Keep trailing-whitespace when indenting."
      (noflet ((scala-lib:delete-trailing-whitespace ()))
              ad-do-it))))

(use-package sbt-mode                   ; Scala build tool
  :ensure t
  :defer t
  :config
  (progn
    (setq sbt:sbt-prompt-regexp
          (rx bol (or (and (optional "scala") ">") ; Default prompt
                      ;; Sbt Prompt plugin
                      (and "[" (1+ (not (any "]")))"] " (1+ word) ":"))
              (0+ " ")))

    (dotemacs-set-leader-keys-for-major-mode 'scala-mode
      "bb" 'sbt-command)

    (defun dotemacs-sbt-buffer-p (buffer-name &rest _)
      "Determine whether BUFFER-OR-NAME denotes an SBT buffer."
      (string-prefix-p sbt:buffer-name-base buffer-name))

    ;; Get SBT buffers under control: Display them below the current
    ;; window, at a third of the height of the current window, but try
    ;; to reuse any existing and visible window for the SBT buffer
    ;; first.
    (add-to-list 'display-buffer-alist
                 '(dotemacs-sbt-buffer-p
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (side            . bottom)
                   (reusable-frames . visible)
                   (window-height   . 0.4)))))

(defvar scala-enable-eldoc nil
  "If non nil then eldoc-mode is enabled in the scala layer")

(use-package ensime                     ; Scala interaction mode
  :ensure t
  :commands (ensime-mode)
  :init
  (progn
    (when scala-enable-eldoc
      (add-hook 'ensime-mode-hook 'scala/enable-eldoc))
    (add-hook 'scala-mode-hook 'scala/configure-flyspell)
    (add-hook 'scala-mode-hook 'scala/configure-ensime)
    (add-hook 'scala-mode-hook 'scala/maybe-start-ensime))
  :config
  (progn
    ;; Automatically open new Ensime sessions if needed
    (setq ensime-auto-connect 'always)

    (setq user-emacs-ensime-directory ".cache/ensime")

    ;; Enable Ensime for all Scala buffers.  We don't do this in :init,
    ;; because `ensime-mode' isn't autoloaded, and ensime-mode makes no
    ;; sense before the first session was started anyway
    (add-hook 'scala-mode-hook #'ensime-mode)

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
      (kbd "q") 'dotemacs-ensime-refactor-cancel
      (kbd "c") 'dotemacs-ensime-refactor-accept
      (kbd "RET") 'dotemacs-ensime-refactor-accept)

    (evil-define-key 'normal ensime-compile-result-map
      (kbd "g") 'ensime-show-all-errors-and-warnings
      (kbd "TAB") 'forward-button
      (kbd "<backtab>") 'backward-button
      (kbd "M-n") 'forward-button
      (kbd "M-p") 'backward-button
      (kbd "n") 'forward-button
      (kbd "N") 'backward-button)

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
      "/"     'ensime-search

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

    ;; Enable Expand Region integration from Ensime.  Ignore load errors to
    ;; handle older Ensime versions gracefully.
    (with-eval-after-load 'expand-region
      (require 'ensime-expand-region nil 'noerror))

    ;; Don't use scala checker if ensime mode is active, since it provides
    ;; better error checking.
    (with-eval-after-load 'flycheck
      (defun scala/disable-flycheck-scala ()
        (push 'scala flycheck-disabled-checkers))
      (add-hook 'ensime-mode-hook 'scala/disable-flycheck-scala))))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'scala-mode))

(use-package ensime-sbt
  :ensure ensime
  :defer t
  ;; Compile on save.
  :config (setq ensime-sbt-perform-on-save "test:compile"))

;; Flycheck Ensime Setup
;; TODO: Determine best way to integrate with
;; (flycheck-ensime-setup)

(defun flycheck-verify-ensime ()
  "Verify the Ensime syntax checker."
  (list
   (flycheck-verification-result-new
    :label "Ensime Mode"
    :message (if ensime-mode "Enabled" "Disabled")
    :face (if ensime-mode 'success '(bold warning)))
   (flycheck-verification-result-new
    :label "Ensime connection"
    :message (if (ensime-connected-p) "open" "closed")
    :face (if (ensime-connected-p) 'success '(bold warning)))))

(defun flycheck-ensime-parse-note (note checker)
  "Parse a single Ensime NOTE for CHECKER into an error."
  (let ((severity (plist-get note :severity)))
    (unless (symbolp severity)
      (setq severity (intern severity)))
    (flycheck-error-new-at
     (plist-get note :line)
     (plist-get note :col)
     severity (plist-get note :msg)
     :checker checker
     :filename (plist-get note :file)
     :buffer (current-buffer))))

(defun flycheck-ensime-parse-notes (notes checker)
  "Parse Ensime NOTES for CHECKER into Flycheck errors."
  (mapcar (lambda (n) (flycheck-ensime-parse-note n checker)) notes))

(defun flycheck-ensime-start (checker callback)
  "Start a syntax CHECKER with Ensime."
  (condition-case err
      (let* ((notes (ensime-scala-compiler-notes (ensime-connection)))
             (errors (flycheck-ensime-parse-notes notes checker)))
        (funcall callback 'finished errors))
    (error (funcall callback 'errored (error-message-string err)))))

;; (with-eval-after-load 'scala-ensime
;;   (flycheck-define-generic-checker 'scala-ensime
;;     "A Scala syntax checker using Ensime.

;; See URL `https://github.com/ensime/ensime-emacs'."
;;     :start #'flycheck-ensime-start
;;     :verify #'flycheck-verify-ensime
;;     :modes '(scala-mode)
;;     :predicate (lambda () (and ensime-mode (ensime-connection-or-nil)))
;;     :next-checkers '((warning . scala-scalastyle))))

(defun flycheck-ensime-setup ()
  "Setup Flycheck for Ensime."
  (interactive)
  (add-to-list 'flycheck-checkers 'scala-ensime)
  (advice-add 'ensime-make-note-overlays :override #'ignore
              '((name . flycheck-ensime-disable-ensime-overlays))))

;; flycheck auto scala style
;; TODO: Determine best way to integrate
;; (with-eval-after-load 'scala-mode2
;;   (add-hook 'flycheck-mode-hook #'flycheck-auto-scalastyle-setup)))

(defcustom flycheck-auto-scalastyle-version '("0.6.0" . "2.10")
  "Version of scala style to use for Flycheck.

A pair of `(VERSION . SCALA-VERSION)'."
  :type '(cons (string :tag "Scalastyle version")
               (string :tag "Scala language version"))
  :group 'flycheck
  :safe '(lambda (value)
           (and (consp value) (stringp (car value)) (stringp (cdr value)))))

(defcustom flycheck-auto-scalastyle-jar-dir (locate-user-emacs-file "scalastyle")
  "Directory for installed Scalastyle JARs."
  :type 'directory
  :group 'flycheck)

(defsubst flycheck-auto-scalastyle-jar-name ()
  "Get the file name of the Scalastyle JAR."
  (pcase-let ((`(,version . ,scala-version) flycheck-auto-scalastyle-version))
    (format "scalastyle_%s-%s-batch.jar" scala-version version)))

(defsubst flycheck-auto-scalastyle-url ()
  "Get the URL to download Scalastyle."
  (pcase-let ((`(,version . ,scala-version) flycheck-auto-scalastyle-version))
    (format "https://oss.sonatype.org/content/repositories/releases/org/scalastyle/scalastyle_%s/%s/%s"
            scala-version version (flycheck-auto-scalastyle-jar-name))))

(defun flycheck-auto-scalastyle-ensure ()
  "Ensure and return the scalastyle JAR for this buffer."
  (let ((file-name (expand-file-name (flycheck-auto-scalastyle-jar-name)
                                    flycheck-auto-scalastyle-jar-dir)))
    (unless (file-exists-p file-name)
      (make-directory flycheck-auto-scalastyle-jar-dir 'parents)
      (message "Downloading scalastyle JAR")
      (url-copy-file (flycheck-auto-scalastyle-url) file-name))
    file-name))

;;;###autoload
(defun flycheck-auto-scalastyle-configure ()
  "Configure Scalastyle for this buffer."
  (interactive)
  (setq flycheck-scalastyle-jar (flycheck-auto-scalastyle-ensure)))

;;;###autoload
(defun flycheck-auto-scalastyle-setup ()
  "Setup Flycheck Scalastyle for this buffer.

For use in `flycheck-mode-hook'."
  (add-hook 'hack-local-variables-hook #'flycheck-auto-scalastyle-configure))

(provide 'module-scala)
;;; module-scala.el ends here
