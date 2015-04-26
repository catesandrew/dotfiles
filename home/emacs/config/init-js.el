(require 'init-programming)

(add-to-list 'auto-mode-alist '("\\.js\\'"    . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pac\\'"   . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc$" . javascript-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

(after 'javascript-mode
  (setq javascript-indent-level 2)) ; javascript-mode

(after 'js-mode
  (setq js-indent-level 2)) ; js-mode

(after 'js2-mode
  (defun my-js2-mode-defaults ()
    (js2-imenu-extras-mode +1)
    (setq mode-name "JS2")
    ; '(define-key js-mode-map "," 'self-insert-command)
    ; '(define-key js-mode-map ";" 'self-insert-command)
    ;; electric-layout-mode doesn't play nice with smartparens
    (setq-local electric-layout-rules '((?\; . after)))
    (message "My JS2 hook"))

  (setq my-js2-mode-hook 'my-js2-mode-defaults)
  (add-hook 'js2-mode-hook (lambda () (run-hooks 'my-js2-mode-hook)))

  (defun my-dotemacs-js-ctrl-c-ctrl-c ()
    (interactive)
    (require 'thingatpt)
    (let ((val (thing-at-point 'list)))
      ;; inside parameter list?
      (when (and (equal (substring val 0 1) "(")
                 (equal (substring val -1) ")"))
        (if (string-match-p "," val)
            (my-macro-ng-add-string-for-last-arg)
          (my-macro-ng-function-to-array-injected)))))

  (add-hook 'js2-mode-hook (lambda ()
    (local-set-key (kbd "C-c C-c") #'my-dotemacs-js-ctrl-c-ctrl-c)))

  (setq indent-tabs-mode nil
        tab-width 2
        js-indent-level 2)
  (setq js2-highlight-level 3)
  (setq js2-basic-offset 2)
  (setq js2-concat-multiline-strings (quote eol))
  (setq js2-include-node-externs t)
  (setq js2-indent-switch-body t)

  (setq js2-allow-rhino-new-expr-initializer nil)
  (setq js2-auto-indent-p nil)
  (setq js2-enter-indents-newline nil)
  (setq js2-global-externs '("setTimeout" "clearTimeout" "setInterval" "clearInterval" "__dirname" "console" "JSON" "_" "assert" "refute" "buster" "require" "global" "exports" "module" "describe" "it" "before" "after" "beforeEach" "afterEach" "chai" "expect" "sinon" "test" "asyncTest" "ok" "equal" "notEqual" "deepEqual" "expect"))
  (setq js2-idle-timer-delay 0.8)
  (setq js2-indent-on-enter-key nil)
  (setq js2-mirror-mode nil)
  (setq js2-strict-inconsistent-return-warning nil)
  (setq js2-include-rhino-externs nil)
  (setq js2-include-gears-externs nil)
  (setq js2-rebind-eol-bol-keys nil)

  ;; Let flycheck handle parse errors
  (setq js2-show-parse-errors nil)
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason

  (define-key js2-mode-map (kbd "C-c RET jt") 'jump-to-test-file)
  (define-key js2-mode-map (kbd "C-c RET ot") 'jump-to-test-file-other-window)
  (define-key js2-mode-map (kbd "C-c RET js") 'jump-to-source-file)
  (define-key js2-mode-map (kbd "C-c RET os") 'jump-to-source-file-other-window)
  (define-key js2-mode-map (kbd "C-c RET jo") 'jump-between-source-and-test-files)
  (define-key js2-mode-map (kbd "C-c RET oo") 'jump-between-source-and-test-files-other-window)

  (define-key js2-mode-map (kbd "C-c RET dp") 'js2r-duplicate-object-property-node)

  (define-key js2-mode-map (kbd "C-c RET ta") 'toggle-assert-refute)

  (defadvice js2r-inline-var (after reindent-buffer activate)
    (cleanup-buffer))

  (defun js2-hide-test-functions ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (ignore-errors
        (while (re-search-forward "\"[^\"]+\": function (")
          (js2-mode-hide-element)))))

  (define-key js2-mode-map (kbd "C-c t") 'js2-hide-test-functions)

  ;; js2-mode steals TAB, let's steal it back for yasnippet
  (defun js2-tab-properly ()
    (interactive)
    (let ((yas-fallback-behavior 'return-nil))
      (unless (yas-expand)
        (indent-for-tab-command)
        (if (looking-back "^\s*")
            (back-to-indentation)))))

  (define-key js2-mode-map (kbd "TAB") 'js2-tab-properly)

  ;; When renaming/deleting js-files, check for corresponding testfile
  (define-key js2-mode-map (kbd "C-x C-r") 'js2r-rename-current-buffer-file)
  (define-key js2-mode-map (kbd "C-x C-k") 'js2r-delete-current-buffer-file)

  (define-key js2-mode-map (kbd "C-k") 'js2r-kill)

  ;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
  ;; add any symbols to a buffer-local var of acceptable global vars
  ;; Note that we also support the "symbol: true" way of specifying names via a hack (remove any ":true"
  ;; to make it look like a plain decl, and any ':false' are left behind so they'll effectively be ignored as
  ;; you can;t have a symbol called "someName:false"
  (add-hook 'js2-post-parse-callbacks
            (lambda ()
              (when (> (buffer-size) 0)
                (let ((btext (replace-regexp-in-string
                              ": *true" " "
                              (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
                  (mapc (apply-partially 'add-to-list 'js2-additional-externs)
                        (split-string
                         (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
                         " *, *" t))
                  ))))

  (require 'json)

  ;; Tern.JS
  (add-to-list 'load-path (expand-file-name "tern/emacs" dotemacs-elisp-dir))
  (autoload 'tern-mode "tern.el" nil t)
  (eval-after-load 'auto-complete
    '(eval-after-load 'tern
       '(progn
          (require 'tern-auto-complete)
          (tern-ac-setup))))

  (defun my-aget (key map)
    (cdr (assoc key map)))

  (defun js2-fetch-autolint-externs (file)
    (let* ((settings (with-temp-buffer
                       (insert-file-literally file)
                       (javascript-mode)
                       (let (kill-ring kill-ring-yank-pointer) (kill-comment 1000))
                       (->> (buffer-substring (point-min) (point-max))
                         (s-trim)
                         (s-chop-prefix "module.exports = ")
                         (s-chop-suffix ";")
                         (json-read-from-string))))
           (predef (->> settings
                     (my-aget 'linterOptions)
                     (my-aget 'predef))))
      (--each (append predef nil)
        (add-to-list 'js2-additional-externs it))))

  (defun cjsp--eldoc-innards (beg)
    (save-excursion
      (goto-char beg)
      (search-forward "=")
      (let ((start (point)))
        (search-forward "*/")
        (forward-char -2)
        (buffer-substring-no-properties start (point)))))

  (defun cjsp--indentation-of-html-line (html line-number)
    (with-temp-buffer
      (insert html)
      (html-mode)
      (indent-region (point-min) (point-max))
      (goto-line line-number)
      (back-to-indentation)
      (current-column)))

  (defun cjsp--line-number-in-eldoc (p beg)
    (save-excursion
      (goto-char p)
      (let ((l (line-number-at-pos)))
        (goto-char beg)
        (- l (line-number-at-pos) -1))))

  (defun js2-lineup-comment (parse-status)
    "Indent a multi-line block comment continuation line."
    (let* ((beg (nth 8 parse-status))
           (first-line (js2-same-line beg))
           (p (point))
           (offset (save-excursion
                     (goto-char beg)
                     (cond

                      ((looking-at "/\\*:DOC ")
                       (+ 2 (current-column)
                          (cjsp--indentation-of-html-line
                           (cjsp--eldoc-innards beg)
                           (cjsp--line-number-in-eldoc p beg))))

                      ((looking-at "/\\*")
                       (+ 1 (current-column)))

                      (:else 0)))))
      (unless first-line
        (indent-line-to offset))))

  (require 'js2-refactor)
  (js2r-add-keybindings-with-prefix "C-c C-m")

  (require 'js2-imenu-extras)
  (js2-imenu-extras-setup)

  ;; jshintrc
  (when (executable-find "tern")
    (require 'tern)
    (add-hook 'js2-mode-hook 'tern-mode)
    (after 'tern
      (after 'auto-complete
        (require 'tern-auto-complete)
        (tern-ac-setup))
      (after 'company-mode
        (require 'company-tern)))))

(provide 'init-js)
