;;; Python
(require 'module-global)
;; (require 'flycheck)

(dotemacs-defvar-company-backends python-mode)
(dotemacs-defvar-company-backends inferior-python-mode)
(dotemacs-defvar-company-backends pip-requirements-mode)

(defvar python-enable-yapf-format-on-save nil
  "If non-nil, automatically format code with YAPF on save.")

(defvar python-test-runner 'nose
  "Test runner to use. Possible values are `nose' or `pytest'.")

(defun annotate-pdb ()
  "Highlight break point lines."
  (interactive)
  (highlight-lines-matching-regexp "import i?pu?db")
  (highlight-lines-matching-regexp "i?pu?db.set_trace()"))

(defun python-toggle-breakpoint ()
  "Add a break point, highlight it."
  (interactive)
  (let ((trace (cond ((executable-find "ipdb") "import ipdb; ipdb.set_trace()")
                     ((executable-find "pudb") "import pudb; pudb.set_trace()")
                     (t "import pdb; pdb.set_trace()")))
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (progn
        (back-to-indentation)
        (insert-string trace)
        (insert-string "\n")
        (python-indent-line)))))

;; from https://www.snip2code.com/Snippet/127022/Emacs-auto-remove-unused-import-statemen
(defun python-remove-unused-imports()
  "Use Autoflake to remove unused function"
  "autoflake --remove-all-unused-imports -i unused_imports.py"
  (interactive)
  (if (executable-find "autoflake")
      (progn
        (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                               (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t))
    (message "Error: Cannot find autoflake executable.")))

(defun python-shell-send-buffer-switch ()
  "Send buffer content to shell and switch to it in insert mode."
  (interactive)
  (python-shell-send-buffer)
  (python-shell-switch-to-shell)
  (evil-insert-state))

(defun python-shell-send-defun-switch ()
  "Send function content to shell and switch to it in insert mode."
  (interactive)
  (python-shell-send-defun nil)
  (python-shell-switch-to-shell)
  (evil-insert-state))

(defun python-shell-send-region-switch (start end)
  "Send region content to shell and switch to it in insert mode."
  (interactive "r")
  (python-shell-send-region start end)
  (python-shell-switch-to-shell)
  (evil-insert-state))

(defun python-start-or-switch-repl ()
  "Start and/or switch to the REPL."
  (interactive)
  (python-shell-switch-to-shell)
  (evil-insert-state))

;; reset compile-command (by default it is `make -k')
(setq compile-command nil)
(defun dotemacs-python-execute-file (arg)
  "Execute a python script in a shell."
  (interactive "P")
  ;; set compile command to buffer-file-name
  ;; universal argument put compile buffer in comint mode
  (setq universal-argument t)
  (if arg
      (call-interactively 'compile)

    (setq compile-command (format "python %s" (file-name-nondirectory
                                               buffer-file-name)))
    (compile compile-command t)
    (with-current-buffer (get-buffer "*compilation*")
      (inferior-python-mode))))

(defun dotemacs-python-execute-file-focus (arg)
  "Execute a python script in a shell and switch to the shell buffer in
`insert state'."
  (interactive "P")
  (dotemacs-python-execute-file arg)
  (switch-to-buffer-other-window "*compilation*")
  (end-of-buffer)
  (evil-insert-state))

(defun python-default ()
  (setq mode-name "Python"
        tab-width 4
        fill-column python-fill-column
        ;; auto-indent on colon doesn't work well with if statement
        electric-indent-chars (delq ?: electric-indent-chars))
  (annotate-pdb)
  ;; make C-j work the same way as RET
  (local-set-key (kbd "C-j") 'newline-and-indent))

(defun python-setup-shell ()
  (setq python-shell-interpreter "python"))

(defun inferior-python-setup-hook ()
  (setq indent-tabs-mode t))

(use-package cython-mode
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'cython-mode
      "hh" 'anaconda-mode-show-doc
      "gg" 'anaconda-mode-find-definitions
      "ga" 'anaconda-mode-find-assignments
      "gu" 'anaconda-mode-find-references)
    (evilified-state-evilify anaconda-mode-view-mode anaconda-mode-view-mode-map
      (kbd "q") 'quit-window)))

(use-package anaconda-mode              ; Powerful Python backend for Emacs
  :defer t
  :init
  (progn
    (setq anaconda-mode-installation-directory
          (concat dotemacs-cache-directory "anaconda-mode"))
    (add-hook 'python-mode-hook 'anaconda-mode))
  :config
  (progn
    (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
      (evil-jumper--push))
    (dotemacs-set-leader-keys-for-major-mode 'python-mode
      "hh" 'anaconda-mode-view-doc
      "gu" 'anaconda-mode-usages
      "gg"  'anaconda-mode-goto)
    (dotemacs-hide-lighter anaconda-mode)))

(use-package pip-requirements           ; requirements.txt files
  :defer t
  :ensure t)

(use-package pony-mode
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'python-mode
      ; d*j*ango f*a*bric
      "jaf" 'pony-fabric
      "jad" 'pony-fabric-deploy
      ; d*j*ango *f*iles
      "jfs" 'pony-goto-settings
      "jfc" 'pony-setting
      "jft" 'pony-goto-template
      "jfr" 'pony-resolve
      ; d*j*ango *i*nteractive
      "jid" 'pony-db-shell
      "jis" 'pony-shell
      ; d*j*ango *m*anage
      ; not including one-off management commands like "flush" and
      ; "startapp" even though they're implemented in pony-mode,
      ; because this is much handier
      "jm" 'pony-manage
      ; d*j*ango *r*unserver
      "jrd" 'pony-stopserver
      "jro" 'pony-browser
      "jrr" 'pony-restart-server
      "jru" 'pony-runserver
      "jrt" 'pony-temp-server
      ; d*j*ango *s*outh/*s*yncdb
      "jsc" 'pony-south-convert
      "jsh" 'pony-south-schemamigration
      "jsi" 'pony-south-initial
      "jsm" 'pony-south-migrate
      "jss" 'pony-syncdb
      ; d*j*ango *t*est
      "jtd" 'pony-test-down
      "jte" 'pony-test-goto-err
      "jto" 'pony-test-open
      "jtt" 'pony-test
      "jtu" 'pony-test-up)))

(use-package pyenv-mode
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'python-mode
      "vs" 'pyenv-mode-set
      "vu" 'pyenv-mode-unset)))

(use-package pyvenv
  :defer t
  :ensure t
  :init
  (dotemacs-set-leader-keys-for-major-mode 'python-mode
    "V" 'pyvenv-workon))

(use-package pylookup
  :quelpa (pylookup :fetcher github :repo "tsgates/pylookup")
  :commands (pylookup-lookup pylookup-update pylookup-update-all)
  :init
  (progn
    (evilified-state-evilify pylookup-mode pylookup-mode-map)
    (dotemacs-set-leader-keys-for-major-mode 'python-mode
      "mhH" 'pylookup-lookup))
  :config
  (progn
    (let ((dir dotemacs-quelpa-build-directory))
      (setq pylookup-dir (concat dir "pylookup/")
            pylookup-program (concat pylookup-dir "pylookup.py")
            pylookup-db-file (concat pylookup-dir "pylookup.db")))))

(use-package py-yapf
  :ensure t
  :init
  (dotemacs-set-leader-keys-for-major-mode 'python-mode "=" 'py-yapf-buffer)
  :config
  (if python-enable-yapf-format-on-save
      (add-hook 'python-mode-hook 'py-yapf-enable-on-save)))

(use-package nose
  :ensure t
  :if (eq 'nose python-test-runner)
  :commands (nosetests-one
             nosetests-pdb-one
             nosetests-all
             nosetests-pdb-all
             nosetests-module
             nosetests-pdb-module
             nosetests-suite
             nosetests-pdb-suite)
  :init
  (dotemacs-set-leader-keys-for-major-mode 'python-mode
    "tA" 'nosetests-pdb-all
    "ta" 'nosetests-all
    "tB" 'nosetests-pdb-module
    "tb" 'nosetests-module
    "tT" 'nosetests-pdb-one
    "tt" 'nosetests-one
    "tM" 'nosetests-pdb-module
    "tm" 'nosetests-module
    "tS" 'nosetests-pdb-suite
    "ts" 'nosetests-suite)
  :config
  (progn
    (add-to-list 'nose-project-root-files "setup.cfg")
    (setq nose-use-verbose nil)))

(use-package pytest
  :if (eq 'pytest python-test-runner)
  :ensure t
  :defer t
  :commands (pytest-one
             pytest-pdb-one
             pytest-all
             pytest-pdb-all
             pytest-module
             pytest-pdb-module)
  :init (dotemacs-set-leader-keys-for-major-mode 'python-mode
          "tA" 'pytest-pdb-all
          "ta" 'pytest-all
          "tB" 'pytest-pdb-module
          "tb" 'pytest-module
          "tT" 'pytest-pdb-one
          "tt" 'pytest-one
          "tM" 'pytest-pdb-module
          "tm" 'pytest-module)
  :config (add-to-list 'pytest-project-root-files "setup.cfg"))

(use-package python
  :defer t
  :ensure t
  :init
  (progn
    (with-eval-after-load 'eldoc
      (add-hook 'python-mode-hook #'eldoc-mode))

    (add-hook 'inferior-python-mode-hook #'inferior-python-setup-hook)
    (dotemacs/add-all-to-hook 'python-mode-hook
                              'python-default
                              'python-setup-shell))
  :config
  (progn
    ;; PEP 8 compliant filling rules, 79 chars maximum
    (add-hook 'python-mode-hook (lambda () (setq fill-column 79)))
    (add-hook 'python-mode-hook #'subword-mode)

    ;; add support for `ahs-range-beginning-of-defun' for python-mode
    (with-eval-after-load 'auto-highlight-symbol
      (add-to-list 'ahs-plugin-bod-modes 'python-mode))

    (dotemacs-declare-prefix-for-mode 'python-mode "mc" "execute")
    (dotemacs-declare-prefix-for-mode 'python-mode "md" "debug")
    (dotemacs-declare-prefix-for-mode 'python-mode "mh" "help")
    (dotemacs-declare-prefix-for-mode 'python-mode "mg" "goto")
    (dotemacs-declare-prefix-for-mode 'python-mode "mt" "test")
    (dotemacs-declare-prefix-for-mode 'python-mode "ms" "send to REPL")
    (dotemacs-declare-prefix-for-mode 'python-mode "mr" "refactor")
    (dotemacs-declare-prefix-for-mode 'python-mode "mv" "venv")
    (dotemacs-set-leader-keys-for-major-mode 'python-mode
      "cc" 'dotemacs-python-execute-file
      "cC" 'dotemacs-python-execute-file-focus
      "db" 'python-toggle-breakpoint
      "ri" 'python-remove-unused-imports
      "sB" 'python-shell-send-buffer-switch
      "sb" 'python-shell-send-buffer
      "sF" 'python-shell-send-defun-switch
      "sf" 'python-shell-send-defun
      "si" 'python-start-or-switch-repl
      "sR" 'python-shell-send-region-switch
      "sr" 'python-shell-send-region)

    ;; the default in Emacs is M-n
    (define-key inferior-python-mode-map (kbd "C-j") 'comint-next-input)
    ;; the default in Emacs is M-p and this key binding overrides default C-k
    ;; which prevents Emacs users to kill line
    (define-key inferior-python-mode-map (kbd "C-k") 'comint-previous-input)
    ;; the default in Emacs is M-r; C-r to search backward old output
    ;; and should not be changed
    (define-key inferior-python-mode-map (kbd "C-r") 'comint-history-isearch-backward)
    ;; this key binding is for recentering buffer in Emacs
    ;; it would be troublesome if Emacs user
    ;; Vim users can use this key since they have other key
    (define-key inferior-python-mode-map (kbd "C-l") 'dotemacs/comint-clear-buffer)

    ;; add this optional key binding for Emacs user, since it is unbound
    (define-key inferior-python-mode-map (kbd "C-c M-l") 'dotemacs/comint-clear-buffer)

    ;; fix for issue #2569 (https://github.com/syl20bnr/spacemacs/issues/2569)
    ;; use `semantic-create-imenu-index' only when `semantic-mode' is enabled,
    ;; otherwise use `python-imenu-create-index'
    (defun dotemacs/python-imenu-create-index-python-or-semantic ()
      (if (bound-and-true-p semantic-mode)
          (semantic-create-imenu-index)
        (python-imenu-create-index)))

    (defadvice wisent-python-default-setup
        (after dotemacs/python-set-imenu-create-index-function activate)
      (setq imenu-create-index-function
            #'dotemacs/python-imenu-create-index-python-or-semantic))))

(with-eval-after-load 'evil-matchit
  (add-hook 'python-mode-hook 'turn-on-evil-matchit-mode))

(use-package evil-matchit-python
  :defer t
  :ensure evil-matchit
  :config
  (plist-put evilmi-plugins 'python-mode' ((evilmi-simple-get-tag evilmi-simple-jump)
                                           (evilmi-python-get-tag evilmi-python-jump))))

(use-package hy-mode
  :ensure t
  :defer t)

(use-package helm-pydoc
  :defer t
  :ensure t
  :init
  (dotemacs-set-leader-keys-for-major-mode 'python-mode "hd" 'helm-pydoc))

;; We can safely declare this function, since we'll only call it in Python Mode,
;; that is, when python.el was already loaded.
(declare-function python-shell-calculate-exec-path "python")

(defun flycheck-virtualenv-set-python-executables ()
  "Set Python executables for the current buffer."
  (let ((exec-path (python-shell-calculate-exec-path)))
    (setq-local flycheck-python-pylint-executable
                (executable-find "pylint"))
    (setq-local flycheck-python-flake8-executable
                (executable-find "flake8"))))

(defun flycheck-virtualenv-setup ()
  "Setup Flycheck for the current virtualenv."
  (when (derived-mode-p 'python-mode)
    (add-hook 'hack-local-variables-hook
              #'flycheck-virtualenv-set-python-executables 'local)))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'python-mode)
  (dotemacs/add-flycheck-hook 'flycheck-virtualenv-setup))

(dotemacs-use-package-add-hook smartparens
  :post-init
  (defadvice python-indent-dedent-line-backspace
      (around python/sp-backward-delete-char activate)
    (let ((pythonp (or (not smartparens-strict-mode)
                       (char-equal (char-before) ?\s))))
      (if pythonp
          ad-do-it
        (call-interactively 'sp-backward-delete-char)))))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook python-mode)
      (push 'company-capf company-backends-pip-requirements-mode)
      (dotemacs-add-company-hook pip-requirements-mode)

      (dotemacs-add-company-hook inferior-python-mode)
      (push '(company-files company-capf) company-backends-inferior-python-mode)
      (add-hook 'inferior-python-mode-hook (lambda ()
                                             (setq-local company-minimum-prefix-length 0)
                                             (setq-local company-idle-delay 0.5))))))

(use-package company-anaconda           ; Python backend for Company
  :ensure t
  :if (eq dotemacs-completion-engine 'company)
  :defer t
  :init
  (progn
    (push 'company-anaconda company-backends-python-mode)))

(dotemacs-use-package-add-hook semantic
  :post-init
  (progn
    (semantic/enable-semantic-mode 'python-mode)
    (defadvice semantic-python-get-system-include-path (around semantic-python-skip-error-advice activate)
      "Don't cause error when Semantic cannot retrieve include
paths for Python then prevent the buffer to be switched. This
issue might be fixed in Emacs 25. Until then, we need it here to
fix this issue."
      (condition-case nil
          ad-do-it
        (error nil)))))

(dotemacs-use-package-add-hook stickyfunc-enhance
  :post-init
  (add-hook 'python-mode-hook 'dotemacs-lazy-load-stickyfunc-enhance))

(provide 'module-python)
;;; module-python.el ends here
