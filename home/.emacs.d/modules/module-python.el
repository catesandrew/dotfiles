;;; module-python.el --- Python Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'use-package)
(require 'evil-evilified-state)
(require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
(require 'core-auto-completion)
(require 'core-use-package-ext)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(dotemacs-defvar-company-backends python-mode)
(dotemacs-defvar-company-backends inferior-python-mode)
(dotemacs-defvar-company-backends pip-requirements-mode)

(defvar python-enable-yapf-format-on-save nil
  "If non-nil, automatically format code with YAPF on save.")

(defvar python-test-runner 'nose
  "Test runner to use. Possible values are `nose' or `pytest'.")

(defvar python-fill-column 79
  "Fill column value for python buffers")

(defvar python-auto-set-local-pyenv-version 'on-visit
  "Automatically set pyenv version from \".python-version\".

Possible values are `on-visit', `on-project-switch' or `nil'.")

;; from http://pedrokroger.net/2010/07/configuring-emacs-as-a-python-ide-2/
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

(defun pyenv-mode-set-local-version ()
  "Set pyenv version from \".python-version\" by looking in parent directories."
  (interactive)
  (let ((root-path (locate-dominating-file default-directory
                                           ".python-version")))
    (when root-path
      (let* ((file-path (expand-file-name ".python-version" root-path))
             (version (with-temp-buffer
                        (insert-file-contents-literally file-path)
                        (buffer-substring-no-properties (line-beginning-position)
                                                        (line-end-position)))))
        (if (member version (pyenv-mode-versions))
            (pyenv-mode-set version)
          (message "pyenv: version `%s' is not installed (set by %s)"
                   version file-path))))))

;; packages

(use-package anaconda-mode              ; Powerful Python backend for Emacs
  :defer t
  :init
  (progn
    (setq anaconda-mode-installation-directory
          (concat dotemacs-cache-directory "anaconda-mode"))
    (add-hook 'python-mode-hook 'anaconda-mode))
  :config
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'python-mode
      "hh" 'anaconda-mode-show-doc
      "gg" 'anaconda-mode-find-definitions
      "ga" 'anaconda-mode-find-assignments
      "gb" 'anaconda-mode-go-back
      "gu" 'anaconda-mode-find-references)
    (evilified-state-evilify anaconda-mode-view-mode anaconda-mode-view-mode-map
      (kbd "q") 'quit-window)
    (dotemacs-hide-lighter anaconda-mode)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook python-mode)
      (dotemacs-add-company-hook inferior-python-mode)
      (push '(company-files company-capf) company-backends-inferior-python-mode)
      (add-hook 'inferior-python-mode-hook (lambda ()
                                             (setq-local company-minimum-prefix-length 0)
                                             (setq-local company-idle-delay 0.5)))))
  (use-package company-anaconda
    :ensure t
    :defer t
    :init
    (push 'company-anaconda company-backends-python-mode)))

(use-package cython-mode
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'cython-mode
      "hh" 'anaconda-mode-view-doc
      "gg" 'anaconda-mode-goto
      "gu" 'anaconda-mode-usages)))

(dotemacs-use-package-add-hook eldoc
  :post-init
  (add-hook 'python-mode-hook 'eldoc-mode))

(dotemacs-use-package-add-hook evil-jumper
  :post-init
  (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
    (evil-jumper--push)))

(dotemacs-use-package-add-hook evil-matchit
  :post-init
  (add-hook `python-mode-hook `turn-on-evil-matchit-mode))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'python-mode))

(dotemacs-use-package-add-hook helm-cscope
  :pre-init
  (dotemacs-use-package-add-hook xcscope
    :post-init
    (dotemacs-setup-helm-cscope 'python-mode)))

(use-package helm-pydoc
  :defer t
  :ensure t
  :init
  (dotemacs-set-leader-keys-for-major-mode 'python-mode "hd" 'helm-pydoc))

(use-package hy-mode
  :ensure t
  :defer t)

(use-package live-py-mode
  :defer t
  :ensure t
  :commands live-py-mode
  :init
  (dotemacs-set-leader-keys-for-major-mode 'python-mode
    "l" 'live-py-mode))

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

(use-package pip-requirements
  :defer t
  :ensure t
  :init
  (progn
    ;; company support
    (push 'company-capf company-backends-pip-requirements-mode)
    (dotemacs-add-company-hook pip-requirements-mode)))

(use-package pyenv-mode
  :if (executable-find "pyenv")
  :ensure t
  :commands (pyenv-mode-versions)
  :init
  (progn
    (pcase python-auto-set-local-pyenv-version
      (`on-visit
       (add-hook 'python-mode-hook 'pyenv-mode-set-local-version))
      (`on-project-switch
       (add-hook 'projectile-after-switch-project-hook 'pyenv-mode-set-local-version)))
    (dotemacs-set-leader-keys-for-major-mode 'python-mode
      "vu" 'pyenv-mode-unset
      "vs" 'pyenv-mode-set)))

(use-package pyvenv
  :defer t
  :ensure t
  :init
  (dotemacs-set-leader-keys-for-major-mode 'python-mode
    "Va" 'pyvenv-activate
    "Vd" 'pyvenv-deactivate
    "Vw" 'pyvenv-workon))

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
    (let ((dir (concat user-emacs-directory "etc/")))
      (setq pylookup-dir (concat dir "pylookup/")
            pylookup-program (concat pylookup-dir "pylookup.py")
            pylookup-db-file (concat pylookup-dir "pylookup.db")))))

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
    (dotemacs-register-repl 'python 'python-start-or-switch-repl "python")

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
      (if (executable-find "ipython")
          (progn
            (setq python-shell-interpreter "ipython")
            (when (version< emacs-version "24.4")
              ;; these settings are unnecessary and even counter-productive on emacs 24.4 and newer
              (setq python-shell-prompt-regexp "In \\[[0-9]+\\]: "
                    python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
                    python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
                    python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
                    python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))
        (setq python-shell-interpreter "python")))

    (defun inferior-python-setup-hook ()
      (setq indent-tabs-mode t))

    (add-hook 'inferior-python-mode-hook #'inferior-python-setup-hook)
    (dotemacs/add-all-to-hook 'python-mode-hook
                              'python-default
                              'python-setup-shell))
  :config
  (progn
    ;; add support for `ahs-range-beginning-of-defun' for python-mode
    (with-eval-after-load 'auto-highlight-symbol
      (add-to-list 'ahs-plugin-bod-modes 'python-mode))

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
    (defun dotemacs/python-execute-file (arg)
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

    (defun dotemacs/python-execute-file-focus (arg)
      "Execute a python script in a shell and switch to the shell buffer in
`insert state'."
      (interactive "P")
      (dotemacs/python-execute-file arg)
      (switch-to-buffer-other-window "*compilation*")
      (end-of-buffer)
      (evil-insert-state))

    (dotemacs-declare-prefix-for-mode 'python-mode "mc" "execute")
    (dotemacs-declare-prefix-for-mode 'python-mode "md" "debug")
    (dotemacs-declare-prefix-for-mode 'python-mode "mh" "help")
    (dotemacs-declare-prefix-for-mode 'python-mode "mg" "goto")
    (dotemacs-declare-prefix-for-mode 'python-mode "mt" "test")
    (dotemacs-declare-prefix-for-mode 'python-mode "ms" "send to REPL")
    (dotemacs-declare-prefix-for-mode 'python-mode "mr" "refactor")
    (dotemacs-declare-prefix-for-mode 'python-mode "mv" "pyenv")
    (dotemacs-declare-prefix-for-mode 'python-mode "mV" "pyvenv")
    (dotemacs-set-leader-keys-for-major-mode 'python-mode
      "'"  'python-start-or-switch-repl
      "cc" 'dotemacs/python-execute-file
      "cC" 'dotemacs/python-execute-file-focus
      "db" 'python-toggle-breakpoint
      "ri" 'python-remove-unused-imports
      "sB" 'python-shell-send-buffer-switch
      "sb" 'python-shell-send-buffer
      "sF" 'python-shell-send-defun-switch
      "sf" 'python-shell-send-defun
      "si" 'python-start-or-switch-repl
      "sR" 'python-shell-send-region-switch
      "sr" 'python-shell-send-region)

    (defun dotemacs/comint-clear-buffer ()
      (interactive)
      (let ((comint-buffer-maximum-size 0))
        (comint-truncate-buffer)))

    ;; Emacs users won't need these key bindings (Only VIM bindings). The
    ;; default in Emacs is M-n
    (define-key inferior-python-mode-map (kbd "C-j") 'comint-next-input)
    ;; The default in Emacs is M-p and this key binding overrides default C-k
    ;; which prevents Emacs users to kill line
    (define-key inferior-python-mode-map (kbd "C-k") 'comint-previous-input)
    ;; The default in Emacs is M-r; C-r to search backward old output and should
    ;; not be changed
    (define-key inferior-python-mode-map (kbd "C-r") 'comint-history-isearch-backward)
    ;; This key binding is for recentering buffer in Emacs. It would be
    ;; troublesome if Emacs user Vim users can use this key since they have
    ;; other key
    (define-key inferior-python-mode-map (kbd "C-l") 'dotemacs/comint-clear-buffer)

    ;; Add this optional key binding for Emacs user, since it is unbound
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

(use-package py-yapf
  :ensure t
  :init
  (dotemacs-set-leader-keys-for-major-mode 'python-mode "=" 'py-yapf-buffer)
  :config
  (when python-enable-yapf-format-on-save
      (add-hook 'python-mode-hook 'py-yapf-enable-on-save)))

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

(dotemacs-use-package-add-hook smartparens
  :post-init
  (defadvice python-indent-dedent-line-backspace
      (around python/sp-backward-delete-char activate)
    (let ((pythonp (or (not smartparens-strict-mode)
                       (char-equal (char-before) ?\s))))
      (if pythonp
          ad-do-it
        (call-interactively 'sp-backward-delete-char)))))

(dotemacs-use-package-add-hook stickyfunc-enhance
  :post-init
  (add-hook 'python-mode-hook 'dotemacs-lazy-load-stickyfunc-enhance))

(dotemacs-use-package-add-hook helm-cscope
  :pre-init
  (dotemacs-use-package-add-hook xcscope
    :post-init
    (dolist (mode '(c-mode c++-mode))
      (dotemacs-setup-helm-cscope mode)
      (dotemacs-set-leader-keys-for-major-mode mode "gi" 'cscope-index-files))))

;; We can safely declare this function, since we'll only call it in Python Mode,
;; that is, when python.el was already loaded.
;; (declare-function python-shell-calculate-exec-path "python")

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
              'flycheck-virtualenv-set-python-executables 'local)))

;; (dotemacs-use-package-add-hook flycheck
;;   :post-init
;;   (dotemacs/add-flycheck-hook 'flycheck-virtualenv-setup))

(provide 'module-python)
;;; module-python.el ends here
