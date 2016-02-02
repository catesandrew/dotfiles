;;; JavaScript
(require 'module-global)

(dotemacs-defvar-company-backends js2-mode)

(setq javascript/key-binding-prefixes '(("mh" . "documentation")
                                        ("mg" . "goto")
                                        ("mr" . "refactor")))
(mapc (lambda (x) (dotemacs-declare-prefix-for-mode
              'js2-mode (car x) (cdr x)))
      javascript/key-binding-prefixes)

;; http://emacs.stackexchange.com/questions/7308/define-key-to-toggle-between-javascript-implementation-and-test-file
(defun dotemacs-js-jump-to (current from to format-name)
  (find-file
   (cl-loop with parts = (reverse current)
            with fname = (file-name-sans-extension (cl-first parts))
            for (name . rest) on (cl-rest parts)
            until (string-equal name from)
            collect name into names
            finally (cl-return
                     (mapconcat 'identity
                                (nconc (reverse rest)
                                       (list to)
                                       (reverse names)
                                       (list (funcall format-name fname) )) "/" )))))

(defun dotemacs-js-format-impl-name (fname)
  (format "%s.js" (replace-regexp-in-string "Spec" "" fname)))

(defun dotemacs-js-format-test-name (fname)
  (format "%sSpec.js" fname))

(defun dotemacs-js-jump-to-implementation-or-test ()
  (interactive)
  (let ((current (split-string (buffer-file-name) "/")))
    (cond
     ((member "test" current) (dotemacs-js-jump-to current "test" "lib" 'dotemacs-js-format-impl-name))
     ((member "lib" current)  (dotemacs-js-jump-to current "lib" "test" 'dotemacs-js-format-test-name))
     (t (error "not within a test or lib directory")))))

(use-package coffee-mode
  :defer t
  :ensure t
  :init
  (progn
    (with-eval-after-load 'flycheck
      (when-let (coffeelint (executable-find "coffeelint"))
        (setq flycheck-coffee-coffeelint-executable coffeelint)))

    (defun javascript/coffee-indent ()
      (if (coffee-line-wants-indent)
          ;; We need to insert an additional tab because the last line was special.
          (coffee-insert-spaces (+ (coffee-previous-indent) coffee-tab-width))
        ;; otherwise keep at the same indentation level
        (coffee-insert-spaces (coffee-previous-indent)))
      )
    ;; indent to right position after `evil-open-below' and `evil-open-above'
    (add-hook 'coffee-mode-hook '(lambda ()
                                   (setq indent-line-function 'javascript/coffee-indent
                                         evil-shift-width coffee-tab-width)))))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook js2-mode))))

(use-package company-tern       ; JavaScript backend for Company
  :if (eq dotemacs-completion-engine 'company)
  :ensure t
  :defer t
  :init
  (progn
    (push 'company-tern company-backends-js2-mode)))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (progn
    (dolist (mode '(coffee-mode js2-mode json-mode))
      (dotemacs/add-flycheck-hook mode))
    (add-hook 'js2-mode-hook 'dotemacs-flycheck-init-javascript)))

(dotemacs-use-package-add-hook flycheck
  :post-config
  (progn
    (dotemacs-flycheck-executables-search)
    (with-eval-after-load 'projectile
      (progn
        (add-hook 'projectile-after-switch-project-hook
                  (lambda ()
                    (dotemacs-eslint-set-local-eslint-from-projectile)
                    (dotemacs-flycheck-executables-updated)))

        (dotemacs-eslint-set-local-eslint-from-projectile)
        (dotemacs-flycheck-executables-updated)))))

(use-package js-doc
  :defer t
  :ensure t
  :init
  (progn
    (defun dotemacs-js-doc-require ()
      "Lazy load js-doc"
      (require 'js-doc))
    (add-hook 'js2-mode-hook 'dotemacs-js-doc-require)

    (setq js-doc-mail-address "catesandrew@gmail.com"
          js-doc-author (format "Andrew Cates <%s>" js-doc-mail-address)
          js-doc-url "https://git.ibaset.com/u/andrew"
          js-doc-license "SEE LICENSE IN LICENSE.md"
          js-doc-parameter-line " * @param {} %p\n")

    (setq js-doc-file-doc-lines
            '(js-doc-top-line
              " * @file\n"
              " * @name %F\n"
              " * @author %a\n"
              " * @license %l\n"
              js-doc-bottom-line))

    (setq js-doc-all-tag-alist
          '(("abstract" . "This member must be implemented (or overridden) by the inheritor.")
            ("virtual" . "This member must be implemented (or overridden) by the inheritor.")
            ("access" . "Specify the access level of this member (private, public, or protected).")
            ("alias" . "Treat a member as if it had a different name.")
            ("augments" . "Indicate that a symbol inherits from, ands adds to, a parent symbol.")
            ("extends" . "Indicate that a symbol inherits from, ands adds to, a parent symbol.")
            ("author" . "Identify the author of an item.")
            ("borrows" . "This object uses something from another object.")
            ("callback" . "Document a callback function.")
            ("class" . "This function is intended to be called with the \"new\" keyword.")
            ("constructor" . "This function is intended to be called with the \"new\" keyword.")
            ("classdesc" . "Use the following text to describe the entire class.")
            ("constant" . "Document an object as a constant.")
            ("const" . "Document an object as a constant.")
            ("constructs" . "This function member will be the constructor for the previous class.")
            ("copyright" . "Document some copyright information.")
            ("default" . "Document the default value.")
            ("defaultvalue" . "Document the default value.")
            ("deprecated" . "Document that this is no longer the preferred way.")
            ("description" . "Describe a symbol.")
            ("desc" . "Describe a symbol.")
            ("enum" . "Document a collection of related properties.")
            ("event" . "Document an event.")
            ("example" . "Provide an example of how to use a documented item.")
            ("exports" . "Identify the member that is exported by a JavaScript module.")
            ("external" . "Identifies an external class, namespace, or module.")
            ("host" . "Identifies an external class, namespace, or module.")
            ("file" . "Describe a file.")
            ("fileoverview" . "Describe a file.")
            ("overview" . "Describe a file.")
            ("fires" . "Describe the events this method may fire.")
            ("emits" . "Describe the events this method may fire.")
            ("function" . "Describe a function or method.")
            ("func" . "Describe a function or method.")
            ("method" . "Describe a function or method.")
            ("global" . "Document a global object.")
            ("ignore" . "Omit a symbol from the documentation.")
            ("implements" . "This symbol implements an interface.")
            ("inheritdoc" . "Indicate that a symbol should inherit its parent's documentation.")
            ("inner" . "Document an inner object.")
            ("instance" . "Document an instance member.")
            ("interface" . "This symbol is an interface that others can implement.")
            ("kind" . "What kind of symbol is this?")
            ("lends" . "Document properties on an object literal as if they belonged to a symbol with a given name.")
            ("license" . "Identify the license that applies to this code.")
            ("listens" . "List the events that a symbol listens for.")
            ("member" . "Document a member.")
            ("var" . "Document a member.")
            ("memberof" . "This symbol belongs to a parent symbol.")
            ("mixes" . "This object mixes in all the members from another object.")
            ("mixin" . "Document a mixin object.")
            ("module" . "Document a JavaScript module.")
            ("name" . "Document the name of an object.")
            ("namespace" . "Document a namespace object.")
            ("override" . "Indicate that a symbol overrides its parent.")
            ("param" . "Document the parameter to a function.")
            ("arg" . "Document the parameter to a function.")
            ("private" . "This symbol is meant to be private.")
            ("property" . "Document a property of an object.")
            ("prop" . "Document a property of an object.")
            ("protected" . "This symbol is meant to be protected.")
            ("public" . "This symbol is meant to be public.")
            ("readonly" . "This symbol is meant to be read-only.")
            ("requires" . "This file requires a JavaScript module.")
            ("returns" . "Document the return value of a function.")
            ("return" . "Document the return value of a function.")
            ("see" . "Refer to some other documentation for more information.")
            ("since" . "When was this feature added?")
            ("static" . "Document a static member.")
            ("summary" . "A shorter version of the full description.")
            ("this" . "What does the 'this' keyword refer to here?")
            ("throws" . "Describe what errors could be thrown.")
            ("exception" . "Describe what errors could be thrown.")
            ("todo" . "Document tasks to be completed.")
            ("tutorial" . "Insert a link to an included tutorial file.")
            ("type" . "Document the type of an object.")
            ("typedef" . "Document a custom type.")
            ("variation" . "Distinguish different objects with the same name.")
            ("version" . "Documents the version number of an item.")))

    (defun dotemacs-js-doc-set-key-bindings (mode)
      "Setup the key bindings for `js2-doc' for the given MODE."
      (dotemacs-set-leader-keys-for-major-mode mode "rdb" 'js-doc-insert-file-doc)
      (dotemacs-set-leader-keys-for-major-mode mode "rdf" 'js-doc-insert-function-doc)
      (dotemacs-set-leader-keys-for-major-mode mode "rdt" 'js-doc-insert-tag)
      (dotemacs-set-leader-keys-for-major-mode mode "rdh" 'js-doc-describe-tag))
    (dotemacs-js-doc-set-key-bindings 'js2-mode)))

(use-package js2-mode                   ; Javascript editing
  :defer t
  :ensure t
  :init
  (progn
    (defun dotemacs-js2-mode-defaults ()
      "Default js2-mode coding hook."
      (unless (bound-and-true-p my-js2mh-ran)
        ;; add buffer-local indicator for whether `js2-prog-mode-hook` has run.
        (set (make-local-variable 'my-js2mh-ran) t)))
    (setq dotemacs-js2-mode-hook #'dotemacs-js2-mode-defaults)
    (add-hook 'js2-mode-hook
              (lambda () (run-hooks #'dotemacs-js2-mode-hook)))

    ;; Let flycheck handle parse errors
    (setq js2-show-parse-errors nil
          js2-use-font-lock-faces t
          js2-strict-missing-semi-warning nil
          js2-highlight-external-variables nil
          js2-include-browser-externs t
          js2-include-node-externs t
          js2-missing-semi-one-line-override t
          js2-strict-cond-assign-warning nil
          js2-strict-inconsistent-return-warning nil
          js2-strict-trailing-comma-warning nil
          js2-strict-var-hides-function-arg-warning nil
          js2-strict-var-redeclaration-warning nil
          js2-warn-about-unused-function-arguments nil
          js2-strict-trailing-comma-warning nil)

    (add-to-list 'auto-mode-alist '("\\.jshintrc$" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.jscsrc$" . json-mode))
    (add-to-list 'auto-mode-alist '("\\.eslintrc$" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook #'js2-highlight-unused-variables-mode))
  :config
  (progn
    (setq-default js2-basic-offset 2)
    (setq-default js-indent-level 2)

    (setq js2-global-externs '("__dirname" "_" "describe" "it" "before" "after" "beforeEach" "afterEach" "chai" "sinon" "asyncTest" "ok" "equal" "notEqual" "deepEqual" "expect"))

    (dotemacs-declare-prefix-for-mode 'js2-mode "mz" "folding")
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode "w" 'js2-mode-toggle-warnings-and-errors)
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode "zc" 'js2-mode-hide-element)
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode "zo" 'js2-mode-show-element)
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode "zr" 'js2-mode-show-all)
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode "ze" 'js2-mode-toggle-element)
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode "zF" 'js2-mode-toggle-hide-functions)
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode "zC" 'js2-mode-toggle-hide-comments)))

(use-package evil-matchit-js2
  :defer t
  :ensure evil-matchit
  :init (add-hook `js2-mode-hook `turn-on-evil-matchit-mode))

(use-package js2-refactor
  :defer t
  :ensure t
  :init
  (progn
    (defun javascript/load-js2-refactor ()
      "Lazy load js2-refactor"
      (require 'js2-refactor))
    (add-hook 'js2-mode-hook 'javascript/load-js2-refactor)

    (defun dotemacs-js2-refactor-set-key-bindings (mode)
      (dotemacs-declare-prefix-for-mode 'js2-mode "mr3" "ternary")
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "r3i" 'js2r-ternary-to-if)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mra" "add/args")
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rag" 'js2r-add-to-globals-annotation)
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rao" 'js2r-arguments-to-object)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mrb" "barf")
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rba" 'js2r-forward-barf)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mrc" "contract")
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rca" 'js2r-contract-array)
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rco" 'js2r-contract-object)
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rcu" 'js2r-contract-function)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mre" "expand/extract")
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rea" 'js2r-expand-array)
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "ref" 'js2r-extract-function)
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rem" 'js2r-extract-method)
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "reo" 'js2r-expand-object)
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "reu" 'js2r-expand-function)
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rev" 'js2r-extract-var)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mri" "inline/inject/introduct")
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rig" 'js2r-inject-global-in-iife)
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rip" 'js2r-introduce-parameter)
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "riv" 'js2r-inline-var)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mrl" "localize/log")
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rlp" 'js2r-localize-parameter)
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rlt" 'js2r-log-this)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mrr" "rename")
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rrv" 'js2r-rename-var)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mrs" "split/slurp")
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rsl" 'js2r-forward-slurp)
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rss" 'js2r-split-string)
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rsv" 'js2r-split-var-declaration)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mrt" "toggle")
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rtf" 'js2r-toggle-function-expression-and-declaration)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mru" "unwrap")
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "ruw" 'js2r-unwrap)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mrv" "var")
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rvt" 'js2r-var-to-this)

      (dotemacs-declare-prefix-for-mode 'js2-mode "mrw" "wrap")
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rwi" 'js2r-wrap-buffer-in-iife)
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rwl" 'js2r-wrap-in-for-loop)

      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "k" 'js2r-kill)
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "mj" 'js2r-move-line-down)
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "mk" 'js2r-move-line-up))

    (dotemacs-js2-refactor-set-key-bindings 'js2-mode)))

(use-package json-mode                  ; JSON files
  :ensure t
  :defer t)

(use-package json-snatcher
  :defer t
  :ensure t
  :config
  (dotemacs-set-leader-keys-for-major-mode 'json-mode
    "hp" 'jsons-print-path))

(use-package js2-imenu-extras
  :ensure js2-mode
  :defer t
  :init
  (progn
    (require 'js2-imenu-extras)
    (setq js2-imenu-enabled-frameworks 'nil)
    (js2-imenu-extras-mode)
    ;; required to make `<LEADER> s l' to work correctly
    (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)))

(use-package json-reformat              ; Reformat JSON
  :ensure t
  :defer t
  :bind (("C-c e j" . json-reformat-region)))

(use-package tern
  :defer t
  :ensure t
  :init (add-hook 'js2-mode-hook 'tern-mode)
  :config
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode "rrV" 'tern-rename-variable)
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode "hd" 'tern-get-docs)
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode "gg" 'tern-find-definition)
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode "gG" 'tern-find-definition-by-name)
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode (kbd "C-g") 'tern-pop-find-definition)
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode "ht" 'tern-get-type)))

(use-package web-beautify
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode  "=" 'web-beautify-js)
    (dotemacs-set-leader-keys-for-major-mode 'json-mode "=" 'web-beautify-js)
    (dotemacs-set-leader-keys-for-major-mode 'web-mode  "=" 'web-beautify-html)
    (dotemacs-set-leader-keys-for-major-mode 'css-mode  "=" 'web-beautify-css)))

(provide 'module-javascript)
;;; module-javascript.el ends here
