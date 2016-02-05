;;; module-javascript.el --- JavaScript Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; npm install -g js-beautify, jshint, eslint, tern
;;
;; - Smart code folding
;; - Refactoring: done using `js2-refactor`.
;; - Auto-completion and documentation: provided by `tern`
;; - CoffeeScript support
;; - Formatting with `web-beautify`
;; - Get the path to a JSON value with `json-snatcher`
;; - REPL available via `skewer-mode` and `livid-mode`
;;
;; (require 'core-vars)
;; (require 'core-funcs)
(require 'core-fonts-support)
(require 'core-toggle)
(require 'core-keybindings)
(require 'core-use-package-ext)
(require 'core-auto-completion)
;; (require 'core-display-init)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(dotemacs-defvar-company-backends js2-mode)

(setq javascript/key-binding-prefixes '(("mh" . "documentation")
                                        ("mg" . "goto")
                                        ("mr" . "refactor")))
(mapc (lambda (x) (dotemacs-declare-prefix-for-mode
              'js2-mode (car x) (cdr x)))
      javascript/key-binding-prefixes)

(defvar javascript-disable-tern-port-files t
  "Stops tern from creating tern port files.")

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
      (dotemacs-add-company-hook js2-mode)))

  (use-package company-tern       ; JavaScript backend for Company
    :ensure t
    :defer t
    :init
    (progn
      (push 'company-tern company-backends-js2-mode))))

;; (dotemacs-use-package-add-hook flycheck
;;   :post-init
;;   (dolist (mode '(coffee-mode js2-mode json-mode))
;;     (dotemacs-add-flycheck-hook mode)))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (progn
    (dolist (mode '(coffee-mode js2-mode json-mode))
      (dotemacs/add-flycheck-hook mode))
    (add-hook 'js2-mode-hook 'dotemacs-flycheck-init-javascript)

    (defun dotemacs/disable-js2-checks-if-flycheck-active ()
      (unless (flycheck-get-checker-for-buffer)
        (set (make-local-variable 'js2-mode-show-parse-errors) t)
        (set (make-local-variable 'js2-mode-show-strict-warnings) t)))
    (add-hook 'js2-mode-hook 'dotemacs/disable-js2-checks-if-flycheck-active))
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
    (defun dotemacs/js-doc-require ()
      "Lazy load js-doc"
      (require 'js-doc))
    (add-hook 'js2-mode-hook 'dotemacs/js-doc-require)

    (setq js-doc-mail-address "catesandrew@gmail.com"
          js-doc-author (format "Andrew Cates <%s>" js-doc-mail-address)
          js-doc-url "https://git.ibaset.com/u/andrew"
          js-doc-license "SEE LICENSE IN LICENSE.md"
          js-doc-parameter-line " * @param {} %p\n")

    (setq js-doc-file-doc-lines
            '(js-doc-top-line
              " * @module %F\n"
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

    (defun dotemacs/js-doc-set-key-bindings (mode)
      "Setup the key bindings for `js2-doc' for the given MODE."
      (dotemacs-set-leader-keys-for-major-mode mode "rdb" 'js-doc-insert-file-doc)
      (dotemacs-set-leader-keys-for-major-mode mode "rdf" 'js-doc-insert-function-doc)
      (dotemacs-set-leader-keys-for-major-mode mode "rdt" 'js-doc-insert-tag)
      (dotemacs-set-leader-keys-for-major-mode mode "rdh" 'js-doc-describe-tag))
    (dotemacs/js-doc-set-key-bindings 'js2-mode)))

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
    (setq dotemacs-js2-mode-hook 'dotemacs-js2-mode-defaults)
    (add-hook 'js2-mode-hook
              (lambda () (run-hooks 'dotemacs-js2-mode-hook)))

    (add-to-list 'auto-mode-alist '("\\.tern-project\\'" . json-mode))
    (add-to-list 'auto-mode-alist '("\\.jshintrc$" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.jscsrc$" . json-mode))
    (add-to-list 'auto-mode-alist '("\\.eslintrc$" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook 'js2-highlight-unused-variables-mode))
  :config
  (progn
    (setq-default js2-basic-offset 2)
    (setq-default js-indent-level 2)
    (setq-default js2-use-font-lock-faces t)
    (setq-default js2-include-browser-externs t)
    (setq-default js2-include-node-externs t)
    (setq-default js2-missing-semi-one-line-override t)
    (setq-default js2-strict-cond-assign-warning nil)
    (setq-default js2-strict-var-hides-function-arg-warning nil)
    (setq-default js2-strict-var-redeclaration-warning nil)
    (setq-default js2-warn-about-unused-function-arguments nil)
    (setq-default js2-allow-rhino-new-expr-initializer nil)
    (setq-default js2-auto-indent-p nil)
    (setq-default js2-enter-indents-newline nil)
    (setq-default js2-mirror-mode nil)
    (setq-default js2-strict-inconsistent-return-warning nil)
    (setq-default js2-include-rhino-externs nil)
    (setq-default js2-include-gears-externs nil)
    (setq-default js2-concat-multiline-strings 'eol)
    (setq-default js2-rebind-eol-bol-keys nil)
    (setq-default js2-bounce-indent nil)
    (setq-default js2-indent-switch-body t)
    (setq-default js2-global-externs '("__dirname"
                                       "_"
                                       "describe"
                                       "it"
                                       "before"
                                       "after"
                                       "beforeEach"
                                       "afterEach"
                                       "chai"
                                       "sinon"
                                       "expect"
                                       ))

    ;; Let flycheck handle parse errors
    (setq-default js2-show-parse-errors nil)
    (setq-default js2-strict-missing-semi-warning nil)
    (setq-default js2-highlight-external-variables t)
    (setq-default js2-strict-trailing-comma-warning nil)

    (dotemacs-declare-prefix-for-mode 'js2-mode "mz" "folding")
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode "w" 'js2-mode-toggle-warnings-and-errors)
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode "zc" 'js2-mode-hide-element)
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode "zo" 'js2-mode-show-element)
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode "zr" 'js2-mode-show-all)
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode "ze" 'js2-mode-toggle-element)
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode "zF" 'js2-mode-toggle-hide-functions)
    (dotemacs-set-leader-keys-for-major-mode 'js2-mode "zC" 'js2-mode-toggle-hide-comments)))

(dotemacs-use-package-add-hook evil-matchit
  :post-init
  (add-hook `js2-mode-hook `turn-on-evil-matchit-mode))

(use-package js2-refactor
  :defer t
  :ensure t
  :init
  (progn
    (defun dotemacs/js2-refactor-require ()
      "Lazy load js2-refactor."
      (require 'js2-refactor))
    (add-hook 'js2-mode-hook 'dotemacs/js2-refactor-require)

    (defun dotemacs/js2-refactor-set-key-bindings (mode)
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

      (dotemacs-declare-prefix-for-mode mode "mx" "text")
      (dotemacs-declare-prefix-for-mode mode "mxm" "move")
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "mj" 'js2r-move-line-down)
      (dotemacs-set-leader-keys-for-major-mode 'js2-mode "mk" 'js2r-move-line-up))

    (dotemacs/js2-refactor-set-key-bindings 'js2-mode)))

(use-package js2-imenu-extras
  :ensure js2-mode
  :defer t
  :init
  (progn
    (require 'js2-imenu-extras)
    (setq js2-imenu-enabled-frameworks 'nil)
    (js2-imenu-extras-mode)
    ;; required to make `<LEADER> s l' to work correctly
    (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)

    (defun js2-imenu-make-index ()
      (interactive)
      (save-excursion
        (imenu--generic-function '(("describe" "\\s-*describe\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("it" "\\s-*it\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("before" "\\s-*before\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("after" "\\s-*after\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("Controller" "[. \t]controller([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Controller" "[. \t]controllerAs:[ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Filter" "[. \t]filter([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("State" "[. \t]state([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Factory" "[. \t]factory([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Service" "[. \t]service([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Module" "[. \t]module([ \t]*['\"]\\([a-zA-Z0-9_\.]+\\)" 1)
                                   ("ngRoute" "[. \t]when(\\(['\"][a-zA-Z0-9_\/]+['\"]\\)" 1)
                                   ("Directive" "[. \t]directive([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Event" "[. \t]\$on([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Config" "[. \t]config([ \t]*function *( *\\([^\)]+\\)" 1)
                                   ("Config" "[. \t]config([ \t]*\\[ *['\"]\\([^'\"]+\\)" 1)
                                   ("OnChange" "[ \t]*\$(['\"]\\([^'\"]*\\)['\"]).*\.change *( *function" 1)
                                   ("OnClick" "[ \t]*\$([ \t]*['\"]\\([^'\"]*\\)['\"]).*\.click *( *function" 1)
                                   ("Watch" "[. \t]\$watch( *['\"]\\([^'\"]+\\)" 1)
                                   ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
                                   ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
                                   ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*:[ \t]*function[ \t]*(" 1)
                                   ("Class" "^[ \t]*var[ \t]*\\([0-9a-zA-Z]+\\)[ \t]*=[ \t]*\\([a-zA-Z]*\\).extend" 1)
                                   ("Class" "^[ \t]*cc\.\\(.+\\)[ \t]*=[ \t]*cc\.\\(.+\\)\.extend" 1)
                                   ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)))))
    ;; (add-hook 'js2-mode-hook
    ;;           (lambda ()
    ;;             (setq imenu-create-index-function 'js2-imenu-make-index)))
    ;; (set-default 'imenu-auto-rescan t)
    ;; (add-hook 'js2-mode-hook 'which-function-mode)
    ))

(use-package json-mode                  ; JSON files
  :ensure t
  :defer t)

(use-package json-snatcher
  :defer t
  :ensure t
  :config
  (dotemacs-set-leader-keys-for-major-mode 'json-mode
    "hp" 'jsons-print-path))

(use-package nodejs-repl
  :ensure t
  :defer t
  :config
  (progn
    ;; nodejs-repl-eval.el --- Summary
    ;; Commentary:
    ;;
    ;; Evaluation functions for the `nodejs-repl' package.  Written on a stormy
    ;; night between days of node hacking.
    ;;
    ;; https://gist.github.com/emallson/0eae865bc99fc9639fac
    ;;
    (defun nodejs-repl-eval-region (start end)
      "Evaluate the region specified by `START' and `END'."
      (let ((proc (get-process nodejs-repl-process-name)))
        (comint-simple-send proc (buffer-substring-no-properties start end))))

    (defun nodejs-repl-eval-node (node)
      "Evaluate `NODE', a `js2-mode' node."
      (let ((beg (js2-node-abs-pos node))
            (end (js2-node-abs-end node)))
        (nodejs-repl-eval-region beg end)))

    (defun nodejs-repl--find-current-or-prev-node (pos &optional include-comments)
      "Locate the first node before `POS'.  Return a node or nil.

If `INCLUDE-COMMENTS' is set to t, then comments are considered
valid nodes.  This is stupid, don't do it."
      (let ((node (js2-node-at-point pos (not include-comments))))
        (if (or (null node)
                (js2-ast-root-p node))
            (unless (= 0 pos)
              (nodejs-repl--find-current-or-prev-node (1- pos) include-comments))
          node)))

    (defun nodejs-repl-eval-function ()
      "Evaluate the current or previous function."
      (interactive)
      (let* ((fn-above-node (lambda (node)
                              (js2-mode-function-at-point (js2-node-abs-pos node))))
             (fn (funcall fn-above-node
                          (nodejs-repl--find-current-or-prev-node
                           (point) (lambda (node)
                                     (not (null (funcall fn-above-node node))))))))
        (unless (null fn)
          (nodejs-repl-eval-node fn))))

    (defun nodejs-repl-eval-first-stmt (pos)
      "Evaluate the first statement found from `POS' by `js2-mode'.

If this statement is a block statement, its first parent
statement is found.  This will be either a function declaration,
function call, or assignment statement."
      (let ((node (js2-mode-find-first-stmt (nodejs-repl--find-current-or-prev-node pos))))
        (cond
         ((js2-block-node-p node) (nodejs-repl-eval-node (js2-node-parent-stmt node)))
         ((not (null node)) (nodejs-repl-eval-node node)))))

    (defun nodejs-repl-eval-dwim ()
      "Heuristic evaluation of JS code in a NodeJS repl.

Evaluates the region, if active, or the first statement found at
or prior to the point.

If the point is at the end of a line, evaluation is done from one
character prior.  In many cases, this will be a semicolon and will
change what is evaluated to the statement on the current line."
      (interactive)
      (cond
       ((use-region-p) (nodejs-repl-eval-region (region-beginning) (region-end)))
       ((= (line-end-position) (point)) (nodejs-repl-eval-first-stmt (1- (point))))
       (t (nodejs-repl-eval-first-stmt (point)))))

    (defun nodejs-repl-eval-buffer (&optional buffer)
      "Evaluate the current buffer or the one given as `BUFFER'.

`BUFFER' should be a string or buffer."
      (interactive)
      (let ((buffer (or buffer (current-buffer))))
        (with-current-buffer buffer
          (nodejs-repl-eval-region (point-min) (point-max)))))))

(use-package json-reformat              ; Reformat JSON
  :ensure t
  :defer t)

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

(use-package livid-mode
  :ensure t
  :defer t
  :init (dotemacs-add-toggle javascript-repl-live-evaluation
          :status livid-mode
          :on (livid-mode)
          :off (livid-mode -1)
          :documentation "Live evaluation of JS buffer change."
          :evil-leader-for-mode (js2-mode . "sa")))

(provide 'module-javascript)
;;; module-javascript.el ends here
