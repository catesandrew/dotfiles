;;; module-web.el --- WEB Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-vars)
;; (require 'core-funcs)
(require 'use-package)
(require 'core-keybindings)
(require 'core-display-init)
(require 'core-auto-completion)
(require 'core-transient-state)
(require 'core-use-package-ext)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(dotemacs-defvar-company-backends css-mode)
(dotemacs-defvar-company-backends web-mode)
(dotemacs-defvar-company-backends jade-mode)
(dotemacs-defvar-company-backends slim-mode)
;; TODO: uncomment this when it becomes available
;; (dotemacs-defvar-company-backends haml-mode)

(when (eq dotemacs-completion-engine 'company)
  ;;TODO: whenever company-web makes a backend for haml-mode it should be added here.
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook css-mode)
      (dotemacs-add-company-hook jade-mode)
      (dotemacs-add-company-hook slim-mode)
      (dotemacs-add-company-hook web-mode)))

  (use-package company-web :ensure t))

;; Thanks to `impatient-mode` you can see the effect of your HTML as you type it.
(use-package impatient-mode
  :ensure t
  :defer t
  :init
  (progn
    (defun dotemacs-impatient-mode-hook()
      "my web mode hook for HTML REPL"
      (interactive)
      (impatient-mode)
      (httpd-start))
    (add-hook 'web-mode-hook 'dotemacs-impatient-mode-hook)))

(use-package css-eldoc                  ; Basic Eldoc for CSS
  :ensure t
  :commands (turn-on-css-eldoc)
  :init (add-hook 'css-mode-hook 'turn-on-css-eldoc))

(use-package css-mode
  :defer t
  :ensure t
  :init
  (progn
    (push 'company-css company-backends-css-mode)

    ;; Mark `css-indent-offset' as safe-local variable
    (put 'css-indent-offset 'safe-local-variable #'integerp)

    ;; Run Prog Mode hooks, because for whatever reason CSS Mode derives from
    ;; `fundamental-mode'.
    ;; (add-hook 'css-mode-hook (lambda () (run-hooks 'dotemacs-prog-mode-hook)))

    ;; Custom iMenu
    (defun css-imenu-make-index ()
      (save-excursion
        (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))
    (add-hook 'css-mode-hook
              (lambda ()
                (setq imenu-create-index-function 'css-imenu-make-index)))

    (defun css-expand-statement ()
      "Expand CSS block"
      (interactive)
      (save-excursion
        (end-of-line)
        (search-backward "{")
        (forward-char 1)
        (while (or (eobp) (not (looking-at "}")))
        (let ((beg (point)))
          (newline)
          (search-forward ";")
          (indent-region beg (point))
          ))
        (newline)))

    (defun css-contract-statement ()
      "Contract CSS block"
      (interactive)
      (end-of-line)
      (search-backward "{")
      (while (not (looking-at "}"))
        (join-line -1)))

    (dotemacs-set-leader-keys-for-major-mode 'css-mode
      "zc" 'css-contract-statement
      "zo" 'css-expand-statement)))

(use-package emmet-mode
  :defer t
  :ensure t
  :init
  (progn
    (setq emmet-indentation 2
          emmet-move-cursor-between-quotes t)
    (dotemacs/add-to-hooks 'emmet-mode '(css-mode-hook
                                         html-mode-hook
                                         sass-mode-hook
                                         scss-mode-hook
                                         web-mode-hook)))
  :config
  (progn
    (evil-define-key 'insert emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
    (evil-define-key 'insert emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
    (evil-define-key 'emacs emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
    (evil-define-key 'emacs emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
    (evil-define-key 'hybrid emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
    (evil-define-key 'hybrid emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
    (dotemacs-hide-lighter emmet-mode)))

(dotemacs-use-package-add-hook evil-matchit
  :post-init
  (add-hook `web-mode-hook `turn-on-evil-matchit-mode))

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dolist (mode '(haml-mode
                  jade-mode
                  less-mode
                  sass-mode
                  scss-mode
                  slim-mode
                  ;; css-mode
                  web-mode))
    (dotemacs/add-flycheck-hook mode))
  :post-config
  (progn
    (dotemacs-flycheck-executables-search)
    (when (bound-and-true-p dotemacs//flycheck-executables-searched)
      (when dotemacs//flycheck-executable-tidy5
        (flycheck-add-mode 'html-tidy 'web-mode)))))

(use-package haml-mode
  :ensure t
  :defer t)

(use-package helm-css-scss
  :defer t
  :ensure t
  :init
  (dolist (mode '(css-mode scss-mode))
    (dotemacs-set-leader-keys-for-major-mode mode "gh" 'helm-css-scss)))

(use-package jade-mode
  :defer t
  :ensure t)

(use-package less-css-mode
  :defer t
  :ensure t
  :mode ("\\.less\\'" . less-css-mode))

(use-package sass-mode
  :ensure t
  :defer t
  :mode ("\\.sass\\'" . sass-mode))

(use-package scss-mode
  :defer t
  :ensure t
  :mode ("\\.scss\\'" . scss-mode))

(use-package slim-mode
  :ensure t
  :defer t)

(dotemacs-use-package-add-hook smartparens
  :post-init
  (progn
    (dotemacs/add-to-hooks
     (if dotemacs-smartparens-strict-mode
         'smartparens-strict-mode
       'smartparens-mode)
     '(css-mode-hook scss-mode-hook sass-mode-hook less-css-mode-hook))
    (add-hook 'web-mode-hook 'dotemacs-toggle-smartparens-off))
  :post-config
  (sp-with-modes '(web-mode)
    (sp-local-pair "<% " " %>")
    (sp-local-pair "{ " " }")
    (sp-local-pair "<%= "  " %>")
    (sp-local-pair "<%# "  " %>")
    (sp-local-pair "<%$ "  " %>")
    (sp-local-pair "<%@ "  " %>")
    (sp-local-pair "<%: "  " %>")
    (sp-local-pair "{{ "  " }}")
    (sp-local-pair "{% "  " %}")
    (sp-local-pair "{%- "  " %}")
    (sp-local-pair "{# "  " #}")))

(use-package tagedit
  :defer t
  :ensure t
  :config
  (progn
    (tagedit-add-experimental-features)
    (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))
    (dotemacs-diminish tagedit-mode " Ⓣ" " T")))

(dotemacs-use-package-add-hook rainbow-delimiters
  :post-init
  (progn
    (dotemacs/add-to-hooks 'rainbow-delimiters-mode '(haml-mode-hook
                                                      jade-mode-hook
                                                      less-css-mode-hook
                                                      sass-mode-hook
                                                      css-mode-hook
                                                      scss-mode-hook
                                                      slim-mode-hook))))

(use-package web-mode                   ; Template editing
  :defer t
  :ensure t
  :init
  (push '(company-web-html company-css) company-backends-web-mode)
  :config
  (progn
    (setq web-mode-enable-auto-pairing nil
          web-mode-markup-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-sql-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-attr-indent-offset 2
          web-mode-style-padding 2
          web-mode-script-padding 2)

    (add-hook 'web-mode-hook
      (lambda ()
        (run-hooks 'dotemacs-prog-mode-hook)
        ; todo: verify this is not needed now
        ; (when (equal web-mode-content-type "jsx")
        ;   (setq-local cursor-type nil)
        ;   (with-eval-after-load 'flycheck
        ;     (flycheck-select-checker 'javascript-eslint)))
        ))

    (dotemacs-set-leader-keys-for-major-mode 'web-mode
      "eh" 'web-mode-dom-errors-show
      "gb" 'web-mode-element-beginning
      "gc" 'web-mode-element-child
      "gp" 'web-mode-element-parent
      "gs" 'web-mode-element-sibling-next
      "hp" 'web-mode-dom-xpath
      "rc" 'web-mode-element-clone
      "rd" 'web-mode-element-vanish
      "rk" 'web-mode-element-kill
      "rr" 'web-mode-element-rename
      "rw" 'web-mode-element-wrap
      "z" 'web-mode-fold-or-unfold
      ;; TODO element close would be nice but broken with evil.
      )

  ;;   (defvar dotemacs--web-mode-ms-doc-toggle 0
  ;;     "Display a short doc when nil, full doc otherwise.")

  ;;     (defun dotemacs-web-mode-ms-doc ()
  ;;       (if (equal 0 dotemacs--web-mode-ms-doc-toggle)
  ;;           "[_?_] for help"
  ;;         "
  ;; [_?_] display this help
  ;; [_k_] previous [_j_] next   [_K_] previous sibling [_J_] next sibling
  ;; [_h_] parent   [_l_] child  [_c_] clone [_d_] delete [_D_] kill [_r_] rename
  ;; [_w_] wrap     [_p_] xpath
  ;; [_q_] quit"))

  ;;   (defun dotemacs-web-mode-ms-toggle-doc ()
  ;;     (interactive)
  ;;     (setq dotemacs--web-mode-ms-doc-toggle
  ;;           (logxor dotemacs--web-mode-ms-doc-toggle 1)))

    (dotemacs-define-transient-state web-mode
      :title "Web-mode Transient State"
      :columns 4
      :foreign-keys run
      :bindings
      ("j" web-mode-element-next "next")
      ("J" web-mode-element-sibling-next "next sibling")
      ("gj" web-mode-element-sibling-next)
      ("k" web-mode-element-previous "previous")
      ("K" web-mode-element-sibling-previous "previous sibling")
      ("gk" web-mode-element-sibling-previous)
      ("h" web-mode-element-parent "parent")
      ("l" web-mode-element-child "child")
      ("c" web-mode-element-clone "clone")
      ("d" web-mode-element-vanish "delete")
      ("D" web-mode-element-kill "kill")
      ("r" web-mode-element-rename "rename" :exit t)
      ("w" web-mode-element-wrap "wrap")
      ("p" web-mode-dom-xpath "xpath")
      ("q" nil "quit" :exit t)
      ("<escape>" nil nil :exit t))
    (dotemacs-set-leader-keys-for-major-mode 'web-mode
      "." 'dotemacs/web-mode-transient-state/body)

  ; todo: verify these settigns work: And if you want to have 2 space indent
  ; also for element's attributes, concatenations and contiguous function
  ; calls:
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

  :mode
  (("\\.phtml\\'"      . web-mode)
   ("\\.tpl\\.php\\'"  . web-mode)
   ("\\.twig\\'"       . web-mode)
   ("\\.html\\'"       . web-mode)
   ("\\.htm\\'"        . web-mode)
   ("\\.[gj]sp\\'"     . web-mode)
   ("\\.as[cp]x?\\'"   . web-mode)
   ("\\.eex\\'"        . web-mode)
   ("\\.erb\\'"        . web-mode)
   ("\\.mustache\\'"   . web-mode)
   ("\\.handlebars\\'" . web-mode)
   ("\\.hbs\\'"        . web-mode)
   ("\\.eco\\'"        . web-mode)
   ("\\.jsx\\'"        . web-mode)
   ("\\.ejs\\'"        . web-mode)
   ("\\.djhtml\\'"     . web-mode)))

(dotemacs-use-package-add-hook yasnippet
  :post-init
  (progn
    (dotemacs/add-to-hooks 'dotemacs-load-yasnippet '(css-mode-hook
                                                      jade-mode
                                                      slim-mode))))

(provide 'module-web)
;;; module-web.el ends here
