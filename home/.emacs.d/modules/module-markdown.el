;;; Markdown
(require 'module-global)

(dotemacs-defvar-company-backends markdown-mode)

;; Insert key for org-mode and markdown a la C-h k
;; from SE endless http://emacs.stackexchange.com/questions/2206/i-want-to-have-the-kbd-tags-for-my-blog-written-in-org-mode/2208#2208
(defun dotemacs-insert-keybinding-markdown (key)
  "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
  (interactive "kType key sequence: ")
  (let* ((tag "~%s~"))
    (if (null (equal key "\r"))
        (insert
         (format tag (help-key-description key nil)))
      (insert (format tag ""))
      (forward-char -6))))

(use-package gh-md
  :ensure t
  :defer t
  :init
  (dotemacs-set-leader-keys-for-major-mode 'markdown-mode
    "cr"  'gh-md-render-buffer))

(dotemacs-use-package-add-hook smartparens
  :post-init
  (progn
    (add-hook 'markdown-mode-hook 'smartparens-mode)))

(use-package markdown-mode              ; Markdown
  :mode (("\\.m[k]d" . markdown-mode)
         ("\\.markdown$" . markdown-mode)
         ("\\.apib$" . markdown-mode))
  :defer t
  :ensure t
  :init
  (progn
    ;; http://www.tychoish.com/posts/imenu-for-markdown-and-writing/
    (setq markdown-imenu-generic-expression
          '(("title"  "^\\(.*\\)[\n]=+$" 1)
            ("h2-"    "^\\(.*\\)[\n]-+$" 1)
            ("h1"   "^# \\(.*\\)$" 1)
            ("h2"   "^## \\(.*\\)$" 1)
            ("h3"   "^### \\(.*\\)$" 1)
            ("h4"   "^#### \\(.*\\)$" 1)
            ("h5"   "^##### \\(.*\\)$" 1)
            ("h6"   "^###### \\(.*\\)$" 1)
            ("fn"   "^\\[\\^\\(.*\\)\\]" 1))))
  :config
  (progn
    ;; Process Markdown with Pandoc, using a custom stylesheet for nice output
    (let ((stylesheet (expand-file-name
                       (locate-user-emacs-file "etc/pandoc.css"))))
      (setq markdown-command
            (mapconcat #'shell-quote-argument
                       `("pandoc" "--toc" "--section-divs"
                         "--css" ,(concat "file://" stylesheet)
                         "--standalone" "-f" "markdown" "-t" "html5")
                       " ")))

    (when (eq system-type 'darwin)
      (setq markdown-open-command "mark"))

    ;; disable auto indent
    (add-hook 'markdown-mode-hook
              (lambda ()
                (electric-indent-local-mode -1)
                (setq imenu-generic-expression markdown-imenu-generic-expression)))

    ;; No filling in GFM, because line breaks are significant.
    (add-hook 'gfm-mode-hook #'turn-off-auto-fill)
    ;; Use visual lines instead
    (add-hook 'gfm-mode-hook #'visual-line-mode)
    (add-hook 'gfm-mode-hook #'dotemacs-whitespace-style-no-long-lines)

    (bind-key "C-c C-s C" #'markdown-insert-gfm-code-block markdown-mode-map)
    (bind-key "C-c C-s P" #'markdown-insert-gfm-code-block markdown-mode-map)

    ;; Declare prefixes and bind keys
    (dolist (prefix '(("mc" . "markdown/command")
                      ("mh" . "markdown/header")
                      ("mi" . "markdown/insert")
                      ("ml" . "markdown/lists")
                      ("mx" . "markdown/text")))
      (dotemacs-declare-prefix-for-mode
       'markdown-mode (car prefix) (cdr prefix)))

    (dotemacs-set-leader-keys-for-major-mode 'markdown-mode
      ;; Movement
      "{"   'markdown-backward-paragraph
      "}"   'markdown-forward-paragraph
      ;; Completion, and Cycling
      "]"   'markdown-complete
      ;; Indentation
      ">"   'markdown-indent-region
      "<"   'markdown-exdent-region
      ;; Buffer-wide commands
      "c]"  'markdown-complete-buffer
      "cc"  'markdown-check-refs
      "ce"  'markdown-export
      "cm"  'markdown-other-window
      "cn"  'markdown-cleanup-list-numbers
      "co"  'markdown-open
      "cp"  'markdown-preview
      "cv"  'markdown-export-and-preview
      "cw"  'markdown-kill-ring-save
      ;; headings
      "hi"  'markdown-insert-header-dwim
      "hI"  'markdown-insert-header-setext-dwim
      "h1"  'markdown-insert-header-atx-1
      "h2"  'markdown-insert-header-atx-2
      "h3"  'markdown-insert-header-atx-3
      "h4"  'markdown-insert-header-atx-4
      "h5"  'markdown-insert-header-atx-5
      "h6"  'markdown-insert-header-atx-6
      "h!"  'markdown-insert-header-setext-1
      "h@"  'markdown-insert-header-setext-2
      ;; Insertion of common elements
      "-"   'markdown-insert-hr
      "if"  'markdown-insert-footnote
      "ii"  'markdown-insert-image
      "ik"  'dotemacs-insert-keybinding-markdown
      "iI"  'markdown-insert-reference-image
      "il"  'markdown-insert-link
      "iL"  'markdown-insert-reference-link-dwim
      "iw"  'markdown-insert-wiki-link
      "iu"  'markdown-insert-uri
      ;; Element removal
      "k"   'markdown-kill-thing-at-point
      ;; List editing
      "li"  'markdown-insert-list-item
      ;; region manipulation
      "xb"  'markdown-insert-bold
      "xi"  'markdown-insert-italic
      "xc"  'markdown-insert-code
      "xC"  'markdown-insert-gfm-code-block
      "xq"  'markdown-insert-blockquote
      "xQ"  'markdown-blockquote-region
      "xp"  'markdown-insert-pre
      "xP"  'markdown-pre-region
      ;; Following and Jumping
      "N"   'markdown-next-link
      "f"   'markdown-follow-thing-at-point
      "P"   'markdown-previous-link
      "<RET>" 'markdown-jump)

    ;; Header navigation in normal state movements
    (evil-define-key 'normal markdown-mode-map
      "gj" 'outline-forward-same-level
      "gk" 'outline-backward-same-level
      "gh" 'outline-up-heading
      ;; next visible heading is not exactly what we want but close enough
      "gl" 'outline-next-visible-heading)

    ;; Promotion, Demotion
    (define-key markdown-mode-map (kbd "M-h") 'markdown-promote)
    (define-key markdown-mode-map (kbd "M-j") 'markdown-move-down)
    (define-key markdown-mode-map (kbd "M-k") 'markdown-move-up)
    (define-key markdown-mode-map (kbd "M-l") 'markdown-demote)))

(use-package markdown-toc
  :ensure t
  :defer t)

(use-package mmm-mode
  :commands mmm-parse-buffer
  :ensure t
  :init
  (dotemacs-set-leader-keys-for-major-mode 'markdown-mode
    ;; Highlight code blocks
    "cs"   'mmm-parse-buffer)
  :config
  (progn
    (mmm-add-classes '((markdown-python
                        :submode python-mode
                        :face mmm-declaration-submode-face
                        :front "^```python[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-html
                        :submode web-mode
                        :face mmm-declaration-submode-face
                        :front "^```html[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-java
                        :submode java-mode
                        :face mmm-declaration-submode-face
                        :front "^```java[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-ruby
                        :submode ruby-mode
                        :face mmm-declaration-submode-face
                        :front "^```ruby[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-c
                        :submode c-mode
                        :face mmm-declaration-submode-face
                        :front "^```c[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-c++
                        :submode c++-mode
                        :face mmm-declaration-submode-face
                        :front "^```c\+\+[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-elisp
                        :submode emacs-lisp-mode
                        :face mmm-declaration-submode-face
                        :front "^```elisp[\n\r]+"
                        :back "^```$")))
    (setq mmm-global-mode t)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-python)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-java)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-ruby)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-c)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-c++)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-elisp)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-html)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook markdown-mode)
      (push 'company-capf company-backends-markdown-mode)))
  (dotemacs-use-package-add-hook company-emoji
    :post-init
    (progn
      (push 'company-emoji company-backends-markdown-mode))))

(dotemacs-use-package-add-hook flyspell
  :post-init
  (spell-checking/add-flyspell-hook 'markdown-mode))

(dotemacs-use-package-add-hook emoji-cheat-sheet-plus
  :post-init
  (add-hook 'markdown-mode-hook 'emoji-cheat-sheet-plus-display-mode))

(provide 'module-markdown)
;;; module-markdown.el ends here
