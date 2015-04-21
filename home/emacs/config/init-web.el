(lazy-major-mode "\\.coffee\\'" coffee-mode)
(lazy-major-mode "\\.jade$" jade-mode)


(after "js2-mode-autoloads"
  (require 'skewer-mode)
  (skewer-setup))


(require 'rainbow-mode)
(add-hook 'js2-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'stylus-mode-hook 'rainbow-mode)
(add-hook 'mustache-mode-hook 'rainbow-mode)
(add-hook 'handlebars-mode-hook 'rainbow-mode)


(defun my-emmet-mode ()
  (require 'emmet-mode)
  (emmet-mode))

(add-hook 'css-mode-hook 'my-emmet-mode)
(add-hook 'sgml-mode-hook 'my-emmet-mode)
(add-hook 'web-mode-hook 'my-emmet-mode)


(lazy-major-mode "\\.html?$" web-mode)


(after 'web-mode
  (after 'yasnippet
    (require 'angular-snippets)
    (angular-snippets-initialize)))


;; indent after deleting a tag
(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))

;; http://www.shallowsky.com/dotfiles/.emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special code for html and text files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Want auto-fill-mode for some text and html files, but not all.
;; So define two derived modes for that, and we'll use auto-mode-alist
;; to choose them based on filename.
(define-derived-mode html-wrap-mode html-mode "HTML wrap mode"
  (auto-fill-mode))
(define-derived-mode text-wrap-mode text-mode "Text wrap mode"
  (auto-fill-mode))

;; Don't fill when hitting return, only on space:
(set-char-table-range auto-fill-chars 10 nil)

;; In text mode, I don't want it auto-indenting for the first
;; line in the file, or lines following blank lines.
;; Everywhere else is okay.
(defun newline-and-text-indent ()
  "Insert a newline, then indent the next line sensibly for text"
  (interactive)
  (cond
   ;; Beginning of buffer, or beginning of an existing line, don't indent:
   ((or (bobp) (bolp)) (newline))

   ;; If we're on a whitespace-only line,
   ((and (eolp)
         (save-excursion (re-search-backward "^\\(\\s \\)*$"
                                             (line-beginning-position) t)))
    ;; ... delete the whitespace, then add another newline:
    (kill-line 0)
    (newline))

   ;; Else (not on whitespace-only) insert a newline,
   ;; then add the appropriate indent:
   (t (newline-and-indent))
   ;; If the previous set-char-table-range doesn't work to prevent Return
   ;; from indenting the current line, use this instead of previous line:
   ;;(t (insert "\n")
   ;;   (indent-according-to-mode))
   ))

(defun text-indent-hook ()
  (local-set-key "\C-m" 'newline-and-text-indent)
  (flyspell-mode 1)
  )
(setq text-mode-hook 'text-indent-hook)

;;
;; Define keys for inserting tags in HTML mode:
;;
(defun html-hook ()
  (local-set-key "\C-cb" (lambda () (interactive) (sgml-tag "b")))
  (local-set-key "\C-ci" (lambda () (interactive) (sgml-tag "i")))
  (local-set-key "\C-cp" (lambda () (interactive) (sgml-tag "pre")))
  (local-set-key "\C-m" (lambda () (interactive) (insert "\n")))
  (flyspell-mode 1)
  )
(setq sgml-mode-hook 'html-hook)

(provide 'init-web)
