(require 'yasnippet)

;; Use only own snippets, do not use bundled ones
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        "~/.emacs.d/yasnippet-snippets"       ;;
        ))
(yas-global-mode 1)

;; Include snippets for stuff
(require 'buster-snippets)
(require 'angular-snippets)

;; Jump to end of snippet definition
; (define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)

(setq yas-fallback-behavior 'return-nil)
(setq yas-also-auto-indent-first-line t)

;; No dropdowns please, yas
(setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))

;; No need to be so verbose
(setq yas-verbosity 1)

;; Wrap around region
(setq yas-wrap-around-region t)

;; Inter-field navigation
(defun yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
        (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
        (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

; (define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
; (define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)

(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'html-mode-hook 'yas-minor-mode)
(provide 'init-yasnippet)
