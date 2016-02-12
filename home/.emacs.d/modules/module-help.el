;;; module-help.el --- Help Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
(require 'core-display-init)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(use-package sx                         ; StackExchange client for Emacs
  :ensure t
  :defer t)

(use-package sx-compose
  :ensure sx
  :defer t
  :config
  (progn
    ;; Don't fill in SX questions/answers, and use visual lines instead.  Plays
    ;; more nicely with the website.
    (add-hook 'sx-compose-mode-hook #'turn-off-auto-fill)
    (add-hook 'sx-compose-mode-hook #'visual-line-mode)
    (add-hook 'sx-compose-mode-hook
              #'dotemacs-whitespace-style-no-long-lines)

    ;; Clean up whitespace before sending questions
    (add-hook 'sx-compose-before-send-hook
              (lambda ()
                (whitespace-cleanup)
                t))

    (bind-key "M-q" #'ignore sx-compose-mode-map)))

(use-package sx-question-mode
  :ensure sx
  :defer t
  ;; Display questions in the same window
  :config (setq sx-question-mode-display-buffer-function #'switch-to-buffer))

(use-package find-func                  ; Find function/variable definitions
  :defer t)

(use-package info                       ; Info manual viewer
  :defer t
  :config
  ;; Fix the stupid `Info-quoted' face. Courier is an abysmal face, so go back
  ;; to the default face.
  (set-face-attribute 'Info-quoted nil :family 'unspecified
                      :inherit font-lock-type-face))

(use-package info+
  :ensure t
  :init
  (progn
    (with-eval-after-load 'info
      (require 'info+))
    (setq Info-fontify-angle-bracketed-flag nil)))

(use-package eldoc                      ; Documentation in minibuffer
  :defer t
  :config
  (progn
    ;; enable eldoc in `eval-expression'
    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
    ;; enable eldoc in IELM
    (add-hook 'ielm-mode-hook #'eldoc-mode)
    ;; don't display eldoc on modeline
    (dotemacs-hide-lighter eldoc-mode)))

(use-package helm-info                  ; Helm tools for Info
  :ensure helm
  :defer t)

(use-package helm-man                   ; Browse manpages with Heml
  :ensure helm
  :defer t)

(use-package helm-descbinds
  :ensure t
  :defer t
  :init
  (progn
    (setq helm-descbinds-window-style 'split)
    (add-hook 'helm-mode-hook 'helm-descbinds-mode)
    (dotemacs-set-leader-keys "?" 'helm-descbinds)))

(use-package helm-make
  :ensure t
  :defer t
  :init
  (dotemacs-set-leader-keys
   "cc" 'helm-make-projectile
   "cm" 'helm-make))

(use-package help-mode
  :config
  (progn
    (define-key help-mode-map (kbd "n") 'next-line)
    (define-key help-mode-map (kbd "p") 'previous-line)
    (define-key help-mode-map (kbd "j") 'next-line)
    (define-key help-mode-map (kbd "k") 'previous-line)))

(defvar dash-helm-dash-docset-path ""
  "Path containing dash docsets.")

(use-package helm-dash
  :defer t
  :disabled t
  :ensure t
  :init
  (progn
    (dotemacs-set-leader-keys
      "dh" 'helm-dash-at-point
      "dH" 'helm-dash))
  :config
  (progn
    (defun dash//activate-package-docsets (path)
      "Add dash docsets from specified PATH."
      (setq helm-dash-docsets-path path
            helm-dash-common-docsets (helm-dash-installed-docsets))
      (message (format "activated %d docsets from: %s"
                       (length helm-dash-common-docsets) path)))
    (dash//activate-package-docsets dash-helm-dash-docset-path)))

(use-package dash-at-point
  :ensure t
  :if (eq system-type 'darwin)
  :defer t
  :init
  (progn
    (dotemacs-set-leader-keys "dd" 'dash-at-point)
    (dotemacs-set-leader-keys "dD" 'dash-at-point-with-docset)))

(use-package zeal-at-point
  :ensure t
  :if (eq system-type 'gnu/linux)
  :defer t
  :init
  (dotemacs-set-leader-keys
    "dd" 'zeal-at-point
    "dD" 'zeal-at-point-set-docset)
  :config
  ;; This lets users seach in multiple docsets
  (push '(web-mode . "html,css,javascript") zeal-at-point-mode-alist))

(use-package engine-mode
  :ensure t
  :commands (defengine dotemacs/search-engine-select)
  :defines search-engine-alist
  :init
  (dotemacs-set-leader-keys
    "a/" 'dotemacs/search-engine-select)
  (setq search-engine-alist
        '((amazon
           :name "Amazon"
           :url "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%%3Daps&field-keywords=%s")
          (duck-duck-go
           :name "Duck Duck Go"
           :url "https://duckduckgo.com/?q=%s")
          (google
           :name "Google"
           :url "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")
          (google-images
           :name "Google Images"
           :url "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")
          (github
           :name "Github"
           :url "https://github.com/search?ref=simplesearch&q=%s")
          (google-maps
           :name "Google Maps"
           :url "http://maps.google.com/maps?q=%s")
          (twitter
           :name "Twitter"
           :url "https://twitter.com/search?q=%s")
          (project-gutenberg
           :name "Project Gutenberg"
           :url "http://www.gutenberg.org/ebooks/search.html/?format=html&default_prefix=all&sort_order=&query=%s")
          (youtube
           :name "YouTube"
           :url "http://www.youtube.com/results?aq=f&oq=&search_query=%s")
          (stack-overflow
           :name "Stack Overflow"
           :url "https://stackoverflow.com/search?q=%s")
          (wikipedia
           :name "Wikipedia"
           :url "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
          (wolfram-alpha
           :name "Wolfram Alpha"
           :url "http://www.wolframalpha.com/input/?i=%s")))
  :config
  (engine-mode t)
  (mapcar (lambda (engine)
            (let* ((cur-engine (car engine))
                   (engine-url (plist-get (cdr engine) :url)))
              (eval `(defengine ,cur-engine ,engine-url))))
          search-engine-alist)
  (defun dotemacs//search-engine-source (engines)
    "return a source for helm selection"
    `((name . "Search Engines")
      (candidates . ,(mapcar (lambda (engine)
                               (cons (plist-get (cdr engine) :name)
                                     (intern (format "engine/search-%S"
                                                     (car engine)))))
                             engines))
      (action . (lambda (candidate) (call-interactively candidate)))))

  (defun dotemacs/helm-search-engine-select ()
    "Set search engine to use with helm."
    (interactive)
    (helm :sources (list (dotemacs//search-engine-source
                          search-engine-alist))))

  (defun dotemacs/search-engine-select ()
    "Set search engine to use."
    (interactive)
    (call-interactively 'dotemacs/helm-search-engine-select)))

(provide 'module-help)
;;; module-help.el ends here
