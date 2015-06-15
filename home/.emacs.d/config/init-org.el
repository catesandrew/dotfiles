;; TODO to be deleted
(add-hook
 'org-load-hook
 (lambda ()
   (unless (file-exists-p org-directory)
     (make-directory org-directory))

   (setq my-inbox-org-file (concat org-directory "/inbox.org"))

   (setq org-default-notes-file my-inbox-org-file)
   (setq org-log-done t)

   (setq org-startup-indented t)
   (setq org-indent-indentation-per-level 2)

   (setq org-agenda-files `(,org-directory))
   (setq org-capture-templates
         '(("t" "Todo" entry (file+headline my-inbox-org-file "INBOX")
            "* TODO %?\n%U\n%a\n")
           ("n" "Note" entry (file+headline my-inbox-org-file "NOTES")
            "* %? :NOTE:\n%U\n%a\n")
           ("m" "Meeting" entry (file my-inbox-org-file)
            "* MEETING %? :MEETING:\n%U")
           ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
            "* %?\n%U\n")))

   (setq org-use-fast-todo-selection t)
   (setq org-treat-S-cursor-todo-selection-as-state-change nil)
   (setq org-todo-keywords
         '((sequence "TODO(t)" "NEXT(n@)" "|" "DONE(d)")
           (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")))

    ;; org-mode colors
    (setq org-todo-keyword-faces
      '(
         ("INPR" . (:foreground "yellow" :weight bold))
         ("DONE" . (:foreground "green" :weight bold))
         ("IMPEDED" . (:foreground "red" :weight bold))
         ))

   (setq org-todo-state-tags-triggers
         ' (("CANCELLED" ("CANCELLED" . t))
            ("WAITING" ("WAITING" . t))
            ("TODO" ("WAITING") ("CANCELLED"))
            ("NEXT" ("WAITING") ("CANCELLED"))
            ("DONE" ("WAITING") ("CANCELLED"))))

   (setq org-refile-targets '((nil :maxlevel . 9)
                              (org-agenda-files :maxlevel . 9)))
   (setq org-completion-use-ido t)

   (with-eval-after-load 'org-mobile
     (setq org-mobile-directory (concat org-directory "/MobileOrg"))
     (unless (file-exists-p org-mobile-directory)
       (make-directory org-mobile-directory))
     (setq org-mobile-inbox-for-pull (concat org-directory "/from-mobile.org")))

   (with-eval-after-load 'evil
     (add-hook 'org-capture-mode-hook #'evil-emacs-state))

   (when (boundp 'org-plantuml-jar-path)
     (org-babel-do-load-languages
      'org-babel-load-languages
      '((plantuml . t))))

   (defun my-org-mode-hook ()
     ;; disable line wrap
     (setq truncate-lines t)
     (setq partial-width-windows nil)
     (toggle-truncate-lines t))
   (add-hook 'org-mode-hook #'my-org-mode-hook)

   (require 'ox-md)
   (require 'ox-ascii)
   ;; ox-confluence.el lets you convert Org files to confluence files
   ;; using the ox.el export engine.
   (require 'ox-confluence)
   ;; Installation
   ;; -------------
   ;; Get a copy of deck.js from http://imakewebthings.com/deck.js/ or
   ;; the gitub repository at https://github.com/imakewebthings/deck.js.
   ;;
   ;; Add the path to the extracted code to the variable
   ;; `org-deck-directories' There are a number of customization in the
   ;; org-export-deck group, most of which can be overrriden with buffer
   ;; local customization (starting with DECK_.)
   (require 'ox-deck)
   (require 'ox-html)
   ;;   (add-to-list 'org-export-backends 'taskjuggler)
   (require 'ox-taskjuggler)

   (require 'org-bullets)
   (setq org-bullets-bullet-list '("✿" "❀" "☢" "☯" "✸" ))
   (add-hook 'org-mode-hook #'org-bullets-mode)))


(provide 'init-org)
