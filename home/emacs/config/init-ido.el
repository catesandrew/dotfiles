;; Interactively Do Things

(require 'ido)
(setq ido-case-fold nil
      ido-auto-merge-work-directories-length -1
      ido-max-prospects 10)
(setq ido-enable-prefix nil)
(setq ido-use-virtual-buffers t)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq ido-create-new-buffer 'always)
(setq ido-use-filename-at-point 'guess)
(setq ido-save-directory-list-file (concat dotemacs-cache-directory "ido.last"))

(ido-mode t)
(ido-everywhere t)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode t)

;; Try out flx-ido for better flex matching between words
(require 'flx-ido)
(flx-ido-mode t)

;; flx-ido looks better with ido-vertical-mode
(require 'ido-vertical-mode)
(ido-vertical-mode)

;; C-n/p is more intuitive in vertical layout
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

(require 'dash)

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")
(add-to-list 'ido-ignore-directories "bower_components")

;; Ido at point (C-,)
(require 'ido-at-point)
(ido-at-point-mode)

;; Use ido everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas-expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas-visit-snippet-file 'yasnippet)

(provide 'init-ido)
