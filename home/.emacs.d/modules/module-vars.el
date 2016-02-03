;;; module-vars.el --- Settings and variables
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;;; Code:

(require 'core-vars)

;; Regexp for useful and useless buffers for smarter buffer switching
(defvar dotemacs-useless-buffers-regexp '("*\.\+")
  "Regexp used to determine if a buffer is not useful.")
(defvar dotemacs-useful-buffers-regexp '("\\*scratch\\*")
  "Regexp used to define buffers that are useful despite matching
`spacemacs-useless-buffers-regexp'.")

;; from Prelude
(defvar dotemacs-indent-sensitive-modes
  '(coffee-mode
    python-mode
    slim-mode
    haml-mode
    yaml-mode
    makefile-mode
    makefile-gmake-mode
    makefile-imake-mode
    makefile-bsdmake-mode)
  "Modes for which auto-indenting is suppressed.")

(defconst dotemacs-editing-style 'vim
  "Always `vim', Evil is always enabled.")

(defvar dotemacs-line-numbers 'relative
  "If non nil line numbers are turned on in all `prog-mode' and `text-mode'.
derivatives.  If set to `relative', also turns on relative line numbers.")

(defconst dotemacs-auto-save-directory
  (expand-file-name (concat dotemacs-cache-directory "auto-save/"))
  "The auto-save directory.")

(defvar dotemacs-themes '(zenburn
                          solarized-dark
                          solarized-light
                          leuven
                          monokai
                          tao
                          ;; monster theme support
                          afternoon
                          ample
                          ample-zen
                          anti-zenburn
                          birds-of-paradise-plus
                          bubbleberry
                          busybee
                          cherry-blossom
                          clues
                          cyberpunk
                          dakrone
                          darkburn
                          darkmine
                          darktooth
                          django
                          espresso
                          farmhouse
                          firebelly
                          flatland
                          flatui
                          gandalf
                          gotham
                          grandshell
                          gruber-darker
                          gruvbox
                          hc-zenburn
                          heroku
                          inkpot
                          ir-black
                          jazz
                          jbeans
                          light-soap
                          lush
                          material
                          minimal
                          molokai
                          monochrome
                          mustang
                          naquadah
                          niflheim
                          noctilux
                          obsidian
                          occidental
                          oldlace
                          organic-green
                          pastels-on-dark
                          phoenix-dark-mono
                          phoenix-dark-pink
                          planet
                          professional
                          purple-haze
                          seti
                          smyx
                          soft-charcoal
                          soft-morning
                          soft-stone
                          soothe
                          spacegray
                          subatomic
                          subatomic256
                          sunny-day
                          tango-2
                          tango-plus
                          tangotango
                          tronesque
                          twilight-anti-bright
                          twilight-bright
                          twilight
                          ujelly
                          underwater
                          zen-and-art)
  "List of themes, the first of the list is loaded when Emacs starts.
Press <Leader> T n to cycle to the next theme in the list (works great
with 2 themes variants, one dark and one light.")

(defvar dotemacs--cur-theme nil
  "The current theme.")

(defvar dotemacs-active-transparency 96
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's active or selected. Transparency
can be toggled through `toggle-transparency'.")

(defvar dotemacs-inactive-transparency 96
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's inactive or deselected. Transparency
can be toggled through `toggle-transparency'.")

(defvar dotemacs-smartparens-strict-mode nil
  "If non-nil smartparens-strict-mode will be enabled in programming modes.")

(provide 'module-vars)
;;; module-vars.el ends here
