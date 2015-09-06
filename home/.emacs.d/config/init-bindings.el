;; auto-indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)

;; improve delete-other-windows
(define-key global-map (kbd "C-x 1") 'dotemacs/toggle-maximize-buffer)

;; alternate binding to search next occurrence with isearch without
;; exiting isearch
(define-key isearch-mode-map (kbd "S-<return>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-S-<return>") 'isearch-repeat-backward)
;; Escape from isearch-mode("/" and "?" in evil-mode) like vim
(define-key isearch-mode-map (kbd "<escape>") 'isearch-cancel)

;; Make <escape> quit as much as possible
(define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key evil-visual-state-map (kbd "<escape>") 'keyboard-quit)
(define-key minibuffer-local-ns-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map (kbd "<escape>") 'keyboard-escape-quit)

;; linum margin bindings-------------------------------------------------------
(global-set-key (kbd "<left-margin> <down-mouse-1>") 'dotemacs-md-select-linum)
(global-set-key (kbd "<left-margin> <mouse-1>") 'dotemacs-mu-select-linum)
(global-set-key (kbd "<left-margin> <double-mouse-1>") 'dotemacs-select-current-block)
(global-set-key (kbd "<left-margin> <drag-mouse-1>") 'dotemacs-mu-select-linum)

;; ---------------------------------------------------------------------------
;; evil-leader key bindings
;; ---------------------------------------------------------------------------

;; Universal argument ---------------------------------------------------------
(evil-leader/set-key "u" 'universal-argument)
;; shell command  -------------------------------------------------------------
(evil-leader/set-key "!" 'shell-command)
;; applications ---------------------------------------------------------------
(evil-leader/set-key
  "ac"  'calc-dispatch
  "ad"  'dired
  "ap"  'proced
  "au"  'undo-tree-visualize)
;; buffers --------------------------------------------------------------------
(evil-leader/set-key
  "bd"  'kill-this-buffer
  "TAB" 'dotemacs-alternate-buffer
  "bh"  'dotemacs-home
  "be"  'dotemacs-safe-erase-buffer
  "bK"  'dotemacs/kill-other-buffers
  "bk"  'ido-kill-buffer
  "b C-k" 'dotemacs/kill-matching-buffers-rudely
  "bP"  'dotemacs/copy-clipboard-to-whole-buffer
  "bn"  'dotemacs-next-useful-buffer
  "bp"  'dotemacs-previous-useful-buffer
  "bR"  'dotemacs-safe-revert-buffer
  "bY"  'dotemacs/copy-whole-buffer-to-clipboard
  "bw"  'read-only-mode)
;; Cycling settings -----------------------------------------------------------
(evil-leader/set-key "Tn" 'dotemacs-cycle-dotemacs-theme)
;; describe functions ---------------------------------------------------------
(defmacro dotemacs-set-helm-key (keys func)
  "Define a key bindings for FUNC using KEYS.
Ensure that helm is required before calling FUNC."
  (let ((func-name (intern (format "dotemacs-%s" (symbol-name func)))))
    `(progn
       (defun ,func-name ()
         ,(format "Wrapper to ensure that `helm' is loaded before calling %s."
                  (symbol-name func))
         (interactive)
         (require 'helm)
         (call-interactively ',func))
       (evil-leader/set-key ,keys ',func-name))))
(dotemacs-set-helm-key "hdb" describe-bindings)
(dotemacs-set-helm-key "hdc" describe-char)
(dotemacs-set-helm-key "hdf" describe-function)
(dotemacs-set-helm-key "hdk" describe-key)
(dotemacs-set-helm-key "hdm" describe-mode)
(dotemacs-set-helm-key "hdp" describe-package)
(dotemacs-set-helm-key "hdt" describe-theme)
(dotemacs-set-helm-key "hdv" describe-variable)
(dotemacs-set-helm-key "hL" helm-locate-library)
;; search functions -----------------------------------------------------------
(dotemacs-set-helm-key "sww" helm-wikipedia-suggest)
(dotemacs-set-helm-key "swg" helm-google-suggest)
;; errors ---------------------------------------------------------------------
(evil-leader/set-key
  "ej" 'dotemacs-next-error
  "ek" 'dotemacs-previous-error
  "en" 'dotemacs-next-error
  "ep" 'dotemacs-previous-error)
;; file -----------------------------------------------------------------------
(evil-leader/set-key
  "fc" 'dotemacs-copy-file
  "fD" 'dotemacs/delete-current-buffer-file
  "fei" 'dotemacs/find-user-init-file
  "fes" 'find-dotemacs-file
  "fec" 'dotemacs/find-contrib-file
  "fed" 'dotemacs/find-dotfile
  "feD" 'ediff-dotfile-and-template
  "fev" 'dotemacs-display-and-copy-version
  "fg" 'rgrep
  "fj" 'dired-jump
  "fl" 'find-file-literally
  "fo" 'dotemacs-open-in-external-app
  "fR" 'dotemacs/rename-current-buffer-file
  "fS" 'evil-write-all
  "fs" 'dotemacs-write-file
  "fy" 'dotemacs/show-and-copy-buffer-filename)
;; insert stuff ---------------------------------------------------------------
(evil-leader/set-key
  "iJ" 'dotemacs-insert-line-below-no-indent
  "iK" 'dotemacs-insert-line-above-no-indent
  "ik" 'dotemacs/evil-insert-line-above
  "ij" 'evil-insert-line-below)
;; format ---------------------------------------------------------------------
;; <SPC> j k key binding for a frequent action: go and indent line below the point
;; <SPC> J split the current line at point and indent it
(evil-leader/set-key
  "J"  'sp-split-sexp
  "jj" 'sp-newline
  "jo" 'open-line
  "j=" 'dotemacs-indent-region-or-buffer
  "jJ" 'dotemacs-split-and-new-line
  "jk" 'dotemacs/evil-goto-next-line-and-indent)

;; navigation -----------------------------------------------------------------
(evil-leader/set-key
  "jh" 'dotemacs-push-mark-and-goto-beginning-of-line
  "jl" 'dotemacs-push-mark-and-goto-end-of-line)
;; Compilation ----------------------------------------------------------------
(evil-leader/set-key "cc" 'helm-make-projectile)
(evil-leader/set-key "cC" 'compile)
(evil-leader/set-key "cr" 'recompile)
;; narrow & widen -------------------------------------------------------------
(evil-leader/set-key
  "yr" 'narrow-to-region
  "yp" 'narrow-to-page
  "yf" 'narrow-to-defun
  "yw" 'widen)
;; spell check  ---------------------------------------------------------------
(evil-leader/set-key
  "Sd" 'ispell-change-dictionary
  "Sn" 'flyspell-goto-next-error)
;; toggle ---------------------------------------------------------------------
(dotemacs-add-toggle highlight-current-line-globally
  :status global-hl-line-mode
  :on (global-hl-line-mode)
  :off (global-hl-line-mode -1)
  :documentation "Globally Highlight the current line."
  :evil-leader "thh")
(dotemacs-add-toggle truncate-lines
  :status nil
  :on (toggle-truncate-lines)
  :documentation "Truncate the long lines (no wrap)."
  :evil-leader "tt")
(dotemacs-add-toggle visual-line-navigation
  :status visual-line-mode
  :on (progn
        (visual-line-mode)
        (define-key evil-motion-state-map "j" 'evil-next-visual-line)
        (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
        (when (bound-and-true-p evil-escape-mode)
          (evil-escape-mode -1)
          (setq evil-escape-motion-state-shadowed-func nil)
          (define-key evil-motion-state-map "j" 'evil-next-visual-line)
          (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
          (evil-escape-mode)))
  :off (progn
         (visual-line-mode -1)
         (define-key evil-motion-state-map "j" 'evil-next-line)
         (define-key evil-motion-state-map "k" 'evil-previous-line)
         (when (bound-and-true-p evil-escape-mode)
           (evil-escape-mode -1)
           (setq evil-escape-motion-state-shadowed-func nil)
           (define-key evil-motion-state-map "j" 'evil-next-line)
           (define-key evil-motion-state-map "k" 'evil-previous-line)
           (evil-escape-mode)))
  :documentation "Move point according to visual lines."
  :evil-leader "tv")
(dotemacs-add-toggle line-numbers
  :status linum-mode
  :on (global-linum-mode)
  :off (global-linum-mode -1)
  :documentation "Show the line numbers."
  :evil-leader "tl")
(dotemacs-add-toggle auto-fill-mode
  :status auto-fill-function
  :on (auto-fill-mode)
  :off (auto-fill-mode -1)
  :documentation "Break line beyond `current-fill-column` while editing."
  :evil-leader "tF")
(dotemacs-add-toggle debug-on-error
  :status nil
  :on (toggle-debug-on-error)
  :documentation "Toggle display of backtrace when an error happens."
  :evil-leader "tD")
(dotemacs-add-toggle fringe
  :status (not (equal fringe-mode 0))
  :on (call-interactively 'fringe-mode)
  :off (fringe-mode 0)
  :documentation "Display the fringe in GUI mode."
  :evil-leader "Tf")
(dotemacs-add-toggle fullscreen-frame
  :status nil
  :on (dotemacs-toggle-frame-fullscreen)
  :documentation "Display the current frame in full screen."
  :evil-leader "TF")
(dotemacs-add-toggle maximize-frame
  :if (version< "24.3.50" emacs-version)
  :status nil
  :on (toggle-frame-maximized)
  :documentation "Maximize the current frame."
  :evil-leader "TM")
(dotemacs-add-toggle mode-line
  :status hidden-mode-line-mode
  :on (hidden-mode-line-mode)
  :off (hidden-mode-line-mode -1)
  :documentation "Toggle the visibility of modeline."
  :evil-leader "tmt")
(dotemacs-add-toggle transparent-frame
  :status nil
  :on (dotemacs-toggle-transparency)
  :documentation "Make the current frame non-opaque."
  :evil-leader "TT")
(dotemacs-add-toggle tool-bar
  :if window-system
  :status tool-bar-mode
  :on (tool-bar-mode)
  :off (tool-bar-mode -1)
  :documentation "Display the tool bar in GUI mode."
  :evil-leader "Tt")
(dotemacs-add-toggle menu-bar
  :if (or window-system (version<= "24.3.1" emacs-version))
  :status menu-bar-mode
  :on (menu-bar-mode)
  :off (menu-bar-mode -1)
  :documentation "Display the menu bar."
  :evil-leader "Tm")
(dotemacs-add-toggle semantic-stickyfunc
  :status semantic-stickyfunc-mode
  :on (semantic-stickyfunc-mode)
  :off (semantic-stickyfunc-mode -1)
  :documentation "Enable semantic-stickyfunc."
  :evil-leader "Ts")
(dotemacs-add-toggle semantic-stickfunc-globally
  :status global-semantic-stickyfunc-mode
  :on (global-semantic-stickyfunc-mode)
  :off (global-semantic-stickyfunc-mode -1)
  :documentation "Enable semantic-stickyfunc globally."
  :evil-leader "T C-s")
;; quit -----------------------------------------------------------------------
(evil-leader/set-key
  "qs" 'dotemacs-save-buffers-kill-emacs
  "qq" 'dotemacs-prompt-kill-emacs
  "qQ" 'dotemacs-kill-emacs
  "qz" 'dotemacs-frame-killer)
;; window ---------------------------------------------------------------------
(defun split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right))

(evil-leader/set-key
  "w2"  'dotemacs/layout-double-columns
  "w3"  'dotemacs/layout-triple-columns
  "wb"  'dotemacs/switch-to-minibuffer-window
  "wc"  'delete-window
  "wd"  'dotemacs/toggle-current-window-dedication
  "wH"  'evil-window-move-far-left
  "wh"  'evil-window-left
  "wJ"  'evil-window-move-very-bottom
  "wj"  'evil-window-down
  "wK"  'evil-window-move-very-top
  "wk"  'evil-window-up
  "wL"  'evil-window-move-far-right
  "wl"  'evil-window-right
  "wm"  'dotemacs/toggle-maximize-buffer
  "wM"  'dotemacs/toggle-maximize-centered-buffer
  "wo"  'other-frame
  "wR"  'dotemacs/rotate-windows
  "ws"  'split-window-below
  "wS"  'split-window-below-and-focus
  "w-"  'split-window-below
  "wU"  'winner-redo
  "wu"  'winner-undo
  "wv"  'split-window-right
  "wV"  'split-window-right-and-focus
  "ww"  'other-window
  "w/"  'split-window-right
  "w="  'balance-windows)
;; text -----------------------------------------------------------------------
(evil-leader/set-key
  "xaa" 'align
  "xar" 'dotemacs/align-repeat
  "xam" 'dotemacs/align-repeat-math-oper
  "xa." 'dotemacs/align-repeat-decimal
  "xa," 'dotemacs/align-repeat-comma
  "xa;" 'dotemacs/align-repeat-semicolon
  "xa:" 'dotemacs/align-repeat-colon
  "xa=" 'dotemacs/align-repeat-equal
  "xa&" 'dotemacs/align-repeat-ampersand
  "xa|" 'dotemacs/align-repeat-bar
  "xa(" 'dotemacs/align-repeat-left-paren
  "xa)" 'dotemacs/align-repeat-right-paren
  "xdw" 'delete-trailing-whitespace
  "xtc" 'transpose-chars
  "xtl" 'transpose-lines
  "xtw" 'transpose-words
  "xU"  'upcase-region
  "xu"  'downcase-region
  "xwC" 'dotemacs/count-words-analysis
  "xwc" 'count-words-region)
;; google translate -----------------------------------------------------------
(evil-leader/set-key
  "xgl" 'dotemacs/set-google-translate-languages)
;; shell ----------------------------------------------------------------------
(eval-after-load "shell"
  '(progn
    (evil-define-key 'insert comint-mode-map [up] 'comint-previous-input)
    (evil-define-key 'insert comint-mode-map [down] 'comint-next-input)))

;; ---------------------------------------------------------------------------
;; Micro-states
;; ---------------------------------------------------------------------------

;; Buffer micro state

(dotemacs-define-micro-state buffer
  :doc "[n]ext [p]revious [K]ill [q]uit"
  :disable-evil-leader t
  :evil-leader "b."
  :bindings
  ("K" kill-this-buffer)
  ("n" dotemacs-next-useful-buffer)
  ("N" dotemacs-previous-useful-buffer)
  ("p" dotemacs-previous-useful-buffer)
  ("q" nil :exit t))

;; end of Buffer micro state

;; Window Manipulation Micro State

(defun dotemacs-shrink-window-horizontally (delta)
  "Wrap `dotemacs-shrink-window-horizontally'."
  (interactive "p")
  (shrink-window delta t))

(defun dotemacs-shrink-window (delta)
  "Wrap `dotemacs-shrink-window'."
  (interactive "p")
  (shrink-window delta))

(defun dotemacs-enlarge-window (delta)
  "Wrap `dotemacs-enlarge-window'."
  (interactive "p")
  (enlarge-window delta))

(defun dotemacs-enlarge-window-horizontally (delta)
  "Wrap `dotemacs-enlarge-window-horizontally'."
  (interactive "p")
  (enlarge-window delta t))

(defun dotemacs-window-manipulation-full-doc ()
  "Full documentation for window manipulation micro-state."
  "
  [?]                       display this help
  [0,9]                     go to numbered window
  [-] [/] [s] [v] [S] [V]   split windows below|right and focus
  [c] [C]                   close current|other windows
  [g]                       toggle golden-ratio
  [h] [j] [k] [l]           go to left|bottom|top|right
  [H] [J] [K] [L]           move windows to far/very left|bottom|top|right
  [[] []] [{] [}]           shrink/enlarge horizontally and vertically respectively
  [o] [w]                   other frame|window
  [R]                       rotate windows
  [u] [U]                   restore previous|next window layout")

(defun dotemacs-window-manipulation-move-doc ()
  "Help string for moving between windows"
  (concat "[h] [j] [k] [l] to move focus, "
          "[H] [J] [K] [L] to move window, "
          "[R]otate windows, other [f]rame, other [w]indow"))

(defun dotemacs-window-manipulation-resize-doc ()
  "Dynamic help string when resizing windows."
  (format
   (concat "[%sx%s] Resize window: [[] []] shrink/enlarge horizontally, "
           "[{] [}] shrink/enlarge vertically.")
   (window-total-width) (window-total-height)))

(defun dotemacs-window-manipulation-split-doc ()
  "Help string for moving between windows"
  (concat "[-], [s] to split horizontally,  [/], [v] to split vertically, "
          "[S], [V] to split and focus"))

(defun dotemacs-window-manipulation-number-doc ()
  "Help string for selecting window with number."
  (format "(selected window #%s) press [0,9] to select the corresponding numbered window."
          (window-numbering-get-number-string)))

(defun dotemacs-window-manipulation-layout-doc ()
  "Help string for layout manipulation"
  (concat "[c]lose window, [C]lose other windows, "
          "[u]ndo window layout, [U] redo window layout."))

(defun dotemacs-window-manipulation-gratio-doc ()
  "Help string for golden ratio"
  (format "(golden-ration %s) toggle with [g]"
          (if (symbol-value golden-ratio-mode) "enabled" "disabled")))

(dotemacs-define-micro-state window-manipulation
  :doc "[?] for help"
  :evil-leader "w."
  :bindings
  ("?" nil                                   :doc (dotemacs-window-manipulation-full-doc))
  ("0" select-window-0                       :doc (dotemacs-window-manipulation-number-doc))
  ("1" select-window-1                       :doc (dotemacs-window-manipulation-number-doc))
  ("2" select-window-2                       :doc (dotemacs-window-manipulation-number-doc))
  ("3" select-window-3                       :doc (dotemacs-window-manipulation-number-doc))
  ("4" select-window-4                       :doc (dotemacs-window-manipulation-number-doc))
  ("5" select-window-5                       :doc (dotemacs-window-manipulation-number-doc))
  ("6" select-window-6                       :doc (dotemacs-window-manipulation-number-doc))
  ("7" select-window-7                       :doc (dotemacs-window-manipulation-number-doc))
  ("8" select-window-8                       :doc (dotemacs-window-manipulation-number-doc))
  ("9" select-window-9                       :doc (dotemacs-window-manipulation-number-doc))
  ("-" split-window-below-and-focus          :doc (dotemacs-window-manipulation-split-doc))
  ("/" split-window-right-and-focus          :doc (dotemacs-window-manipulation-split-doc))
  ("[" dotemacs-shrink-window-horizontally   :doc (dotemacs-window-manipulation-resize-doc))
  ("]" dotemacs-enlarge-window-horizontally  :doc (dotemacs-window-manipulation-resize-doc))
  ("{" dotemacs-shrink-window                :doc (dotemacs-window-manipulation-resize-doc))
  ("}" dotemacs-enlarge-window               :doc (dotemacs-window-manipulation-resize-doc))
  ("c" delete-window                         :doc (dotemacs-window-manipulation-layout-doc))
  ("C" delete-other-windows                  :doc (dotemacs-window-manipulation-layout-doc))
  ("g" dotemacs-toggle-golden-ratio          :doc (dotemacs-window-manipulation-gratio-doc))
  ("h" evil-window-left                      :doc (dotemacs-window-manipulation-move-doc))
  ("j" evil-window-down                      :doc (dotemacs-window-manipulation-move-doc))
  ("k" evil-window-up                        :doc (dotemacs-window-manipulation-move-doc))
  ("l" evil-window-right                     :doc (dotemacs-window-manipulation-move-doc))
  ("H" evil-window-move-far-left             :doc (dotemacs-window-manipulation-move-doc))
  ("J" evil-window-move-very-bottom          :doc (dotemacs-window-manipulation-move-doc))
  ("K" evil-window-move-very-top             :doc (dotemacs-window-manipulation-move-doc))
  ("L" evil-window-move-far-right            :doc (dotemacs-window-manipulation-move-doc))
  ("o" other-frame                           :doc (dotemacs-window-manipulation-move-doc))
  ("R" dotemacs/rotate-windows               :doc (dotemacs-window-manipulation-move-doc))
  ("s" split-window-below                    :doc (dotemacs-window-manipulation-split-doc))
  ("S" split-window-below-and-focus          :doc (dotemacs-window-manipulation-split-doc))
  ("u" winner-undo                           :doc (dotemacs-window-manipulation-layout-doc))
  ("U" winner-redo                           :doc (dotemacs-window-manipulation-layout-doc))
  ("v" split-window-right                    :doc (dotemacs-window-manipulation-split-doc))
  ("V" split-window-right-and-focus          :doc (dotemacs-window-manipulation-split-doc))
  ("w" other-window                          :doc (dotemacs-window-manipulation-move-doc)))

;; end of Window Manipulation Micro State

;; text Manipulation Micro State

(defun dotemacs-scale-up-or-down-font-size (direction)
  "Scale the font. If DIRECTION is positive or zero the font is scaled up,
otherwise it is scaled down."
  (interactive)
  (let ((scale 0.5))
    (if (eq direction 0)
        (text-scale-set 0)
      (if (< direction 0)
          (text-scale-decrease scale)
        (text-scale-increase scale)))))

(defun dotemacs-scale-up-font ()
  "Scale up the font."
  (interactive)
  (dotemacs-scale-up-or-down-font-size 1))

(defun dotemacs-scale-down-font ()
  "Scale up the font."
  (interactive)
  (dotemacs-scale-up-or-down-font-size -1))

(defun dotemacs-reset-font-size ()
  "Reset the font size."
  (interactive)
  (dotemacs-scale-up-or-down-font-size 0))

(dotemacs-define-micro-state scale-font
  :doc "[+] scale up [-] scale down [=] reset font [q]uit"
  :evil-leader "zx"
  :bindings
  ("+" dotemacs-scale-up-font)
  ("-" dotemacs-scale-down-font)
  ("=" dotemacs-reset-font-size)
  ("q" nil :exit t))

;; end of Text Manipulation Micro State

;; Transparency micro-state

(defun dotemacs-toggle-transparency ()
  "Toggle between transparent or opaque display."
  (interactive)
  ;; Define alpha if it's nil
  (if (eq (frame-parameter (selected-frame) 'alpha) nil)
      (set-frame-parameter (selected-frame) 'alpha '(100 100)))
  ;; Do the actual toggle
  (if (/= (cadr (frame-parameter (selected-frame) 'alpha)) 100)
      (set-frame-parameter (selected-frame) 'alpha '(100 100))
    (set-frame-parameter (selected-frame) 'alpha
                         (list dotemacs-active-transparency
                               dotemacs-inactive-transparency)))
  ;; Immediately enter the micro-state
  (dotemacs-scale-transparency-micro-state))

(defun dotemacs-increase-transparency ()
  "Increase transparency of current frame."
  (interactive)
  (let* ((current-alpha (car (frame-parameter (selected-frame) 'alpha)))
         (increased-alpha (- current-alpha 5)))
    (when (>= increased-alpha frame-alpha-lower-limit)
      (set-frame-parameter (selected-frame) 'alpha (list increased-alpha increased-alpha)))))

(defun dotemacs-decrease-transparency ()
  "Decrease transparency of current frame."
  (interactive)
  (let* ((current-alpha (car (frame-parameter (selected-frame) 'alpha)))
         (decreased-alpha (+ current-alpha 5)))
    (when (<= decreased-alpha 100)
      (set-frame-parameter (selected-frame) 'alpha (list decreased-alpha decreased-alpha)))))

(dotemacs-define-micro-state scale-transparency
  :doc "[+] increase [-] decrease [T] toggle transparency [q] quit"
  :bindings
  ("+" dotemacs-increase-transparency)
  ("-" dotemacs-decrease-transparency)
  ("T" dotemacs-toggle-transparency)
  ("q" nil :exit t))

(provide 'init-bindings)
