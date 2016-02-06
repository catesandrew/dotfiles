;;; module-keybindings.el --- Key Bindings Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'core-vars)
;; (require 'core-funcs)
(require 'core-transient-state)
;; (require 'core-keybindings)
;; (require 'core-display-init)
(require 'core-themes-support)
(require 'module-vars)
;; (require 'module-common)
(require 'module-core)
(require 'module-utils)

;;; Code:

(declare-function dotemacs-home "module-utils")

;; instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)

;; have no use for these default bindings
(global-unset-key (kbd "C-x m"))  ;; send email

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

;; let TAB jump between links in help buffers
(evil-define-key 'motion help-mode-map (kbd "TAB") 'forward-button)
(evil-define-key 'motion help-mode-map (kbd "S-TAB") 'backward-button)

(global-set-key (kbd "C-a") 'dotemacs/smart-move-beginning-of-line)
(global-set-key (kbd "C-w") 'dotemacs/backward-kill-word-or-region)

;; ---------------------------------------------------------------------------
;; evil-leader key bindings
;; ---------------------------------------------------------------------------

;; Universal argument ---------------------------------------------------------
(dotemacs-set-leader-keys "u" 'universal-argument)
; (when (memq dotemacs-editing-style '(vim hybrid))
;   (define-key universal-argument-map
;     (kbd (concat dotemacs-leader-key " u"))
;     'universal-argument-more))
;; shell command  -------------------------------------------------------------
(dotemacs-set-leader-keys "!" 'shell-command)
;; applications ---------------------------------------------------------------
(dotemacs-set-leader-keys
  "ac"  'calc-dispatch
  "ad"  'dired
  "ap"  'list-processes
  "aP"  'proced
  "au"  'undo-tree-visualize)
;; buffers --------------------------------------------------------------------
(dotemacs-set-leader-keys
  "bc"  'kill-this-buffer
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
  "bs"  'dotemacs/switch-to-scratch-buffer
  "bY"  'dotemacs/copy-whole-buffer-to-clipboard
  "bw"  'read-only-mode)
;; Cycling settings -----------------------------------------------------------
(dotemacs-set-leader-keys "Tn" 'dotemacs-cycle-dotemacs-theme)
;; errors ---------------------------------------------------------------------
(dotemacs-set-leader-keys
  "en" 'dotemacs-next-error
  "eN" 'dotemacs-previous-error
  "ep" 'dotemacs-previous-error)
;; file -----------------------------------------------------------------------
(dotemacs-set-leader-keys
  "fc" 'dotemacs-copy-file
  "fD" 'dotemacs/delete-current-buffer-file
  "fei" 'dotemacs/find-user-init-file
  "fed" 'dotemacs/find-dotfile
  "feD" 'ediff-dotfile-and-template
  "fCd" 'dotemacs/unix2dos
  "fCu" 'dotemacs/dos2unix
  "fg" 'rgrep
  "fj" 'dired-jump
  "fl" 'find-file-literally
  "fE" 'dotemacs/sudo-edit
  "fo" 'dotemacs-open-in-external-app
  "fR" 'dotemacs/rename-current-buffer-file
  "fS" 'evil-write-all
  "fs" 'save-buffer
  "fvd" 'add-dir-local-variable
  "fvf" 'add-file-local-variable
  "fvp" 'add-file-local-variable-prop-line
  "fy" 'dotemacs/show-and-copy-buffer-filename)
;; help -----------------------------------------------------------------------
(dotemacs-set-leader-keys
 "hdb" 'describe-bindings
 "hdc" 'describe-char
 "hdd" 'helm-apropos
 "hdf" 'describe-function
 "hdk" 'describe-key
 "hdl" 'dotemacs/describe-last-keys
 "hdp" 'describe-package
 "hdt" 'describe-theme
 "hdv" 'describe-variable
 "hn"  'view-emacs-news)
;; insert stuff ---------------------------------------------------------------
(dotemacs-set-leader-keys
  "iJ" 'dotemacs-insert-line-below-no-indent
  "iK" 'dotemacs-insert-line-above-no-indent
  "ik" 'dotemacs/evil-insert-line-above
  "ij" 'dotemacs/evil-insert-line-below)
;; format ---------------------------------------------------------------------
;; , j k key binding for a frequent action: go and indent line below the point
;; , J split the current line at point and indent it
(dotemacs-set-leader-keys
  "jo" 'open-line
  "j=" 'dotemacs-indent-region-or-buffer
  "jJ" 'dotemacs-split-and-new-line
  "jk" 'dotemacs/evil-goto-next-line-and-indent)

;; navigation -----------------------------------------------------------------
(dotemacs-set-leader-keys
  "j0" 'dotemacs-push-mark-and-goto-beginning-of-line
  "j$" 'dotemacs-push-mark-and-goto-end-of-line
  "jb" 'bookmark-jump
  "jd" 'dired-jump
  "jD" 'dired-jump-other-window
  "jf" 'find-function-at-point
  "ji" 'dotemacs-jump-in-buffer
  "jv" 'find-variable-at-point)

;; Compilation ----------------------------------------------------------------
(dotemacs-set-leader-keys
  "cC" 'compile
  "ck" 'kill-compilation
  "cr" 'recompile
  "cq" 'dotemacs/close-compilation-window)

;; narrow & widen -------------------------------------------------------------
(dotemacs-set-leader-keys
  "nr" 'narrow-to-region
  "np" 'narrow-to-page
  "nf" 'narrow-to-defun
  "nw" 'widen)
;; toggle ---------------------------------------------------------------------
(dotemacs-add-toggle highlight-current-line-globally
  :status global-hl-line-mode
  :on (global-hl-line-mode)
  :off (global-hl-line-mode -1)
  :documentation "Globally highlight the current line."
  :evil-leader "thh")
(dotemacs-add-toggle truncate-lines
  :status nil
  :on (toggle-truncate-lines)
  :documentation "Truncate long lines (no wrap)."
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
  :on (linum-mode)
  :off (linum-mode -1)
  :documentation "Show the line numbers."
  :evil-leader "tl")
(dotemacs-add-toggle auto-fill-mode
  :status auto-fill-function
  :on (auto-fill-mode)
  :off (auto-fill-mode -1)
  :documentation "Break line beyond `current-fill-column` while editing."
  :evil-leader "tF")
(dotemacs-add-toggle debug-on-error
  :status debug-on-error
  :on (setq debug-on-error t)
  :off (setq debug-on-error nil)
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
  :status (not hidden-mode-line-mode)
  :on (hidden-mode-line-mode -1)
  :off (hidden-mode-line-mode)
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
(dotemacs-add-toggle semantic-stickyfunc-globally
  :status global-semantic-stickyfunc-mode
  :on (global-semantic-stickyfunc-mode)
  :off (global-semantic-stickyfunc-mode -1)
  :documentation "Enable semantic-stickyfunc globally."
  :evil-leader "T C-s")
;; quit -----------------------------------------------------------------------
(dotemacs-set-leader-keys
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

(dotemacs-set-leader-keys
  "w2"  'dotemacs/layout-double-columns
  "w3"  'dotemacs/layout-triple-columns
  "wb"  'dotemacs/switch-to-minibuffer-window
  "wc"  'delete-window
  "wd"  'dotemacs/toggle-current-window-dedication
  "wH"  'evil-window-move-far-left
  "w <S-left>"  'evil-window-move-far-left
  "wh"  'evil-window-left
  "w <left>"  'evil-window-left
  "wJ"  'evil-window-move-very-bottom
  "w <S-down>"  'evil-window-move-very-bottom
  "wj"  'evil-window-down
  "w <down>"  'evil-window-down
  "wK"  'evil-window-move-very-top
  "w <S-up>"  'evil-window-move-very-top
  "wk"  'evil-window-up
  "w <up>"  'evil-window-up
  "wL"  'evil-window-move-far-right
  "w <S-right>"  'evil-window-move-far-right
  "wl"  'evil-window-right
  "w <right>"  'evil-window-right
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
(defalias 'count-region 'count-words-region)

(dotemacs-set-leader-keys
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
  "xc"  'count-region
  "xdw" 'delete-trailing-whitespace
  "xls" 'dotemacs/sort-lines
  "xlu" 'dotemacs/uniquify-lines
  "xtc" 'transpose-chars
  "xtl" 'transpose-lines
  "xtw" 'transpose-words
  "xU"  'upcase-region
  "xu"  'downcase-region
  "xwc" 'dotemacs/count-words-analysis)

;; shell ----------------------------------------------------------------------
(with-eval-after-load 'shell
  (progn
    (evil-define-key 'insert comint-mode-map [up] 'comint-previous-input)
    (evil-define-key 'insert comint-mode-map [down] 'comint-next-input)))

;; ---------------------------------------------------------------------------
;; Transient-states
;; ---------------------------------------------------------------------------

;; Buffer transient state
(dotemacs-define-transient-state buffer
  :title "Buffer Selection Transient State"
  :bindings
  ("n" dotemacs-next-useful-buffer "next")
  ("N" dotemacs-previous-useful-buffer "previous")
  ("p" dotemacs-previous-useful-buffer "previous")
  ("K" kill-this-buffer "kill")
  ("q" nil "quit" :exit t))
(dotemacs-set-leader-keys "b." 'dotemacs/buffer-transient-state/body)

;; end of Buffer transient state

;; Window Manipulation Transient State

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

(dotemacs-define-transient-state window-manipulation
  :title "Window Manipulation Transient State"
  :doc
  "
Select^^^^               Move^^^^              Split^^                Resize^^                     Other^^
------^^^^------------- -----^^^^------------ ------^^-------------- -------^^------------------- ------^^-------------------
[_j_/_k_] down/up        [_J_/_K_] down/up     [_s_] vertical         [_[_] shrink horizontally    [_q_] quit
                                                                        [_h_/_l_] left/right     [_h_/_l_] left/right  [_S_] vert & follow    [_]_] enlarge horizontally   [_u_] restore prev layout
[_0_-_9_] window N       [_R_]^^   rotate      [_v_] horizontal       [_{_] shrink vertically      [_U_] restore next layout
[_w_]^^   other window   ^^^^                  [_V_] horiz & follow   [_}_] enlarge vertically     [_c_] close current
[_o_]^^   other frame    ^^^^                  ^^                     ^^                           [_C_] close other
^^^^                     ^^^^                  ^^                     ^^                           ^^
"
  :bindings
  ("q" nil :exit t)
  ("0" select-window-0)
  ("1" select-window-1)
  ("2" select-window-2)
  ("3" select-window-3)
  ("4" select-window-4)
  ("5" select-window-5)
  ("6" select-window-6)
  ("7" select-window-7)
  ("8" select-window-8)
  ("9" select-window-9)
  ("-" split-window-below-and-focus)
  ("/" split-window-right-and-focus)
  ("[" dotemacs-shrink-window-horizontally)
  ("]" dotemacs-enlarge-window-horizontally)
  ("{" dotemacs-shrink-window)
  ("}" dotemacs-enlarge-window)
  ("c" delete-window)
  ("C" delete-other-windows)
  ("h" evil-window-left)
  ("<left>" evil-window-left)
  ("j" evil-window-down)
  ("<down>" evil-window-down)
  ("k" evil-window-up)
  ("<up>" evil-window-up)
  ("l" evil-window-right)
  ("<right>" evil-window-right)
  ("H" evil-window-move-far-left)
  ("<S-left>" evil-window-move-far-left)
  ("J" evil-window-move-very-bottom)
  ("<S-down>" evil-window-move-very-bottom)
  ("K" evil-window-move-very-top)
  ("<S-up>" evil-window-move-very-top)
  ("L" evil-window-move-far-right)
  ("<S-right>" evil-window-move-far-right)
  ("o" other-frame)
  ("R" dotemacs/rotate-windows)
  ("s" split-window-below)
  ("S" split-window-below-and-focus)
  ("u" winner-undo)
  ("U" winner-redo)
  ("v" split-window-right)
  ("V" split-window-right-and-focus)
  ("w" other-window))
(dotemacs-set-leader-keys "w."
  'dotemacs/window-manipulation-transient-state/body)

;; end of Window Manipulation Transient State

;; text Manipulation Transient State

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

(dotemacs-define-transient-state scale-font
  :title "Font Scaling Transient State"
  :doc "\n[_+_/_=_] scale up [_-_] scale down [_0_] reset font [_q_] quit"
  :bindings
  ("+" dotemacs-scale-up-font)
  ("=" dotemacs-scale-up-font)
  ("-" dotemacs-scale-down-font)
  ("0" dotemacs-reset-font-size)
  ("q" nil :exit t))
(dotemacs-set-leader-keys "zx" 'dotemacs/scale-font-transient-state/body)

;; end of Text Manipulation Transient State

;; Transparency transient-state

(defun dotemacs-toggle-transparency-core ()
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
                               dotemacs-inactive-transparency))))

(defun dotemacs-toggle-transparency ()
  "Toggle between transparent or opaque display, then enter the micro-state."
  (interactive)
  (dotemacs-toggle-transparency-core)
  ;; Immediately enter the micro-state
  (dotemacs-scale-transparency-micro-state))

(defun dotemacs-increase-transparency ()
  "Increase transparency of current frame."
  (interactive)
  (let* ((current-alpha (car (frame-parameter (selected-frame) 'alpha)))
         (increased-alpha (- current-alpha 5)))
    (when (>= increased-alpha frame-alpha-lower-limit)
      (set-frame-parameter (selected-frame) 'alpha
                           (list increased-alpha increased-alpha)))))

(defun dotemacs-decrease-transparency ()
  "Decrease transparency of current frame."
  (interactive)
  (let* ((current-alpha (car (frame-parameter (selected-frame) 'alpha)))
         (decreased-alpha (+ current-alpha 5)))
    (when (<= decreased-alpha 100)
      (set-frame-parameter (selected-frame) 'alpha
                           (list decreased-alpha decreased-alpha)))))

(dotemacs-define-transient-state scale-transparency
  :title "Frame Transparency Transient State"
  :bindings
  ("+" dotemacs-increase-transparency "increase")
  ("-" dotemacs-decrease-transparency "decrease")
  ("T" dotemacs-toggle-transparency "toggle")
  ("q" nil "quit" :exit t))
(dotemacs-set-leader-keys "TT"
  'dotemacs/scale-transparency-transient-state/dotemacs/toggle-transparency)

(provide 'module-key-bindings)
;;; module-key-bindings.el ends here
