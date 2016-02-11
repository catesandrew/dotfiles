;;; module-highlight.el --- Highlight Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'use-package)
(require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
;; (require 'core-display-init)
(require 'core-display-init)
(require 'core-fonts-support)
(require 'core-toggle)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:


;; vars

(defvar colors-enable-rainbow-identifiers t
  "If non nil the `rainbow-identifers' package is enabled.")

(defvar colors-enable-nyan-cat-progress-bar nil
  "If non nil all nyan cat packges are enabled (for now only `nyan-mode').")

(defvar colors-theme-identifiers-sat&light
  '((jazz . (50 55))
    (gotham . (45 60))
    (leuven . (100 40))
    (material . (95 105))
    (monokai . (55 60))
    (solarized-dark . (65 55))
    (solarized-light . (60 55))
    (zenburn . (40 65)))
  "alist of theme symbols and pair of saturation and lightness values.")


;; command prefixes

(when colors-enable-rainbow-identifiers
  (setq colors/key-binding-prefixes '(("Ci" . "colors-identifiers")))
  (mapc (lambda (x) (dotemacs-declare-prefix (car x) (cdr x)))
        colors/key-binding-prefixes))


;; packages

(use-package nyan-mode
  :ensure t
  :if colors-enable-nyan-cat-progress-bar
  :config
  (progn
    (setq nyan-wavy-trail t)
    (setq nyan-animate-nyancat t)
    (nyan-mode)
    ;; explicitly re-enable the cat for the first GUI client
    (dotemacs|do-after-display-system-init
     (nyan-mode -1)
     (nyan-mode))

    (dotemacs-add-toggle nyan-cat-progress-bar
      :status nyan-mode
      :on (nyan-mode)
      :off (nyan-mode -1)
      :documentation "Show a nyan cat progress bar in the mode-line."
      :evil-leader "tmn")))


;; rainbow

(use-package rainbow-identifiers
  :ensure t
  :if colors-enable-rainbow-identifiers
  :commands rainbow-identifiers-mode
  :init
  (progn
    (setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face
          rainbow-identifiers-cie-l*a*b*-saturation 100
          rainbow-identifiers-cie-l*a*b*-lightness 40
          ;; override theme faces
          rainbow-identifiers-faces-to-override '(highlight-quoted-symbol
                                                  font-lock-keyword-face
                                                  font-lock-function-name-face
                                                  font-lock-variable-name-face))

    (dotemacs-add-toggle rainbow-identifier-globally
      :status rainbow-identifiers-mode
      :on (rainbow-identifiers-mode)
      :off (rainbow-identifiers-mode -1)
      :documentation "Colorize identifiers globally."
      :evil-leader "tCi")

    (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)

    (defun colors//tweak-theme-colors (theme)
      "Tweak color themes by adjusting rainbow-identifiers."
      (interactive)
      ;; tweak the saturation and lightness of identifier colors
      (let ((sat&light (assq theme colors-theme-identifiers-sat&light)))
        (if sat&light
            (setq rainbow-identifiers-cie-l*a*b*-saturation (cadr sat&light)
                  rainbow-identifiers-cie-l*a*b*-lightness (caddr sat&light))
          (setq rainbow-identifiers-cie-l*a*b*-saturation 80
                rainbow-identifiers-cie-l*a*b*-lightness 45))))
    (colors//tweak-theme-colors dotemacs--cur-theme)

    (defadvice dotemacs-post-theme-init (after colors/post-theme-init activate)
      "Adjust lightness and brightness of rainbow-identifiers on post theme init."
      (colors//tweak-theme-colors dotemacs--cur-theme)))

  :config
  (progn
    ;; functions to change saturation and lightness of colors
    (defun colors//change-color-mini-mode-doc (component)
      "Display a short documentation in the mini buffer."
      (let ((var (intern (format
                          "rainbow-identifiers-cie-l*a*b*-%s" component))))
        (dotemacs/echo "Change color %s mini-mode (value: %s)
+ to increase %s
- to decrease %s
= to reset
Press any other key to exit." component (eval var) component component)))

    (defun colors/change-color-component-overlay-map (component)
      "Set a temporary overlay map to easily change a color COMPONENT from
 rainbow-identifier mode. The color COMPONENT can be 'saturation' or
 'lightness'."
      (set-temporary-overlay-map
       (let ((map (make-sparse-keymap))
             (up-func (intern (format "colors/change-color-%s-up" component)))
             (down-func (intern (format "colors/change-color-%s-down" component)))
             (reset-func (intern (format "colors/change-color-%s-reset" component))))
         (define-key map (kbd "+") up-func)
         (define-key map (kbd "-") down-func)
         (define-key map (kbd "=") reset-func)
         map) t)
      (colors//change-color-mini-mode-doc component))

    (defun colors/start-change-color-saturation ()
      "Initiate the overlay map to change the saturation."
      (interactive)
      (colors/change-color-component-overlay-map "saturation"))
    (defun colors/change-color-saturation-up ()
      "Increase the saturation by 5 units."
      (interactive)
      (colors//change-color-component-func "saturation" 5))
    (defun colors/change-color-saturation-down ()
      "Decrease the saturation by 5 units."
      (interactive)
      (colors//change-color-component-func "saturation" -5))
    (defun colors/change-color-saturation-reset ()
      "Reset the saturation to 100."
      (interactive)
      (colors//change-color-component-func "saturation" 100 t))
    (defun colors/start-change-color-lightness ()
      "Initiate the overlay map to change the lightness."
      (interactive)
      (colors/change-color-component-overlay-map "lightness"))
    (defun colors/change-color-lightness-up ()
      "Increase the lightness by 5 units."
      (interactive)
      (colors//change-color-component-func "lightness" 5))
    (defun colors/change-color-lightness-down ()
      "Decrease the lightness by 5 units."
      (interactive)
      (colors//change-color-component-func "lightness" -5))
    (defun colors/change-color-lightness-reset ()
      "Reset the lightness to 40."
      (interactive)
      (colors//change-color-component-func "lightness" 40 t))

    (defun colors//change-color-component-func
        (component inc &optional reset)
      "Change the color component by adding INC value to it. If RESET is not
 nil the color component is set to INC."
      (let* ((var (intern (format
                           "rainbow-identifiers-cie-l*a*b*-%s" component)))
             (new-value (+ (eval var) inc)))
        (if reset
            (set var inc)
          (progn
            (if (< new-value 0)
                (setq new-value 0))
            (set var new-value)))
        (font-lock-fontify-buffer)
        (colors/change-color-component-overlay-map component)))
    ;; key bindings
    (dotemacs-set-leader-keys "Cis" 'colors/start-change-color-saturation)
    (dotemacs-set-leader-keys "Cil" 'colors/start-change-color-lightness)))

(use-package rainbow-mode               ; Fontify color values in code
  :commands rainbow-mode
  :ensure t
  :init
  (progn
    (dotemacs-set-leader-keys "tCc" 'rainbow-mode)
    (dolist (hook '(prog-mode-hook sgml-mode-hook css-mode-hook web-mode-hook))
      (add-hook hook #'rainbow-mode)))
  :config
  (dotemacs-hide-lighter rainbow-mode))

(use-package rainbow-delimiters         ; Highlight delimiters by depth,  is a "rainbow parentheses"-like
  :ensure t
  :defer t
  :init
  (progn
    (dotemacs-set-leader-keys "tCd" 'rainbow-delimiters-mode)
    (when (member dotemacs-highlight-delimiters '(any all))
      (dotemacs/add-to-hooks 'rainbow-delimiters-mode '(prog-mode-hook)))))


;; hl
(use-package hl-line                    ; Highlight the current line
  :init (global-hl-line-mode 1))

(use-package hl-todo
  :defer t
  :ensure t
  :init (dotemacs/add-to-hooks 'hl-todo-mode '(text-mode-hook
                                                prog-mode-hook)))

(use-package hl-anything ;; Highlight things at point, selections, enclosing parentheses
  :ensure t
  :init
  (progn
    (hl-highlight-mode)
    (setq-default hl-highlight-save-file
                  (concat dotemacs-cache-directory ".hl-save"))
    (dotemacs-set-leader-keys
      "hc"  'hl-unhighlight-all-local
      "hC"  'hl-unhighlight-all-global
      "hh"  'hl-highlight-thingatpt-local
      "hH"  'hl-highlight-thingatpt-global
      "hn"  'hl-find-next-thing
      "hN"  'hl-find-prev-thing
      "hr"  'hl-restore-highlights
      "hs"  'hl-save-highlights))
  :config (dotemacs-hide-lighter hl-highlight-mode))

(use-package hi-lock                    ; Custom regexp highlights
  :init (global-hi-lock-mode)
  :diminish hi-lock-mode)


;; highlight
(use-package highlight-numbers          ; Fontify number literals
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'highlight-numbers-mode)
    (add-hook 'asm-mode-hook (lambda () (highlight-numbers-mode -1)))))

(use-package highlight-indentation
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-add-toggle highlight-indentation
      :status highlight-indentation-mode
      :on (highlight-indentation-mode)
      :off (highlight-indentation-mode -1)
      :documentation "Highlight indentation levels."
      :evil-leader "thi")
    (dotemacs-add-toggle highlight-indentation-current-column
      :status highlight-indentation-current-column-mode
      :on (highlight-indentation-current-column-mode)
      :off (highlight-indentation-current-column-mode -1)
      :documentation "Highlight indentation level at point."
      :evil-leader "thc"))
  :config
  (progn
    (dotemacs-diminish highlight-indentation-mode " ⓗ" " h")
    (dotemacs-diminish highlight-indentation-current-column-mode " ⓗⒸ" " hC")))

(use-package highlight-parentheses
  :defer t
  :ensure t
  :init
  (progn
    (when (member dotemacs-highlight-delimiters '(all current))
      (add-hook 'prog-mode-hook #'highlight-parentheses-mode))
    (setq hl-paren-delay 0.2)
    (dotemacs-set-leader-keys "tCp" 'highlight-parentheses-mode)
    (setq hl-paren-colors '("Springgreen3"
                            "IndianRed1"
                            "IndianRed3"
                            "IndianRed4")))
  :config
  (dotemacs-hide-lighter highlight-parentheses-mode)
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold))

(use-package highlight-quoted
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-quoted-mode))

(use-package highlight-symbol           ; Highlighting and commands for symbols
  :ensure t
  :defer t
  ;; Navigate occurrences of the symbol under point with M-n and M-p, and
  ;; highlight symbol occurrences
  :init (progn (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
               (add-hook 'prog-mode-hook 'highlight-symbol-mode))
  :config
  (setq highlight-symbol-idle-delay 0.3     ; Highlight almost immediately
        highlight-symbol-on-navigation-p t) ; Highlight immediately after
                                        ; navigation
  :diminish highlight-symbol-mode)


;; other
(use-package adaptive-wrap              ; Choose wrap prefix automatically
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode))

(use-package volatile-highlights
  :ensure t
  :config
  (progn
    (volatile-highlights-mode t)
    (dotemacs-hide-lighter volatile-highlights-mode)))

(use-package fill-column-indicator
  :defer t
  :ensure t
  :init
  (progn
    (setq fci-rule-width 1)
    (setq fci-rule-color "#D0BF8F")
    ;; manually register the minor mode since it does not define any lighter
    (push '(fci-mode "") minor-mode-alist)
    (dotemacs-add-toggle fill-column-indicator
      :status fci-mode
      :on (turn-on-fci-mode)
      :off (turn-off-fci-mode)
      :documentation "Display the fill column indicator."
      :evil-leader "tf"))
  :config (dotemacs-hide-lighter fci-mode))

(use-package indent-guide
  :defer t
  :ensure t
  :init
  (progn
    (setq indent-guide-delay 0.3)
    (dotemacs-add-toggle indent-guide
      :status indent-guide-mode
      :on (indent-guide-mode)
      :off (indent-guide-mode -1)
      :documentation
      "Highlight indentation level at point. (alternative to highlight-indentation)."
      :evil-leader "ti")
    (dotemacs-add-toggle indent-guide-globally
      :status indent-guide-mode
      :on (indent-guide-global-mode)
      :off (indent-guide-global-mode -1)
      :documentation
      "Highlight indentation level at point globally. (alternative to highlight-indentation)."
      :evil-leader "t TAB"))
  :config
  (dotemacs-diminish indent-guide-mode " ⓘ" " i"))

(provide 'module-highlight)
;;; module-highlight.el ends here
