(setq visible-bell t
      font-lock-maximum-decoration t)

;; Highlight current line
(use-package hl-line                    ; Highlight the current line
  :init (global-hl-line-mode 1))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(provide 'init-appearance)
