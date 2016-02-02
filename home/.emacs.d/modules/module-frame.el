;;; Frame
(require 'module-global)

(setq truncate-partial-width-windows nil ; Make side by side buffers function
                                         ; the same as the main window.
      frame-resize-pixelwise t           ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

(defvar dotemacs//frame-width nil)
(defvar dotemacs//frame-height nil)

(use-package frame
  :bind (("C-c u F" . toggle-frame-fullscreen))
  :init
  (progn
    (defun dotemacs-set-frame-size ()
      (when (display-graphic-p)
        ;; for the height, subtract 60 pixels from the screen height (for
        ;; panels, menubars and whatnot), then divide by the height of a char to
        ;; get the height we want.
        ;; use 120 char wide window for largeish displays and smaller 90 column
        ;; windows for smaller displays.
        (let* ((fwp (if (> (x-display-pixel-width) 1680) 120 90))
               (fhp (/ (- (x-display-pixel-height) 60)
                       (frame-char-height))))
          (setq dotemacs//frame-width fwp)
          (setq dotemacs//frame-height fhp)
          (add-to-list 'initial-frame-alist `(width . ,dotemacs//frame-width))
          (add-to-list 'initial-frame-alist `(height . ,dotemacs//frame-height))
          (add-to-list 'default-frame-alist `(height . ,dotemacs//frame-height))
          (add-to-list 'default-frame-alist `(width  . ,dotemacs//frame-width))
          (set-frame-height (selected-frame) fhp)
          (set-frame-width (selected-frame) fwp))))

    (dotemacs|do-after-display-system-init
      (dotemacs-set-frame-size))

    ;; Kill `suspend-frame'
    (global-set-key (kbd "C-z") nil)
    (global-set-key (kbd "C-x C-z") nil)))

(use-package zoom-frm
  :commands (zoom-frm-unzoom
             zoom-frm-out
             zoom-frm-in)
  :ensure t
  :init
  (progn
    (dotemacs-define-micro-state zoom-frm
      :doc "[+/=] zoom frame in [-] zoom frame out [0] reset zoom [q]uit"
      :evil-leader "zf"
      :use-minibuffer t
      :bindings
      ("+" dotemacs-zoom-frm-in :post (dotemacs-zoom-frm-powerline-reset))
      ("=" dotemacs-zoom-frm-in :post (dotemacs-zoom-frm-powerline-reset))
      ("-" dotemacs-zoom-frm-out :post (dotemacs-zoom-frm-powerline-reset))
      ("0" dotemacs-zoom-frm-unzoom :post (dotemacs-zoom-frm-powerline-reset))
      ("q" nil :exit t))

    (defun dotemacs-zoom-frm-powerline-reset ()
      (when (fboundp 'powerline-reset)
        (setq-default powerline-height (dotemacs-compute-powerline-height))
        (powerline-reset)))

    (defun dotemacs-zoom-frm-do (arg)
      "Perform a zoom action depending on ARG value."
      (let ((zoom-action (cond ((eq arg 0) 'zoom-frm-unzoom)
                               ((< arg 0) 'zoom-frm-out)
                               ((> arg 0) 'zoom-frm-in)))
            (fm (cdr (assoc 'fullscreen (frame-parameters))))
            (fwp (* (frame-char-width) (frame-width)))
            (fhp (* (frame-char-height) (frame-height))))
        (when (equal fm 'maximized)
          (toggle-frame-maximized))
        (funcall zoom-action)
        (set-frame-size nil fwp fhp t)
        (when (equal fm 'maximized)
          (toggle-frame-maximized))))

    (defun dotemacs-zoom-frm-in ()
      "zoom in frame, but keep the same pixel size"
      (interactive)
      (dotemacs-zoom-frm-do 1))

    (defun dotemacs-zoom-frm-out ()
      "zoom out frame, but keep the same pixel size"
      (interactive)
      (dotemacs-zoom-frm-do -1))

    (defun dotemacs-zoom-frm-unzoom ()
      "Unzoom current frame, keeping the same pixel size"
      (interactive)
      (dotemacs-zoom-frm-do 0))

    ;; Font size, either with ctrl + mouse wheel
    (global-set-key (kbd "<C-wheel-up>") 'dotemacs-zoom-frm-in)
    (global-set-key (kbd "<C-wheel-down>") 'dotemacs-zoom-frm-out)))

(provide 'module-frame)
;;; module-frame.el ends here
