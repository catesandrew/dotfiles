;;; module-perspective.el --- Perspective Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'use-package)
(require 'core-vars)
;; (require 'core-funcs)
(require 'core-keybindings)
;; (require 'core-display-init)
(require 'core-use-package-ext)
(require 'core-buffers)
(require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
(require 'module-utils)
(require 'use-package)

;;; Code:

(declare-function dotemacs-home "module-utils")

(defvar dotemacs-layouts-directory
  (expand-file-name (concat dotemacs-cache-directory "layouts/"))
  "Save layouts in this directory.")

(defvar layouts-enable-autosave nil
  "If true, saves perspectives to file per `layouts-autosave-delay'.")

(defvar layouts-autosave-delay 900
  "Delay in seconds between each layouts auto-save.")

(defun dotemacs//current-layout-name ()
  "Get name of the current perspective."
  (safe-persp-name (get-frame-persp)))

;; Helm related functions --------------------------------------------------

(defun dotemacs/persp-helm-mini ()
  "As `helm-mini' but restricts visible buffers by perspective."
  (interactive)
  (with-persp-buffer-list ()
                          (helm-mini)))

(defun dotemacs//helm-perspectives-source ()
  (helm-build-in-buffer-source
      (concat "Current Perspective: " (dotemacs//current-layout-name))
    :data (persp-names)
    :fuzzy-match t
    :action
    '(("Switch to perspective" . persp-switch)
      ("Close perspective(s)" . (lambda (candidate)
                                  (mapcar
                                   'persp-kill-without-buffers
                                   (helm-marked-candidates))))
      ("Kill perspective(s)" . (lambda (candidate)
                                 (mapcar 'persp-kill
                                         (helm-marked-candidates)))))))

(defun dotemacs/helm-perspectives ()
  "Control Panel for perspectives. Has many actions.
If match is found
f1: (default) Select perspective
f2: Close Perspective(s) <- mark with C-SPC to close more than one-window
f3: Kill Perspective(s)

If match is not found
<enter> Creates perspective

Closing doesn't kill buffers inside the perspective while killing
perspectives does."
  (interactive)
  (helm
   :buffer "*Helm Perspectives*"
   :sources
   `(,(dotemacs//helm-perspectives-source)
     ,(helm-build-dummy-source "Create new perspective"
        :requires-pattern t
        :action
        '(("Create new perspective" .
           (lambda (name)
             (let ((persp-reset-windows-on-nil-window-conf t))
               (persp-switch name)
               (unless (member name (persp-names-current-frame-fast-ordered))
                 (dotemacs-home))))))))))

;; ability to use helm find files but also adds to current perspective
(defun dotemacs/helm-persp-close ()
  "Kills perspectives without killing the buffers"
  (interactive)
  (helm
   :buffer "*Helm Kill Perspectives (without killing buffers)*"
   :sources
   (helm-build-in-buffer-source
       (concat "Current Perspective: " (dotemacs//current-layout-name))
     :data (persp-names)
     :fuzzy-match t
     :action
     '(("Close perspective(s)" . (lambda (candidate)
                                   (mapcar
                                    'persp-kill-without-buffers
                                    (helm-marked-candidates))))))))

(defun dotemacs/helm-persp-kill ()
  "Kills perspectives with all their buffers"
  (interactive)
  (helm
   :buffer "*Helm Kill Perspectives with all their buffers*"
   :sources (helm-build-in-buffer-source
                (s-concat "Current Perspective: "
                          (dotemacs//current-layout-name))
              :data (persp-names)
              :fuzzy-match t
              :action
              '(("Kill perspective(s)" .
                 (lambda (candidate)
                   (mapcar 'persp-kill
                           (helm-marked-candidates))))))))

;; Helm Projectile related functions ---------------------------------------

(defun dotemacs/helm-persp-switch-project (arg)
  (interactive "P")
  (helm
   :sources
   (helm-build-in-buffer-source "*Helm Switch Project Layout*"
     :data (lambda ()
             (if (projectile-project-p)
                 (cons (abbreviate-file-name (projectile-project-root))
                       (projectile-relevant-known-projects))
               projectile-known-projects))
     :fuzzy-match helm-projectile-fuzzy-match
     :mode-line helm-read-file-name-mode-line-string
     :action '(("Switch to Project Perspective" .
                (lambda (project)
                  (let ((persp-reset-windows-on-nil-window-conf t))
                    (persp-switch project)
                    (let ((projectile-completion-system 'helm))
                      (projectile-switch-project-by-name project)))))))
   :buffer "*Helm Projectile Layouts*"))

(defun dotemacs/ivy-persp-switch-project (arg)
  (interactive "P")
  (ivy-read "Switch to Project Perspective:"
            (if (projectile-project-p)
                (cons (abbreviate-file-name (projectile-project-root))
                      (projectile-relevant-known-projects))
              projectile-known-projects)
            :action (lambda (project)
                      (let ((persp-reset-windows-on-nil-window-conf t))
                        (persp-switch project)
                        (let ((projectile-completion-system 'ivy))
                          (projectile-switch-project-by-name project))))
            )

  )

;; Autosave ----------------------------------------------------------------

(defun dotemacs//layout-autosave ()
  "Perspectives mode autosave.
Autosaves perspectives layouts every `persp-autosave-interal' seconds.
Cancels autosave on exiting perspectives mode."
  (if (and persp-mode layouts-enable-autosave)
      (progn
        (message "Perspectives mode autosaving enabled.")
        (setq dotemacs--layouts-autosave-timer
              (run-with-timer
               layouts-autosave-delay
               layouts-autosave-delay
               (lambda ()
                 (message "Saving perspectives to file.")
                 (persp-save-state-to-file)))))
    (when dotemacs--layouts-autosave-timer
      (cancel-timer dotemacs--layouts-autosave-timer)
      (setq dotemacs--layouts-autosave-timer nil))))

;; Eyebrowse - allow perspective-local workspaces --------------------------

(defun dotemacs/load-eyebrowse-for-perspective (&optional frame)
  "Load an eyebrowse workspace according to a perspective's parameters.
FRAME's perspective is the perspective that is considered, defaulting to
the current frame's perspective.
If the perspective doesn't have a workspace, create one."
  (let* ((persp (get-frame-persp frame))
         (window-configs (persp-parameter 'eyebrowse-window-configs persp))
         (current-slot (persp-parameter 'eyebrowse-current-slot persp))
         (last-slot (persp-parameter 'eyebrowse-last-slot persp)))
    (if window-configs
        (progn
          (eyebrowse--set 'window-configs window-configs frame)
          (eyebrowse--set 'current-slot current-slot frame)
          (eyebrowse--set 'last-slot last-slot frame)
          (eyebrowse--load-window-config current-slot))
      (eyebrowse--set 'window-configs nil frame)
      (eyebrowse-init frame)
      (dotemacs/save-eyebrowse-for-perspective frame))))

(defun dotemacs/update-eyebrowse-for-perspective (_new-persp-name)
  "Update and save current frame's eyebrowse workspace to its perspective.
Parameter _NEW-PERSP-NAME is ignored, and exists only for compatibility with
`persp-before-switch-functions'."
  (eyebrowse--update-window-config-element
   (eyebrowse--current-window-config (eyebrowse--get 'current-slot)
                                     (eyebrowse--get 'current-tag)))
  (dotemacs/save-eyebrowse-for-perspective))

(defun dotemacs/save-eyebrowse-for-perspective (&optional frame)
  "Save FRAME's eyebrowse workspace to FRAME's perspective.
FRAME defaults to the current frame."
  (let ((persp (get-frame-persp frame)))
    (set-persp-parameter
     'eyebrowse-window-configs (eyebrowse--get 'window-configs frame) persp)
    (set-persp-parameter
     'eyebrowse-current-slot (eyebrowse--get 'current-slot frame) persp)
    (set-persp-parameter
     'eyebrowse-last-slot (eyebrowse--get 'last-slot frame) persp)))

(defun dotemacs/layout-workspaces-transient-state ()
  "Launches the workspaces transient state, if defined."
  (interactive)
  (if (fboundp 'dotemacs/workspaces-transient-state/body)
      (call-interactively 'dotemacs/workspaces-transient-state/body)
    (message "You need the eyebrowse layer to use this feature.")))

(use-package persp-mode
  :diminish persp-mode
  :ensure t
  :init
  (progn
    (setq persp-auto-resume-time (if dotemacs-auto-resume-layouts 1 -1)
          persp-nil-name dotemacs-default-layout-name
          persp-reset-windows-on-nil-window-conf nil
          persp-set-last-persp-for-new-frames nil
          persp-save-dir dotemacs-layouts-directory)

    ;; always activate persp-mode
    (persp-mode)

    (defvar dotemacs--layouts-ms-doc-toggle 0
      "Display a short doc when nil, full doc otherwise.")

    (defvar dotemacs--last-selected-layout persp-nil-name
      "Previously selected layout.")

    (defvar dotemacs--custom-layout-alist nil
      "List of custom layouts with their bound keys.
 Do not modify directly, use provided `dotemacs-define-custom-layout'")

    (defvar dotemacs--layouts-autosave-timer nil
      "Timer for layouts auto-save.")

    (defun dotemacs/jump-to-last-layout ()
      "Open the previously selected layout, if it exists."
      (interactive)
      (unless (eq 'non-existent
                  (gethash dotemacs--last-selected-layout
                           *persp-hash* 'non-existent))
        (persp-switch dotemacs--last-selected-layout)))

    ;; Perspectives transient-state -------------------------------------------

    (defun dotemacs//layouts-ms-toggle-doc ()
      "Toggle the full documenation for the layouts transient-state."
      (interactive)
      (setq dotemacs--layouts-ms-doc-toggle
            (logxor dotemacs--layouts-ms-doc-toggle 1)))

    (defun dotemacs//layout-format-name (name pos)
      "Format the layout name given by NAME for display in mode-line."
      (let* ((layout-name (if (file-directory-p name)
                              (file-name-nondirectory (directory-file-name name))
                            name))
             (string-name (format "%s" layout-name))
             (current (equal name (dotemacs//current-layout-name)))
             (caption (concat (number-to-string (if (eq 9 pos) 0 (1+ pos)))
                              ":" string-name)))
        (if current
            (concat (when current "[") caption (when current "]"))
          caption)))

    (defun dotemacs//layouts-ms-doc ()
      "Return the docstring for the layouts transient-state."
      (let* ((persp-list (or (persp-names-current-frame-fast-ordered)
                             (list persp-nil-name)))
             (formatted-persp-list
              (concat
               (mapconcat
                (lambda (persp)
                  (dotemacs//layout-format-name
                   persp (position persp persp-list))) persp-list " | "))))
        (concat formatted-persp-list
                (when (equal 1 dotemacs--layouts-ms-doc-toggle)
                  dotemacs--layouts-ms-documentation))))

    (dotemacs-define-transient-state layouts
        :title "Layouts Transient State"
        :additional-docs
        (dotemacs--layouts-ms-documentation .
                                             "

  [_?_]^^^^       toggle this help
  [_0_,_9_]^^     switch to nth layout
  [_<tab>_]^^^^   switch to the last
  [_A_]^^^^       add all buffers from another layout
  [_a_]^^^^       add all the buffers from another layout in the current one
  [_b_]^^^^       select a buffer in the current layout
  [_c_]^^^^       close the current layout and keep its buffers
  [_C_]^^^^       close the other layouts and keep their buffers
  [_h_]^^^^       go to default layout
  [_l_]^^^^       select/create a layout with helm
  [_L_]^^^^       load layouts from file
  [_n_/_C-l_]^^   next layout in list
  [_N_/_p_/_C-h_] previous layout in list
  [_o_]^^^^       open a custom layout
  [_r_]^^^^       remove current buffer from layout
  [_R_]^^^^       rename current layout
  [_s_]^^^^       save all layouts
  [_S_]^^^^       save layouts by names
  [_t_]^^^^       show a buffer without adding it to current layout
  [_w_]^^^^       workspaces micro state
  [_x_]^^^^       kill current layout with its buffers
  [_X_]^^^^       kill other layouts with their buffers")
        :bindings
        ;; need to exit in case number doesn't exist
        ("?" dotemacs//layouts-ms-toggle-doc)
        ("1" dotemacs/persp-switch-to-1 :exit t)
        ("2" dotemacs/persp-switch-to-2 :exit t)
        ("3" dotemacs/persp-switch-to-3 :exit t)
        ("4" dotemacs/persp-switch-to-4 :exit t)
        ("5" dotemacs/persp-switch-to-5 :exit t)
        ("6" dotemacs/persp-switch-to-6 :exit t)
        ("7" dotemacs/persp-switch-to-7 :exit t)
        ("8" dotemacs/persp-switch-to-8 :exit t)
        ("9" dotemacs/persp-switch-to-9 :exit t)
        ("0" dotemacs/persp-switch-to-0 :exit t)
        ("<tab>" dotemacs/jump-to-last-layout)
        ("<return>" nil :exit t)
        ("C-h" persp-prev)
        ("C-l" persp-next)
        ("a" persp-add-buffer :exit t)
        ("A" persp-import-buffers :exit t)
        ("b" dotemacs/persp-helm-mini :exit t)
        ("c" dotemacs/layouts-ms-close)
        ("C" dotemacs/layouts-ms-close-other :exit t)
        ("h" dotemacs/layout-goto-default :exit t)
        ("l" dotemacs/helm-perspectives :exit t)
        ("L" persp-load-state-from-file :exit t)
        ("n" persp-next)
        ("N" persp-prev)
        ("o" dotemacs/select-custom-layout :exit t)
        ("p" persp-prev)
        ("r" persp-remove-buffer :exit t)
        ("R" dotemacs/layouts-ms-rename :exit t)
        ("s" persp-save-state-to-file :exit t)
        ("S" persp-save-to-file-by-names :exit t)
        ("t" persp-temporarily-display-buffer :exit t)
        ("w" dotemacs/layout-workspaces-transient-state :exit t)
        ("x" dotemacs/layouts-ms-kill)
        ("X" dotemacs/layouts-ms-kill-other :exit t))
    (dotemacs-set-leader-keys "l" 'dotemacs/layouts-transient-state/body)

    ;; This enables the dynamic behavior from micro-states using ?
    (add-hook 'dotemacs-post-user-config-hook
              (lambda ()
                (setq dotemacs/layouts-transient-state/hint
                      `(concat
                        ,(when dotemacs-show-transient-state-title
                           (concat
                            (propertize "Layouts Transient State"
                                        'face 'dotemacs-transient-state-title-face)
                            "\n"))
                        (dotemacs//layouts-ms-doc))))
              t)

     (defun dotemacs/layout-switch-by-pos (pos)
       "Switch to perspective of position POS."
       (let ((persp-to-switch
              (nth pos (persp-names-current-frame-fast-ordered))))
         (if persp-to-switch
             (persp-switch persp-to-switch)
           (when (y-or-n-p
                  (concat "Perspective in this position doesn't exist.\n"
                          "Do you want to create one? "))
             (let ((persp-reset-windows-on-nil-window-conf t))
               (persp-switch nil)
               (dotemacs-home-delete-other-windows))))))

     ;; Define all `dotemacs/persp-switch-to-X' functions
     (dolist (i (number-sequence 9 0 -1))
       (eval `(defun ,(intern (format "dotemacs/persp-switch-to-%s" i)) nil
                ,(format "Switch to layout %s." i)
                (interactive)
                (dotemacs/layout-switch-by-pos ,(if (eq 0 i) 9 (1- i)))
                (dotemacs/layouts-transient-state/body))))

     (defun dotemacs/layout-goto-default ()
       "Go to `dotemacs-default-layout-name` layout"
       (interactive)
       (when dotemacs-default-layout-name
         (persp-switch dotemacs-default-layout-name)))

     (defun dotemacs/layouts-ms-rename ()
       "Rename a layout and get back to the perspectives transient-state."
       (interactive)
       (call-interactively 'persp-rename)
       (spacemacs/layouts-transient-state/body))

     (defun dotemacs/layouts-ms-close ()
       "Kill current perspective"
       (interactive)
       (persp-kill-without-buffers (dotemacs//current-layout-name)))

     (defun dotemacs/layouts-ms-close-other ()
       (interactive)
       (call-interactively 'dotemacs/helm-persp-close)
       (spacemacs/layouts-transient-state/body))

     (defun dotemacs/layouts-ms-kill ()
       "Kill current perspective"
       (interactive)
       (persp-kill (dotemacs//current-layout-name)))

     (defun dotemacs/layouts-ms-kill-other ()
       (interactive)
       (call-interactively 'dotemacs/helm-persp-kill)
       (spacemacs/layouts-transient-state/body))

     (defun dotemacs/layouts-ms-last ()
       "Switch to the last active perspective"
       (interactive)
       (persp-switch persp-last-persp-name))

     ;; Custom perspectives transient-state -------------------------------------

     (defun dotemacs//custom-layout-func-name (name)
       "Return the name of the custom-perspective function for NAME."
       (intern (concat "dotemacs/custom-perspective-" name)))

     (defmacro dotemacs-define-custom-layout (name &rest props)
       "Define a custom-perspective called NAME.

FUNC is a FUNCTION defined using NAME and the result of
`dotemacs//custom-layout-func-name', it takes care of
creating the perspective NAME and executing the expressions given
in the :body property to this macro.

NAME is a STRING.

Available PROPS:

`:binding STRING'
   Key to be bound to the function FUNC

`:body EXPRESSIONS'
  One or several EXPRESSIONS that are going to be evaluated after
  we change into the perspective NAME."
      (declare (indent 1))
      (let* ((func (dotemacs//custom-layout-func-name name))
             (binding (car (dotemacs-mplist-get props :binding)))
             (body (dotemacs-mplist-get props :body))
             (already-defined? (cdr (assoc binding
                                           dotemacs--custom-layout-alist))))
        `(progn
           (defun ,func ()
             ,(format "Open custom perspective %s" name)
             (interactive)
             (let ((initialize (not (gethash ,name *persp-hash*))))
               (persp-switch ,name)
               (when initialize
                 (delete-other-windows)
                 ,@body)))
           ;; Check for Clashes
           (if ,already-defined?
               (unless (equal ,already-defined? ,name)
                 (dotemacs-buffer/warning "Replacing existing binding \"%s\" for %s with %s"
                       ,binding ,already-defined? ,name )
                 (push '(,binding . ,name) dotemacs--custom-layout-alist))
             (push '(,binding . ,name) dotemacs--custom-layout-alist)))))

    (dotemacs-define-custom-layout "@Dotemacs"
      :binding "e"
      :body
      (dotemacs/find-dotfile))

    (defun dotemacs/select-custom-layout ()
      "Update the custom-perspectives microstate and then activate it."
      (interactive)
      (dotemacs//update-custom-layouts)
      (dotemacs/custom-layouts-transient-state/body))

    (defun dotemacs//custom-layouts-ms-documentation ()
      "Return the docstring for the custom perspectives transient-state."
      (if dotemacs--custom-layout-alist
          (mapconcat (lambda (custom-persp)
                       (format "[%s] %s"
                               (car custom-persp) (cdr custom-persp)))
                     dotemacs--custom-layout-alist " ")
        (dotemacs-buffer/warning (format "`dotemacs--custom-layout-alist' variable is empty" ))))

    (defun dotemacs//update-custom-layouts ()
      "Ensure the custom-perspectives transient-state is updated.
Takes each element in the list `dotemacs--custom-layout-alist'
format so they are supported by the
`dotemacs/custom-layouts-transient-state' macro."
      (let (bindings)
        (dolist (custom-persp dotemacs--custom-layout-alist bindings)
          (let* ((binding (car custom-persp))
                 (name (cdr custom-persp))
                 (func-name (dotemacs//custom-layout-func-name name)))
            (push (list binding func-name) bindings)))
        (eval `(dotemacs-define-transient-state custom-layouts
                 :doc (concat (dotemacs//custom-layouts-ms-documentation))
                 :bindings
                 ,@bindings)))))
  :config
  (progn
    (defadvice persp-activate (before dotemacs//save-toggle-layout activate)
      (setq dotemacs--last-selected-layout persp-last-persp-name))
    (add-hook 'persp-mode-hook 'dotemacs//layout-autosave)

    (defun dotemacs/alternate-buffer-in-persp ()
      "Switch back and forth between current and last buffer in the
current perspective."
      (interactive)
      (with-persp-buffer-list ()
        (switch-to-buffer (other-buffer (current-buffer) t))))

    (defun dotemacs-layouts/non-restricted-buffer-list ()
      (interactive)
      (remove-hook 'ido-make-buffer-list-hook  #'persp-restrict-ido-buffers)
      (helm-mini)
      (add-hook 'ido-make-buffer-list-hook  #'persp-restrict-ido-buffers))

    (dotemacs-declare-prefix "b" "persp-buffers")
    (dotemacs-declare-prefix "B" "global-buffers")

    ;; Override SPC TAB to only change buffers in perspective
    (dotemacs-set-leader-keys
      "TAB"  'dotemacs/alternate-buffer-in-persp
      "ba"   'persp-add-buffer
      "br"   'persp-remove-buffer
      "Bb"   'dotemacs-layouts/non-restricted-buffer-list)))

(dotemacs-use-package-add-hook spaceline-config
  :post-init
  (setq spaceline-display-default-perspective
        dotemacs-display-default-layout))

(dotemacs-use-package-add-hook eyebrowse
  :post-init
  (add-hook 'persp-before-switch-functions #'dotemacs/update-eyebrowse-for-perspective)
  (add-hook 'eyebrowse-post-window-switch-hook #'dotemacs/save-eyebrowse-for-perspective)
  (add-hook 'persp-activated-hook #'dotemacs/load-eyebrowse-for-perspective))

(dotemacs-use-package-add-hook helm
  :post-init
  (dotemacs-set-leader-keys
    "pl" 'dotemacs/helm-persp-switch-project))

(dotemacs-use-package-add-hook swiper
  :post-init
  (dotemacs-set-leader-keys
    "pl" 'dotemacs/ivy-persp-switch-project))

(provide 'module-perspective)
;;; module-perspective.el ends here
