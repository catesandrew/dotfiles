(defun latex/build ()
  (interactive)
  (progn
    (let ((TeX-save-query nil))
      (TeX-save-document (TeX-master-file)))
    (TeX-command dotemacs-latex-build-command 'TeX-master-file -1)))
    ;; (setq build-proc (TeX-command dotemacs-latex-build-command 'TeX-master-file -1))
    ;; ;; Sometimes, TeX-command returns nil causing an error in set-process-sentinel
    ;; (when build-proc
    ;;   (set-process-sentinel build-proc 'latex//build-sentinel))))

(defun latex//build-sentinel (process event)
  (if (string= event "finished\n")
      (TeX-view)
    (message "Errors! Check with C-`")))

(defun latex//autofill ()
  "Check whether the pointer is ucrrently inside on the
environments described in `dotemacs-latex-nofill-env' and if so, inhibits
the automatic filling of the current paragraph."
  (let ((do-auto-fill t)
        (current-environment "")
        (level 0))
    (while (and do-auto-fill (not (string= current-environment "document")))
      (setq level (1+ level)
            current-environment (LaTeX-current-environment level)
            do-auto-fill (not (member current-environment dotemacs-latex-nofill-env))))
    (when do-auto-fill
      (do-auto-fill))))

(defun latex/auto-fill-mode ()
  "Toggle uato-fill-mode using the custom auto-fill function."
  (interactive)
  (auto-fill-mode)
  (setq auto-fill-function 'latex//autofill))

(provide 'init-latex)
