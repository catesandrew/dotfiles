;;; module-csv.el --- CSV Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
(require 'core-transient-state)
(require 'core-auto-completion)
(require 'core-fonts-support)
;; (require 'core-display-init)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(use-package csv-mode
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-declare-prefix-for-mode 'csv-mode "ms" "sort")
    (dotemacs-declare-prefix-for-mode 'csv-mode "mv" "yank")
    (dotemacs-set-leader-keys-for-major-mode 'csv-mode
      "a"  'csv-align-fields
      "d"  'csv-kill-fields
      "i"  'csv-toggle-invisibility
      "n"  'csv-forward-field
      "p"  'csv-backward-field
      "r"  'csv-reverse-region
      "sf" 'csv-sort-fields
      "sn" 'csv-sort-numeric-fields
      "so" 'csv-toggle-descending
      "t"  'csv-transpose
      "u"  'csv-unalign-fields
      "vf" 'csv-yank-fields
      "vt" 'csv-yank-as-new-table)))

(provide 'module-csv)
;;; module-csv.el ends here
