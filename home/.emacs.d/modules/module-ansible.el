;;; module-ansible.el --- Ansible Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-vars)
;; (require 'core-funcs)
;; (require 'core-keybindings)
;; (require 'core-display-init)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(setq ansible/ansible-filename-re
      "\\(site\.yml\\|roles/.+\.yml\\|group_vars/.+\\|host_vars/.+\\)")

(defun ansible/ansible-should-enable? ()
  (and (stringp buffer-file-name)
       (string-match ansible/ansible-filename-re buffer-file-name)))

(defun ansible/ansible-maybe-enable ()
  (when (ansible/ansible-should-enable?)
    (ansible 1)))

(defun ansible/ansible-doc-maybe-enable ()
  (when (ansible/ansible-should-enable?)
    (ansible-doc-mode 1)))

(use-package ansible
  ;; Tracking here:
  ;; https://github.com/k1LoW/emacs-ansible/issues/4
  ;; Ansible wants to hook into yasnippet and load its snippets always
  :disabled t
  :defer t
  :init
  (progn
    (with-eval-after-load 'yaml-mode
      (add-hook 'yaml-mode-hook 'ansible/ansible-maybe-enable))))

(use-package ansible-doc                ; Documentation lookup for Ansible
  :ensure t
  :defer t
  :init (with-eval-after-load 'yaml-mode
          '(add-hook 'yaml-mode-hook 'ansible/ansible-doc-maybe-enable))
  :diminish (ansible-doc-mode . "❓"))

(provide 'module-ansible)
;;; module-ansible.el ends here
