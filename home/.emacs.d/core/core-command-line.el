;;; core-command-line.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun dotemacs//parse-command-line (args)
  "Handle Dotemacs specific command line arguments.
The reason why we don't use the Emacs hooks for processing user defined
arguments is that we want to process these arguments as soon as possible."
  (let ((i 0) new-args)
    (while (< i (length args))
      (let ((arg (nth i args))
            (next-arg-digit
             (when (< (1+ i) (length args))
               (string-to-number (nth (1+ i ) args)))))
        (when (or (null next-arg-digit) (= 0 next-arg-digit))
          (setq next-arg-digit nil))
        (pcase arg
          ("--profile"
           (setq dotemacs-debug-with-profile t)
           (setq dotemacs-debugp t))
          ("--timed-requires"
           (setq dotemacs-debug-with-timed-requires t)
           (when next-arg-digit
             (setq dotemacs-debug-timer-threshold next-arg-digit
                   i (1+ i)))
           (setq dotemacs-debugp t))
          ("--adv-timers"
           (setq dotemacs-debug-with-adv-timers t)
           (when next-arg-digit
             (setq dotemacs-debug-timer-threshold next-arg-digit
                   i (1+ 1)))
           (setq dotemacs-debugp t))
          ("--insecure"
           (setq dotemacs-elpa-https nil))
          (_ (push arg new-args))))
      (setq i (1+ i)))
    (nreverse new-args)))

(setq command-line-args (dotemacs//parse-command-line command-line-args))

(provide 'core-command-line)
;;; core-command-line.el ends here
