;;; module-location.el --- Location Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-vars)
;; (require 'core-funcs)
(require 'core-keybindings)
(require 'evil-evilified-state)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)
(require 'use-package)

;;; Code:

(defvar geolocation-enable-osx-location-service-support t
  "If non nil enable the OS X location service support.")

(defvar geolocation-enable-weather-forecast t
  "If non nil enable the weather forecast service.")

(defvar geolocation-enable-automatic-theme-changer nil
  "If non nil enable the automatic change of theme based on the current time.")

(use-package theme-changer
  :if geolocation-enable-automatic-theme-changer
  :ensure t
  :config
  (progn
    (when (> (length dotemacs-themes) 1)
      (change-theme (nth 0 dotemacs-themes)
                    (nth 1 dotemacs-themes)))))

(use-package osx-location
  :if geolocation-enable-osx-location-service-support
  :ensure t
  :defer t
  :init
  (progn
    (when (dotemacs/system-is-mac)
      (add-hook 'osx-location-changed-hook
                (lambda ()
                  (let ((location-changed-p nil)
                        (_longitude (/ (truncate (* osx-location-longitude 10)) 10.0)) ; one decimal point, no rounding
                        (_latitdue (/ (truncate (* osx-location-latitude 10)) 10.0)))
                    (unless (equal (bound-and-true-p calendar-longitude) _longitude)
                      (setq calendar-longitude _longitude
                            location-changed-p t))
                    (unless (equal (bound-and-true-p  calendar-latitude) _latitdue)
                      (setq calendar-latitude _latitdue
                            location-changed-p t))
                    (when location-changed-p
                      (message "Location changed %s %s (restarting rase-timer)" calendar-latitude calendar-longitude)
                      (rase-start t)
                      ))))
      (osx-location-watch))))

(use-package rase
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'osx-location-changed-hook
              (lambda ()
                (setq calendar-latitude osx-location-latitude
                      calendar-longitude osx-location-longitude)
                (unless (bound-and-true-p calendar-location-name)
                  (setq calendar-location-name
                        (format "%s, %s"
                                osx-location-latitude
                                osx-location-longitude)))))
    (osx-location-watch)
    (defadvice rase-start (around test-calendar activate)
      "Don't call `raise-start' if `calendar-latitude' or
`calendar-longitude' are not bound yet, or still nil.

This is setup this way because `rase.el' does not test these
values, and will fail under such conditions, when calling
`solar.el' functions.

Also, it allows users who enabled service such as `osx-location'
to not have to set these variables manually when enabling this layer."
      (if (and (bound-and-true-p calendar-longitude)
               (bound-and-true-p calendar-latitude))
          ad-do-it))
    (rase-start t)))

(use-package sunshine
  :if geolocation-enable-weather-forecast
  :ensure t
  :init
  (progn
    (setq sunshine-appid "bedbfc11dac244208e29f486c82412b6"
          sunshine-location "Huntington Beach, CA")
    (dotemacs-set-leader-keys
      "aw" 'sunshine-forecast
      "aW" 'sunshine-quick-forecast))
  :config
  (progn
    (evilified-state-evilify-map sunshine-mode-map
      :mode sunshine-mode
      :bindings
      (kbd "q") 'quit-window
      (kbd "i") 'sunshine-toggle-icons)

    ;; just in case location was not set by user, or on OS X,
    ;; if wasn't set up automatically, will not work with Emac's
    ;; default for ;; `calendar-location-name'
    (when (not (boundp 'sunshine-location))
      (setq sunshine-location (format "%s, %s"
                                      calendar-latitude
                                      calendar-longitude)))))

(dotemacs-use-package-add-hook popwin
  :post-config
  (push '("*Sunshine*" :dedicated t :position bottom)
        popwin:special-display-config))

(provide 'module-location)
;;; module-location.el ends here
