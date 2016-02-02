;;; Location
(require 'module-global)

(defvar geolocation-enable-osx-location-service-support t
  "If non nil enable the OS X location service support.")

(defvar geolocation-enable-weather-forecast t
  "If non nil enable the weather forecast service.")

(defvar geolocation-enable-automatic-theme-changer nil
  "If non nil enable the automatic change of theme based on the current time.")

(use-package osx-location
  :if geolocation-enable-osx-location-service-support
  :ensure t
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
    (osx-location-watch)))

(use-package sunshine
  :if geolocation-enable-weather-forecast
  :ensure t
  :defer t
  :init
  (progn
    (dotemacs-set-leader-keys
      "aw" 'sunshine-forecast
      "aW" 'sunshine-quick-forecast)

    (evilified-state-evilify sunshine-mode sunshine-mode-map
      (kbd "q") 'quit-window
      (kbd "i") 'sunshine-toggle-icons))
  :config
  (progn
    (setq sunshine-appid "bedbfc11dac244208e29f486c82412b6"
          sunshine-location "Huntington Beach, CA")

    ;; just in case location was not set by user, or on OS X,
    ;; if wasn't set up automatically, will not work with Emac's
    ;; default for ;; `calendar-location-name'
    (when (not (boundp 'sunshine-location))
      (setq sunshine-location (format "%s, %s"
                                      calendar-latitude
                                      calendar-longitude)))))

(provide 'module-location)
;;; module-location.el ends here
