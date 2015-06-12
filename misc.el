;;

;; Do not pause on redisplay
(setq redisplay-dont-pause t)
;; Do not show startup message
(setq inhibit-startup-message t)
;; Show keystrokes in minibuffer early
(setq echo-keystrokes 0.1)

;; Set default browser
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "google-chrome")
(setq browse-url-generic-args (list "--incognito"))

;; Indent with spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Do not blink cursor
(blink-cursor-mode 1)

