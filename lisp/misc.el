;;; misc --- do misc stuff
;;; Commentary:


;;; Code:

;; Do not pause on redisplay, obsolete since 24.5.
(if (version< emacs-version "24.5")
    (setq redisplay-dont-pause t))

;; Show keystrokes in minibuffer early
(setq echo-keystrokes 0.1)

;; Set default browser
;; On Mac, in case default isn't what we want:
;; (setq browse-url-generic-program (expand-file-name "~/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))
(cond (*is-a-mac* (setq browse-url-browser-function 'browse-url-default-macosx-browser))
      (t (progn (setq browse-url-browser-function 'browse-url-generic)
                (setq browse-url-generic-program "chromium-browser"))))
(setq browse-url-generic-args (list "--incognito"))

;; Indent with spaces instead of tabs
;(setq-default indent-tabs-mode nil)


;; Avoid GC when in minibuffer
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq default-gc-cons-threshold gc-cons-threshold)

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold default-gc-cons-threshold))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)


;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
(define-key ctl-x-map "\C-i"
  #'endless/ispell-word-then-abbrev)

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (and (setq bef (thing-at-point 'word))
                  (not (ispell-word nil 'quiet))
                  (not (bobp)))
        (backward-word))
      (setq aft (thing-at-point 'word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)


(provide 'misc)
;;; misc.el ends here
