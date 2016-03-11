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

(setq save-abbrevs 'silently
      abbrev-file-name (concat emacs-etc "abbrev_defs"))
(setq-default abbrev-mode t)


;; http://endlessparentheses.com/improving-emacs-file-name-completion.html
;; Ignore various file extensions when completing file/buffer names
;; (setq read-file-name-completion-ignore-case t) ;; seems to be default
(setq read-buffer-completion-ignore-case t)
(mapc (lambda (x)
        (add-to-list 'completion-ignored-extensions x))
      '(".aux" ".exe" ".log" ".meta" ".out" ".pdf" "-pkg.el" "-autoloads.el" "auto/"))


;;; This section from
;;; http://www.wilfred.me.uk/.emacs.d/init.html#orgheadline19
;;; BSD licensed http://www.wilfred.me.uk/.emacs.d/init.html#orgheadline1
;;; Copyright (c) 2012-2013, Wilfred Hughes
(defun beginning-of-line-dwim ()
  "Toggles between moving point to the first non-whitespace character, and
the start of the line."
  (interactive)
  (let ((start-position (point)))
    ;; Move to the first non-whitespace character.
    (back-to-indentation)

    ;; If we haven't moved position, go to start of the line.
    (when (= (point) start-position)
      (move-beginning-of-line nil))))

(global-set-key (kbd "C-a") 'beginning-of-line-dwim)
;;; End BSD section



(provide 'misc)
;;; misc.el ends here
