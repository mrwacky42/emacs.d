;;; init-misc --- do misc stuff
;;; Commentary:


;;; Code:

(setq visible-bell nil)
(setq ring-bell-function
      (lambda
        ()
        "Inspired by https://www.emacswiki.org/emacs/AlarmBell"
        (unless (memq this-command
	              '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
          (invert-face 'mode-line)
          (run-with-timer 0.2 nil 'invert-face 'mode-line))))

;; Show keystrokes in minibuffer early
(setq echo-keystrokes 0.1)

;; Set default browser
;; On Mac, in case default isn't what we want:
;; (setq browse-url-generic-program (expand-file-name "~/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))
(defun wacky/browse-url-default-macosx-browser (url &optional _new-window)
  "Invoke the MacOS X system's default Web browser, incognito.
The optional NEW-WINDOW argument is not used.  It had better be Chrome or Chromium"
  (interactive (browse-url-interactive-arg "URL: "))
  (start-process (concat "open " url " --incognito") nil "open" url))

(cond (*is-a-mac* (setq browse-url-browser-function 'wacky/browse-url-default-macosx-browser
                        browse-url-generic-args (list "--incognito")))
      (t (progn (setq browse-url-browser-function 'browse-url-firefox)
                (setq browse-url-firefox-arguments (list "--private-window")))))


;; Indent with spaces instead of tabs
;(setq-default indent-tabs-mode nil)


;; Avoid GC when in minibuffer
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/

;; (defun my-minibuffer-setup-hook ()
;;   (setq gc-cons-threshold most-positive-fixnum))

;; (defun my-minibuffer-exit-hook ()
;;   (setq gc-cons-threshold my-gc-cons-threshold))

;; Disabled 2016-05-09
;; (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
;; (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)



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


(global-set-key (kbd "M-SPC") 'cycle-spacing)


(provide 'init-misc)
;;; misc.el ends here
