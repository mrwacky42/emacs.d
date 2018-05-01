
(define-prefix-command 'launcher-map)
;; C-x l is `count-lines-page' by default. If you
;; use that, you can try s-l or <C-return>.
(define-key ctl-x-map "l" 'launcher-map)
;(global-set-key (kbd "s-l") 'launcher-map)
(define-key launcher-map "c" #'calc)
(define-key launcher-map "d" #'ediff-buffers)
(define-key launcher-map "f" #'find-dired)
(define-key launcher-map "g" #'lgrep)
(define-key launcher-map "G" #'rgrep)
(define-key launcher-map "h" #'man) ; Help
;; (define-key launcher-map "i" #'package-install-from-buffer)
;; (define-key launcher-map "n" #'nethack)
(define-key launcher-map "p" #'paradox-list-packages)
(define-key launcher-map "s" #'shell)
(define-key launcher-map "t" #'proced) ; top


(defmacro browse (url)
  "Return a function that calls `browse-url' on URL."
  (let ((func-name (intern (concat "endless/browse-" url))))
    `(progn
       (defun ,func-name ()
         ,(format "Browse to the url %s." url)
         (interactive)
         (browse-url ,url))
       ',func-name)))

(define-key launcher-map "r" (browse "https://www.reddit.com/r/emacs/"))
(define-key launcher-map "w" (browse "http://www.emacswiki.org/"))
(define-key launcher-map "?" (browse "http://emacs.stackexchange.com/"))
(define-key launcher-map "+" (browse "https://plus.google.com/communities/114815898697665598016"))
