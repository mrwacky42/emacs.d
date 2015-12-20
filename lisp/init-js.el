;;; init-js --- set it up
;;; Commentary:
;;; Partially adapted from: http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;;; Code:

;; TODO: Verify eslint is available!

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; probably change this to a list of javascript modes (js-mode, js2-mode, others?)
(add-hook 'js-mode-hook
            (lambda ()
              (push '("function" . ?λ) prettify-symbols-alist)))

(provide 'init-js)
;;; init-js.el ends here
