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

;; probably change this to a list of javascript modes (js-mode, js2-mode, others?)
(add-hook 'js-mode-hook
            (lambda ()
              (push '("function" . ?λ) prettify-symbols-alist)))

(use-package typescript-mode
  :ensure)

(use-package tide
  :ensure
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(provide 'init-js)
;;; init-js.el ends here
