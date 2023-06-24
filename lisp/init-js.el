;;; init-js --- set it up
;;; Commentary:
;;; Partially adapted from: http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;;; Code:

;; TODO: Verify eslint is available!
;; TODO: Check to see if https://gist.github.com/deviantfero/45b9354b433f44450de51c827f63cc68 is insightful

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
         ))

(use-package prettier-js
  :ensure
  :config
  (eval-after-load 'typescript-mode
    '(progn
       (add-hook 'typescript-mode-hook #'add-node-modules-path)
       (add-hook 'typescript-mode-hook #'prettier-js-mode)))
  )

(provide 'init-js)
;;; init-js.el ends here
