;;; init-python --- set it up
;;; Commentary:
;;; Code:


(use-package elpy
  :ensure
  :init (with-eval-after-load 'python (elpy-enable))
  :config
  ;; Disable highlight columns and flymake (since we have flycheck)
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (delete 'elpy-module-flymake elpy-modules)

  (when (executable-find "ipython") (elpy-use-ipython)))

(use-package jinja2-mode
  :ensure)

(provide 'init-python)
;;; init-python.el ends here
