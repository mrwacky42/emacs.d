;;; init-python --- set it up
;;; Commentary:
;;; Code:


(use-package elpy
  :ensure
  :config
  (progn
    ;; Disable highlight columns
    (delete 'elpy-module-highlight-indentation elpy-modules)
    (elpy-enable)
    (when (executable-find "ipython") (elpy-use-ipython))))

(use-package jinja2-mode
  :ensure)

(provide 'init-python)
;;; init-python.el ends here
