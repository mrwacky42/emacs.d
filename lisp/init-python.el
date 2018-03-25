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

  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt")))

(use-package jinja2-mode
  :ensure)

(use-package electric-operator
  :ensure
  :config
  (add-hook 'python-mode-hook #'electric-operator-mode))
(provide 'init-python)
;;; init-python.el ends here
