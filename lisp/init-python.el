(use-package elpy
  :ensure
  :config
  (progn
    ;; Disable highlight columns
    (delete 'elpy-module-highlight-indentation elpy-modules)
    (elpy-enable)
    (elpy-use-ipython)
    ))

(provide 'init-python)
