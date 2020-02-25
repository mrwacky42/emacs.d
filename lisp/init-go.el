;;; init-go --- set it up
;;; Commentary:
;;; Code:

(use-package go-mode
  :ensure
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook 'wacky-tab-hook)
  (add-hook 'go-mode-hook 'lsp-deferred)
  (use-package go-eldoc
    :ensure
    :config
    (add-hook 'go-mode-hook 'go-eldoc-setup)))

(provide 'init-go)
;;; init-go.el ends here
