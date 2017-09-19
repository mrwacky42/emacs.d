;;; init-go --- set it up
;;; Commentary:
;;; Code:

(use-package go-mode
  :ensure
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook 'wacky-tab-hook))

(provide 'init-go)
;;; init-go.el ends here
