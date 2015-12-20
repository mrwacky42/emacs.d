(use-package paren-face
  :ensure
  :config
  (global-paren-face-mode))

(use-package paredit
  :ensure
  :commands paredit-mode
  :diminish paredit-mode
  :config
  (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp)
  (dolist (mode paren-face-modes)
    (add-hook (intern (concat (symbol-name mode) "-hook")) 'paredit-mode))
  )

(use-package elisp-slime-nav
  :ensure
  :diminish elisp-slime-nav-mode
  :config (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))

(provide 'init-lisp)
