(use-package paren-face
  :ensure
  :config
  (global-paren-face-mode))

(use-package paredit
  :ensure
  :diminish paredit-mode
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseud-structural editing of Lisp code.")
  (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp)
  (dolist (mode paren-face-modes)
    (add-hook (intern (concat (symbol-name mode) "-hook")) #'enable-paredit-mode)))

(use-package elisp-slime-nav
  :ensure
  :diminish elisp-slime-nav-mode
  :config (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))

;; Redundant since I disable flycheck for emacs-lisp-mode already.
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(emacs-lisp-checkdoc)))

(provide 'init-lisp)
