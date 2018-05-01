(require 'init-git)

(use-package yagist
  :ensure)

(use-package github-browse-file
  :ensure)

(use-package bug-reference-github
  :ensure
  :config
  (add-hook 'prog-mode-hook 'bug-reference-github-set-url-format)
  (add-hook 'prog-mode-hook 'bug-reference-prog-mode))

(use-package github-clone
  :ensure)

(use-package gist
  :ensure
  :config
  ;; When I fetch a gist, from gist-list I never seem to want to edit it.
  ;; This moves the point right back to the gist list buffer.
  (advice-add 'gist-fetch-current :after #'wacky/other-window))

(use-package magithub
  :ensure
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/src"))

(provide 'init-github)
