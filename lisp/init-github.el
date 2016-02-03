(require 'init-git)

(require-package 'yagist)
(require-package 'github-browse-file)
(require-package 'bug-reference-github)
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

(when (eval-when-compile (> emacs-major-version 23))
  (require-package 'github-clone)
  (require-package 'magit-gh-pulls))

(use-package gist
  :ensure
  :config
  ;; When I fetch a gist, from gist-list I never seem to want to edit it.
  ;; This moves the point right back to the gist list buffer.
  (advice-add 'gist-fetch-current :after #'wacky/other-window))

(provide 'init-github)
