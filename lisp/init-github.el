(require 'init-git)

(use-package bug-reference-github
  :ensure
  :config
  (add-hook 'prog-mode-hook 'bug-reference-github-set-url-format)
  (add-hook 'prog-mode-hook 'bug-reference-prog-mode))

(use-package github-browse-file
  :ensure)

(use-package github-clone
  :ensure)

(use-package magithub
  :ensure
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/src"))

(use-package igist
  :ensure
  :config
  ;; Go populate ~/.authinfo.gpg as per igist docs: https://github.com/KarimAziev/igist#secure-way-using-auth-sources
  (setq igist-auth-marker 'igist
        igist-current-user-name "mrwacky42")
  :bind (("M-o" . igist-dispatch)))

(provide 'init-github)
