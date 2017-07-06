(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t)
  (use-package flycheck-rust
    :after flycheck
    :commands flycheck-rust-setup
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package racer
  :commands racer-mode
  :diminish racer-mode
  :init
  (add-hook 'rust-mode-hook 'racer-mode)
  :bind (:map rust-mode-map
	 ("M-." . racer-find-definition))
  :config
  (racer-turn-on-eldoc)
  (use-package company-racer
    ;; :config
    ;; (add-to-list 'company-backends 'company-racer)
    ;; (setq company-tooltip-align-annotations t)
    :bind (:map rust-mode-map
		("M-." . racer-find-definition)))
  )

(use-package cargo
  :commands cargo-minor-mode
  :diminish cargo-minor-mode
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode)))

(provide 'init-rust)
