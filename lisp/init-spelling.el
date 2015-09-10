;; use aspell instead of ispell
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(when (executable-find ispell-program-name)
  (use-package flyspell
    :init
    (use-package ispell
      ;; consider putting these in a launcher map instead.
      :bind (("C-c i c" . ispell-comments-and-strings)
             ("C-c i d" . ispell-change-dictionary)
             ("C-c i k" . ispell-kill-ispell)
             ("C-c i m" . ispell-message)
             ("C-c i r" . ispell-region)))
    :config
    ;; Disable flyspell-auto-correct-word
    (unbind-key "C-." flyspell-mode-map)
    (add-hook 'text-mode-hook 'turn-on-flyspell)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)))


(provide 'init-spelling)
;;; init-spelling.el ends here
