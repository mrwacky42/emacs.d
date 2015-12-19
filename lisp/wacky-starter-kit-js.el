;;; GPLv3 Yo!
;;; https://github.com/eschulte/emacs24-starter-kit/blob/master/COPYING

(font-lock-add-keywords
 'espresso-mode `(("\\(function *\\)("
                   (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                             "ƒ")
                             nil)))))

(font-lock-add-keywords 'espresso-mode
                        '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
                           1 font-lock-warning-face t)))

(autoload 'espresso-mode "espresso" "Start espresso-mode" t)

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))
(add-hook 'espresso-mode-hook 'moz-minor-mode)
(add-hook 'espresso-mode-hook 'turn-on-paredit)
(add-hook 'espresso-mode-hook 'run-starter-kit-coding-hook)
;; (add-hook 'espresso-mode-hook 'idle-highlight)

;; (use-package flymake-jshint
;;   :ensure)

(add-hook 'js-mode-hook 'flycheck-mode)

(setq espresso-indent-level 2)

(use-package coffee-mode
  :ensure)

(provide 'wacky-starter-kit-js)
