;;; init-helm --- Configure helm
;;; Commentary:
;;; I haven't tried this yet, for future reference, courtesy of slackorama
;;; Code:
(use-package helm
  :bind (("C-c h" . helm-mini)
         ("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x))
  :init
  (progn
    (require 'helm-config)
    (require 'helm-ack)
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z
    (setq helm-input-idle-delay 0.1
          helm-candidate-number-limit 100
          helm-ack-use-ack-grep t)
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))
    (helm-mode 1))
  :config
  (progn
    (set-face-background 'helm-selection "#073642")
    (set-face-background 'helm-selection "#073642")))

(provide 'init-helm)
;;; init-git.el ends here
