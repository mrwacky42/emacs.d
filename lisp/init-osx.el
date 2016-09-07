;;; init-osx --- Configure OSX specific customizations
;;; Commentary:
;;; Flycheck is annoying

;;; Code:


;; Mouse scrolling goes crazy in OSX
;; See also:
;; http://stackoverflow.com/a/25438277
;; https://github.com/purcell/emacs.d/commit/99128a7c801e3c938b9d029275b607bb12d0a2ea#diff-2fc9ca58848b1b2a948d59fb006501fa
(mouse-wheel-mode -1)
(dolist (multiple '("" "double-" "triple-"))
  (dolist (direction '("right" "left" "up" "down"))
    (global-set-key (kbd (concat "<" multiple "wheel-" direction ">")) 'ignore)))

;; vbell is broken in HomeBrew Emacs 24.x
(when (version< emacs-version "25")
  (setq visible-bell nil)
  (setq ring-bell-function
        (lambda
          ()
          (invert-face 'mode-line)
          (run-with-timer 0.1 nil 'invert-face 'mode-line))))

;; From: http://emacsredux.com/blog/2015/05/09/emacs-on-os-x/
;; Although he does not say what the problem is.
;; I am not a dired power user.
(let ((gls (executable-find "gls")))
  (if gls
      (setq insert-directory-program gls)
    (message "gls is not found, 'brew install coreutils'")))

(add-to-list 'ido-ignore-files "\\.DS_Store")

(provide 'init-osx)
;;; init-osx ends here
