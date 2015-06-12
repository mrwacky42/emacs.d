;;; init-osx --- Configure OSX specific customizations
;;; Commentary:
;;; Flycheck is annoying

;;; Code:

(when *is-a-mac*

  ;; Mouse scrolling goes crazy in OSX
  ;; See also:
  ;; http://stackoverflow.com/a/25438277
  ;; https://github.com/purcell/emacs.d/commit/99128a7c801e3c938b9d029275b607bb12d0a2ea#diff-2fc9ca58848b1b2a948d59fb006501fa
  (mouse-wheel-mode -1)
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left" "up" "down"))
      (global-set-key (kbd (concat "<" multiple "wheel-" direction ">")) 'ignore))))

(provide 'init-osx)
;;; init-osx ends here
