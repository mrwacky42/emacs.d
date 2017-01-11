(defun wg/kludge-gpg-agent ()
  "Pinentry where I am.
From https://www.emacswiki.org/emacs/EasyPG#toc5"
  (if (display-graphic-p)
      (setenv "DISPLAY" (terminal-name))
    (setenv "GPG_TTY" (terminal-name))
    (setenv "DISPLAY")))

(provide 'init-gpg)
;;; init-gpg.el ends here
