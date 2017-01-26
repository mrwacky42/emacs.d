
(use-package cperl-mode
  :mode ("\\.t\\'" . cperl-mode)
  :init (defalias 'perl-mode 'cperl-mode) ;; always use cperl-mode
  :config
  (custom-set-variables
   '(cperl-close-paren-offset -4)
   '(cperl-continued-statement-offset 4)
   '(cperl-indent-level 4)
   '(cperl-indent-parens-as-block t))
  ;; http://stackoverflow.com/questions/318553/getting-emacs-to-untabify-when-saving-certain-file-types-and-only-those-file-typ
  (add-hook 'cperl-mode-hook
            '(lambda () (add-hook 'before-save-hook 'perltidy-buffer nil t))))

;; Consider also factoring out the executable-find bit:
;; http://emacs-fu.blogspot.com/2008/12/running-some-external-program-only-if.html
(defun perltidy-region () "Run perltidy on the current region."
       (interactive)
       (if (executable-find "perltidy")
           (save-excursion
             (shell-command-on-region (point) (mark) "perltidy -q" nil t))
         (message "Unable to find perltidy")))

(defun perltidy-defun () "Run perltidy on the current defun."
       (interactive)
       (save-excursion (mark-defun)
                       (perltidy-region)))

(defun perltidy-buffer () "Run perltidy on current buffer."
       (interactive)
       (if (executable-find "perltidy")
           (let ((where_i_was (point)))
             (shell-command-on-region (point-min) (point-max) "perltidy -q" nil t)
             (goto-char where_i_was))
         (message "Unable to find perltidy")))




(provide 'init-perl)
