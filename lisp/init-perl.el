
(require 'cperl-mode)
(defalias 'perl-mode 'cperl-mode)      ; always use cperl-mode

(custom-set-variables
 '(cperl-close-paren-offset -4)
 '(cperl-continued-statement-offset 4)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t))

(define-key cperl-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key cperl-mode-map (kbd "C-M-h") 'backward-kill-word)
(global-set-key (kbd "C-h P") 'cperl-perldoc)

(add-to-list 'auto-mode-alist '("\\.p[lm]$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pod$" . pod-mode))
(add-to-list 'auto-mode-alist '("\\.tt$" . tt-mode))
(add-to-list 'auto-mode-alist '("\\.t\\'" . cperl-mode))

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
       (let ((where_i_was (point)))
         (save-excursion (mark-whole-buffer)
                         (perltidy-region))
         (goto-char where_i_was)))

;;http://stackoverflow.com/questions/318553/getting-emacs-to-untabify-when-saving-certain-file-types-and-only-those-file-typ
(add-hook 'cperl-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'perltidy-buffer nil t)))

(provide 'init-perl)
