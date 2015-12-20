(use-package org
  :defer
  :config
  (use-package org-habit)
  (setq org-agenda-files (list "~/info/orgfiles") ; default "~/.agenda_files"
        org-agenda-start-on-weekday nil
        org-default-notes-file (concat org-directory "/notes.org")
        org-directory "~/info/orgfiles"
        org-completion-use-ido t)
  (add-to-list 'org-modules 'org-habit)
  :bind
  ("C-c a" . org-agenda)
  ("C-c l" . org-store-link)
  ("C-c c" . org-capture))

;; Attempts at prettier timestamps
;; (setq org-time-stamp-custom-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))
;; (setq org-display-custom-times t)

;; Trying advice from here; didn't get desired results..
;; https://emacs.stackexchange.com/questions/477/how-do-i-automatically-save-org-mode-buffers

(provide 'init-org)
;;; init-org.el ends here
