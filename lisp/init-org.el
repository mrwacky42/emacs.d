(use-package org
  :defer
  :config
  (use-package org-habit)
  (setq org-agenda-files (list "~/info/orgfiles") ; default "~/.agenda_files"
        org-agenda-start-on-weekday nil
        org-default-notes-file (concat org-directory "/notes.org")
        org-directory "~/info/orgfiles")
  (add-to-list 'org-modules 'org-habit)
  :bind
  ("C-c a" . org-agenda)
  ("C-c l" . org-store-link)
  ("C-c c" . org-capture))

(provide 'init-org)
;;; init-org.el ends here
