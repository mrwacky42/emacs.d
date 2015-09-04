(use-package org
  :defer
  :config
  (use-package org-habit)
  (setq org-agenda-start-on-weekday nil
        org-directory "~/info/orgfiles"
        org-default-notes-file (concat org-directory "/notes.org"))
  (add-to-list 'org-modules 'org-habit)
  :bind
  ("C-c a" . org-agenda)
  ("C-c l" . org-store-link)
  ("C-c c" . org-capture))

(provide 'init-org)
;;; init-org.el ends here
