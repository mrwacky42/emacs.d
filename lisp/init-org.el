(use-package org
  :defer
  :config
  (use-package org-habit)
  (use-package org-protocol)

  (setq org-directory "~/info/orgfiles")
  (setq
   org-agenda-files (list "~/info/orgfiles") ; default "~/.agenda_files"
   org-agenda-start-on-weekday nil
   org-completion-use-ido t
   org-default-notes-file (concat org-directory "/notes.org")
   org-log-done (quote time)
   org-log-into-drawer t
   org-special-ctrl-a/e t
   org-special-ctrl-k t
   org-yank-adjusted-subtrees t

   org-capture-templates (quote (("t" "todo" entry (file "~/info/orgfiles/notes.org")
                                  "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                                 ("r" "respond" entry (file "~/info/orgfiles/notes.org")
                                  "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                                 ("n" "note" entry (file "~/info/orgfiles/notes.org")
                                  "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                                 ("j" "Journal" entry (file+datetree "~/info/orgfiles/diary.org")
                                  "* %?\n%U\n" :clock-in t :clock-resume t)
                                 ("w" "org-protocol" entry (file "~/info/orgfiles/notes.org")
                                  "* TODO Review %c\n%U\n" :immediate-finish t)
                                 ("m" "Meeting" entry (file "~/info/orgfiles/notes.org")
                                  "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                                 ("p" "Phone call" entry (file "~/info/orgfiles/notes.org")
                                  "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                                 ("h" "Habit" entry (file "~/info/orgfiles/notes.org")
                                  "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))
   org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
                                  ("NEXT" :foreground "blue" :weight bold)
                                  ("DONE" :foreground "forest green" :weight bold)
                                  ("WAITING" :foreground "orange" :weight bold)
                                  ("HOLD" :foreground "magenta" :weight bold)
                                  ("CANCELLED" :foreground "forest green" :weight bold)
                                  ("MEETING" :foreground "forest green" :weight bold)
                                  ("PHONE" :foreground "forest green" :weight bold)))
   org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                             (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
   org-todo-state-tags-triggers (quote (("CANCELLED" ("CANCELLED" . t))
                                        ("WAITING" ("WAITING" . t))
                                        ("HOLD" ("WAITING") ("HOLD" . t))
                                        (done ("WAITING") ("HOLD"))
                                        ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                                        ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                                        ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
   org-treat-S-cursor-todo-selection-as-state-change nil)

  (add-to-list 'org-modules 'org-habit)
  :bind
  ("C-c a" . org-agenda)
  ("C-c l" . org-store-link)
  ("C-c c" . org-capture)
  ("C-c b" . org-iswitchb))

;; Attempts at prettier timestamps
;; (setq org-time-stamp-custom-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))
;; (setq org-display-custom-times t)

(provide 'init-org)
;;; init-org.el ends here
