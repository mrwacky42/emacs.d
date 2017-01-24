;;; init-org --- Configure org-mode my way
;;
;;; Commentary:
;;
;; org-capture from browsers is provided by emacs-capture in ~/bin
;; This site recommends how to set up for Chrome to use xdg-open:
;; http://www.diegoberrocal.com/blog/2015/08/19/org-protocol/
;; However, it skips the part about registering the MIME handler with xdg, but that is covered in superuser comments here:
;; https://superuser.com/questions/162092/how-can-i-register-a-custom-protocol-with-xdg/309343#comment1336968_309343
;; In short: xdg-mime default emacsclient.desktop x-scheme-handler/org-protocol
;; Similarly, for Firefox, we can set the path to emacsclient to be our new `emacs-capture` tool.
;; http://chadok.info/firefox-org-capture/
;; For Chrome, we're now using https://github.com/sprig/org-capture-extension which has good instructions as well.
;; See also:
;; http://orgmode.org/worg/org-contrib/org-protocol.html
;; http://tech.memoryimprintstudio.com/org-capture-from-external-applications/
;;; Code:

(use-package org
  :defer
  :config
  (use-package org-bullets)
  (use-package org-habit)
  (use-package org-protocol)

  (setq org-directory "~/info/orgfiles/")
  (setq
   org-agenda-files (list org-directory) ; default "~/.agenda_files"
   org-agenda-start-on-weekday nil
   org-completion-use-ido t
   org-default-notes-file (concat org-directory "notes.org")
   org-hide-emphasis-markers t
   org-log-done (quote time)
   org-log-into-drawer t
   org-special-ctrl-a/e t
   org-special-ctrl-k t
   org-src-fontify-natively t
   ;; org-src-tab-acts-natively t ;; Not quite right https://github.com/joaotavora/yasnippet/pull/760
   org-yank-adjusted-subtrees t

   org-capture-templates (quote (("t" "todo" entry (file+headline org-default-notes-file "Captured")
                                  "* TODO %?\n%U\n%A\n" :clock-in t :clock-resume t :empty-line t)
                                 ("r" "respond" entry (file org-default-notes-file)
                                  "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                                 ("n" "note" entry (file org-default-notes-file)
                                  "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                                 ("j" "Journal" entry (file+datetree (concat org-directory "diary.org"))
                                  "* %?\n%U\n" :clock-in t :clock-resume t)
                                 ("L" "org-capture" entry (file+headline
                                                           org-default-notes-file
                                                           "Captured from web for review")
                                  "* TODO %c\n%U\n" :immediate-finish t)
                                 ("p" "org-capture-with-text" entry (file+headline
                                                                     org-default-notes-file
                                                                     "Captured from web for review")
                                  "* TODO %^{Title}\nSource: %c\n%U\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n")
                                 ("m" "Meeting" entry (file org-default-notes-file)
                                  "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                                 ("h" "Habit" entry (file org-default-notes-file)
                                  "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))

   ;; org-capture Firefox plugin will not work if this is set.
   ;; I have been unable to figure out a context that works with the
   ;; plugin, but not interactively
   ;; org-capture-templates-contexts (quote (("l" ((in-mode . "non-existent-mode")))))

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
  ;; Silliness from https://github.com/howardabrams/dot-files/blob/master/emacs-org.org
  (font-lock-add-keywords
   'org-mode `(("^\\*+ \\(TODO\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "⚑")
                          nil)))
               ("^\\*+ \\(DOING\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "⚐")
                          nil)))
               ("^\\*+ \\(CANCELED\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "✘")
                          nil)))
               ("^\\*+ \\(DONE\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "✔"))))))
  (add-to-list 'org-modules 'org-habit)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

  ;; After #760 lands in yasnippet, we can try this
  ;; (add-hook 'org-mode-hook
  ;;           (lambda () (setq-local yas-buffer-local-condition '(not (org-in-src-block-p t)))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (sh . t)))

  (defadvice org-capture
      (after make-full-window-frame activate)
    "Advise capture to be the only window when used as a popup"
    (if (equal "emacs-capture" (frame-parameter nil 'name))
        (delete-other-windows)))

  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (if (equal "emacs-capture" (frame-parameter nil 'name))
        (delete-frame)))
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
