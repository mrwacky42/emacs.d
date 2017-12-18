;; use aspell instead of ispell
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(when (executable-find ispell-program-name)
  (use-package flyspell
    :init
    (use-package ispell
      ;; consider putting these in a launcher map instead.
      :bind (("C-c i c" . ispell-comments-and-strings)
             ("C-c i d" . ispell-change-dictionary)
             ("C-c i k" . ispell-kill-ispell)
             ("C-c i m" . ispell-message)
             ("C-c i r" . ispell-region)))
    :config
    ;; Disable flyspell-auto-correct-word
    (unbind-key "C-." flyspell-mode-map)
    (add-hook 'text-mode-hook 'turn-on-flyspell)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)))



;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
(define-key ctl-x-map "\C-i"
  #'endless/ispell-word-then-abbrev)

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (and (setq bef (thing-at-point 'word))
                  (not (ispell-word nil 'quiet))
                  (not (bobp)))
        (backward-word))
      (setq aft (thing-at-point 'word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(setq save-abbrevs 'silently
      abbrev-file-name (concat emacs-etc "abbrev_defs"))
(setq-default abbrev-mode t)

(provide 'init-spelling)
;;; init-spelling.el ends here
