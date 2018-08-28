;; use aspell instead of ispell
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(when (executable-find ispell-program-name)
  (use-package flyspell
      :custom
  (flyspell-abbrev-p t)
  (flyspell-use-global-abbrev-table-p t)
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
    :init
    (use-package ispell
      :custom (ispell-silently-savep t)
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

(defun endless/simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (endless/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (endless/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

(provide 'init-spelling)
;;; init-spelling.el ends here
