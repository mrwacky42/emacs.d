;; TODO consider delete-trailing-whitespace only for danger modes, or
;; blacklist for each operation or something
(defun iwb ()
  "Indent and cleanup whitespace on buffer for major-modes where it is meaningful"
  (interactive)
  (let ((danger-modes '(yaml-mode makefile-mode)))
    (if (not (memq major-mode danger-modes))
        (progn
          (untabify (point-min) (point-max))
          (indent-region (point-min) (point-max))
          (delete-trailing-whitespace))
      (progn
        (delete-trailing-whitespace)
        (message "%s is dangerous!" major-mode)))))

(defalias 'dtw 'delete-trailing-whitespace)


;; re-builder functions from http://www.emacswiki.org/emacs/ReBuilder
;; reb-query-replace modified to push point to beginning of buffer.
(defun reb-query-replace (to-string)
  "Replace current RE from point with `query-replace-regexp'."
  (interactive
   (progn (barf-if-buffer-read-only)
          (list (query-replace-read-to (reb-target-binding reb-regexp)
                                       "Query replace"  t))))
  (with-current-buffer reb-target-buffer
    (goto-char (point-min))
    (query-replace-regexp (reb-target-binding reb-regexp) to-string)))

(defun reb-beginning-of-buffer ()
  "In re-builder, move target buffer point position back to beginning."
  (interactive)
  (set-window-point (get-buffer-window reb-target-buffer)
                    (with-current-buffer reb-target-buffer (point-min))))

(defun reb-end-of-buffer ()
  "In re-builder, move target buffer point position back to beginning."
  (interactive)
  (set-window-point (get-buffer-window reb-target-buffer)
                    (with-current-buffer reb-target-buffer (point-max))))



(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; http://whattheemacsd.com/key-bindings.el-01.html
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defun wacky-darken ()
  "Load dark theme, since I have odd reasons for not doing this at startup"
  (interactive)
  (require 'color-theme-solarized)
  (color-theme-solarized-dark) )

(defun wacky-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(_?ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\|XXX\\)"
          1 font-lock-warning-face t))))

(defun wacky/other-window ()
  "Unceremoniously switch to the other window."
  (other-window 1))

(defun maybe-suspend-frame ()
  "Only suspend in TTY mode"
  (interactive)
  (if (window-system)
      (message "Use C-x C-z to suspend-frame")
    (suspend-frame)))

;; http://stackoverflow.com/a/358740
;; Don't kill *scratch*
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

(provide 'wacky-defuns)
