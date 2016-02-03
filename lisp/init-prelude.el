;;; init-prelude --- Init code from Prelude
;;; Commentary:
;;; Bits of code I have borrowed from Prelude
;;; https://github.com/bbatsov/prelude
;;; License: GPLv3


;;; Code:
(require 'tramp)

(defun prelude-file-owner-uid (filename)
  "Return the UID of the FILENAME as an integer.

See `file-attributes' for more info."
  (nth 2 (file-attributes filename 'integer)))

(defun prelude-file-owned-by-user-p (filename)
  "Return t if file FILENAME is owned by the currently logged in user."
  (equal (prelude-file-owner-uid filename)
         (user-uid)))

(defun prelude-find-alternate-file-as-root (filename)
  "Wraps `find-alternate-file' with opening a FILENAME as root."
  (find-alternate-file (concat "/sudo:root@localhost:" filename)))

(defun prelude-reopen-as-root ()
  "Find file as root if necessary."
  (unless (or (tramp-tramp-file-p buffer-file-name)
              (equal major-mode 'dired-mode)
              (not (file-exists-p (file-name-directory buffer-file-name)))
              (file-writable-p buffer-file-name)
              (prelude-file-owned-by-user-p buffer-file-name))
    (prelude-find-alternate-file-as-root buffer-file-name)))

;; I originally advised ido-find-file with code from
;; http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
;; But it is incomplete, and this version from Prelude is better.
(add-hook 'find-file-hook 'prelude-reopen-as-root)

(provide 'init-prelude)
;;; init-prelude.el ends here
