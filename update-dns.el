;;; update-dns.el --- Update serial number in DNS files

;; Copyright (C) 1994, 2000 Roland McGrath
;; Copyright (C) 1999, 2000 Noah Friedman

;; Author: Roland McGrath <roland@frob.com>
;; Maintainer: Noah Friedman <friedman@splode.com>
;; Keywords: extensions

;; $Id: update-dns.el,v 1.3 2000/02/11 00:51:00 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Every time you change the data in a DNS file, no matter how trivial the
;; change, make sure you increase the serial number.
;; Serial field is should be YYYYMMDDNN where NN is the update within a day.
;; This program depends on a comment of the form "Serial (YYYYMMDDNN)", which
;; should be structured as in this example:
;;
;;        @ IN SOA ns.foo.com. hostmaster.foo.com. (
;;                 1994102100 ; Serial (YYYYMMDDNN)
;;                 3600       ; Refresh
;;                 300        ; Retry
;;                 3600000    ; Expire
;;                 3600 )     ; Minumum
;;          IN NS ns.foo.com.
;;          IN NS secondary-ns.elsewhere.com.

;; Put these local variables at the bottom of DNS files.
;;
;; local variables:
;; eval: (if (fboundp 'update-dns-serial) (add-hook (make-local-variable 'write-file-hooks) 'update-dns-serial))
;; End:

;;; Code:

(provide 'update-dns)

(defvar update-dns-serial-fts-format
  '(("YYYYMMDDNN" "%Y%m%d"  2)
    ("YYMMDDNN"   "%y%m%d"  2)
    ;; This was a typo that one of the authors introduced into his serial
    ;; number at one point.  Don't use this.
    ("2YYYYMMDDN" "2%Y%m%d" 1))
  "*Format strings for `format-time-string'")

(defun update-dns-serial ()
  (interactive)
  (if (or (interactive-p)
          (y-or-n-p "Update DNS serial number? "))
      (save-excursion
        (save-restriction
          (save-match-data
            (widen)
            (goto-char (point-min))
            (cond ((let ((case-fold-search t))
                     (re-search-forward
                      "^\\s-*\\([0-9]+\\)\\s-*;\\s-*serial[^(]*(\\([^)]*\\))"
                      nil t))
                   (let* ((fmt (or (assoc (match-string 2)
                                          update-dns-serial-fts-format)
                                   (error "unknown serial number format")))
                          (today (format-time-string (nth 1 fmt)))
                          (iwidth (nth 2 fmt))
                          (prev (match-string 1))
                          (old-day (substring prev 0 (- iwidth)))
                          (oincr (string-to-int (substring prev (- iwidth))))
                          (incr (format (format "%%0%dd" iwidth)
                                        (if (string= old-day today)
                                            (1+ oincr)
                                          0))))
                     (goto-char (match-beginning 1))
                     (delete-region (match-beginning 1) (match-end 1))
                     (insert today incr))))))))
  ;; Always return nil in case we are put on write-file-hooks.
  nil)


;; The preceding form feed prevents emacs from evaluating the local
;; variables example in the commentary.

;;; update-dns.el ends here
