(defalias 'wacky/ibuffer-fn
  (cond
   ((< emacs-major-version 26) 'ibuffer)
   (t 'ibuffer-jump)))

(use-package ibuffer
  :bind ("C-x C-b" . wacky/ibuffer-fn)
  :config
  (fullframe ibuffer ibuffer-quit)
  (use-package ibuffer-vc
    :ensure)
  (setq ibuffer-filter-group-name-face 'font-lock-doc-face
        ibuffer-show-empty-filter-groups nil
        ibuffer-saved-filter-groups (quote (("home"
                                             ("Magit" (name . "\*magit"))
                                             ("emacs-config" (filename . ".emacs.d"))
                                             ("Org" (or (mode . org-mode)
                                                        (filename . "OrgMode")))
                                             ("Web Dev" (or (mode . html-mode)
                                                            (mode . css-mode)))
                                             ("Tramp" (or (name . "\*tramp")
                                                          (filename . "\/ssh:")))
                                             ("ERC" (mode . erc-mode))
                                             ("Help" (or (name . "\*Help\*")
                                                         (name . "\*Apropos\*")
                                                         (name . "\*info\*")))
                                             ("Special" (name . "\*")))))

        ;; Modify the default ibuffer-formats (toggle with `)
        ibuffer-formats (quote ((mark modified read-only vc-status-mini " "
                                      (name 18 18 :left :elide)
                                      " "
                                      (size-h 9 -1 :right)
                                      " "
                                      (mode 16 16 :left :elide)
                                      " "
                                      filename-and-process)
                                (mark modified read-only vc-status-mini " "
                                      (name 18 18 :left :elide)
                                      " "
                                      (size-h 9 -1 :right)
                                      " "
                                      (mode 16 16 :left :elide)
                                      " "
                                      (vc-status 16 16 :left)
                                      " "
                                      filename-and-process))))

  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  (defun ibuffer-vc-add-vc-filter-groups ()
    "Add vc-ibuffer generated groups to each ibuffer-saved-filter-group."
    (dolist (group (ibuffer-vc-generate-filter-groups-by-vc-root))
      (add-to-list 'ibuffer-filter-groups group t)))

  (add-hook 'ibuffer-hook
            '(lambda ()
               (ibuffer-auto-mode t)
               (ibuffer-switch-to-saved-filter-groups "home")
               (ibuffer-vc-add-vc-filter-groups)
               (unless (eq ibuffer-sorting-mode 'filename/process)
                 (ibuffer-do-sort-by-filename/process)))))

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
