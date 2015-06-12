;;; init-git --- Configure git the way I like
;;; Commentary:
;;; Heavily inspired by https://github.com/purcell/emacs.d/blob/master/lisp/init-git.el
;;; Code:
(use-package magit
  :ensure
  :bind ("C-c g" . magit-status)
  :config (progn
            (fullframe magit-status magit-mode-quit-window)
            (setq magit-last-seen-setup-instructions "1.4.0"
                  magit-auto-revert-mode nil)))

(use-package git-blame
  :ensure)
(use-package git-commit-mode
  :ensure)
(use-package git-rebase-mode
  :ensure)
(use-package gitignore-mode
  :ensure)
(use-package gitconfig-mode
  :ensure)
(use-package git-messenger ;; Though see also vc-annotate's "n" & "p" bindings
  :ensure)
(use-package git-timemachine
  :ensure)

(setq-default
 magit-save-some-buffers nil
 magit-process-popup-time 10
 magit-diff-refine-hunk t
 magit-completing-read-function 'magit-ido-completing-read)

;; Hint: customize `magit-repo-dirs' so that you can use C-u M-F12 to
;; quickly open magit on any one of your projects.
(global-set-key [(meta f12)] 'magit-status)

;; (after-load 'magit
;;   (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-goto-parent-section))

(use-package fullframe
  :ensure)

;; (after-load 'magit
;;   (fullframe magit-status magit-mode-quit-window))

(add-hook 'git-commit-mode-hook 'goto-address-mode)


;;; When we start working on git-backed files, use git-wip if available

;; (after-load 'magit
;;   (global-magit-wip-save-mode)
;;   (diminish 'magit-wip-save-mode))

;; (after-load 'magit
;;   (diminish 'magit-auto-revert-mode))


;; (when *is-a-mac*
;;   (after-load 'magit
;;     (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))



;; Convenient binding for vc-git-grep
(global-set-key (kbd "C-x v f") 'vc-git-grep)



;;; git-svn support
(use-package magit-svn
  :ensure)

;; (autoload 'magit-svn-enabled "magit-svn")
;; (defun sanityinc/maybe-enable-magit-svn-mode ()
;;   (when (magit-svn-enabled)
;;     (magit-svn-mode)))
;; (add-hook 'magit-status-mode-hook #'sanityinc/maybe-enable-magit-svn-mode)

;; (after-load 'compile
;;   (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
;;                       '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
;;     (add-to-list 'compilation-error-regexp-alist-alist defn)
;;     (add-to-list 'compilation-error-regexp-alist (car defn))))

;; (defvar git-svn--available-commands nil "Cached list of git svn subcommands")

;; (defun git-svn (dir)
;;   "Run a git svn subcommand in DIR."
;;   (interactive "DSelect directory: ")
;;   (unless git-svn--available-commands
;;     (setq git-svn--available-commands
;;           (sanityinc/string-all-matches
;;            "^  \\([a-z\\-]+\\) +"
;;            (shell-command-to-string "git svn help") 1)))
;;   (let* ((default-directory (vc-git-root dir))
;;          (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
;;     (compile (concat "git svn "
;;                      (ido-completing-read "git-svn command: " git-svn--available-commands nil t)))))


;; (use-package git-messenger
;;   :ensure
;;   :bind "C-x v p" . #'git-messenger:popup-message)
;; ;(global-set-key (kbd "C-x v p") )

(use-package gist
  :ensure)

(provide 'init-git)
;;; init-git.el ends here
