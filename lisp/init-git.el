;;; init-git --- Configure git the way I like
;;; Commentary:
;;; Heavily inspired by https://github.com/purcell/emacs.d/blob/master/lisp/init-git.el
;;; Code:

(if (version< "24.4" emacs-version)
    (progn (use-package magit
             :ensure
             :bind ("C-c g" . magit-status)
             :commands magit-status
             :config
             (fullframe magit-status magit-mode-quit-window)
             (setq magit-completing-read-function 'magit-ido-completing-read
                   magit-diff-refine-hunk t
                   magit-process-popup-time 10
                   magit-save-repository-buffers nil)

             ;; Re-center when moving forward in magit-diffs. This way
             ;; we always can see as much of the diff as possible.
             (defadvice magit-section-forward (after magit-section-forward-before-advice activate)
               (recenter-top-bottom 0))

             (use-package magit-svn
               :ensure))

      (use-package gist
        :ensure)))

(use-package git-blame
  :ensure)
;; (use-package git-commit-mode
;;   :ensure)
;; (use-package git-rebase-mode
;;   :ensure)

(use-package gitignore-mode
  :ensure)
(use-package gitconfig-mode
  :ensure)
(use-package git-messenger ;; Though see also vc-annotate's "n" & "p" bindings
  :ensure)
(use-package git-timemachine
  :commands git-timemachine
  :ensure)

;; (after-load 'magit
;;   (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-goto-parent-section))

;; (after-load 'magit
;;   (fullframe magit-status magit-mode-quit-window))


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

(provide 'init-git)
;;; init-git.el ends here
