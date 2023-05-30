;;; init-git --- Configure git the way I like
;;; Commentary:
;;; Heavily inspired by https://github.com/purcell/emacs.d/blob/master/lisp/init-git.el
;;; Code:

(progn
  ;; Micro-optimization, I rarely use non-git anymore
                                        ;(setq vc-handled-backends '(Git))

  (use-package magit
    :ensure
    :bind ("C-c g" . magit-status)
    :commands magit-status
    :diminish auto-revert-mode
    :config
    (fullframe magit-status magit-mode-quit-window)
    (setq magit-completing-read-function 'magit-ido-completing-read
          magit-diff-refine-hunk t
          magit-process-popup-time 10
          magit-save-repository-buffers nil)
    (when *is-a-mac* (setq magit-git-executable "/usr/local/bin/git"))

    ;; Disable auto-fill when committing because yuk.
    (setq git-commit-setup-hook (remove 'git-commit-turn-on-auto-fill git-commit-setup-hook))

    ;; Re-center when moving forward in magit-diffs. This way
    ;; we always can see as much of the diff as possible.
    (defadvice magit-section-forward (after magit-section-forward-before-advice activate)
      (recenter-top-bottom 0))


    (use-package magit-svn
      :ensure
      :disabled))

  (use-package forge
    :ensure
    :after magit)

  (use-package gist
    :ensure))

(use-package git-modes
  :ensure
  :config
  (add-hook 'gitconfig-mode-hook
            (lambda () "Tabs are for heathens"
              (setq indent-tabs-mode nil
                    tab-width 4)))
  (add-to-list 'auto-mode-alist
	       (cons "/.dockerignore\\'" 'gitignore-mode)))

;; Though see also vc-annotate's "n" & "p" bindings
(use-package git-messenger
  :ensure
  :bind ("C-x v p" . git-messenger:popup-message))

(use-package git-link
  :ensure)

(use-package git-timemachine
  :ensure)


;; Convenient binding for vc-git-grep
(global-set-key (kbd "C-x v f") 'vc-git-grep)



;;; Attic

;; (when *is-a-mac*
;;   (after-load 'magit
;;     (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))

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

(provide 'init-git)
;;; init-git.el ends here
