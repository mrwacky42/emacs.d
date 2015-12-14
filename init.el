;;; init ---  Configure emacs
;;; Commentary:
;; The astute observer will notice that I've borrowed plenty from
;; https://github.com/purcell/emacs.d
;; https://github.com/rejeep/emacs
;;

;;; Code:


;; Try to avoid GC during startup
(setq gc-cons-threshold 200000000)

(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode))

(setq initial-scratch-message ";;")

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (string= (buffer-name) "*scratch*")
              (animate-string ";; Mr Wacky Heavy Industries is online!!\n" 4)
              (next-line))))

;; Strive for less clutter in user-emacs-directory
(setq emacs-etc (concat user-emacs-directory "etc/"))
(when
    (not (file-exists-p emacs-etc))
  (make-directory emacs-etc))

(random t) ;; Seed the random-number generator


;; backup options
(setq
 backup-by-copying t ;; Set backups to not hose sym/hard links
 backup-directory-alist `(("." . ,(concat emacs-etc "backups")))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

(setq auto-save-list-file-prefix (concat emacs-etc "auto-save-list")
      color-theme-is-global t
      column-number-mode t
      diff-switches "-u"
      ediff-window-setup-function 'ediff-setup-windows-plain
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      line-number-mode t
      mouse-yank-at-point t
      oddmuse-directory (concat emacs-etc "oddmuse")
      require-final-newline t
      save-interprogram-paste-before-kill t
      save-place-file (concat emacs-etc "places")
      sentence-end-double-space nil
      shift-select-mode nil
      visible-bell t
      whitespace-line-column 80
      whitespace-style '(face trailing lines-tail tabs))


;; Wrap the rest in this let to bootstrap faster perhaps
(let ((file-name-handler-alist nil))
  (load-theme 'misterioso)
  (set-cursor-color "light gray")
  (blink-cursor-mode 1)
  (setq blink-cursor-interval .4)

  ;; Bootstrappin'
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (require 'init-benchmarking) ;; Measure startup time


  (defconst *spell-check-support-enabled* t)
  (defconst *is-a-mac* (eq system-type 'darwin))

  (require 'package)
  (setq package-user-dir (expand-file-name "elpa" emacs-etc))

  
;;; Standard package repositories
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

  (add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

  
  (defun sanityinc/package-maybe-enable-signatures ()
    "Conditionally enable package signatures.
If gpg cannot be found, signature checking will fail, so we
conditionally enable it according to whether gpg is available.  We
re-run this check once $PATH has been configured"
    (setq package-check-signature (when (executable-find "gpg") 'allow-unsigned)))

  (sanityinc/package-maybe-enable-signatures)

  ;; (after-load 'init-exec-path
  ;;  (sanityinc/package-maybe-enable-signatures))
  ;;(package-refresh-contents)

  
;;; On-demand installation of packages

  (defun require-package (package &optional min-version no-refresh)
    "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
    (if (package-installed-p package min-version)
        t
      (if (or (assoc package package-archive-contents) no-refresh)
          (package-install package)
        (progn
          (package-refresh-contents)
          (require-package package min-version t)))))




  
  ;;----------------------------------------------------------------------------
  ;; Allow access from emacsclient
  ;;----------------------------------------------------------------------------
  (require 'server)
  (unless (server-running-p)
    (server-start))
  (add-hook 'server-done-hook 'delete-frame)
  (add-hook 'server-done-hook (lambda nil (kill-buffer nil)))
  ;; (add-hook 'server-switch-hook (lambda nil
  ;;                                 (let ((server-buf (current-buffer)))
  ;;                                   (bury-buffer)
  ;;                                   (switch-to-buffer-other-frame server-buf))))


  (setq custom-file
        (concat (expand-file-name user-emacs-directory) "custom.el"))
  (load custom-file :noerror)

  
;;; Fire up package.el
  (setq package-enable-at-startup nil)
  (package-initialize)
  (require-package 'use-package)
  (require 'use-package)

  
  ;; Packages
  (use-package uniquify
    :config
    (setq
     uniquify-buffer-name-style 'reverse
     uniquify-separator ":"
     uniquify-after-kill-buffer-p t      ; rename after killing dupes
     uniquify-ignore-buffers-re "^\\*"; don't muck with special buffers
     ))

  (use-package multiple-cursors
    :ensure
    :bind (("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this)))

  ;; The cool kids have moved to avy
  ;; http://emacsredux.com/blog/2015/07/19/ace-jump-mode-is-dead-long-live-avy/
  (use-package ace-jump-mode
    :ensure
    :bind ("C-c SPC" . ace-jump-mode))

  ;; Nifty, but not using.
  (use-package command-log-mode
    :ensure
    :disabled t)

  (use-package expand-region
    :ensure
    :bind ("C-=" . er/expand-region))

  (use-package paredit
    :ensure
    :commands paredit-mode
    :diminish paredit-mode)

  (use-package elisp-slime-nav
    :ensure
    :diminish elisp-slime-nav-mode)

  (use-package match-paren
    :bind ("%" . match-paren))

  (use-package fullframe
    :ensure
    :config (fullframe list-packages quit-window))

  ;; (use-package diff-hl
  ;;   :ensure
  ;;   :init (global-diff-hl-mode)
  ;;   :config (progn
  ;;             (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  ;;             (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
  ;;             (add-hook 'vc-checkin-hook 'diff-hl-update)))

  (use-package yasnippet
    :ensure
    :defer t
    :config
    (defun snippet-delete-trailing-whitespace ()
      "Delete trailing whitespace in yasnippet definitions."
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "^# --\n" nil t)
            (delete-trailing-whitespace (point-min) (point)))))

    (defun snippet-delete-trailing-whitespace-hook ()
      "Register snippet-delete-trailing-whitespace as a before-save-hook in snippet-mode"
      (make-local-variable `before-save-hook)
      (add-hook 'before-save-hook `snippet-delete-trailing-whitespace))

    (add-hook 'snippet-mode-hook 'snippet-delete-trailing-whitespace-hook)
    (yas-global-mode 1))

  (use-package yaml-mode
    :ensure)

  (use-package ansible
    :ensure
    :config
    (defun enable-ansible-mode ()
      "Wrapper for ansible mode hook"
      (ansible t))
    (add-hook 'yaml-mode-hook 'enable-ansible-mode)
    :diminish ansible)

  (use-package undo-tree
    :ensure undo-tree)

  (use-package projectile
    :ensure)

  (use-package saveplace
    :config (progn
              (setq save-place-file (concat emacs-etc "places"))
              (setq save-place t)))

  (use-package smooth-scrolling
    :ensure
    :config (progn
              (setq smooth-scroll-margin 5)
              (setq scroll-conservatively 9999 ;; OVER 9000!
                    scroll-preserve-screen-position t)))

  (use-package flycheck
    :ensure
    :init (add-hook 'after-init-hook 'global-flycheck-mode)
    :config (setq
             flycheck-check-syntax-automatically '(save idle-change mode-enabled)
             flycheck-idle-change-delay 0.8
             flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

  
  (recentf-mode 1)
  (setq recentf-max-saved-items 1000
        recentf-exclude '("/tmp/" "/ssh:"))

  
  ;; My abbreviated version of the starter-kit package.
  (use-package wacky-starter-kit)
  (use-package wacky-starter-kit-lisp)
  (use-package wacky-starter-kit-bindings)
  ;; (use-package wacky-starter-kit-js)

  (use-package update-dns)
  (use-package wacky-defuns)
  (use-package misc)
  
  ;; Many of the init-* are modified parts of https://github.com/purcell/emacs.d
  ;; Others just follow this pattern.
  (use-package init-git)
  (use-package init-isearch)
  (use-package init-windows)
  (use-package init-sessions)
  (use-package init-spelling)
  (use-package init-python)
  (use-package init-js)
  (use-package init-json)
  (use-package init-puppet)

  (use-package init-fonts
    :disabled (not *is-a-mac*))

  (use-package init-perl)
  (use-package init-org)

  
  ;;
  (use-package php-mode
    :ensure)
  (use-package web-mode
    :ensure)


  ;; Somehow I cannot get the editorconfig package NOT to install when I
  ;; don't want it.
  (if (executable-find "editorconfig")
      (setq editorconfig-available t)
    (setq editorconfig-available nil))
  (use-package editorconfig
    :if editorconfig-available
    :no-require t
    :disabled t
    :ensure)

  (use-package terraform-mode
    :ensure
    :config (progn
              (setq terraform-indent-level 4)
              (add-to-list 'auto-mode-alist '("\\.tfstate\\'" . json-mode))
              (add-to-list 'auto-mode-alist '("\\.json.tftemplate\\'" . json-mode))))

  ;; attic

  ;; something like:
  ;; (defun magit-such and such nil)
  ;; (magit-init-fns magit-such)
  ;; (use-package magit
  ;;   :config
  ;;   (magit-init-fns))

  ;; starter-kit bug.  This isn't needed with newer inf-ruby.
  ;; https://github.com/technomancy/emacs-starter-kit/pull/145
  ;;(remove-hook 'ruby-mode-hook 'inf-ruby-keys)

  (remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

  
  (use-package ido
    :init (progn
            (ido-mode t)
            (ido-everywhere t))
    :config
    (progn
      (setq
       ido-auto-merge-work-directories-length nil
       ido-create-new-buffer 'always
       ido-enable-flex-matching t
       ido-enable-prefix nil
       ido-handle-duplicate-virtual-buffers 2
       ido-max-prospects 10
       ido-use-filename-at-point nil
       ido-use-virtual-buffers t
       )
      (add-to-list 'ido-ignore-files "\\.DS_Store"))) ;; Move this to OSX specific section.

  (use-package ido-ubiquitous
    :ensure
    :config (ido-ubiquitous-mode t))

  
  (use-package smex
    :ensure
    :config (smex-initialize)
    :init (setq smex-save-file (concat emacs-etc ".smex-items"))
    :bind (("M-x" . smex)
           ("C-c x" . smex)))

  
  (use-package ibuffer
    :ensure
    :bind ("C-x C-b" . ibuffer)
    :config (progn
              ;; (use-package ibuffer-vc
              ;;   :ensure)
              (fullframe ibuffer ibuffer-quit)
              (setq ibuffer-show-empty-filter-groups nil)))

  ;; (use-package diff-hl
  ;;   :ensure
  ;;   :init (global-diff-hl-mode)
  ;;   :config (add-hook 'vc-checkin-hook 'diff-hl-update))

  
  (use-package markdown-mode
    :ensure
    :config (setq markdown-command "pandoc --smart -f markdown -t html"))

  (use-package gh-md
    :ensure
    :config (define-key markdown-mode-map (kbd "C-c C-c p") 'gh-md-render-buffer))

  (use-package ssh-config-mode
    :ensure
;    :config (add-hook 'ssh-config-mode-hook 'turn-on-font-lock)
    :mode (("\\.ssh/config\\'" . ssh-config-mode)
           ("sshd?_config\\'"  . ssh-config-mode)))

  ;; (use-package flymake-cursor
  ;;   :ensure)

  (use-package smooth-scrolling
    :ensure
    :config (setq smooth-scroll-margin 5
                  scroll-conservatively 9999
                  scroll-preserve-screen-position t))

  (if (string< emacs-version "24.4")
      (message "Package 'paradox' requires emacs 24.4 or newer. Skipping...")
    (use-package paradox
      :ensure
      :config
      (progn
        (use-package async :ensure)
        (paradox-enable)
        (setq paradox-execute-asynchronously t))))

  (when *is-a-mac*
    (use-package init-osx))

  ;; (defun load-local (file)
  ;;   (load (f-expand file user-emacs-directory)))

  ;; (load-local "defuns")
  ;; (load-local "misc")
  ;; (load-local "launcher")

  )

;; Back to default
(setq gc-cons-threshold 800000)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
