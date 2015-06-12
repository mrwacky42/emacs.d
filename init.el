;; The astute observer will notice that I've borrowed plenty from
;; https://github.com/purcell/emacs.d
;; https://github.com/rejeep/emacs
;;

;;
(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode))

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (string= (buffer-name) "*scratch*")
              (animate-string ";; Mr Wacky Heavy Industries is online!!" (/ (frame-height) 2)))))

;; Strive for less clutter in user-emacs-directory
(setq emacs-etc (concat user-emacs-directory "etc/"))
(when
    (not (file-exists-p emacs-etc))
  (make-directory emacs-etc))

(random t) ;; Seed the random-number generator

(setq backup-by-copying t ;; Set backups to not hose sym/hard links
      color-theme-is-global t
      column-number-mode t
      diff-switches "-u"
      ediff-window-setup-function 'ediff-setup-windows-plain
      inhibit-startup-message t
      line-number-mode t
      mouse-yank-at-point t
      save-interprogram-paste-before-kill t
      sentence-end-double-space nil
      shift-select-mode nil
      whitespace-line-column 80
      whitespace-style '(face trailing lines-tail tabs)
      visible-bell t
      oddmuse-directory (concat emacs-etc "oddmuse")
      save-place-file (concat emacs-etc "places")
      backup-directory-alist `(("." . ,(concat emacs-etc "backups")))
      auto-save-list-file-prefix (concat emacs-etc "auto-save-list"))
;; (setq-default c-basic-offset 4)


(load-theme 'misterioso)
;(add-to-list 'load-path "~/git/emacs-color-theme-solarized")
;(require 'color-theme-solarized)
;(color-theme-solarized-dark)

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


;; If gpg cannot be found, signature checking will fail, so we
;; conditionally enable it according to whether gpg is available. We
;; re-run this check once $PATH has been configured
(defun sanityinc/package-maybe-enable-signatures ()
  (setq package-check-signature (when (executable-find "gpg") 'allow-unsigned)))

(sanityinc/package-maybe-enable-signatures)
;(after-load 'init-exec-path
;  (sanityinc/package-maybe-enable-signatures))

;(package-refresh-contents)


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

(use-package ace-jump-mode
  :ensure
  :bind ("C-c SPC" . ace-jump-mode))

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

(use-package yaml-mode
  :ensure
  :config (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

(use-package yasnippet
  :ensure)

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
            (setq scroll-conservatively 9999
                  scroll-preserve-screen-position t)))


;; (use-package flycheck
;;   :ensure
;;   :init (add-hook 'after-init-hook 'global-flycheck-mode)
;;   :config (setq
;;            flycheck-check-syntax-automatically '(save idle-change mode-enabled)
;;            flycheck-idle-change-delay 0.8
;;            flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))


(recentf-mode 1)
(setq recentf-max-saved-items 1000
      recentf-exclude '("/tmp/" "/ssh:"))


;; My abbreviated version of the starter-kit package.
(use-package wacky-starter-kit)
(use-package wacky-starter-kit-lisp)
(use-package wacky-starter-kit-bindings)

(use-package update-dns)
(use-package wacky-defuns)
(use-package misc)


;; Modified bits of https://github.com/purcell/emacs.d
(use-package init-git)
(use-package init-isearch)
(use-package init-windows)
(use-package init-sessions)
(use-package init-python)
(use-package init-json)


;;
(use-package php-mode
  :ensure)
(use-package web-mode
  :ensure)


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
     (setq ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-auto-merge-work-directories-length nil
          ido-create-new-buffer 'always
          ido-use-filename-at-point 'guess
          ido-use-virtual-buffers t
          ido-handle-duplicate-virtual-buffers 2
          ido-max-prospects 10)
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


(use-package yaml-mode
  :config (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

(use-package markdown-mode
  :ensure
  :config
  (progn
    (setq markdown-command "pandoc --smart -f markdown -t html")
    (add-hook 'markdown-mode-hook
              (lambda ()
                (flyspell-mode t)))))

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

;; (defun load-local (file)
;;   (load (f-expand file user-emacs-directory)))

;; (load-local "defuns")
;; (load-local "misc")
;; (load-local "launcher")
;; (when (eq system-type 'darwin)
;;   (load-local "osx"))
