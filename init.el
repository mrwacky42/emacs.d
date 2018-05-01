;;; init ---  Configure emacs
;;; Commentary:
;; The astute observer will notice that I've borrowed plenty from
;; https://github.com/purcell/emacs.d
;; https://github.com/rejeep/emacs
;;

;;; Code:


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
              (forward-line))))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(defun wacky/package-selected-packages-sorter (old-function &optional thing)
  "Use this as advice around package--save-selected-packages to ensure
you always store the package-selected-packages sorted."
  (let ((package-list (copy-seq (if thing
                           thing
                         package-selected-packages))))
    (funcall old-function (cl-sort package-list #'string< :key #'symbol-name))))

(advice-add 'package--save-selected-packages :around #'wacky/package-selected-packages-sorter)


(random t) ;; Seed the random-number generator


(defvar file-name-handler-alist-old file-name-handler-alist)
(setq package-enable-at-startup nil
      file-name-handler-alist nil
      gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold (* 20 1024 1024))
             (garbage-collect)) t)

;; Bootstrappin'
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *spell-check-support-enabled* t)
(defconst *is-a-mac* (eq system-type 'darwin))

(require 'package)


;;; Standard package repositories
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(use-package diminish :ensure t)
(use-package bind-key :ensure t)

;; Strive for less clutter in user-emacs-directory
(use-package no-littering :ensure)

(setq load-prefer-newer t)


;; backup options
(setq
 backup-by-copying t ;; Set backups to not hose sym/hard links
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 vc-make-backup-files t
 version-control t)

(setq color-theme-is-global t
      column-number-mode t
      diff-switches "-u"
      ediff-window-setup-function 'ediff-setup-windows-plain
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      line-number-mode t
      mouse-yank-at-point t
      require-final-newline t
      save-interprogram-paste-before-kill t
      sentence-end-double-space nil
      shift-select-mode nil
      visible-bell t)



(load-theme 'misterioso)
(set-cursor-color "light gray")
(blink-cursor-mode 1)
(setq blink-cursor-interval .4)


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
(load custom-file :noerror :nomessage)


;; Packages
(use-package init-benchmarking)
(use-package beacon
  :ensure t
  :demand t
  :bind ("C-c C-b" . beacon-blink)
  :config
  (setq beacon-color "DarkOrange2"
        beacon-blink-duration 1)
  (beacon-mode t))

(use-package uniquify
  :config
  (setq
   uniquify-buffer-name-style 'reverse
   uniquify-separator ":"
   uniquify-after-kill-buffer-p t      ;; rename after killing dupes
   uniquify-ignore-buffers-re "^\\*")) ;; don't muck with special buffers

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
  :disabled)

(use-package expand-region
  :ensure
  :bind ("C-=" . er/expand-region))

(use-package match-paren
  :bind ("%" . match-paren))

(use-package fullframe
  :ensure
  :config (fullframe list-packages quit-window))

(use-package comment-dwim-2
  :ensure
  :bind ("M-;" . comment-dwim-2))

;; (use-package diff-hl
;;   :ensure
;;   :init (global-diff-hl-mode)
;;   :config (progn
;;             (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
;;             (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
;;             (add-hook 'vc-checkin-hook 'diff-hl-update)))

(use-package yasnippet
  :ensure
  :pin gnu
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
  (use-package yasnippet-snippets :ensure)
  (yas-global-mode 1))

(use-package yaml-mode
  :ensure
  :config
  (add-hook 'yaml-mode-hook 'turn-off-flyspell)
  :bind (:map yaml-mode-map
              ("C-c h a" . ansible-doc)))

(use-package ansible-doc
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
  :ensure)

(use-package projectile
  :ensure)

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-forget-unreadable-files t
                                        ;      save-place-skip-check-regexp "\\`/\\(?:cdrom\\|floppy\\|mnt\\|/[0-9]\\|\\(?:[^@/:]*@\\)?[^@/:]*[^@/:.]:\\)"
        ))

(use-package flycheck
  :ensure
  :init (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (setq
   flycheck-check-syntax-automatically '(save idle-change mode-enabled)
   flycheck-idle-change-delay 0.8
   flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))


(use-package recentf
  :after (no-littering)
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 100
        recentf-exclude '("/tmp/" "/ssh:"))
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package whitespace
  :config
  (setq whitespace-line-column 80
        whitespace-style '(face trailing lines-tail tabs)))

(global-prettify-symbols-mode 1)


;; My abbreviated/modified/smothered/covered/chunked version of the starter-kit package.
(use-package wacky-starter-kit)
;; (use-package wacky-starter-kit-js)

(use-package wacky-defuns
  :demand t
  :bind ("C-z" . maybe-suspend-frame))

(use-package misc)
(use-package init-prelude)


;; Many of the init-* are modified parts of https://github.com/purcell/emacs.d
;; Others just follow this pattern.
(when *is-a-mac*
  (use-package init-fonts))
(use-package init-git)
(use-package init-go)
(use-package init-gpg
  :config
  (setq epa-armor t
        epg-gpg-program "gpg"))
(use-package init-ibuffer)
(use-package init-isearch)
(use-package init-js)
(use-package init-json)
(use-package init-lisp)
(use-package init-org)
(use-package init-perl)
(use-package init-puppet)
(use-package init-python)
(use-package init-rust)
(use-package init-sessions)
(use-package init-spelling)
(use-package init-windows)



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
  :disabled
  :ensure)

(use-package terraform-mode
  :ensure
  :config
  (defun terraform-syntax-override ()
    "Treat underscore as whitespace for easier nav"
    (modify-syntax-entry ?_ "-" hcl-mode-syntax-table))
  (add-hook 'terraform-mode-hook 'terraform-syntax-override)
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
  (add-to-list 'auto-mode-alist '("\\.tfstate\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.json.tftemplate\\'" . json-mode)))

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
  (setq
   ido-auto-merge-work-directories-length nil
   ido-create-new-buffer 'always
   ido-enable-flex-matching t
   ido-enable-prefix nil
   ido-handle-duplicate-virtual-buffers 2
   ido-max-prospects 10
   ido-use-filename-at-point nil
   ido-use-virtual-buffers t)
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  (use-package ido-completing-read+
    :ensure
    :config (ido-ubiquitous-mode t)))


(use-package smex
  :ensure
  :config (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c x" . smex)))


(use-package dired+
  :disabled t
  :ensure nil
  :init (setq diredp-hide-details-initially-flag nil)
  :config
  ;; enable `a' command which replaces the current dired buffer rather
  ;; than opening an additional buffer when you move to another
  ;; directory
  (put 'dired-find-alternate-file 'disabled nil)  ;enable `a' command
  ;; Replace buffers instead of making new ones when navigating in dired
  ;; From: http://ergoemacs.org/emacs/emacs_dired_tips.html
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "<return>")
                'dired-find-alternate-file) ; was dired-advertised-find-file
              (define-key dired-mode-map (kbd "^")
                (lambda () (interactive) (find-alternate-file "..")))))
  )
                                        ; was dired-up-directory


;; (use-package diff-hl
;;   :ensure
;;   :init (global-diff-hl-mode)
;;   :config (add-hook 'vc-checkin-hook 'diff-hl-update))


(use-package markdown-mode
  :ensure
  :config (setq markdown-command "pandoc --smart -f markdown -t html"))

(use-package gh-md
  :ensure
  :bind (:map markdown-mode-map
              ("C-c C-c p" . gh-md-render-buffer))
  :commands gh-md-render-buffer)

(use-package ssh-config-mode
  :ensure
  ;; :config (add-hook 'ssh-config-mode-hook 'turn-on-font-lock)
  :mode (("\\.ssh/config\\'" . ssh-config-mode)
         ("sshd?_config\\'"  . ssh-config-mode))
  :config)

;; (use-package flymake-cursor
;;   :ensure)

(use-package smooth-scrolling
  :ensure
  :config (progn
            (setq smooth-scroll-margin 5)
            (setq scroll-conservatively 9999 ;; OVER 9000!
                  scroll-preserve-screen-position t)))

(use-package tramp
  :defer 5
  :config
  (put 'temporary-file-directory 'standard-value '("/tmp")))

(use-package zop-to-char
  :ensure
  :config (global-set-key [remap zap-to-char] 'zop-to-char))

(when (version< "24.4" emacs-version)
  (use-package paradox
    :ensure
    :config
    (progn
      (use-package async :ensure)
      (paradox-enable)
      (setq paradox-execute-asynchronously t))))

(use-package update-dns)

(when *is-a-mac*
  (use-package init-osx))

(use-package calendar
  :defer 1
  :config (progn
            (setq calendar-week-start-day 1)
            (calendar-set-date-style 'iso)
            (setq calendar-latitude 33.99
                  calendar-longitude -118.42)))
                                        ;33.9921976,-118.4235076

;; (defun load-local (file)
;;   (load (f-expand file user-emacs-directory)))

;; (load-local "defuns")
;; (load-local "launcher")

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init.el ends here
