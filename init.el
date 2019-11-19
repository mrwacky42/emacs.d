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
    (funcall old-function (cl-sort package-list #'string-lessp))))

(advice-add 'package--save-selected-packages :around #'wacky/package-selected-packages-sorter)


(random t) ;; Seed the random-number generator


(defvar file-name-handler-alist-old file-name-handler-alist)
(defvar my-gc-cons-threshold (* 20 1024 1024))
(setq package-enable-at-startup nil
      file-name-handler-alist nil
      gc-cons-threshold most-positive-fixnum)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold my-gc-cons-threshold)
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
(use-package delight :ensure t)
(use-package diminish :ensure t)
(use-package bind-key :ensure t)

;; Strive for less clutter in user-emacs-directory
(use-package no-littering
  :ensure
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(setq load-prefer-newer t)


;; backup options
(setq
 backup-by-copying t ;; Set backups to not hose sym/hard links
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 vc-make-backup-files t
 version-control t)

(setq column-number-mode t
      diff-switches "-u"
      help-window-select t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      line-number-mode t
      mouse-yank-at-point t
      require-final-newline t
      save-interprogram-paste-before-kill t
      sentence-end-double-space nil
      shift-select-mode nil)



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
  :bind ("C-c C-b" . beacon-blink)
  :config
  (setq beacon-color "DarkOrange2"
        beacon-blink-duration 1)
  (beacon-mode t))

;; https://emacs.stackexchange.com/a/24800
(use-package company
  :ensure
  :demand t
  :bind ( ;; Replace `completion-at-point' and `complete-symbol' with
         ;; `company-manual-begin'. You might think this could be put
         ;; in the `:bind*' declaration below, but it seems that
         ;; `bind-key*' does not work with remappings.
         ([remap completion-at-point] . company-manual-begin)
         ([remap complete-symbol] . company-manual-begin)

         ;; The following are keybindings that take effect whenever
         ;; the completions menu is visible, even if the user has not
         ;; explicitly interacted with Company.

         :map company-active-map

         ;; Make TAB always complete the current selection. Note that
         ;; <tab> is for windowed Emacs and TAB is for terminal Emacs.
         ("<tab>" . company-complete-selection)
         ("TAB" . company-complete-selection)

         ;; Prevent SPC from ever triggering a completion.
         ("SPC" . nil)

         ;; The following are keybindings that only take effect if the
         ;; user has explicitly interacted with Company.

         :map company-active-map
         :filter (company-explicit-action-p)

         ;; Make RET trigger a completion if and only if the user has
         ;; explicitly interacted with Company. Note that <return> is
         ;; for windowed Emacs and RET is for terminal Emacs.
         ("<return>" . company-complete-selection)
         ("RET" . company-complete-selection)

         ;; We then do the same for the up and down arrows. Note that
         ;; we use `company-select-previous' instead of
         ;; `company-select-previous-or-abort'. I think the former
         ;; makes more sense since the general idea of this `company'
         ;; configuration is to decide whether or not to steal
         ;; keypresses based on whether the user has explicitly
         ;; interacted with `company', not based on the number of
         ;; candidates.

         ("<up>" . company-select-previous)
         ("<down>" . company-select-next))

  :bind* ( ;; The default keybinding for `completion-at-point' and
          ;; `complete-symbol' is M-TAB or equivalently C-M-i. Here we
          ;; make sure that no minor modes override this keybinding.
          ("M-TAB" . company-manual-begin))

  :diminish company-mode
  :config

  ;; Turn on Company everywhere.
  (global-company-mode 1)

  (setq company-idle-delay .3)

  ;; Show completions after typing a single character, rather than
  ;; after typing three characters.
  (setq company-minimum-prefix-length 1)

  ;; Show a maximum of 10 suggestions. This is the default but I think
  ;; it's best to be explicit.
  (setq company-tooltip-limit 10)

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  (setq company-frontends '(company-pseudo-tooltip-frontend))

  ;; Show quick-reference numbers in the tooltip. (Select a completion
  ;; with M-1 through M-0.)
  (setq company-show-numbers t)

  ;; Prevent non-matching input (which will dismiss the completions
  ;; menu), but only if the user interacts explicitly with Company.
  (setq company-require-match #'company-explicit-action-p)

  ;; Company appears to override our settings in `company-active-map'
  ;; based on `company-auto-complete-chars'. Turning it off ensures we
  ;; have full control.
  (setq company-auto-complete-chars nil)

  ;; Prevent Company completions from being lowercased in the
  ;; completion menu. This has only been observed to happen for
  ;; comments and strings in Clojure.
  (setq company-dabbrev-downcase nil)

  ;; Only search the current buffer to get suggestions for
  ;; company-dabbrev (a backend that creates suggestions from text
  ;; found in your buffers). This prevents Company from causing lag
  ;; once you have a lot of buffers open.
  (setq company-dabbrev-other-buffers nil)

  ;; Make company-dabbrev case-sensitive. Case insensitivity seems
  ;; like a great idea, but it turns out to look really bad when you
  ;; have domain-specific words that have particular casing.
  (setq company-dabbrev-ignore-case nil)

  ;; Make it so that Company's keymap overrides Yasnippet's keymap
  ;; when a snippet is active. This way, you can TAB to complete a
  ;; suggestion for the current field in a snippet, and then TAB to
  ;; move to the next field. Plus, C-g will dismiss the Company
  ;; completions menu rather than cancelling the snippet and moving
  ;; the cursor while leaving the completions menu on-screen in the
  ;; same location.

  (with-eval-after-load 'yasnippet
    ;; TODO: this is all a horrible hack, can it be done with
    ;; `bind-key' instead?

    ;; This function translates the "event types" I get from
    ;; `map-keymap' into things that I can pass to `lookup-key'
    ;; and `define-key'. It's a hack, and I'd like to find a
    ;; built-in function that accomplishes the same thing while
    ;; taking care of any edge cases I might have missed in this
    ;; ad-hoc solution.
    (defun radian--normalize-event (event)
      (if (vectorp event)
          event
        (vector event)))

    ;; Here we define a hybrid keymap that delegates first to
    ;; `company-active-map' and then to `yas-keymap'.
    (setq radian--yas-company-keymap
          ;; It starts out as a copy of `yas-keymap', and then we
          ;; merge in all of the bindings from
          ;; `company-active-map'.
          (let ((keymap (copy-keymap yas-keymap)))
            (map-keymap
             (lambda (event company-cmd)
               (let* ((event (radian--normalize-event event))
                      (yas-cmd (lookup-key yas-keymap event)))
                 ;; Here we use an extended menu item with the
                 ;; `:filter' option, which allows us to
                 ;; dynamically decide which command we want to
                 ;; run when a key is pressed.
                 (define-key keymap event
                   `(menu-item
                     nil ,company-cmd :filter
                     (lambda (cmd)
                       ;; There doesn't seem to be any obvious
                       ;; function from Company to tell whether or
                       ;; not a completion is in progress (à la
                       ;; `company-explicit-action-p'), so I just
                       ;; check whether or not `company-my-keymap'
                       ;; is defined, which seems to be good
                       ;; enough.
                       (if company-my-keymap
                           ',company-cmd
                         ',yas-cmd))))))
             company-active-map)
            keymap))

    ;; The function `yas--make-control-overlay' uses the current
    ;; value of `yas-keymap' to build the Yasnippet overlay, so to
    ;; override the Yasnippet keymap we only need to dynamically
    ;; rebind `yas-keymap' for the duration of that function.
    (defun radian--advice-company-overrides-yasnippet
        (yas--make-control-overlay &rest args)
      "Allow `company' to override `yasnippet'.
This is an `:around' advice for `yas--make-control-overlay'."
      (let ((yas-keymap radian--yas-company-keymap))
        (apply yas--make-control-overlay args)))

    (advice-add #'yas--make-control-overlay :around
                #'radian--advice-company-overrides-yasnippet)))

(use-package copy-as-format
  :ensure
  :bind (("C-c w g" . copy-as-format-github)
         ("C-c w j" . copy-as-format-jira)
         ("C-c w m" . copy-as-format-markdown)
         ("C-c w o" . copy-as-format-org-mode)
         ("C-c w s" . copy-as-format-slack))
  :config
  (copy-as-format))

(use-package eldoc
  :diminish
  :hook ((emacs-lisp-mode . turn-on-eldoc-mode)
         (lisp-interaction-mode . turn-on-eldoc-mode)
         (ielm-mode . turn-on-eldoc-mode)))

(use-package elisp-mode
  :delight emacs-lisp-mode "ξ")

(use-package engine-mode ;; Bound to C-x / <key>
  :ensure
  :defer 3
  :config
  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias=aps&field-keywords=%s"
    :keybinding "a")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")

  (defengine google-images
    "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
    :keybinding "i")

  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :keybinding "m"
    :docstring "Mappin' it up.")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis.")
  (engine-mode t))


(use-package epa
  :defer t
  :config
  ;; Let Emacs query the passphrase through the minibuffer
  ;; Add 'allow-loopback-pinentry' to ~/.gnupg/gpg-agent.conf
  (setq epa-pinentry-mode 'loopback)
  ;; Always replace encrypted text with plain text version
  (setq epa-replace-original-text t))

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

(use-package holidays
  :custom
  (holiday-bahai-holidays nil))

(use-package comment-dwim-2
  :ensure
  :bind ("M-;" . comment-dwim-2))

(use-package unfill
  :ensure
  :bind ("M-q" . unfill-toggle))

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
  (save-place-mode)
  (setq save-place-forget-unreadable-files t))
;; save-place-skip-check-regexp "\\`/\\(?:cdrom\\|floppy\\|mnt\\|/[0-9]\\|\\(?:[^@/:]*@\\)?[^@/:]*[^@/:.]:\\)"

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
;  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
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

(if (executable-find "editorconfig")
  (use-package editorconfig
    :no-require t
    :disabled
    :ensure))

(use-package terraform-mode
  :ensure
  :config
  (defun terraform-syntax-override ()
    "Treat underscore as whitespace for easier nav"
    (modify-syntax-entry ?_ "-" hcl-mode-syntax-table))
  (add-hook 'terraform-mode-hook 'terraform-syntax-override)
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
  (add-to-list 'auto-mode-alist '("\\.tfstate\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.json.tftemplate\\'" . json-mode))
  (use-package company-terraform
               :ensure
               :defer
               (company-terraform-init)))


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


(use-package markdown-mode
  :ensure
  :config (setq markdown-command "pandoc -f markdown+smart -t html"))

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
  :config
  (global-set-key [remap zap-to-char] 'zop-up-to-char))

(when (version< "24.4" emacs-version)
  (use-package paradox
    :ensure
    :config
    (progn
      (use-package async :ensure)
      (paradox-enable))
    :custom
    (paradox-column-width-status 7)
    (paradox-column-width-package 27)
    (paradox-execute-asynchronously t)
    (paradox-hide-wiki-packages t)
    (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print)))


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

(use-package dockerfile-mode
  :ensure)

(use-package which-key
  :ensure
  :defer 0.2
  :diminish
  :config (which-key-mode))

;; s-q usually bound to save-buffers-kill-emacs.
;; What kind of person needs to quit emacs?
(global-unset-key (kbd "s-q"))

;; (defun load-local (file)
;;   (load (f-expand file user-emacs-directory)))

;; (load-local "defuns")
;; (load-local "launcher")

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init.el ends here
