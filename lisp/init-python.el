(use-package elpy
  :ensure
  :idle (progn
          (elpy-enable)
          (elpy-use-ipython))
  :config
  ;; Monkey patch to not tell me which function I'm in always
  (progn
    ;; disabling highlight columns
    (setq elpy-modules '(elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults))
    ;; ;; Make wacky friendly.
    ;; (defun elpy-eldoc-documentation ()
    ;;   "Return a call tip for the python call at point."
    ;;   (elpy-rpc-get-calltip
    ;;    (lambda (calltip)
    ;;      (eldoc-message
    ;;       (cond
    ;;        ((not calltip)
    ;;         (let ((current-defun (python-info-current-defun)))
    ;;           (when current-defun
    ;;                                     ;            (format "In: %s()" current-defun) ;; Mr Wacky didn't like this.
    ;;             nil)))
    ;;        ((stringp calltip)
    ;;         calltip)
    ;;        (t
    ;;         (let ((name (cdr (assq 'name calltip)))
    ;;               (index (cdr (assq 'index calltip)))
    ;;               (params (cdr (assq 'params calltip))))
    ;;           (when index
    ;;             (setf (nth index params)
    ;;                   (propertize (nth index params)
    ;;                               'face
    ;;                               'eldoc-highlight-function-argument)))
    ;;           (format "%s(%s)"
    ;;                   name
    ;;                   (mapconcat #'identity params ", "))))))))
    ;;   ;; Return the last message until we're done
    ;;   eldoc-last-message)
    ))

(provide 'init-python)
