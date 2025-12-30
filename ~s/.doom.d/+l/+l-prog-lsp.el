;;; +l-prog-lsp.el -*- lexical-binding: t; -*-

(defun salih/rename-or-iedit ()
  "If current buffer is in eglot-mode, call eglot-rename. Otherwise, call
iedit-mode."
  (interactive)
  (if (and (featurep 'lsp) (bound-and-true-p lsp-mode))
      (call-interactively #'lsp-rename)
    (call-interactively #'iedit-mode)))

(defun salih/find-definition-or-lookup ()
  (interactive)
  "If current buffer is in eglot-mode, call eglot-find-definition. Otherwise, call
lookup."
  (if (and (featurep 'lsp) (bound-and-true-p lsp-mode))
      (call-interactively #'xref-find-definitions)
    (call-interactively #'+lookup/file)))


(defun salih/list-errors ()
  (interactive)
  (if (and (featurep 'lsp) (bound-and-true-p lsp-mode))
      (call-interactively #'consult-lsp-diagnostics)
    (call-interactively #'flycheck-projectile-list-errors)))


(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)


(setq lsp-enable-symbol-highlighting nil)
(setq lsp-ui-doc-show-with-cursor nil)
(setq lsp-ui-doc-show-with-mouse nil)
(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-code-actions t)
(setq lsp-ui-sideline-enable nil)
(setq lsp-modeline-code-actions-enable t)
(setq lsp-ui-sideline-enable nil)
(setq lsp-eldoc-enable-hover t)
(setq lsp-signature-auto-activate t) ;; you could manually request them via `lsp-signature-activate`
(setq lsp-signature-render-documentation nil)
(setq lsp-headerline-breadcrumb-enable t)




(provide '+l-prog-lsp)
