;;; +l-prog-lsp.el -*- lexical-binding: t; -*-

;; LSP advice
(advice-add 'lsp-resolve-final-command
            :around   #'lsp-booster--advice-final-command)

;; Programming utilities
(defun salih/rename-or-iedit ()
  "If current buffer is in eglot-mode, call eglot-rename. Otherwise, call
iedit-mode."
  (interactive)
  (if (and (featurep 'eglot) (eglot-managed-p))
      (call-interactively #'eglot-rename)
    (call-interactively #'iedit-mode)))

(defun salih/find-definition-or-lookup ()
  (interactive)
  "If current buffer is in eglot-mode, call eglot-find-definition. Otherwise, call
lookup."
  (if (eglot-managed-p)
      (call-interactively #'xref-find-definitions)
    (call-interactively #'+lookup/file)))

(provide '+l-prog-lsp) 