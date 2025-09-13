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



(provide '+l-prog-lsp)
