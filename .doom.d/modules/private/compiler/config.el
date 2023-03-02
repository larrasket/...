;;; mine/compiler/config.el -*- lexical-binding: t; -*-


(setq lsp-enable-symbol-highlighting t
      lsp-ui-doc-enable t
      lsp-ui-doc-show-with-cursor t
      lsp-ui-doc-show-with-mouse t
      lsp-lens-enable t
      lsp-headerline-breadcrumb-enable t
      lsp-ui-sideline-enable t
      lsp-ui-sideline-enable t
      lsp-modeline-code-actions-enable t
      lsp-ui-sideline-enable t
      lsp-ui-sideline-show-diagnostics t
      lsp-eldoc-enable-hover t
      lsp-modeline-diagnostics-enable t
      lsp-signature-auto-activate t
      lsp-signature-render-documentation t
      lsp-completion-show-detail t
      lsp-completion-show-kind t)

;; (add-hook 'company-mode-hook 'company-box-mode)

(after! 'sly
 (setq sly-complete-symbol-function 'sly-flex-completions))
