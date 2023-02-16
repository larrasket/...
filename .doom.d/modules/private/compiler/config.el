;;; mine/compiler/config.el -*- lexical-binding: t; -*-


(defun compileandrun()
 (save-buffer)
 (compile (concat "g++ "  (file-name-nondirectory (buffer-file-name)) " -o "
           (file-name-sans-extension   (file-name-nondirectory (buffer-file-name))) " && ./"
           (file-name-sans-extension  (file-name-nondirectory (buffer-file-name))) " && rm "
           (file-name-sans-extension  (file-name-nondirectory (buffer-file-name)))) t  ) (other-window t)
 (end-of-add-hook 'c++-mode))





;; TODO Refactor runner methods



(defun sharprun()
 (interactive)
 (save-buffer)
 (compile (concat "dotnet run") t  ) (other-window t)
 (end-of-add-hook 'csharp-mode))



(defun gorun()
 (interactive)
 (save-buffer)
 (compile (concat "go run .") t  ) (other-window t)
 (end-of-add-hook 'go-mode))



(defun rungo()
 (interactive)
 (save-buffer)
 (compile (concat "go run "  (file-name-nondirectory (buffer-file-name))) t)
 (other-window t)
 (end-of-add-hook 'go-mode))



(add-hook 'c++-mode-hook
          (lambda () (local-set-key (kbd "C-c C-c") 'compileandrun)))

(add-hook 'csharp-mode-hook
          (lambda () (local-set-key (kbd "C-c C-c") 'sharprun)))


(add-hook 'go-mode-hook
          (lambda () (local-set-key (kbd "C-c C-c") 'gorun)
                (local-set-key (kbd "<f2>") 'rungo)))



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

(add-hook 'company-mode-hook 'company-box-mode)
