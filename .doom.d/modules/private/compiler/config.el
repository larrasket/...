;;; mine/compiler/config.el -*- lexical-binding: t; -*-



;; C++


(defun my-cpp-mode-setup ()
  (company-mode)
  (flycheck-mode)
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4))


(add-hook 'c++-mode-hook 'my-csharp-mode-setup t)
(add-hook 'c-mode-hook 'my-csharp-mode-setup t)

(defun compileandrun()
(interactive)
(save-buffer)
(compile (concat "g++ "  (file-name-nondirectory (buffer-file-name)) " -o "
(file-name-sans-extension   (file-name-nondirectory (buffer-file-name))) " && ./"
(file-name-sans-extension  (file-name-nondirectory (buffer-file-name))) " && rm "
(file-name-sans-extension  (file-name-nondirectory (buffer-file-name)))
) t  ) (other-window t)
(end-of-add-hook 'c++-mode))



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
(compile (concat "go run "  (file-name-nondirectory (buffer-file-name))
;; (file-name-sans-extension  (file-name-nondirectory (buffer-file-name)))
) t  )
(other-window t)
;; (+debugger/start)
(end-of-add-hook 'go-mode))


(defun compiledep()
(interactive)
(save-buffer)
(compile (concat "g++ "  (file-name-nondirectory (buffer-file-name)) " -g -o "
(file-name-sans-extension  (file-name-nondirectory (buffer-file-name)))
) t  )
(other-window t)
;; (+debugger/start)
(end-of-add-hook 'c++-mode))









(add-hook 'c++-mode-hook
          (lambda () (local-set-key (kbd "C-c C-c") 'compileandrun)))


(add-hook 'sql-mode-hook
          (lambda () (local-set-key (kbd "C-c C-c") 'lsp-mssql-execute-region)))

(add-hook 'csharp-mode-hook
          (lambda () (local-set-key (kbd "C-c C-c") 'sharprun)))


(add-hook 'go-mode-hook
          (lambda () (local-set-key (kbd "C-c C-c") 'gorun)))


(add-hook 'go-mode-hook
          (lambda () (local-set-key (kbd "<f2>") 'rungo)))



(add-hook 'c++-mode-hook
          (lambda () (local-set-key (kbd "M-<f5>") 'compiledep)))


(add-hook 'lsp-mode-hook
          (lambda ()
             (add-hook 'after-save-hook 'lsp-format-buffer nil 'make-it-local)))






;; (defun my-tex ()
;;   (interactive)
;;   (save-buffer)
;;   (TeX-command "LaTeX" 'TeX-master-file -1))
;; (defun aftaa () (add-hook 'after-save-hook 'my-tex))
;; (add-hook 'LaTeX-mode-hook #'aftaa)
;; (defun JH/remove-electric-indent-mode ()
;;   (electric-indent-local-mode -1))
;; (setq LaTeX-indent-environment-list '())
;; (setq LaTeX-indent-level 0)
;; (setq LaTeX-item-indent 0)
;; (setq LaTeX-left-right-indent-level 0)
;; (setq TeX-brace-indent-level 0)
;; (add-hook 'LaTeX-mode-hook 'JH/remove-electric-indent-mode)
;; (add-hook 'tex-mode-hook 'JH/remove-electric-indent-mode)
;; (setq TeX-brace-indent-level 4)
;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell (replace-regexp-in-string
;;                           "[ \t\n]*$"
;;                           ""
;;                           (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq eshell-path-env path-from-shell) ; for eshell users
;;     (setq exec-path (split-string path-from-shell path-separator))))

(defun my-csharp-mode-setup ()
  (lsp)
  (company-mode)
  (flycheck-mode)
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)



(setq term-default-fg-color "#FFFFFF") (setq term-default-bg-color "#FFFFFF")




(setq lsp-enable-symbol-highlighting t)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-ui-doc-show-with-mouse t)
(setq lsp-lens-enable t)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-sideline-enable t)
(setq lsp-modeline-code-actions-enable t)
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-eldoc-enable-hover t)
(setq lsp-modeline-diagnostics-enable t)
(setq lsp-signature-auto-activate t) ;; you could manually request them via `lsp-signature-activate`
(setq lsp-signature-render-documentation t)
(setq lsp-completion-show-detail t)
(setq lsp-completion-show-kind t)
(setq quickrun-focus-p nil)




(defun execute-c++-program ()
  (interactive)
  (defvar foo)
  (setq foo (concat "g++ " (buffer-name) " && ./a.out" ))
  (shell foo))

(add-hook 'company-mode-hook 'company-box-mode)
