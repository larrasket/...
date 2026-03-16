;; -*- no-byte-compile: t; -*-
;;; packages.el — Package declarations

;; UI & Themes
(package! ef-themes)
(package! kaolin-themes)
(package! breadcrumb)
(package! spacious-padding)
(package! mixed-pitch)

;; Editing
(package! iedit)
(package! jinx)
(package! evil-snipe :disable t)

;; Completion
(package! consult-lsp)
(package! consult-flycheck)
(package! consult-org-roam)

;; Org
(package! org-modern)
(package! org-ql)
(package! org-download)
(package! org-fc)
(package! org-noter :pin "9d42ebc626981b6726b8078fb216b30cc5e34d21")
(package! org-present)
(package! org-web-tools)
(package! org-ref)
(package! org-roam-bibtex)
(package! org-roam-ui)
(package! vulpea)
(package! visual-fill-column)

;; Academic
(package! citar)
(package! citar-org-roam)
(unpin! bibtex-completion helm-bibtex ivy-bibtex)
(unpin! parsebib)
(unpin! ess)
(package! ess)

;; Epub
(package! nov)

;; Git
(package! git-auto-commit-mode)
(package! git-gutter)
(package! git-gutter-fringe)

;; LSP & Prog
(package! lsp-treemacs)
(package! flycheck-projectile)
(package! flycheck-golangci-lint)
(package! dap-mode)
(package! protobuf-mode
  :recipe (:host github :repo "protocolbuffers/protobuf"
           :files ("editors/protobuf-mode.el")))
(unpin! eglot)
(unpin! lsp-pyright)
(unpin! tree-sitter-langs)

;; Tools
(package! gt)
(package! wiki-summary)
(package! dired-preview)

;; Email
(package! mu4e-alert)

;; Hugo / Blog
(package! ox-tufte
  :recipe (:host github :repo "luggages/ox-tufte"))
(package! ox-html-stable-ids
  :recipe (:host github :repo "luggages/ox-html-stable-ids.el"))

;; Writing / Grammar
(package! lsp-ltex)
(package! awqat
  :recipe (:host github :repo "larrasket/awqat"
           :branch "feature/notification"))

