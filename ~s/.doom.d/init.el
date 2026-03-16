;;; init.el -*- lexical-binding: t; -*-

(setq evil-respect-visual-line-mode t)
(doom! :completion
       vertico
       (corfu +icons)

       :ui
       doom
       doom-dashboard
       hl-todo
       (ligatures +pragmata-pro)
       modeline
       (popup +defaults)
       (vc-gutter +pretty)
       workspaces
       indent-guides

       :editor
       (evil +everywhere)
       file-templates
       fold
       snippets
       word-wrap
       (format +onsave)

       :emacs
       (dired +icons)
       electric
       undo
       vc

       :term
       eshell
       vterm

       :checkers
       (syntax +childframe)

       :tools
       biblio
       tree-sitter
       (lookup +dictionary +offline)
       (lsp +peek)
       magit
       pdf

       :os
       tty

       :lang
       (cc +lsp +tree-sitter)
       (clojure +lsp +tree-sitter)
       emacs-lisp
       ess
       (go +lsp +tree-sitter)
       (graphql +lsp +tree-sitter)
       (javascript +lsp +tree-sitter)
       json
       (julia +lsp +tree-sitter)
       (latex +latexmk +lsp)
       (org +roam +pandoc)
       (rest +jq)
       (scala +lsp +tree-sitter)
       sh
       web
       yaml

       :email
       (mu4e +mbsync)

       :app
       irc
       everywhere

       :config
       (default +bindings +smartparens))

(add-to-list 'load-path (expand-file-name "modules" doom-user-dir))
(add-to-list 'load-path (expand-file-name "pkg" doom-user-dir))
