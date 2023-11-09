;;; init.el -*- lexical-binding: t; -*-
(doom! :input
       :completion
       company
       (vertico +icons)

       :ui
       vc-gutter
       doom
       doom-dashboard
       hl-todo
       (treemacs +lsp)
       (ligatures
        +pragmata-pro)
       modeline
       (popup +defaults)
       workspaces

       :editor
       (evil
        +everywhere)
       file-templates
       fold
       (format +onsave)
       lispy
       parinfer
       snippets
       word-wrap

       :emacs
       dired
        
       electric
       undo
       vc

       :term
       eshell
       vterm

       :checkers
       (syntax +childframe)
       (spell  +flyspell)

       :tools
       biblio
       tree-sitter
       (lookup
        +dictionary
        +offline)
       (lsp +peak)
       (magit)
       pdf
       ;; debugger

       :os
       (tty)

       :lang
       (cc +lsp)
       common-lisp
       (csharp +lsp)
       emacs-lisp
       (go +lsp
           +tree-sitter)
       json
       (latex +flod
              +latexmk
              +lsp)
       (org
        +roam2
        +noter)
       (rest +jq)
       sh
       yaml
       (julia
        +lsp
        +tree-sitter)
       ess
       (java +lsp +tree-sitter)

       :email
       mu4e

       :app
       ;; irc
       rss

       :config
       (default ;; +bindings
        +smartparens)

       :private
       compiler
       gen
       genorg
       cite
       roam
       bind)
