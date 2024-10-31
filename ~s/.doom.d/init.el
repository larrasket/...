;;; init.el -*- lexical-binding: t; -*-
(doom! :input
       :completion
       (vertico)
       company
       (corfu +icons)

       :ui
       (vc-gutter +pretty)                        ; git annotions
       doom
       doom-dashboard
       hl-todo
       (ligatures +pragmata-pro)
       (popup +defaults)
       modeline
       workspaces
       indent-guides

       :editor
       (evil +everywhere)
       file-templates
       fold                             ; activate za in evil mode
       lispy                            ; better lisp edit
       parinfer                         ; (better lisp edit)
       snippets
       word-wrap

       :emacs
       (dired +icons)
       electric                         ; better tabs (as in tabs vs spaces)
       undo
       vc                               ; magit

       :term
       eshell
       vterm

       :checkers
       (syntax +childframe)
       (spell  +flyspell)

       :tools
       biblio
       tree-sitter
       (lookup +dictionary +offline)
       (lsp +peak +eglot)
       magit
       pdf

       :os
       (tty)

       :lang
       (csharp +lsp +dotnet +tree-sitter)
       (clojure +lsp +tree-sitter)
       (latex +flod +latexmk +lsp)
       (julia +lsp +tree-sitter)
       (cc +lsp +tree-sitter)
       (go +lsp +tree-sitter)
       (org +roam2)
       (javascript +lsp)
       (rest +jq)
       emacs-lisp
       yaml
       json
       ess
       sh

       :email
       mu4e
       :app
       irc
       :config
       (default +smartparens +bindings))
