;;; init.el -*- lexical-binding: t; -*-
(doom! :input
       :completion
       company
       (vertico +icons)

       :ui
       vc-gutter                        ; git annotions
       doom
       doom-dashboard
       hl-todo
       (ligatures +pragmata-pro)
       (popup +defaults)
       modeline
       workspaces

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
       (spell +flyspell)

       :tools
       biblio
       tree-sitter
       (lookup +dictionary +offline)
       (lsp +peak)
       magit
       pdf

       :os
       (tty)

       :lang
       (clojure +lsp +tree-sitter)
       (latex +flod +latexmk +lsp)
       (julia +lsp +tree-sitter)
       (cc +lsp +tree-sitter)           ; C [2024-08-29 Thu 14:46] why I've this "C"? here
       (csharp +lsp +dotnet +tree-sitter)
       (go +lsp +tree-sitter)
       (javascript +lsp)
       (org +roam2 +noter)
       (rest +jq)
       emacs-lisp
       yaml
       json
       ess                              ; R and other stuff
       sh

       :email
       mu4e
       :app
       irc
       :config
       (default +smartparens))
