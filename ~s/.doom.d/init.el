;;; init.el -*- lexical-binding: t; -*-
(doom! :input
       :completion
       company
       (vertico +icons)

       :ui
       vc-gutter                        ; git annotions
       doom                             ; doom emacs
       doom-dashboard
       hl-todo                          ; make TODO, FIXME, KILLYOURSELF, more
                                        ; visible in your source tree.
       (ligatures +pragmata-pro)
       (popup +defaults)                ; doom better management for pop-ups
       modeline
       workspaces

       :editor
       (evil +everywhere)
       file-templates                   ; automatically fill files with sane
                                        ; templates
       fold                             ; activate za in evil mode
       lispy                            ; better lisp edit
       parinfer                         ; (better lisp edit)
       snippets
       word-wrap

       :emacs
       dired
       electric                         ; better tabs
       undo                             ; better undo tree
       vc

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
       (latex +flod +latexmk +lsp)
       (julia +lsp +tree-sitter)
       (java +lsp +tree-sitter)
       (cc +lsp +tree-sitter)           ; C
       (go +lsp +tree-sitter)
       (org +roam2 +noter)
       (rest +jq)
       common-lisp
       emacs-lisp
       yaml
       json
       ess                              ; R and other stuff
       sh

       :email
       mu4e

       :config
       (default +smartparens))
