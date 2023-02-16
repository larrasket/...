;;; init.el -*- lexical-binding: t; -*-
(setq inhibit-automatic-native-compilation t)
(doom! :input
       :completion
       company
       vertico
       :ui
       doom
       doom-dashboard
       hl-todo
       (ligatures
        +pragmata-pro)
       modeline
       (popup +defaults)
       vi-tilde-fringe
       window-select
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
       (dired
        +icons)
        ;; +ranger

       electric

       electric
       (ibuffer +icon)
       undo
       vc

       :term
       eshell
       vterm

       :checkers
       (syntax +childframe)
       (spell
        +flyspell)
       grammar

       :tools
       biblio
       gist
       (lookup
        +dictionary
        +offline)
       (lsp +peak)
       (magit +forge)
       ;; pdf
       rgb
       ;;terraform

       :os
       (tty)
       :lang
       (cc +lsp)
       common-lisp
       (csharp +lsp)
       emacs-lisp
       (go +lsp
           +tree-sitter)
       (json
        +lsp
        +tree-sitter)
       (latex +flod
              +latexmk
              +lsp)
       markdown
       (org
        +roam2)
       ;; +noter
       (rest +jq)
       (sh
        +lsp)
       (yaml
        +lsp)
       ;;agda
       ;;beancount
       ;;clojure
       ;;coq
       ;;crystal
       ;;data
       ;;(dart +flutter)
       ;;elixir
       ;;elm
       ;;erlang
       ;;ess
       ;;factor
       ;;faust
       ;;fsharp
       ;;fstar
       ;;gdscript
       ;;(haskell +dante)
       ;;hy
       ;;idris
       ;;(java +meghanada)
       ;;javascript
       ;;julia
       ;;kotlin
       ;;lean
       ;;ledger
       ;;lua
       ;;nim
       ;;nix
       ;;ocaml
       ;;php
       ;;plantuml
       ;;purescript
       ;;python
       ;;qt
       ;;racket
       ;;raku
       ;;rst
       ;;(ruby +rails)
       ;;rust
       ;;scala
       ;;(scheme +guile)
       ;;sml
       ;;solidity
       ;;swift
       ;;terra
       ;;web
       ;;zig
       ;; :email
       ;; (mu4e +org)

       ;; ;; notmuch
       ;; ;; (wanderlust)

       :app
       irc
       :config
       (default +bindings
                +smartparens)
       :private
       awqat
       compiler
       dired
       email
       gen
       genorg
       gitcolor
       langtool
       leetcode
       shell
       sql
       translate
       mytheme
       caption
       chess
       cite)



(setq package-native-compile t)
