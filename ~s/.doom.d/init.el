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
       ;; (ibuffer +icon)
       undo
       vc

       :term
       eshell
       vterm

       :checkers
       (syntax +childframe)
       (spell
        +flyspell)

       :tools
       biblio
       ;; gist
       tree-sitter
       (lookup
        +dictionary
        +offline)
       (lsp +peak)
       (magit)
       ;; pdf
       ;; rgb
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
       ;;markdown
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
       (julia
        +lsp
        +tree-sitter)
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
       :email
       mu4e


       ;; ;; notmuch
       ;; ;; (wanderlust)

       :app
       ;; irc
       :config
       (default ;; +bindings
                +smartparens)
       :private
       compiler
;
      ;; src==irc
      ;; for the options `erc-prompt-for-password' and
      ;; `erc-prompt-for-nickserv-password', you should have a .authinfo file
      ;; conatins your password for the nickname. For Example, using
      ;; yournickname and yourpassword as a NickName and password (repsectively
      ;; machine irc.libera.chat login yournickname password yourpassword
      ;; See. https://www.gnu.org/software/emacs/manual/html_node/emacs/Authentication.html
       ;; src
       gen
       genorg
       gitcolor
       chess
       cite
       pdf
       roam
       bind)



(setq package-native-compile t)
