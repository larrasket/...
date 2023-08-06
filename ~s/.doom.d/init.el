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


       electric
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
       tree-sitter
       (lookup
        +dictionary
        +offline)
       (lsp +peak)
       (magit)
       pdf

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
       ;;markdown
       (org
        +roam2)
       (rest +jq)
       (sh
        +lsp)
       (yaml
        +lsp)
       (julia
        +lsp
        +tree-sitter)
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
;
      ;; src==irc
      ;; for the options `erc-prompt-for-password' and
      ;; `erc-prompt-for-nickserv-password', you should have a .authinfo file
      ;; conatins your password for the nickname. For Example, using
      ;; yournickname and yourpassword as a NickName and password (repsectively
      ;; machine irc.libera.chat login yournickname password yourpassword
      ;; See. https://www.gnu.org/software/emacs/manual/html_node/emacs/Authentication.html
       src
       gen
       genorg
       gitcolor
       chess
       cite
       roam
       bind)



(setq package-native-compile t)
