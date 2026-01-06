;;; init.el -*- lexical-binding: t; -*-
(doom! :input
       :completion
       vertico
       ;; company
       (corfu +icons)

       :ui
       (vc-gutter +pretty)              ; git annotions
       doom
       doom-dashboard
       hl-todo
       (ligatures +pragmata-pro)
       (popup +defaults)
       modeline
       workspaces
       indent-guides

       :editor
       (evil +everywhere +respect-visual)
       file-templates
       fold                             ; activate za in evil mode
       lispy                            ; better lisp edit
       parinfer                         ; (better lisp edit)
       snippets
       word-wrap
       (format +onsave)

       :emacs
       (dired +icons)
       electric                         ; better tabs (as in tabs vs spaces)
       undo
       vc                               ; magit

       :term
       eshell
       vterm

       :checkers
       (syntax +childframe +icons)
       ;; (spell  +flyspell)

       :tools
       biblio
       terraform
       tree-sitter
       (lookup +dictionary +offline)
       (lsp +peak)
       magit
       pdf

       :os
       (tty)

       :lang
       (latex +flod +latexmk +lsp)
       (clojure +lsp +tree-sitter)
                                        ;(julia +lsp +tree-sitter)
       (javascript +lsp +tree-sitter)
       (cc +lsp +tree-sitter)
       (go +lsp +tree-sitter)
       (org +roam2 +pandoc)
       (rest +jq)
       (graphql +lsp +tree-sitter)
       (scala +lsp +tree-sitter)
       json
       web
       yaml
       emacs-lisp
       yaml
       json
       ess
       sh

       :app
       irc

       :config
       (default +smartparens +bindings))

(add-to-list 'load-path (expand-file-name "lisp" doom-user-dir))
(add-to-list 'load-path (expand-file-name "pkg"  doom-user-dir))
(add-to-list 'load-path (expand-file-name "+l"  doom-user-dir))
