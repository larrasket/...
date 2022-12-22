;;; init.el -*- lexical-binding: t; -*-
(setq inhibit-automatic-native-compilation t)
(doom! :input
       :completion
       company                          ; the ultimate code completion backend
       ivy
       vertico
       :ui
       doom                             ; what makes DOOM look the way it does
       doom-dashboard                   ; a nifty splash screen for Emacs
       hl-todo                          ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       (ligatures
        +pragmata-pro)
       modeline                         ; snazzy, Atom-inspired modeline, plus API
       (popup +defaults)                ; tame sudden yet inevitable temporary windows
       vi-tilde-fringe                  ; fringe tildes to mark beyond EOB
       window-select                    ; visually switch windows
       workspaces                       ; tab emulation, persistence & separate workspaces

       :editor
       (evil
        +everywhere)                    ; come to the dark side, we have cookies
       file-templates                   ; auto-snippets for empty files
       fold                             ; (nigh) universal code folding
       (format +onsave)                 ; automated prettiness
       lispy                            ; vim for lisp, for people who don't like vim
       parinfer                         ; turn lisp into python, sort of
       snippets                         ; my elves. They type so I don't have to

       :emacs
       (dired
        +icons)
       electric                      ; smarter, keyword-based electric-indent
       (ibuffer +icon)               ; interactive buffer management
       undo                          ; persistent, smarter undo for your inevitable mistakes
       vc                            ; version-control and Emacs, sitting in a tree

       :term
       eshell                           ; the elisp shell that works everywhere
       vterm                            ; the best terminal emulation in Emacs

       :checkers
       (syntax +childframe)             ; tasing you for every semicolon you forget
       (spell
        +flyspell)                      ; tasing you for misspelling mispelling
       grammar                          ; tasing grammar mistake every you make

       :tools
       gist                             ; interacting with github gists
       (lookup
        +dictionary
        +offline)                       ; navigate your code and its documentation
       (lsp +peak)
       (magit +forge)                   ; a git porcelain for Emacs
       pdf                              ; pdf enhancements
       rgb                              ; creating color strings
       ;;terraform         ; infrastructure as code

       :os
       (tty)                            ; improve the terminal Emacs experience
       :lang
       (cc +lsp)                        ; C > C++ == 1
       common-lisp                      ; if you've seen one lisp, you've seen them all
       (csharp +lsp)                    ; unity, .NET, and mono shenanigans
       emacs-lisp                       ; drown in parentheses
       (go +lsp
           +tree-sitter)                ; the hipster dialect
       (json
        +lsp
        +tree-sitter)                   ; At least it ain't XML
       (latex +flod
              +latexmk
              +lsp)                     ; writing papers in Emacs has never been so fun
       markdown                         ; writing docs for people to ignore
       (org
        +roam2)
       (rest +jq)                       ; Emacs as a REST client
       (sh
        +lsp)                           ; she sells {ba,z,fi}sh shells on the C xor
       (yaml
        +lsp)    ; JSON, but readable
       ;;agda              ; types of types of types of types...
       ;;beancount         ; mind the GAAP
       ;;clojure           ; java with a lisp
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       ;;(haskell +dante)  ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       ;;javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       ;;lean              ; for folks with too much to prove
       ;;ledger            ; be audit you can be
       ;;lua               ; one-based indices? one-based indices
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       ;;python            ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       ;;rst               ; ReST in peace
       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;(scheme +guile)   ; a fully conniving family of lisps
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       ;;web               ; the tubes
       ;;zig               ; C, but simpler
       ;; :email           ; TODO
       ;; (mu4e +org)
       ;; notmuch
       ;; (wanderlust)

       :app
       irc                              ; how neckbeards socialize
       :config
       (default +bindings
                +smartparens)
       :private
       awqat
       compiler
       dired
       ;; email
       ;; feeds
       ;; flyivy
       gen
       genorg
       gitcolor
       langtool
       leetcode
       shell
       sql
       translate
       mytheme
       caption)


(setq package-native-compile t)
