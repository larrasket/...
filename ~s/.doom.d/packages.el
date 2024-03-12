(package! wiki-summary)                 ; get a wiki line summary for new
                                        ; topics. also ueful when writing notes
                                        ; about new things

(package! iedit)                        ; JetBrains' rename function.
                                        ; Also see `salih/rename-or-iedit`

(package! git-auto-commit-mode)         ; auto commit

(package! grip-mode)                    ; I do not remember what's that for but
                                        ; sounds useful

(package! column-enforce-mode)          ; I use this instead of
                                        ; `display-fill-column-indicator`

(package! go-translate)                 ; google API for lingustics

(package! consult-org-roam)             ; useful org-roam functions

(package! sage-shell-mode)              ; for me when I like to be a
                                        ; mathematician

(package! maxima)                       ; a maxima repl.. not sure about it (and
                                        ; about using maxima in general)

(package! nov)                          ; browse epubs from emacs
(package! srht)                         ; sr.ht support. Good for paste.sr.ht
(package! vulpea)                       ; roam advanced functions
(package! org-drill)                    ; anki
(package! format-all)                   ; destroy my code
(package! org-bullets)                  ; nicer org view
(package! lsp-treemacs)                 ; make emacs loks like and EDE (ide)
(package! pretty-hydra)                 ; for the next line
(package! centaur-tabs)                 ; nice tabs
(package! modus-themes)                 ; my favorite themes from my greek dude
(package! wakatime-mode)                ; voluntary tracking
(package! org-web-tools)                ; viewing urls in org
(package! ob-julia-vterm)               ; for usage, see maxima or sage-shell

(package! org-media-note
  :recipe (:host github
           :repo "yuchen-lea/org-media-note"))

(package! indent-bars
  :recipe (:host github
           :repo "jdtsmith/indent-bars"))

(package! explain-pause-mode
  :recipe (:host github
           :repo "lastquestion/explain-pause-mode"))

(package! dired-auto-readme
  :recipe (:host github
           :repo "amno1/dired-auto-readme"))

(package! ox-tufte
  :recipe (:host github
           :repo "luggages/ox-tufte"))  ; for my website.

(package! ox-html-stable-ids
  :recipe (:host github
           :repo "larrasket/ox-html-stable-ids.el"))

(package! awqat
  :recipe (:host github
           :repo "luggages/awqat"))     ; obeying the almighty.


(unpin! vertico)
(package! vertico)

(unpin! ess)
(package! ess)

(package! company-box)





(package! org-ref)
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))
(unpin! org-roam)

(unpin! bibtex-completion helm-bibtex ivy-bibtex)
(package! citar)
(package! citar-org-roam)


(package! emacs-neotree)
(package! csv-mode)


(package! ob-mermaid)
(package! plantuml-mode)
(package! org-download)


(package! git-gutter)
(package! git-gutter-fringe)

(package! org-ql
  :recipe
  (:host github
   :repo "larrasket/org-ql"
   :branch "priority"))


(unpin! org-roam)
(package! org-roam-ui)
(package! emacsql-sqlite3)
(package! emacsql-sqlite-module)
(package! evil-snipe :disable t)
(package! ef-themes)
(package! breadcrumb)

(package! spacious-padding)
