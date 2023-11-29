(package! wakatime-mode)                ; voluntary tracking
(package! org-web-tools)                ; viewing urls in org

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

(package! awqat
  :recipe (:host github
           :repo "luggages/awqat"))     ; obeying the almighty.

(unpin! evil-collection)                ; fixes evil for mu4e
(package! evil-collection
  :recipe (:repo "luggages/evil-collection"
           :branch "mu4e-development"))


(package! ox-tufte
  :recipe (:host github
           :repo "luggages/ox-tufte"))  ; for my website.

(package! ox-html-stable-ids
  :recipe (:host github
           :repo "larrasket/ox-html-stable-ids.el"))

(package! centaur-tabs)                 ; nice tabs

(package! vulpea)                       ; roam advanced functions
(package! nov)                          ; browse epubs from emacs
(package! org-bullets)                  ; nicer org view

(package! maxima)                       ; a maxima repl.. not sure about it (and
                                        ; about using maxima in general)


(package! pretty-hydra)                 ; for the next line

(package! org-media-note
  :recipe (:host github
           :repo "yuchen-lea/org-media-note"))


(package! indent-bars
  :recipe (:host github
           :repo "jdtsmith/indent-bars"))


(package! ob-julia-vterm)
(unpin! ess)
(package! ess)



(package! distinguished-theme)
(package! kaolin-themes)
(unpin! vertico)
(package! vertico)

(package! srht)

(package! org-drill)

(package! lsp-treemacs)

(package! format-all)



(package! explain-pause-mode
  :recipe (:host github
           :repo "lastquestion/explain-pause-mode"))

(package! modus-themes)
