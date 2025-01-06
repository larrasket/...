(package! wiki-summary)                 ; get a wiki line summary.
(package! iedit)                        ; JetBrains' rename function.
(package! git-auto-commit-mode)         ; auto commit

;; [2024-10-15 Tue 06:19] I'm back to using the native emacs way
;; (package! column-enforce-mode)          ; instead of `display-fill-column`


;; [2024-10-15 Tue 06:19] I'm not bein a mathematician anymore (for now )
;; (package! sage-shell-mode)              ; I like to be a mathematician
;; (package! maxima)                       ; a maxima repl.. not sure about it


;; [2024-10-15 Tue 06:17]  commented both currently as I'm not using tabs
;; (package! pretty-hydra)                 ; needed for the next line
;; (package! centaur-tabs)                 ; nice tabs


;; [2024-10-15 Tue 06:18] Not writing in Julia anymore.
;; (package! ob-julia-vterm)               ; for usage, see maxima or sage-shell

(package! go-translate)                 ; google API for translations

(package! srht)                         ; sr.ht support. Good for paste.sr.ht
(package! format-all)                   ; destroy my code
(package! lsp-treemacs)                 ; make emacs loks like and EDE (ide)
(package! modus-themes)                 ; my favorite themes from my greek dude

(package! awqat
  :recipe (:host github
           :repo "luggages/awqat"))     ; obeying the almighty.


(unpin! vertico)
(package! vertico)                      ; not sure why I unpin this

(package! emacs-neotree)                ; better side panal


(package! evil-snipe :disable t)        ; don't remember why I disabled it
(package! ef-themes)                    ; sick themes
(package! breadcrumb)                   ; sick line

(package! kaolin-themes)                ; sick sick themes
(package! mixed-pitch)                  ; I like to use variable pitch for org


(package! cocaine-line
  :recipe
  (:host github
   :repo "luggages/cocaine-line"))      ; sick modeline

(package! mu4e-alert)

(package! dired-preview)

(unpin! parsebib)

(package! git-gutter)
(package! git-gutter-fringe)

(package! jinx)
