                                        ; -*- no-byte-compile: t; -*-

(package! wakatime-mode)

(package! org-web-tools)
(package! org-present)
(package! org-preview-html)

(package! wiki-summary)

(package! iedit)

(package! protobuf-mode)
(package! git-auto-commit-mode)
(package! highlight-indent-guides)
(package! grip-mode)
(package! column-enforce-mode)








(package! go-translate)
(package! distinguished-theme)

(unpin! vertico)
(package! vertico)

(package! consult-org-roam)

(package! sage-shell-mode)
(package! leetcode)
(package! awqat
  :recipe (:host github
           :repo "luggages/awqat"))
(package! ob-sagemath)
(package! lsp-jedi)


(unpin! evil-collection)
(package! evil-collection
  :recipe (:repo "luggages/evil-collection" :branch "mu4e-development"))


(package! ox-tufte
  :recipe (:host github
           :repo "luggages/ox-tufte"))

(package! ox-html-stable-ids
  :recipe (:host github
           :repo "larrasket/ox-html-stable-ids.el"))
(package! centaur-tabs)

(package! vulpea)


(package! nov)

(package! gomacro-mode)


(package! org-bullets)
(package! elfeed-tube)

(package! maxima)


(package! pretty-hydra)  ;; dependency
(package! org-media-note :recipe (:host github :repo "yuchen-lea/org-media-note"))
(package! ob-julia-vterm)


(unpin! ess)
(package! ess)

(package! kaolin-themes)
