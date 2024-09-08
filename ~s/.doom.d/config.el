(add-to-list 'load-path "~/.doom.d/lisp/")
(add-to-list 'load-path "~/.doom.d/pkg/")
(add-to-list 'doom-emoji-fallback-font-families "Symbola")
(require 'awqat)                        ; for prayer support in the agenda
(require 'vulpea)                       ; org-roam project tasks in org-agenda
(require '+early)                       ; personal utilities
(require 'go-translate)                 ; define trnaslation engine in config.el


(setq user-full-name                                    "Salih Muhammed"
      user-mail-address                                 "lr0@gmx.com"
      user-stmp-server                                  "mail.gmx.com"
      user-stmp-port                                    587
      user-short-username                               "lr0"
      user-config-repo-path                             "/home/l/configs/~s"
      salih/blog-content-path                           "~/blog/content"
      user-first-name                                   (s/ufn)
      org-roam-directory                                (file-truename "~/roam")
      srht-username                                     user-short-username

      ;; appearanc
      ;; font `:size` value of 29 is prefect for filming
      ;; with high dpi use `(set-frame-font "PragmataPro Mono Liga")`
      ;; or just remove `:size`.
      ;; [2024-08-06 Tue 06:33] `ef-deuteranopia-light' is an amazing light theme.
      ;; [2024-09-01 Sun 00:43] `doom-rouge` is an amazing dark theme
      ;; [2024-09-02 Mon 03:01] and `ef-elea-dark` too.
      ;; [2024-09-04 Wed 02:03] `ef-maris-dark` too.
      doom-theme                                        (car (salih/get-random-theme 0))
      doom-font                                         "Pragmasevka:pixelsize=17:antialias=true:hinting=true:autohint=false:hintstyle=3"
      doom-unicode-font                                 "Amiri UI:pixelsize=16:antialias=true:hinting=true:autohint=false:hintstyle=3"
      doom-variable-pitch-font                          (font-spec :family "Arimo")
      doom-modeline-height                              27
      display-line-numbers-type                         (when (s/not
                                                                (doom-theme?)
                                                                (< (random 100) 90))
                                                          'relative)

      ;; prayer time
      calendar-latitude                                 30.0
      calendar-longitude                                31.2
      salih/awqat-show-mode-line                        t

      ;; org
      org-directory                                     org-roam-directory
      org-id-locations-file                             (s/pr ".orgids")
      +org-capture-changelog-file                       (s/pb "nice.org")
      +org-capture-journal-file                         (s/pb "stack.org")
      salih/org-roam-fleet-file                         (s/pr "main" "lr.org")
      +org-capture-todo-file                            (s/pr "main" "life.org")

      ;; this option is useful when you are up after 00:00. set 0 to the value
      ;; yoe sleep at. if you sleep at 02:00 it should be 2, if you sleep at
      ;; 02:30 it should be 3 and so on. Org agenda for the day will not overlap
      ;; until your day is done.
      ;; [2024-08-07 Wed 19:43] currently I sleep at 07:00.
      ;; [2024-08-08 Wed 23:41] Not anymore.
      ;; org-extend-today-until                            7

      ;; school
      salih/source-directory                            (s/pr "source")
      salih/books                                       (s/pl salih/source-directory)
      bibtex-completion-bibliography                    (s/pc "ref.bib")
      bibtex-completion-notes-path                      (s/pr "references")
      org-cite-csl-styles-dir                           (s/pc "assets" "csl")
      bibtex-completion-library-path                    (l salih/source-directory)
      org-cite-global-bibliography                      (l bibtex-completion-bibliography)
      citar-bibliography                                bibtex-completion-bibliography
      org-cite-csl--fallback-style-file                 (f-join org-cite-csl-styles-dir "chicago-ibid.csl")
      org-fc-flashcard-tag                              "drill"
      org-fc-directories                                (l (s/pr "main")
                                                           (s/pr "other")
                                                           (s/pr "references"))

      ;; other
      auto-save-no-message                              t
      dired-preview-delay                               0.1
      safe-local-variable-values                        '((org-download-image-dir
                                                           . "../i")
                                                          (salih/rebuild . t))
      ;; currently org causes some annoying warnings because of org-element
      ;; breaking api updates.
      ;; [2024-04-26 Fri 02:01] I wrote "currently" above a long time ago
      ;; (perhaps can be detected from the git history, too lazy tho). Not sure
      ;; if it is still the case
      warning-minimum-level                             :error)


(s/require
 '+sets-email                           ; mu4e
 '+sets-school                          ; school settings (TeX & BibTeX)
 '+sets-org                             ; org mode settings
 '+sets-inhibit                         ; other settings
 '+bind '+helper '+advice '+bindings '+custom '+erc '+deep '+line
 (unless (featurep 'tadwin) '+hooks))
