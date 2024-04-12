(add-to-list 'load-path "~/.doom.d/lisp/")
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

      doom-theme                                        (salih/get-random-theme)
      doom-font                                         "Pragmasevka:pixelsize=16:antialias=true:hinting=true:autohint=false:hintstyle=3"
      doom-unicode-font                                 "Noto Naskh Arabic UI:pixelsize=16:antialias=true:hinting=true:autohint=false:hintstyle=3"
      doom-modeline-height                              27
      centaur-tabs-height                               22
      +doom-dashboard-ascii-banner-fn                   'salih/banner
      display-line-numbers-type                         nil
      all-the-icons-color-icons                         nil
      treemacs-position                                 'right

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
      ;; org-extend-today-until                            9

      ;; school
      salih/source-directory                            (s/pr "source")
      salih/books                                       (s/pl salih/source-directory)
      bibtex-completion-bibliography                    (s/pc "ref.bib")
      bibtex-completion-notes-path                      (s/pr "references")
      org-cite-csl-styles-dir                           (s/pc "assets" "csl")
      bibtex-completion-library-path                    `(,salih/source-directory)
      org-cite-global-bibliography                      `(,bibtex-completion-bibliography)
      citar-bibliography                                bibtex-completion-bibliography
      org-cite-csl--fallback-style-file                 (f-join
                                                         org-cite-csl-styles-dir
                                                         "chicago-ibid.csl")
      modus-themes-org-blocks                           'gray-background
      modus-themes-common-palette-overrides             '((bg-mode-line-active bg-inactive)
                                                          (fg-mode-line-active fg-main)
                                                          (bg-mode-line-inactive bg-inactive)
                                                          (fg-mode-line-active fg-dim)
                                                          (bg-line-number-active unspecified)
                                                          (bg-line-number-inactive unspecified)
                                                          (border-mode-line-active bg-main)
                                                          (border-mode-line-inactive bg-inactive))
                                                          

      ;; indent highlight
      indent-bars-highlight-current-depth               nil
      indent-bars-treesit-support                       t

      ;; other
      safe-local-variable-values                        '((org-download-image-dir
                                                           . "../i")
                                                          (salih/rebuild . t))
      ;; currently org causes some annoying warnings because of org-element
      ;; breaking api updates.
      warning-minimum-level                             :error)
      
 

(require '+sets-email)                  ; mu4e
(require '+sets-school)                 ; school settings (TeX)
(require '+sets-org)                    ; org mode settings
(require '+sets-inhibit)                ; other settings

(require '+bind)
(require '+helper)
(unless (featurep 'tadwin) (require '+hooks))
(require '+advice)
(require '+bindings)
(require '+custom)
(require '+erc)
(require '+deep)
(require '+line)

