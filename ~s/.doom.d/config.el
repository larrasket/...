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
      user-first-name                                   (salih/user-first-name)
      org-roam-directory                                (file-truename "~/roam")
      srht-username                                     user-short-username

      ;; appearanc
      ;; font `:size` value of 29 is prefect for filming
      ;; with high dpi use (set-frame-font "PragmataPro Mono Liga")
      ;; or just remove `:size`.
      doom-font                                         "Pragmata Pro:pixelsize=12:antialias=true:hinting=true:autohint=false:hintstyle=3"
      doom-theme                                        'modus-vivendi
      +doom-dashboard-ascii-banner-fn                   'salih/banner
      display-line-numbers-type                         'nil
      all-the-icons-color-icons                         nil
      treemacs-position                                 'right

      ;; prayer time
      calendar-latitude                                 30.0
      calendar-longitude                                31.2
      awqat-mode-line-format                            " ${prayer} (${hours}h${minutes}m) "
      salih/awqat-show-mode-line                        t

      ;; org
      +org-capture-journal-file                         (salih/path-blog "stack.org")
      +org-capture-changelog-file                       (salih/path-blog "nice.org")
      +org-capture-todo-file                            (salih/path-roam "main" "life.org")
      org-id-locations-file                             (salih/path-roam ".orgids")
      org-directory                                     org-roam-directory
      ;; this option is useful when you are up after 00:00. set 0 to the value
      ;; yoe sleep at. if you sleep at 02:00 it should be 2, if you sleep at
      ;; 02:30 it should be 3 and so on. Org agenda for the day will not overlap
      ;; until your day is done.
      org-extend-today-until                            0



      ;; school
      salih/source-directory                            (salih/path-roam "source")
      salih/books                                       (salih/path-list salih/source-directory)
      bibtex-completion-bibliography                    (salih/path-configs "ref.bib")
      bibtex-completion-notes-path                      (salih/path-roam "references")
      org-cite-csl-styles-dir                           (salih/path-configs "assets" "csl")
      bibtex-completion-library-path                    `(,salih/source-directory)
      org-cite-global-bibliography                      `(,bibtex-completion-bibliography)
      citar-bibliography                                bibtex-completion-bibliography
      org-cite-csl--fallback-style-file                 (f-join org-cite-csl-styles-dir "chicago-ibid.csl")

      ;; modus theme
      modus-themes-bold-constructs                      t
      modus-themes-fringes                              nil
      modus-themes-italic-constructs                    t
      modus-themes-org-blocks                           'gray-background
      modus-themes-common-palette-overrides             '((border-mode-line-active unspecified) (border-mode-line-inactive unspecified))

      ;; indent highlight
      indent-bars-highlight-current-depth               nil
      indent-bars-treesit-support                       t

      ;; other
      safe-local-variable-values                        '((org-download-image-dir
                                                           . "../i")
                                                          (salih/rebuild . t)))





  

(require '+sets-email)                  ; mu4e
(require '+sets-school)                 ; school settings (TeX)
(require '+sets-org)                    ; org mode settings
(require '+sets-inhibit)                ; other settings
(require '+bind)                        ; set doom bindings (not mine)
(require '+helper)                      ; my salih/*


(require '+hooks)
(require '+advice)
(require '+bindings)
(require '+custom)
(require '+erc)
(require '+deep)
