;;; +l-academic-citar.el -*- lexical-binding: t; -*-

;; Citar configuration
(require 'citar-org-roam)


(use-package citar-org-roam
  :no-require
  :config (citar-org-roam-mode))

(use-package! org-roam-bibtex
  :config
  (require 'org-ref))


(use-package citar-org-roam
  :config (citar-org-roam-mode)
  :custom
  (setq bibtex-completion-notes-template-multiple-files
        (concat "* ${author-or-editor}, ${title}, "
                "${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n")

        citar-bibliography "~/configs/~s/ref.bib"


        orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword                          t
        orb-attached-file-extensions                      '("pdf")
        citar-org-roam-note-title-template
        "${title}\n ${title} is a book by ${author}\n * ${title}\n "

        citar-templates
        `((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
          (suffix
           . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
          (preview . ,(concat "${author editor} (${year issued date}) ${title},"
                              " ${journal journaltitle publisher "
                              "container-title collection-title}.\n"))
          (note . "Notes on ${author editor}, ${title}"))

        bibtex-autokey-year-length                        4
        citar-symbol-separator                            "  "
        bibtex-autokey-name-year-separator                "-"
        bibtex-autokey-year-title-separator               "-"
        bibtex-autokey-titleword-separator                "-"
        bibtex-autokey-titlewords                         2
        bibtex-autokey-titlewords-stretch                 1
        bibtex-autokey-titleword-length                   5
        bibtex-completion-additional-search-fields        '(keywords)


        bibtex-completion-display-formats
        '((article . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}")))


  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (nerd-icons-mdicon
              "nf-md-notebook"
              :face 'nerd-icons-blue
              :v-adjust -0.3)
     :function #'citar-has-notes
     :padding "  "
     :tag "has:notes"))

  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (nerd-icons-octicon
              "nf-oct-link"
              :face 'nerd-icons-orange
              :v-adjust -0.1)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))

  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-file"
              :face 'nerd-icons-green
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  "
     :tag "has:files"))

  (setq citar-indicators
        (list citar-indicator-files-icons
              citar-indicator-notes-icons
              citar-indicator-links-icons)))

(use-package! org-roam-bibtex
  :config
  (require 'org-ref))

(provide '+l-academic-citar)
