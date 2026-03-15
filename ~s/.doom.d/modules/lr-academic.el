(after! bibtex-completion
  (setq bibtex-completion-bibliography "~/configs/~s/ref.bib"
        bibtex-completion-notes-path "~/roam/references"
        bibtex-completion-library-path (list salih/source-directory)
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-notes-template-multiple-files
        (concat "* ${author-or-editor}, ${title}, "
                "${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n")
        bibtex-completion-display-formats
        '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))))

;;; --- Bibtex autokey ---
(after! bibtex
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5))

;;; --- Org-cite ---
(after! org
  (setq org-cite-global-bibliography (list "~/configs/~s/ref.bib")
        jinx-languages "en_US ar_EG")

  (setq org-cite-csl-styles-dir "~/configs/~s/.pandoc/csl"
        org-cite-csl--fallback-style-file
        (expand-file-name "chicago-ibid.csl" "~/configs/~s/.pandoc/csl")))

;;; --- Citar ---
(after! citar
  (setq citar-bibliography "~/configs/~s/ref.bib"
        citar-symbol-separator "  "
        citar-templates
        `((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
          (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
          (preview . ,(concat "${author editor} (${year issued date}) ${title},"
                              " ${journal journaltitle publisher "
                              "container-title collection-title}.\n"))
          (note . "Notes on ${author editor}, ${title}")))

  ;; Citar indicators
  (setq citar-indicators
        (list
         (citar-indicator-create
          :symbol (nerd-icons-faicon "nf-fa-file" :face 'nerd-icons-green :v-adjust -0.1)
          :function #'citar-has-files :padding "  " :tag "has:files")
         (citar-indicator-create
          :symbol (nerd-icons-mdicon "nf-md-notebook" :face 'nerd-icons-blue :v-adjust -0.3)
          :function #'citar-has-notes :padding "  " :tag "has:notes")
         (citar-indicator-create
          :symbol (nerd-icons-octicon "nf-oct-link" :face 'nerd-icons-orange :v-adjust -0.1)
          :function #'citar-has-links :padding "  " :tag "has:links"))))

;;; --- Citar-org-roam ---
(after! citar-org-roam
  (setq citar-org-roam-note-title-template
        "${title}\n ${title} is a book by ${author}\n * ${title}\n ")

  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t
        orb-attached-file-extensions '("pdf"))

  (citar-org-roam-mode))

(provide 'lr-academic)
