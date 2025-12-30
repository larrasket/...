;;; +l-academic-bibtex.el -*- lexical-binding: t; -*-

;; Bibliography configuration
;;; Code:
(use-package bibtex-completion
  :custom
  (bibtex-completion-bibliography "~/configs/~s/ref.bib")
  (bibtex-completion-notes-path "~/roam/references")
  (bibtex-completion-library-path (l salih/source-directory))
  (bibtex-completion-additional-search-fields '(keywords))
  (bibtex-completion-notes-template-multiple-files
   (concat "* ${author-or-editor}, ${title}, "
           "${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"))
  (bibtex-completion-display-formats
   '((article . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
     (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
     (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
     (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
     (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))))

;; Bibtex configuration
(use-package bibtex
  :custom
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-name-year-separator "-")
  (bibtex-autokey-year-title-separator "-")
  (bibtex-autokey-titleword-separator "-")
  (bibtex-autokey-titlewords 2)
  (bibtex-autokey-titlewords-stretch 1)
  (bibtex-autokey-titleword-length 5))

;; Org-cite configuration
(use-package org
  :config
  (setq
   bibtex-completion-library-path (l salih/source-directory)
   org-cite-global-bibliography (l bibtex-completion-bibliography)
   jinx-languages                                    "en_US ar_EG")

  :custom
  (org-cite-global-bibliography bibtex-completion-bibliography)
  (org-cite-csl-styles-dir "~/configs/~s/.pandoc/csl")
  (org-cite-csl--fallback-style-file
   (f-join org-cite-csl-styles-dir "chicago-ibid.csl")))

(provide '+l-academic-bibtex)
