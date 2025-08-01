;;; +l-academic-citar.el -*- lexical-binding: t; -*-

;; Citar configuration
(use-package citar
  :custom
  (citar-bibliography bibtex-completion-bibliography)
  (citar-templates
   `((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
     (suffix
      . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
     (preview . ,(concat "${author editor} (${year issued date}) ${title},"
                         " ${journal journaltitle publisher "
                         "container-title collection-title}.\n"))
     (note . "Notes on ${author editor}, ${title}")))
  (citar-symbol-separator "  ")
  :config
  ;; Citar indicators
  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (nerd-icons-mdicon "nf-md-notebook" :face 'nerd-icons-blue :v-adjust -0.3)
     :function #'citar-has-notes :padding "  " :tag "has:notes"))

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

;; Citar-org-roam configuration
(use-package citar-org-roam
  :after (citar org-roam)
  :no-require t
  :config
  (citar-org-roam-mode)
  (setq citar-org-roam-note-title-template
        "${title}\n ${title} is a book by ${author}\n * ${title}\n "))

(provide '+l-academic-citar)
