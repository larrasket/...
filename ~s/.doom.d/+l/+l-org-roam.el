;;; +l-org-roam.el -*- lexical-binding: t; -*-

;; Org-roam configuration
(use-package org-roam
  :config
  (require 'vulpea)
  (defun salih/org-roam-node-insert ()
    (interactive)
    (setq salih/temp-roam-insert t)
    (consult-buffer (list org-roam-buffer-source)))
  :custom
  (org-roam-capture-templates
   '(("k" "knowledge" plain "%?"
      :if-new
      (file+head "main/${slug}.org"
                 "#+title: ${title}\n#+FILETAGS: permanent")
      :immediate-finish t
      :unnarrowed t)
     ("e" "encrypted knowledge" plain "%?"
      :if-new
      (file+head "main/${slug}.org.gpg"
                 "#+title: ${title}\n#+FILETAGS: permanent")
      :immediate-finish t
      :unnarrowed t)
     ("l" "links" plain "%?"
      :if-new
      (file+head "things/${slug}.org"
                 "#+title: ${title}\n#+FILETAGS: link\n")
      :immediate-finish t
      :unnarrowed t)
     ("f" "fleeting" plain "%?"
      :target
      (file+olp "main/lr.org" ("notes" "${title}"))
      :immediate-finish t
      :unnarrowed nil)
     ("r" "bibliography reference" plain
      (file "~/configs/~s/orb")
      :target
      (file+head "references/${citekey}.org" "#+title: ${title}\n")))))

;; Org-roam-bibtex configuration
(use-package org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref))

;; Org-roam capture hooks
(add-hook! 'org-roam-capture-new-node-hook
  (setq roam-titles
        (salih/org-roam-get-node-titles
         (org-roam-node-read--completions))))

;; Org-ql advice
(advice-add 'org-ql-view--format-element
            :around #'salih/org-ql-view--format-element)

(provide '+l-org-roam)
