;;; +l-org-roam.el -*- lexical-binding: t; -*-

;; Org-roam configuration
(use-package org-roam
  :after consult
  :config

  (setq roam-titles (salih/org-roam-get-node-titles
                     (org-roam-node-read--completions)))
  (defun salih/get-org-roam-titles () roam-titles)

  (setq consult-org-roam-buffer-narrow-key ?r)
  (setq org-roam-buffer-source
        `(:name     "Org-roam"
          :hidden   nil
          :narrow   ,consult-org-roam-buffer-narrow-key
          :annotate ,(lambda (cand)
                       (let* ((name (org-roam-node-from-title-or-alias cand)))
                         (if name (file-name-nondirectory
                                   (org-roam-node-file name)) "")))

          :action ,(lambda (name)
                     (if salih/temp-roam-insert
                         (progn
                           (setq salih/temp-roam-insert nil)
                           (let* ((node (org-roam-node-from-title-or-alias name))
                                  (description (org-roam-node-title node))
                                  (id (org-roam-node-id node)))
                             (insert (org-link-make-string
                                      (concat "id:" id)
                                      description))
                             (run-hook-with-args 'org-roam-post-node-insert-hook
                                                 id
                                                 description)))
                       (org-roam-node-visit
                        (org-roam-node-from-title-or-alias name))))

          :new ,(lambda (name)
                  (let* ((n (org-roam-node-create :title name)))
                    (org-roam-capture- :node n)
                    (when salih/temp-roam-insert
                      (progn
                        (setq salih/temp-roam-insert nil)
                        (let* ((node (org-roam-node-from-title-or-alias name))
                               (description (org-roam-node-title node))
                               (id (org-roam-node-id node)))
                          (insert (org-link-make-string
                                   (concat "id:" id)
                                   description))
                          (run-hook-with-args 'org-roam-post-node-insert-hook
                                              id
                                              description)))))


                  (setq roam-titles (salih/org-roam-get-node-titles
                                     (org-roam-node-read--completions))))

          :items    ,#'salih/get-org-roam-titles))
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
