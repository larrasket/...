;;; +l-org-roam.el -*- lexical-binding: t; -*-

;; Org-roam configuration
(use-package org-roam
  :after consult
  :config

  (setq org-roam-dailies-capture-templates
        `(("d" "default" entry "* %<%H:%M> \n %?"
           :if-new
           (file+head
            "%<%Y-%m-%d>.org.gpg"
            ,(concat "#+title: %<%Y-%m-%d>\n#+DATE: <%<%Y-%m-%d>>"
                     "\n#+FILETAGS: journal\n- tags :: [[roam:Journaling]] \n"))
           :unnarrowed t))
        ;; org-roam-database-connector             'sqlite
        org-roam-dailies-directory              "journal/")

  (setq roam-titles (mapcar #'org-roam-node-title (org-roam-node-list))
        org-roam-dailies-directory "~/roam/journal")
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


                  (setq roam-titles (mapcar #'org-roam-node-title (org-roam-node-list))))

          :items    ,#'salih/get-org-roam-titles))
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
  (setq roam-titles (mapcar #'org-roam-node-title (org-roam-node-list))
        org-roam-dailies-directory "~/roam/journal"))

(add-hook! 'org-roam-find-file-hook #'git-auto-commit-mode)

;; Org-ql advice
(advice-add 'org-ql-view--format-element
            :around #'salih/org-ql-view--format-element)




(use-package vulpea
  :after  org-roam
  :config
  ;; Vulpea utilities
  ;;
  ;;

  ;; Org utilities
  (defun salih/org-roam-get-node-titles (completions)
    "Extract titles from org-roam node completions."
    (mapcar (lambda (completion)
              (if (stringp completion)
                  completion
                (car completion)))
            completions))



  (defun salih/vulpea-project-update-tag ()
    "Update project tag for current buffer."
    (when (and (featurep 'vulpea)
               (not (eq major-mode 'org-agenda-mode)))
      (vulpea-project-update-tag)))

  (defun vulpea-project-files ()
    "Return a list of note files containing 'project' tag." ;
    (if salih/vulpea-show-full
        (vulpea-project-files-full)
      (seq-uniq
       (seq-map
        #'car
        (org-roam-db-query
         [:select [nodes:file]
          :from tags
          :left-join nodes
          :on (= tags:node-id nodes:id)
          :where (like tag (quote "%\"project\"%"))])))))

  (defun vulpea-project-files-full ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (or (like tag (quote "%\"project\"%"))
                   (like tag (quote "%\"project_archived\"%")))]))))

  (defun vulpea-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (vulpea-project-files)))



  (defun vulpea-project-p ()
    "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (seq-find                           ; (3)
     (lambda (type)
       (or (eq type 'todo)))
     (org-element-map                         ; (2)
         (org-element-parse-buffer 'headline) ; (1)
         'headline
       (lambda (h)
         (org-element-property :todo-type h)))))

  (defun vulpea-project-done-p ()
    "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (seq-find                           ; (3)
     (lambda (type)
       (or (eq type 'done)))
     (org-element-map                         ; (2)
         (org-element-parse-buffer 'headline) ; (1)
         'headline
       (lambda (h)
         (org-element-property :todo-type h)))))

  (defun vulpea-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))


          (cond ((vulpea-project-p)
                 (and (or (setq tags (cons "project" tags)) t)
                      (setq tags (remove "project_archived" tags))))

                ((vulpea-project-done-p)
                 (and (or (setq tags (cons "project_archived" tags)) t)
                      (setq tags (remove "project" tags))))
                (t (and (or (setq tags (remove "project" tags)) t)
                        (or (setq tags (remove "project_archived" tags)) t))))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))


  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name)))))

(provide '+l-org-roam)
