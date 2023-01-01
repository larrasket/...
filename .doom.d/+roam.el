;;; ../configs/.doom.d/+roam.el -*- lexical-binding: t; -*-

;; (cl-defmethod org-roam-node-backlinkscount-number ((node org-roam-node))
;;     "Access slot \"backlinks\" of org-roam-node struct CL-X. This
;;     is identical to `org-roam-node-backlinkscount' with the
;;     difference that it returns a number instead of a fromatted
;;     string. This is to be used in
;;     `org-roam-node-sort-by-backlinks'"
;;     (let* ((count (caar (org-roam-db-query
;; 			 [:select (funcall count source)
;; 				  :from links
;; 				  :where (= dest $s1)
;; 				  :and (= type "id")]
;; 			 (org-roam-node-id node)))))
;;       count))

;; (defun org-roam-node-sort-by-backlinks (completion-a completion-b)
;;   "Sorting function for org-roam that sorts the list of nodes by
;; the number of backlinks. This is the sorting function in
;; `org-roam-node-find-by-backlinks'"
;;   (let ((node-a (cdr completion-a))
;; 	(node-b (cdr completion-b)))
;;     (>= (org-roam-node-backlinkscount-number node-a)
;; 	(org-roam-node-backlinkscount-number node-b))))

;; (defun org-roam-node-find-by-backlinks ()
;;   "Essentially works like `org-roam-node-find' (although it uses
;; a combination of `find-file' and `org-roam-node-read' to
;; accomplish that and not `org-roam-node-find' as only
;; `org-roam-node-read' can take a sorting function as an argument)
;; but the list of nodes is sorted by the number of backlinks
;; instead of most recent nodes. Sorting is done with
;; `org-roam-node-sort-by-backlinks'"
;;   (interactive)
;;   (find-file (org-roam-node-file (org-roam-node-read nil nil #'org-roam-node-sort-by-backlinks))))



(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/roam"))
  (org-roam-dailies-capture-templates
    '(("d" "default" entry "* %<%H:%M> \n %?"
       :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%A, %d %B %Y>\n- tags :: [[id:fe8618df-c476-44b8-8169-a210bff989d7][Journaling]]\n"))))
  :config
  (setq org-roam-database-connector 'sqlite3)
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)

  )
    (defun cm/deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
      (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
	(if begin
	    (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
	  (deft-base-filename file))))

    (advice-add 'deft-parse-title :override #'cm/deft-parse-title)

    (setq deft-strip-summary-regexp
	  (concat "\\("
		  "[\n\t]" ;; blank
		  "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
		  "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
		  "\\)"))


(setq org-roam-dailies-directory "journal/")









(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))



(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new
         (file+head "main/${slug}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("p" "People" plain
         "%?"
         :if-new
         (file+head "main/${slug}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("s" "saved" plain "%?"
         :if-new
         (file+head "saved/${slug}.org" "#+title: ${title}\n- tags :: [[roam:saved things]]")
         :immediate-finish t
         :unnarrowed t)
        ("c" "contemplations" plain "%?"
         :if-new
         (file+head "contemplations/${slug}.org" "#+title: ${title}\n- tags :: [[roam:Contemplation]]")
         :immediate-finish t
         :unnarrowed t)
        ("q" "quotes" plain "%?"
         :if-new
         (file+head "quotes/${slug}.org" "#+title: ${title}\n— [[roam:Quotes]]")
         :immediate-finish t
         :unnarrowed t)
        ("l" "literature" plain "%?"
         :if-new
         (file+head "literature/${slug}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("h" "history" plain "%?"
         :if-new
         (file+head "everything/${slug}.org" "#+title: ${title}\n#+filetags: History \n- tags :: ")
         :immediate-finish t
         :unnarrowed t)
        ("k" "knowledge" plain "%?"
         :if-new
         (file+head "everything/${slug}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "bibliography reference" plain
         (file "~/configs/orb")
         :target
         (file+head "references/${citekey}.org" "#+title: ${title}\n"))
        ))


(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))

(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))



(use-package org-roam
  :bind (("C-c r l" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r g" . org-roam-graph)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r c" . org-roam-capture)
         ;; Dailies
         ("C-c r j" . org-roam-dailies-capture-today)))


(map! :leader
      :desc "roam graph"
      "r g" #'org-roam-graph)


(map! :leader
      :desc "add tag"
      "r t" #'org-roam-tag-add)



(map! :leader
      :desc "switch to raom buffer"
      "r b" #'org-roam-buffer-toggle)


(map! :leader
      :desc "capture"
      "r c" #'org-roam-capture)


(map! :leader
      :desc "insert"
      "r i" #'org-roam-node-insert)


(map! :leader
      :desc "find file"
      "r f" #'org-roam-node-find)

(map! :leader
      :desc "roam"
      "r r" #'org-roam-buffer-display-dedicated)


(map! :leader
      :desc "org caputer"
      "x" #'org-capture)


(map! :leader
      :desc "org caputer"
      "r j" #'org-roam-dailies-capture-today)


(map! :leader
      :desc "Add alias"
      "r a" #'org-roam-alias-add)


(after! popup
  (set-popup-rule! "\\*org-roam\\*"
    :side 'right
    :width 0.40
    :slot 0
    :parameters '((no-other-window . t)
                  (no-delete-other-windows . t))))


(defvar org-roam-list-most-linked-count 5)
(cl-defmethod org-roam-node-backlinkscount-number ((node org-roam-node)) "Access slot \"backlinks\" of org-roam-node struct CL-X. This is identical toorg-roam-node-backlinkscount' with the difference that it returns a number instead of a fromatted string. This is to be used in `org-roam-node-sort-by-backlinks'" (let* ((count (caar (org-roam-db-query [:select (funcall count source) :from links :where (= dest $s1) :and (= type "id")] (org-roam-node-id node))))) count))
(defun org-roam-node-sort-by-backlinks (completion-a completion-b) "Sorting function for org-roam that sorts the list of nodes by the number of backlinks. This is the sorting function in `org-roam-node-find-by-backlinks'" (let ((node-a (cdr completion-a)) (node-b (cdr completion-b))) (>= (org-roam-node-backlinkscount-number node-a) (org-roam-node-backlinkscount-number node-b))))
(defun org-roam-node-find-by-backlinks () "Essentially works like org-roam-node-find' (although it uses a combination offind-file' and org-roam-node-read' to accomplish that and notorg-roam-node-find' as only org-roam-node-read' can take a sorting function as an argument) but the list of nodes is sorted by the number of backlinks instead of most recent nodes. Sorting is done with org-roam-node-sort-by-backlinks'" (interactive) (find-file (org-roam-node-file (org-roam-node-read nil nil #'org-roam-node-sort-by-backlinks))))


(provide '+roam)

(add-hook 'org-roam-find-file-hook #'git-auto-commit-mode)
