;;; lr-org-roam.el --- Org-roam, vulpea, dailies -*- lexical-binding: t; -*-

;;; --- Org-roam (deferred) ---
(after! org-roam
  ;; Exclude .gpg files — decrypting them on every DB sync is slow
  ;; and causes passphrase prompts. Encrypted dailies are still writable,
  ;; they just won't appear in the roam graph.
  (setq org-roam-file-exclude-regexp "\\.gpg$")

  ;; Incremental DB updates: only process files that actually changed.
  ;; Prevents full resync on every org-roam operation.
  (setq org-roam-db-update-on-save t)

  ;; Dailies
  (setq org-roam-dailies-capture-templates
        `(("d" "default" entry "* %<%H:%M> \n %?"
           :if-new
           (file+head
            "%<%Y-%m-%d>.org.gpg"
            ,(concat "#+title: %<%Y-%m-%d>\n#+DATE: <%<%Y-%m-%d>>"
                     "\n#+FILETAGS: journal\n- tags :: [[roam:Journaling]] \n"))
           :unnarrowed t))
        org-roam-dailies-directory "~/roam/journal")

  ;; Capture templates
  (setq org-roam-capture-templates
        '(("k" "knowledge" plain "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n#+FILETAGS: permanent")
           :immediate-finish t :unnarrowed t)
          ("e" "encrypted knowledge" plain "%?"
           :if-new (file+head "main/${slug}.org.gpg"
                              "#+title: ${title}\n#+FILETAGS: permanent")
           :immediate-finish t :unnarrowed t)
          ("l" "links" plain "%?"
           :if-new (file+head "things/${slug}.org"
                              "#+title: ${title}\n#+FILETAGS: link\n")
           :immediate-finish t :unnarrowed t)
          ("f" "fleeting" plain "%?"
           :target (file+olp "main/lr.org" ("notes" "${title}"))
           :immediate-finish t :unnarrowed nil)
          ("r" "bibliography reference" plain
           (file "~/configs/~s/orb")
           :target (file+head "references/${citekey}.org"
                              "#+title: ${title}\n"))))

) ;; end after! org-roam

;;; --- Org-roam hooks ---
(add-hook! 'org-roam-find-file-hook #'git-auto-commit-mode)

;;; --- Git auto-commit ---
(after! git-auto-commit-mode
  (setq gac-debounce-interval 200
        gac-silent-message-p  t))

;;; --- Interactive functions ---
(defun salih/org-roam-dailies-capture-today ()
  (interactive)
  (setq salih/org-roam-dailies-capture-p t)
  (call-interactively #'org-roam-dailies-capture-today))

(defun salih/org-roam-buffer ()
  "Display Org Roam buffer for the node at point."
  (interactive)
  (when-let ((node (org-roam-node-at-point)))
    (org-roam-buffer-display-dedicated node)))

(defun salih/consult-org-roam-search ()
  (interactive)
  (consult-org-roam-search))

(defun salih/consult-org-roam-search-org-only ()
  (interactive)
  (let ((consult-ripgrep-args (concat consult-ripgrep-args " -g *.org")))
    (consult-org-roam-search)))

;;; --- Vulpea (deferred) ---
(after! vulpea
  (defun salih/vulpea-project-update-tag ()
    "Update project tag for current buffer."
    (when (and (featurep 'vulpea)
               (not (eq major-mode 'org-agenda-mode)))
      (vulpea-project-update-tag)))

  (defun vulpea-project-files ()
    "Return note files containing 'project' tag."
    (if salih/vulpea-show-full
        (vulpea-project-files-full)
      (seq-uniq
       (seq-map #'car
                (org-roam-db-query
                 [:select [nodes:file]
                  :from tags
                  :left-join nodes :on (= tags:node-id nodes:id)
                  :where (like tag (quote "%\"project\"%"))])))))

  (defun vulpea-project-files-full ()
    (seq-uniq
     (seq-map #'car
              (org-roam-db-query
               [:select [nodes:file]
                :from tags
                :left-join nodes :on (= tags:node-id nodes:id)
                :where (or (like tag (quote "%\"project\"%"))
                           (like tag (quote "%\"project_archived\"%")))]))))

  (defun vulpea-agenda-files-update (&rest _)
    "Update `org-agenda-files'."
    (setq org-agenda-files (vulpea-project-files)))

  (defun vulpea-project-p ()
    "Non-nil if buffer has incomplete TODO entries."
    (seq-find (lambda (type) (eq type 'todo))
              (org-element-map (org-element-parse-buffer 'headline) 'headline
                (lambda (h) (org-element-property :todo-type h)))))

  (defun vulpea-project-done-p ()
    "Non-nil if buffer has completed TODO entries."
    (seq-find (lambda (type) (eq type 'done))
              (org-element-map (org-element-parse-buffer 'headline) 'headline
                (lambda (h) (org-element-property :todo-type h)))))

  (defun vulpea-project-update-tag ()
    "Update PROJECT tag in current buffer."
    (when (and (not (active-minibuffer-window)) (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (cond
           ((vulpea-project-p)
            (setq tags (cons "project" (remove "project_archived" tags))))
           ((vulpea-project-done-p)
            (setq tags (cons "project_archived" (remove "project" tags))))
           (t (setq tags (remove "project" (remove "project_archived" tags)))))
          (setq tags (seq-uniq tags))
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

  (defun vulpea-buffer-p ()
    "Non-nil if current buffer is in org-roam directory."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name)))))

;;; --- Org-roam-bibtex ---
(after! org-roam-bibtex
  (require 'org-ref))

(provide 'lr-org-roam)
