;;; lr-org-roam.el --- Org-roam, vulpea, dailies -*- lexical-binding: t; -*-

;;; --- iCloud symlink compatibility ---
;; Doom Emacs sets `find-file-visit-truename t', which means opening a file
;; via a symlink (e.g. ~/roam/main/foo.org → iCloud) sets buffer-file-name to
;; the TRUENAME (/Users/l/Library/Mobile Documents/.../main/foo.org), not the
;; symlink path.  org-roam-file-p checks if file-truename is under
;; file-truename(org-roam-directory) = /Users/l/roam — but iCloud truenames are
;; NOT under /Users/l/roam, so org-roam-file-p always returns nil for these files.
;; That breaks DB updates on save, find-file-hook integration, etc.
;;
;; Fix: pre-compute the truenames of symlinked subdirs of org-roam-directory,
;; then extend org-roam-file-p to return t when a file is under any of them.



(defvar salih/--roam-symlink-truenames nil
  "Truenames of symlinked subdirectories of `org-roam-directory'.
Pre-computed on org-roam load; used to extend `org-roam-file-p' so files
accessed via iCloud symlinks are recognised as org-roam files.")

(defun salih/--compute-roam-symlink-truenames ()
  "Populate `salih/--roam-symlink-truenames'."
  (when (bound-and-true-p org-roam-directory)
    (let ((roam-dir (expand-file-name org-roam-directory)))
      (setq salih/--roam-symlink-truenames
            (delq nil
                  (mapcar (lambda (entry)
                            (let ((path (expand-file-name entry roam-dir)))
                              (when (and (file-directory-p path)
                                         (file-symlink-p path))
                                (file-truename path))))
                          (directory-files roam-dir nil "^[^.]")))))))

;;; --- Org-roam per-buffer setup via org-mode-hook ---
;; org-roam normally wires these up only via org-roam-find-file-hook, which
;; fires for files under org-roam-directory after org-roam-db-autosync-mode
;; is active.  Any timing gap (file opened before autosync enables) or path
;; outside org-roam-directory breaks completion and link replacement.
;; Adding them directly to org-mode-hook makes them bullet-proof.
(with-eval-after-load 'org-roam
  (salih/--compute-roam-symlink-truenames)
  ;; Completion: adds org-roam-complete-link-at-point + org-roam-complete-everywhere
  (add-hook 'org-mode-hook #'org-roam--register-completion-functions-h)
  ;; Link replacement: adds org-roam-link-replace-all to before-save-hook
  ;; so [[roam:Title]] → [[id:...]] on every save
  (add-hook 'org-mode-hook #'org-roam--replace-roam-links-on-save-h))

;;; --- Org-roam (deferred) ---
(after! org-roam
  ;; Extend org-roam-file-p to handle iCloud-symlinked files.
  ;; When find-file-visit-truename=t (Doom default), buffer-file-name for
  ;; ~/roam/main/foo.org is the iCloud truename, not the ~/roam path.
  ;; This advice returns t when the file's path is under any symlinked subdir
  ;; of org-roam-directory (pre-computed in salih/--roam-symlink-truenames).
  (defadvice! salih/org-roam-file-p-symlink-a (fn &optional file)
    :around #'org-roam-file-p
    (or (funcall fn file)
        (when salih/--roam-symlink-truenames
          (let ((path (expand-file-name
                       (or file (buffer-file-name (buffer-base-buffer))))))
            (and path
                 (not (auto-save-file-name-p path))
                 (not (backup-file-name-p path))
                 (string-suffix-p ".org" path t)
                 (not (and (bound-and-true-p org-roam-file-exclude-regexp)
                           (string-match-p org-roam-file-exclude-regexp path)))
                 (cl-some (lambda (dir)
                            (string-prefix-p (file-name-as-directory dir) path))
                          salih/--roam-symlink-truenames))))))

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
                              "#+title: ${title}\n"))))) ;; end after! org-roam

;;; --- Org-roam hooks ---
;; (add-hook! 'org-roam-find-file-hook #'git-auto-commit-mode)

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
  "Full-text search across org-roam-directory, following symlinks."
  (interactive)
  (let ((consult-ripgrep-args (concat consult-ripgrep-args " --follow")))
    (consult-ripgrep org-roam-directory)))

(defun salih/consult-org-roam-search-org-only ()
  "Full-text search across org-roam-directory (.org files only), following symlinks."
  (interactive)
  (let ((consult-ripgrep-args (concat consult-ripgrep-args " --follow -g *.org")))
    (consult-ripgrep org-roam-directory)))

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
