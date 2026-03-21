;;; lr-roam-lint.el --- Zettelkasten health tools for org-roam -*- lexical-binding: t; -*-
;;
;; Integrates roam-lint (the Python graph checker) with Emacs, and provides
;; interactive tools for maintaining a healthy zettelkasten.
;;
;; See ~/roam/NOTE_GUIDE.org for the philosophy behind these checks.

(require 'org-roam)

;;; ---------------------------------------------------------------------------
;;; Configuration
;;; ---------------------------------------------------------------------------

(defvar salih/roam-lint-script (expand-file-name "roam-lint" org-roam-directory)
  "Path to the roam-lint Python script.")

;;; ---------------------------------------------------------------------------
;;; Core: run roam-lint on a file
;;; ---------------------------------------------------------------------------

(defun salih/roam-lint-buffer ()
  "Run roam-lint on the current buffer and display warnings."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (let* ((file (file-relative-name buffer-file-name
                                   (expand-file-name org-roam-directory)))
         (output (shell-command-to-string
                  (format "%s --file %s --json"
                          (shell-quote-argument salih/roam-lint-script)
                          (shell-quote-argument file))))
         (warnings (condition-case nil
                       (json-read-from-string output)
                     (error nil))))
    (if (null warnings)
        (message "roam-lint: could not parse output")
      (let ((msgs (mapcar (lambda (w)
                            (let ((level (cdr (assq 'level w)))
                                  (msg (cdr (assq 'msg w))))
                              (format "[%s] %s" (upcase level) msg)))
                          warnings)))
        (if (and (= (length warnings) 1)
                 (string= (cdr (assq 'level (aref warnings 0))) "ok"))
            (message "roam-lint: ✓ Note looks healthy")
          (message "roam-lint: %s" (string-join msgs " | ")))))))

;;; ---------------------------------------------------------------------------
;;; After-save: warn about unlinked notes
;;; ---------------------------------------------------------------------------

(defun salih/roam-lint--check-links-on-save ()
  "After saving an org-roam file, warn if it has 0 outgoing links."
  (when (and buffer-file-name
             (org-roam-file-p buffer-file-name)
             (not (string-match-p "_index\\.org" buffer-file-name))
             (not (string-match-p "microblog" buffer-file-name)))
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "\\[\\[id:" nil t)
        (message "roam-lint: ⚠ This note has no outgoing links. Consider connecting it.")))))

(add-hook 'after-save-hook #'salih/roam-lint--check-links-on-save)

;;; ---------------------------------------------------------------------------
;;; Interactive: browse stubs
;;; ---------------------------------------------------------------------------

(defun salih/roam-find-stubs ()
  "Browse stub notes (< 100 words) — these need your thinking."
  (interactive)
  (let* ((output (shell-command-to-string
                  (format "%s --stubs --json"
                          (shell-quote-argument salih/roam-lint-script))))
         (items (condition-case nil
                    (json-read-from-string output)
                  (error nil))))
    (unless items
      (user-error "Could not parse roam-lint output"))
    (let* ((candidates
            (mapcar (lambda (item)
                      (let ((title (cdr (assq 'title item)))
                            (path (cdr (assq 'path item)))
                            (bl (cdr (assq 'backlinks item)))
                            (wc (cdr (assq 'word_count item))))
                        (cons (format "%s  (%d←, %dw)  %s" title bl wc path)
                              (expand-file-name path org-roam-directory))))
                    items))
           (choice (completing-read
                    (format "Stubs (%d total): " (length candidates))
                    candidates nil t)))
      (when choice
        (find-file (cdr (assoc choice candidates)))))))

;;; ---------------------------------------------------------------------------
;;; Interactive: browse dead-end hubs
;;; ---------------------------------------------------------------------------

(defun salih/roam-dead-ends ()
  "Browse dead-end hubs — notes with many backlinks but 0 outgoing."
  (interactive)
  (let* ((output (shell-command-to-string
                  (format "%s --dead-ends --json"
                          (shell-quote-argument salih/roam-lint-script))))
         (items (condition-case nil
                    (json-read-from-string output)
                  (error nil))))
    (unless items
      (user-error "Could not parse roam-lint output"))
    (if (= (length items) 0)
        (message "No dead-end hubs found — nice!")
      (let* ((candidates
              (mapcar (lambda (item)
                        (let ((title (cdr (assq 'title item)))
                              (path (cdr (assq 'path item)))
                              (bl (cdr (assq 'backlinks item))))
                          (cons (format "%s  (%d backlinks → nowhere)  %s" title bl path)
                                (expand-file-name path org-roam-directory))))
                      items))
             (choice (completing-read "Dead-end hubs: " candidates nil t)))
        (when choice
          (find-file (cdr (assoc choice candidates))))))))

;;; ---------------------------------------------------------------------------
;;; Interactive: browse orphans
;;; ---------------------------------------------------------------------------

(defun salih/roam-find-orphans ()
  "Browse true orphan notes — completely disconnected from the graph."
  (interactive)
  (let* ((output (shell-command-to-string
                  (format "%s --orphans --json"
                          (shell-quote-argument salih/roam-lint-script))))
         (items (condition-case nil
                    (json-read-from-string output)
                  (error nil))))
    (unless items
      (user-error "Could not parse roam-lint output"))
    (let* ((candidates
            (mapcar (lambda (item)
                      (let ((title (cdr (assq 'title item)))
                            (path (cdr (assq 'path item)))
                            (wc (cdr (assq 'word_count item))))
                        (cons (format "%s  (%dw)  %s" title wc path)
                              (expand-file-name path org-roam-directory))))
                    items))
           (choice (completing-read
                    (format "Orphans (%d total): " (length candidates))
                    candidates nil t)))
      (when choice
        (find-file (cdr (assoc choice candidates)))))))

;;; ---------------------------------------------------------------------------
;;; Link suggestion: find notes that might be relevant to current buffer
;;; ---------------------------------------------------------------------------

(defun salih/roam-suggest-links ()
  "Suggest org-roam notes that might be relevant to the current buffer.
Uses word overlap between the current buffer's content and note titles
in the org-roam database."
  (interactive)
  (unless (org-roam-file-p buffer-file-name)
    (user-error "Not an org-roam file"))
  (let* ((current-id (org-roam-id-at-point))
         ;; Get significant words from current buffer (skip short/common words)
         (content (buffer-substring-no-properties (point-min) (point-max)))
         (words (seq-filter
                 (lambda (w) (> (length w) 4))
                 (split-string (downcase content) "[^a-z0-9]+" t)))
         (word-set (seq-uniq words))
         ;; Get all nodes from org-roam DB
         (all-nodes (org-roam-db-query
                     [:select [id title file]
                      :from nodes
                      :where (= level 0)]))
         ;; Already linked IDs
         (already-linked
          (save-excursion
            (goto-char (point-min))
            (let (ids)
              (while (re-search-forward "\\[\\[id:\\([^]]+\\)\\]" nil t)
                (push (match-string 1) ids))
              ids)))
         ;; Score each node by word overlap with its title
         (scored
          (seq-filter
           (lambda (entry) (> (car entry) 0))
           (mapcar
            (lambda (row)
              (let* ((nid (car row))
                     (title (cadr row))
                     (file (caddr row))
                     (title-words (split-string (downcase (or title "")) "[^a-z0-9]+" t))
                     (overlap (seq-length
                               (seq-intersection title-words word-set #'string=))))
                (list overlap nid title file)))
            all-nodes)))
         ;; Remove self and already-linked
         (filtered
          (seq-filter
           (lambda (entry)
             (let ((nid (nth 1 entry)))
               (and (not (string= nid (or current-id "")))
                    (not (member nid already-linked)))))
           scored))
         ;; Sort by score descending, take top 20
         (sorted (seq-take (seq-sort (lambda (a b) (> (car a) (car b))) filtered) 20)))
    (if (null sorted)
        (message "No suggestions found (note may already be well-linked)")
      (let* ((candidates
              (mapcar (lambda (entry)
                        (let ((score (car entry))
                              (nid (nth 1 entry))
                              (title (nth 2 entry)))
                          (cons (format "[%d] %s" score title)
                                nid)))
                      sorted))
             (choice (completing-read "Link suggestion (score = word overlap): "
                                      candidates nil t)))
        (when choice
          (let ((chosen-id (cdr (assoc choice candidates))))
            ;; Insert a link at point
            (insert (format "[[id:%s][%s]]"
                            chosen-id
                            (nth 2 (seq-find (lambda (e) (string= (nth 1 e) chosen-id))
                                             sorted))))))))))

;;; ---------------------------------------------------------------------------
;;; Full report in a buffer
;;; ---------------------------------------------------------------------------

(defun salih/roam-lint-report ()
  "Run full roam-lint and display report in a buffer."
  (interactive)
  (let ((buf (get-buffer-create "*roam-lint*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (shell-command-to-string
                 (shell-quote-argument salih/roam-lint-script)))
        (ansi-color-apply-on-region (point-min) (point-max)))
      (special-mode)
      (goto-char (point-min)))
    (display-buffer buf)))

;;; ---------------------------------------------------------------------------
;;; Updated capture templates
;;; ---------------------------------------------------------------------------

(after! org-roam
  (setq org-roam-capture-templates
        '(;; Zettel: an atomic thought — the core unit
          ("z" "zettel" plain "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t :unnarrowed t)

          ;; Entity/link node (backward compatible with "l")
          ;; The template now includes a prompt to write YOUR thought
          ("l" "entity" plain
           "\n%?\n\n(What is your thought about ${title}? A definition is not enough.)\n"
           :if-new (file+head "things/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t :unnarrowed t)

          ;; Structure note: a map of content
          ("s" "structure note" plain
           "#+begin_comment\nThis is a structure note — a map, not content.\nList links to zettel with brief annotations.\n#+end_comment\n\n%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t :unnarrowed t)

          ;; Fleeting: quick capture into fleet file
          ("f" "fleeting" plain "%?"
           :target (file+olp "main/lr.org" ("notes" "${title}"))
           :immediate-finish t :unnarrowed nil)

          ;; Encrypted zettel
          ("e" "encrypted zettel" plain "%?"
           :if-new (file+head "main/${slug}.org.gpg"
                              "#+title: ${title}\n")
           :immediate-finish t :unnarrowed t)

          ;; Bibliography reference (unchanged)
          ("r" "bibliography reference" plain
           (file "~/configs/~s/orb")
           :target (file+head "references/${citekey}.org"
                              "#+title: ${title}\n")))))

;;; ---------------------------------------------------------------------------
;;; Keybindings (under SPC n r prefix, alongside org-roam defaults)
;;; ---------------------------------------------------------------------------

(map! :leader
      (:prefix ("n" . "notes")
       (:prefix ("r" . "roam")
        :desc "Lint current note"    "L" #'salih/roam-lint-buffer
        :desc "Full lint report"     "R" #'salih/roam-lint-report
        :desc "Find stubs"           "S" #'salih/roam-find-stubs
        :desc "Find orphans"         "O" #'salih/roam-find-orphans
        :desc "Dead-end hubs"        "D" #'salih/roam-dead-ends
        :desc "Suggest links"        "K" #'salih/roam-suggest-links)))

(provide 'lr-roam-lint)
