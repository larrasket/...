;;; lr-org-core.el --- Org settings, agenda, capture, modern -*- lexical-binding: t; -*-

;;; --- Org basic display (safe to set early) ---
(setq org-hide-emphasis-markers t
      org-pretty-entities t
      org-agenda-tags-column 'auto)

;;; --- Org core config (deferred) ---
(after! org
  (require 'vulpea)
  (setq org-id-method 'org
        org-log-done nil
        org-agenda-skip-scheduled-if-done nil
        org-use-tag-inheritance t
        org-log-reschedule 'note
        org-agenda-block-separator #x2501
        org-element-use-cache t
        org-element-cache-persistent t          ; persist parse cache to disk
        org-noter-auto-save-last-location t
        org-startup-folded 'show2levels
        org-image-actual-width 600
        org-link-file-path-type 'relative
        org-ellipsis nil
        org-agenda-show-future-repeats nil
        org-clock-mode-line-total 'current
        org-agenda-current-time-string
        "◀── now ─────────────────────────────────────────────────"
        org-clock-string-limit 7
        org-agenda-dim-blocked-tasks 'invisible
        org-agenda-inhibit-startup t            ; don't apply startup folding in agenda files
        org-agenda-sticky t                     ; keep agenda buffers alive (press g to refresh)
        org-crypt-key user-mail-address
        org-hide-leading-stars t
        org-tags-column 70
        org-archive-location "%s_archive.org::"
        org-agenda-start-on-weekday nil
        org-agenda-start-day "0d"
        org-agenda-start-with-log-mode t)

  ;; Todo keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DAILY(e)" "PROJ(p)"
           "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)"
           "IDEA(i)" "|" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
          (sequence "|" "OKAY(o)" "YES(y)" "NO(n)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("DAILY" . +org-todo-project)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO"   . +org-todo-cancel)
          ("KILL" . +org-todo-cancel)))

  ;; Tags
  (setq org-tag-alist
        '((:startgroup)
          ("@personal" . nil)
          (:grouptags)
          ("@read" . ?r) ("@idea" . ?i) ("@write" . ?W)
          ("@check" . ?c) ("@watch" . ?w) ("@else" . ?e)
          (:endgroup)
          (:startgroup)
          ("@nothing" . ?N)
          (:grouptags)
          ("@people" . ?p)
          (:endgroup)
          ("noexport" . ?n) ("anthology" . ?a) ("selected" . ?s)
          ("@later" . ?l) ("@current" . ?C) ("@long" . ?L)
          ("drill" . ?d) ("@daily" . ?D) ("@general" . ?g)))

  (dolist (tag '("noexport" "project" "permanent" "link"
                 "@read" "@write" "@watch" "@current" "drill"))
    (add-to-list 'org-tags-exclude-from-inheritance tag))

  ;; Log note format
  (setf (cdr (assoc 'note org-log-note-headings)) "%t")
  (custom-set-faces! '(org-done :strike-through nil :weight bold))
  (custom-set-faces! `(jinx-misspelled
                       :underline (:style wave :color ,(face-foreground 'error))))

  ;; Source directory & books (lazy list)
  (setq salih/books
        (when (file-directory-p salih/source-directory)
          (mapcar 'file-truename
                  (directory-files-recursively salih/source-directory "" nil t))))

  ;; EWW link type
  (org-link-set-parameters
   "eww"
   :follow (lambda (link) (eww link))
   :store (lambda ()
            (when (eq major-mode 'eww-mode)
              (let ((url (eww-current-url))
                    (title (or (plist-get eww-data :title) "No title")))
                (unless url (error "No URL found"))
                (org-store-link-props
                 :type "eww" :link url :description title))))))

;;; --- Capture templates ---
(after! org
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %? :@general:\n%<[%Y-%m-%d %a %H:%M]>\n" :prepend t)
          ("w" "WATCH" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %? :@watch:\n%<[%Y-%m-%d %a %H:%M]>\n" :prepend t)
          ("r" "READ" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %? :@read:\n%<[%Y-%m-%d %a %H:%M]>\n" :prepend t)
          ("c" "CHECK" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %? :@check:\n%<[%Y-%m-%d %a %H:%M]>\n" :prepend t)
          ("f" "Empty" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%U" :prepend t)
          ("p" "Project-local todo" entry
           (file +org-capture-project-todo-file)
           "* TODO %?\n%i\n%a" :prepend t)
          ("i" "Got a new idea?" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %? :@idea:\n%<[%Y-%m-%d %a %H:%M]>\n" :prepend t)
          ("n" "Now doing" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?" :clock-in t :clock-keep t)
          ("j" "Journal" entry
           (file+headline +org-capture-journal-file "Posts")
           "*** %?\n:DATE:\n%<[%Y-%m-%d %a %H:%M]>\n:END:" :prepend t))))

;;; --- Agenda custom commands (deferred to agenda open) ---
(after! org
  (require 'ts)
  (setq org-agenda-custom-commands
        `(("f" "Agenda Tasks"
           ((org-ql-block '(and (priority "A") (todo "TODO"))
                          ((org-ql-block-header "High-priority tasks")))
            (org-ql-block '(and (todo "TODO")
                                (scheduled :to ,(ts-adjust 'day -1 (ts-now))))
                          ((org-ql-block-header "Late tasks")))
            (org-ql-block '(and (scheduled) (not (done)) (ts-active :on today))
                          ((org-ql-block-header "Today's tasks only")))
            (agenda "" ((org-agenda-span 4)))
            (org-ql-block '(and (todo "TODO") (tags "@general")
                                (not (tags "@later")) (not (deadline)) (not (scheduled)))
                          ((org-ql-block-header "Get something done")))
            (org-ql-block '(and (todo "TODO") (tags "@daily"))
                          ((org-ql-block-header "Daily Task")))
            (org-ql-block '(and (todo "TODO") (not (tags "@daily"))
                                (or (scheduled :from today :to +10) (deadline)))
                          ((org-ql-block-header "Soon")))))

          ("v" "General Tasks"
           ((org-ql-block '(and (priority "A") (todo "TODO")
                                (not (deadline)) (not (scheduled)))
                          ((org-ql-block-header "High-priority tasks")))
            (org-ql-block '(and (todo "TODO") (not (scheduled)) (not (deadline))
                                (not (tags "@later")) (tags "@current"))
                          ((org-ql-block-header "Current:")))
            (org-ql-block '(and (todo "TODO") (tags "@long"))
                          ((org-ql-block-header "Long term goals:")))
            (org-ql-block '(and (todo "TODO") (tags "@read") (not (tags "@later"))
                                (not (tags "project")) (not (deadline)) (not (scheduled)))
                          ((org-ql-block-header "Read something:")))
            (org-ql-block '(and (todo "TODO") (tags "@read") (tags "project")
                                (scheduled :from today :to +100))
                          ((org-ql-block-header "Scheduled readings")))
            (org-ql-block '(and (todo "TODO") (tags "@read") (tags "project")
                                (not (tags "@later")) (not (deadline)) (not (scheduled)))
                          ((org-ql-block-header "Read a book:")))
            (org-ql-block '(and (todo "TODO") (tags "@write") (not (tags "@later"))
                                (not (tags "project")) (not (deadline)) (not (scheduled)))
                          ((org-ql-block-header "Write something:")))
            (org-ql-block '(and (todo "TODO") (tags "@check") (not (tags "@later"))
                                (not (deadline)) (not (scheduled)))
                          ((org-ql-block-header "Check this out")))
            (org-ql-block '(and (todo "TODO") (tags "@watch") (not (tags "@later"))
                                (not (deadline)) (not (scheduled)))
                          ((org-ql-block-header "Your ungoogled watch later:")))
            (org-ql-block '(and (todo "TODO") (tags "@idea") (not (tags "@later"))
                                (not (deadline)) (not (scheduled)))
                          ((org-ql-block-header "Looking for an idea?")))))

          ("l" "General Later Tasks"
           ((org-ql-block '(and (or (todo) (done)) (not (tags))
                                (not (deadline)) (not (scheduled)))
                          ((org-ql-block-header "Tag title:")))
            (org-ql-block '(and (todo "TODO") (tags "@read") (tags "@later")
                                (not (tags "project")) (not (deadline)) (not (scheduled)))
                          ((org-ql-block-header "Read something:")))
            (org-ql-block '(and (todo "TODO") (tags "@read") (tags "project")
                                (tags "@later") (not (deadline)) (not (scheduled)))
                          ((org-ql-block-header "Read a book:")))
            (org-ql-block '(and (todo "TODO") (tags "@write") (tags "@later")
                                (not (tags "project")) (not (deadline)) (not (scheduled)))
                          ((org-ql-block-header "Write something:")))
            (org-ql-block '(and (todo "TODO") (tags "@check") (tags "@later")
                                (not (deadline)) (not (scheduled)))
                          ((org-ql-block-header "Check this out")))
            (org-ql-block '(and (todo "TODO") (tags "@watch") (tags "@later")
                                (not (deadline)) (not (scheduled)))
                          ((org-ql-block-header "Your ungoogled watch later:")))
            (org-ql-block '(and (todo "TODO") (tags "@idea") (tags "@later")
                                (not (deadline)) (not (scheduled)))
                          ((org-ql-block-header "Looking for an idea?"))))))))

;;; --- Org helper functions ---
(defun salih/org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(defun salih/org-archive-killed-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/KILL" 'file))

(defun salih/org-capture-general ()
  (interactive)
  (salih/capture-- 'org-capture "f"))

(defun salih/org-capture-journal ()
  (interactive)
  (salih/capture-- 'org-capture "j"))

(defun salih/org-roam-capture-fleet ()
  (interactive)
  (salih/capture-- 'org-roam-capture "f"))

(defun salih/capture-- (fn key &optional fleet?)
  (with-current-buffer
      (find-file-noselect (if fleet?
                              salih/org-roam-fleet-file
                            +org-capture-todo-file))
    (funcall fn nil key)))

(defun salih/toggle-agenda-late ()
  (interactive)
  (if (eq org-extend-today-until 9)
      (setq org-extend-today-until 0)
    (setq org-extend-today-until 9))
  (salih/org-agenda-no-full-f))

(defun salih/set-custom-id-to-id (&rest _)
  "Set CUSTOM_ID to match ID."
  (when-let ((id (org-entry-get nil "ID")))
    (org-entry-put nil "CUSTOM_ID" id)))

(defun salih/org-id-get-create-with-custom-id ()
  (interactive)
  (when (org-before-first-heading-p)
    (user-error "Not inside a heading"))
  (let ((org-id (or (org-id-get) (org-id-new))))
    (org-entry-put nil "ID" org-id)
    (org-entry-put nil "CUSTOM_ID" org-id)
    org-id))

(defun salih/org-calendar-goto-agenda ()
  (interactive)
  (let ((org-agenda-span 1))
    (org-calendar-goto-agenda)))

(defun salih/insert-now-timestamp ()
  (interactive)
  (org-insert-time-stamp (current-time) t))

(defun salih/paste-markdown-as-org ()
  "Convert markdown from clipboard to org via pandoc."
  (interactive)
  (let ((md-content (current-kill 0)))
    (with-temp-buffer
      (insert md-content)
      (let ((exit-code (call-process-region (point-min) (point-max)
                                            "pandoc" t t nil "-f" "markdown" "-t" "org")))
        (if (= exit-code 0)
            (let ((org-content (buffer-string)))
              (with-current-buffer (window-buffer)
                (insert org-content)))
          (error "Pandoc conversion failed"))))))

(defun salih/org-open-file-link-in-macos ()
  "Open the Org file link at point using macOS `open`."
  (interactive)
  (let ((context (org-element-context)))
    (if (and (eq (org-element-type context) 'link)
             (string= (org-element-property :type context) "file"))
        (let ((full-path (expand-file-name (org-element-property :path context))))
          (start-process "macos-open" nil "open" full-path)
          (message "Opened: %s" full-path))
      (message "Not on a file link."))))

;;; --- Org-ql formatting ---
(defun salih/org-ql-view--format-element (orig-fun &rest args)
  "Add category prefix to org-ql results."
  (if (not args) ""
    (let* ((element args)
           (properties (cadar element))
           (result (apply orig-fun element))
           (category (org-entry-get (plist-get properties :org-marker) "CATEGORY"))
           (category (if (> (length category) 11)
                         (substring category 0 10)
                       category))
           (smt (make-string (max 0 (- 11 (length category))) ?\s)))
      (org-add-props
          (format "   %-8s %s" (concat category ":" smt) result)
          (text-properties-at 0 result)))))

(advice-add 'org-ql-view--format-element :around #'salih/org-ql-view--format-element)

;;; --- Org advice ---
(advice-add 'org-id-get-create :after #'salih/set-custom-id-to-id)

;; Org-download: skip org-id-get-create
(after! org-download
  (advice-add 'org-download-clipboard :around
              (lambda (orig-fun &rest args)
                (cl-letf (((symbol-function 'org-id-get-create) #'ignore))
                  (apply orig-fun args)))))

;;; --- Logbook/clock advice ---
(defun salih/logbook-on (&rest _)  (setq org-log-into-drawer t))
(defun salih/logbook-off (&rest _) (setq org-log-into-drawer nil))

(defun salih/stats-on (&rest _)
  (if salih/adding-note?
      (setq salih/adding-note? nil org-log-into-drawer nil)
    (setq org-log-into-drawer "STATS")))

(defun salih/start-note (&rest _) (setq salih/adding-note? t))

(advice-add 'org-clock-in      :before 'salih/logbook-on)
(advice-add 'org-clock-in      :after  'salih/logbook-off)
(advice-add 'org-store-log-note :before 'salih/stats-on)
(advice-add 'org-store-log-note :after  'salih/logbook-off)
(advice-add 'org-add-note      :before 'salih/start-note)
(advice-add 'org-add-note      :before 'salih/stats-on)
(advice-add 'org-add-note      :after  'salih/logbook-on)

;;; --- Org mode hooks ---
(add-hook! 'org-mode-hook
  (display-line-numbers-mode -1)
  (visual-fill-column-mode 1)
  (setq-local fill-column 90))

(add-hook! 'org-mode-hook
  (add-hook 'before-save-hook #'vulpea-project-update-tag nil 'local))

;;; --- Agenda advice (deferred until agenda opens) ---
(advice-add 'org-agenda      :before 'vulpea-agenda-files-update)
(advice-add 'org-todo-list   :before 'vulpea-agenda-files-update)
(advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)

;;; --- File templates ---
(after! org
  (set-file-template! "\\.org$"
    :trigger
    (lambda ()
      (let ((title (string-join
                    (split-string (file-name-base (buffer-file-name)) "[-_ ]+") " ")))
        (insert (format "#+title: %s\n#+DATE: <%s>\n\n"
                        (capitalize title)
                        (format-time-string "%Y-%m-%d %a %H:%M")))))
    :mode 'org-mode
    :project nil))

;;; --- Org-modern (deferred to org-mode) ---
(after! org-modern
  (setq org-modern-star 'replace
        org-modern-replace-stars "◉○✸✿✤✜◆▶"
        org-modern-tag nil
        org-modern-timestamp nil
        org-modern-keyword t
        org-modern-todo t
        org-modern-block-name t
        org-modern-priority t
        org-modern-list '((42 . "•") (43 . "‒") (45 . "-"))
        org-modern-horizontal-rule '("─" 2)
        org-modern-block-fringe t
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-label-border 0.5))

(add-hook 'org-mode-hook #'org-modern-mode)

;;; --- Org-present ---
(after! org-present
  (add-hook! 'org-present-mode-hook
    (set-fringe-style 0)
    (hl-line-mode -1)
    (mixed-pitch-mode 1)
    (org-display-inline-images)
    (setq visual-fill-column-width 140)
    (visual-fill-column-mode))
  (add-hook! 'org-present-mode-quit-hook
    (set-fringe-style '(1 . 1))
    (hl-line-mode 1)
    (mixed-pitch-mode -1)
    (org-remove-inline-images)
    (visual-fill-column-mode -1)))

;;; --- Org-fc ---
(after! org-fc
  (setq org-fc-flashcard-tag "drill"
        org-fc-directories '("~/roam/main" "~/roam/other" "~/roam/references")))

;;; --- Nov breadcrumb ---
(add-hook! 'org-noter-doc-mode-hook (breadcrumb-local-mode -1))
(add-hook! 'nov-mode-hook (breadcrumb-local-mode -1))

;;; --- Nov epub mode ---
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(provide 'lr-org-core)
