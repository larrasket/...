;;; +l-org-core.el -*- lexical-binding: t; -*-

;; Org-mode core configuration
;; using the after to set that afer doom's. There should be a better way for that.
(after! org
  (use-package org
    :custom
    (org-id-method 'org)
    (org-log-done nil)
    (org-agenda-skip-scheduled-if-done nil)
    (org-use-tag-inheritance t)
    (org-log-reschedule 'note)
    (org-agenda-block-separator #x2501)
    (org-element-use-cache t)
    (org-noter-auto-save-last-location t)
    (org-startup-folded 'show2levels)
    (org-image-actual-width 600)
    (org-link-file-path-type 'relative)
    (org-ellipsis nil)
    (org-agenda-show-future-repeats nil)
    (org-clock-mode-line-total 'current)
    (org-agenda-current-time-string
     "◀── now ─────────────────────────────────────────────────")
    (org-clock-string-limit 7)
    (org-agenda-dim-blocked-tasks 'invisible)
    (org-crypt-key user-mail-address)
    (org-todo-keywords
     '((sequence
        "TODO(t)" "DAILY(e)" "PROJ(p)"
        "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)"
        "IDEA(i)" "|" "DONE(d)" "KILL(k)")
       (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
       (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))
    (org-todo-keyword-faces
     '(("[-]"        . +org-todo-active)
       ("STRT"       . +org-todo-active)
       ("DAILY"      . +org-todo-project)
       ("[?]"        . +org-todo-onhold)
       ("WAIT"       . +org-todo-onhold)
       ("HOLD"       . +org-todo-onhold)
       ("PROJ"       . +org-todo-project)
       ("NO"         . +org-todo-cancel)
       ("KILL"       . +org-todo-cancel)))
    (org-hide-leading-stars t)
    (org-tags-column 70)
    (org-archive-location "%s_archive.org::")
    (org-agenda-start-on-weekday nil)
    (org-agenda-start-day "0d")
    (org-agenda-start-with-log-mode t)
    :config
    (setq
     salih/source-directory                            "~/roam/source"
     salih/books
     (mapcar 'file-truename
             (directory-files-recursively salih/source-directory "" nil t)))

    (setq org-tag-alist   '((:startgroup)
                            ("@personal" . nil)
                            (:grouptags)
                            ("@read" . ?r)
                            ("@idea" . ?i)
                            ("@write" . ?W)
                            ("@check" . ?c)
                            ("@watch" . ?w)
                            ("@else" . ?e) ;; if there's a note that have an else
                            ;; and general tag, then general
                            ;; prevails
                            (:endgroup)
                            (:startgroup)
                            ("@nothing" . ?N)
                            (:grouptags)
                            ("@people" . ?p)
                            (:endgroup)
                            ("noexport" . ?n)
                            ("anthology" . ?a)
                            ("selected" . ?s)
                            ("@later" . ?l)
                            ("@current" . ?C)
                            ("@long" . ?L)
                            ("drill" . ?d)
                            ("@daily" . ?D)
                            ("@general" . ?g)))




    (dolist (tag '("noexport"
                   "project"
                   "permanent"
                   "link"
                   "@read"
                   "@write"
                   "@watch"
                   "@current"
                   "drill"))
      (add-to-list 'org-tags-exclude-from-inheritance tag))





    ;; Org archive functions
    (defun salih/org-archive-done-tasks ()
      (interactive)
      (org-map-entries 'org-archive-subtree "/DONE" 'file))

    (defun salih/org-capture-general ()
      (interactive)
      (salih/capture-- 'org-capture "f"))

    (defun salih/org-capture-journal ()
      (interactive)
      (salih/capture-- 'org-capture "j"))

    (defun salih/org-capture-log ()
      (interactive)
      (salih/capture-- 'org-capture "n"))

    (defun salih/org-roam-capture-fleet ()
      (interactive)
      (salih/capture-- 'org-roam-capture "f"))

    (defun salih/org-agenda-no-full-f ()
      (interactive)
      (setq salih/vulpea-show-full nil)
      (org-agenda nil "f"))

    (defun salih/org-agenda-no-full-l ()
      (interactive)
      (setq salih/vulpea-show-full nil)
      (org-agenda nil "l"))

    (defun salih/org-agenda-full-f ()
      (interactive)
      (setq salih/vulpea-show-full t)
      (org-agenda nil "l"))

    (defun salih/open-agenda ()
      (interactive)
      (org-agenda-remove-restriction-lock)
      (org-agenda nil "v"))

    (defun salih/capture-- (fn key &optional fleet?)
      (with-current-buffer
          (find-file-noselect (if fleet?
                                  salih/org-roam-fleet-file
                                +org-capture-todo-file))
        (funcall fn nil key)))


    (defun salih/set-custom-id-to-id (&rest _)
      "Set the CUSTOM_ID property to match the ID property in the current entry."
      (when-let ((id (org-entry-get nil "ID")))
        (org-entry-put nil "CUSTOM_ID" id)))








    ;; Start note function

    ;; Org-ql utilities
    (defun salih/org-ql-view--format-element (orig-fun &rest args)
      "This function will intercept the original function and
add the category to the result.
ARGS is `element' in `org-ql-view--format-element'"
      (if (not args)
          ""
        (let* ((element args)
               (properties (cadar element))
               (result (apply orig-fun element))
               (smt "")
               (category (org-entry-get (plist-get properties :org-marker) "CATEGORY")))
          (if (> (length category) 11)
              (setq category (substring category 0 10)))
          (if (< (length category) 11)
              (setq smt (make-string (- 11 (length category)) ?\s)))
          (org-add-props
              (format "   %-8s %s" (concat category ":" smt) result)
              (text-properties-at 0 result)))))


    ;; Org media utilities
    (defun salih/org-media-note-insert-link (orgin)
      (let ((org-link-file-path-type 'absolute))
        (funcall orgin)))

    (defun salih/org-id-get-create-with-custom-id ()
      (interactive)
      (when (org-before-first-heading-p)
        (user-error "Not inside a heading"))
      (let* ((org-id (org-id-get))
             (custom-id-property "CUSTOM_ID"))

        (unless org-id
          (setq org-id (org-id-new))
          (org-entry-put nil "ID" org-id)
          (org-entry-put nil custom-id-property org-id))
        org-id))

    (defun salih/org-archive-killed-tasks ()
      (interactive)
      (org-map-entries 'org-archive-subtree "/KILL" 'file))


    (custom-set-faces!
      '(org-agenda-done :strike-through nil)
      '(org-document-title :height 2.0)
      '(org-list-dt :inherit default)
      `(jinx-misspelled
        :underline (:style wave :color ,(face-foreground 'error))))))



(require 'ts)
(require 'org-download)
(setq org-agenda-custom-commands
        `(("f" "Agenda Tasks"
           ((org-ql-block '(and
                            (priority "A")
                            (todo "TODO"))
                          ((org-ql-block-header "High-priority tasks")))


            (org-ql-block '(and
                            (todo "TODO")
                            (scheduled  :to ,(ts-adjust 'day -1 (ts-now))))
                          ((org-ql-block-header "Late tasks")))


            (org-ql-block '(and
                            (scheduled)
                            (not (done))
                            (ts-active :on today))
                          ((org-ql-block-header "Today's tasks only")))



            (agenda ""
                    ((org-agenda-span 4)))


            ;; FIXME this should support sorting functionality.
            ;; Waiting for the next org-ql update.
            (org-ql-block '(and
                            (todo "TODO")
                            (tags "@general")
                            (not (tags "@later"))
                            (not (deadline))
                            (not (scheduled)))
                          ((org-ql-block-header "Get something done")))

            (org-ql-block '(and
                            (todo "TODO")
                            (tags "@daily"))
                          ((org-ql-block-header "Daily Task")))




            (org-ql-block '(and
                            (todo "TODO")
                            (not (tags "@daily"))
                            (or (scheduled :from today :to +10)
                                (deadline)))
                          ((org-ql-block-header "Soon")))))


          ("v" "General Tasks"
           ((org-ql-block '(and
                            (priority "A")
                            (todo "TODO")
                            (not (deadline))
                            (not (scheduled)))
                          ((org-ql-block-header "High-priority tasks")))

            (org-ql-block '(and
                            (todo "TODO")
                            (not (scheduled))
                            (not (deadline))
                            (not (tags "@later"))
                            (tags "@current"))
                          ((org-ql-block-header "Current:")))

            (org-ql-block '(and
                            (todo "TODO")
                            (tags "@long"))
                          ((org-ql-block-header "Long term goals:")))

            (org-ql-block '(and
                            (todo "TODO")
                            (tags "@read")
                            (not (tags "@later"))
                            (not (tags "project"))
                            (not (deadline))
                            (not (scheduled)))
                          ((org-ql-block-header "Read something:")))

            (org-ql-block '(and
                            (todo "TODO")
                            (tags "@read")
                            (tags "project")
                            (scheduled :from today :to +100))
                          ((org-ql-block-header
                            "Scheduled readings")))

            (org-ql-block '(and
                            (todo "TODO")
                            (tags "@read")
                            (tags "project")
                            (not (tags "@later"))
                            (not (deadline))
                            (not (scheduled)))
                          ((org-ql-block-header "Read a book:")))


            (org-ql-block '(and
                            (todo "TODO")
                            (tags "@write")
                            (not (tags "@later"))
                            (not (tags "project"))
                            (not (deadline))
                            (not (scheduled)))
                          ((org-ql-block-header "Write something:")))


            (org-ql-block '(and
                            (todo "TODO")
                            (tags "@check")
                            (not (tags "@later"))
                            (not (deadline))
                            (not (scheduled)))
                          ((org-ql-block-header "Check this out")))

            (org-ql-block '(and
                            (todo "TODO")
                            (tags "@watch")
                            (not (tags "@later"))
                            (not (deadline))
                            (not (scheduled)))
                          ((org-ql-block-header "Your ungoogled watch later:")))

            (org-ql-block '(and
                            (todo "TODO")
                            (tags "@idea")
                            (not (tags "@later"))
                            (not (deadline))
                            (not (scheduled)))
                          ((org-ql-block-header
                            "Looking for an idea?")))))


          ("l" "General Later Tasks"
           ((org-ql-block '(and
                            (or (todo) (done))
                            (not (tags))
                            (not (deadline))
                            (not (scheduled)))
                          ((org-ql-block-header "Tag title:")))

            (org-ql-block '(and
                            (todo "TODO")
                            (tags "@read")
                            (tags "@later")
                            (not (tags "project"))
                            (not (deadline))
                            (not (scheduled)))
                          ((org-ql-block-header "Read something:")))


            (org-ql-block '(and
                            (todo "TODO")
                            (tags "@read")
                            (tags "project")
                            (tags "@later")
                            (not (deadline))
                            (not (scheduled)))
                          ((org-ql-block-header "Read a book:")))


            (org-ql-block '(and
                            (todo "TODO")
                            (tags "@write")
                            (tags "@later")
                            (not (tags "project"))
                            (not (deadline))
                            (not (scheduled)))
                          ((org-ql-block-header "Write something:")))


            (org-ql-block '(and
                            (todo "TODO")
                            (tags "@check")
                            (tags "@later")
                            (not (deadline))
                            (not (scheduled)))
                          ((org-ql-block-header "Check this out")))

            (org-ql-block '(and
                            (todo "TODO")
                            (tags "@watch")
                            (tags "@later")
                            (not (deadline))
                            (not (scheduled)))
                          ((org-ql-block-header "Your ungoogled watch later:")))

            (org-ql-block '(and
                            (todo "TODO")
                            (tags "@idea")
                            (tags "@later")
                            (not (deadline))
                            (not (scheduled)))
                          ((org-ql-block-header
                            "Looking for an
                                                       idea?")))))))



;; Org link configuration
(use-package org
  :config
  (org-link-set-parameters
   "eww"
   :follow (lambda (link) (eww link))
   :store (lambda ()
            (when (eq major-mode 'eww-mode)
              (let ((url (eww-current-url))
                    (title (or (plist-get eww-data :title) "No title")))
                (unless url
                  (error "No URL found in the current eww buffer"))
                (org-store-link-props
                 :type "eww"
                 :link url
                 :description title))))))

;; Org log note configuration
(use-package org
  :config
  (setf (cdr (assoc 'note org-log-note-headings)) "%t")
  (custom-set-faces! '(org-done :strike-through nil :weight bold)))

;; Org-mode advice
(advice-add 'org-agenda          :before 'vulpea-agenda-files-update)
(advice-add 'org-todo-list       :before 'vulpea-agenda-files-update)
(advice-add 'org-agenda-quit     :before 'org-save-all-org-buffers)


(defun salih/logbook-on (&rest _)
  "Enable logbook for org mode."
  (setq org-log-into-drawer t))


(defun salih/logbook-off (&rest _)
  "Enable logbook for org mode."
  (setq org-log-into-drawer nil))

(defun salih/stats-on (&rest _)
  "Disable log into drawer for org mode."
  (if salih/adding-note?
      (setq salih/adding-note? nil
            org-log-into-drawer nil)
    (setq org-log-into-drawer "STATS"))
  (message (format "after: %s" org-log-into-drawer)))


(defun salih/start-note (&rest _) (setq salih/adding-note? t))


(advice-add 'org-clock-in        :before 'salih/logbook-on)
(advice-add 'org-clock-in        :after  'salih/logbook-off)

(advice-add 'org-store-log-note :before 'salih/stats-on)
(advice-add 'org-store-log-note :after  'salih/logbook-off)

(advice-add 'org-add-note        :before 'salih/start-note)
(advice-add 'org-add-note        :before 'salih/stats-on)
(advice-add 'org-add-note        :after  'salih/logbook-on)

;; (advice-add 'org-agenda-add-note :before 'salih/start-note)
;; (advice-add 'org-agenda-add-note :before 'salih/toggle-stats-off)
;; (advice-add 'org-agenda-add-note :after  'salih/toggle-stats-on)


(advice-add 'org-media-note-insert-link
            :around #'salih/org-media-note-insert-link)
(advice-add 'org-id-get-create :after    #'salih/set-custom-id-to-id)

;; Org-download advice
(advice-add 'org-download-clipboard :around
            (lambda (orig-fun &rest args)
              (cl-letf (((symbol-function 'org-id-get-create) #'ignore))
                (apply orig-fun args))))

;; Org-mode hooks
(add-hook! 'org-mode-hook
  (add-hook 'before-save-hook  #'vulpea-project-update-tag nil 'local)
  (add-hook 'find-file-hook    #'vulpea-project-update-tag nil 'local))

(defun salih/insert-now-timestamp()
  (interactive)
  (org-insert-time-stamp (current-time) t))


(provide '+l-org-core)
