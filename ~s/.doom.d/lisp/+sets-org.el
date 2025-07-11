(after! org
  (require 'org-fc)
  (require 'ts)
  (setq org-id-method                                           'org
        org-log-into-drawer                                     "STATS"
        org-log-done                                            nil
        org-agenda-skip-scheduled-if-done                       nil
        org-use-tag-inheritance                                 t
        org-log-reschedule                                      'note
        org-agenda-block-separator                              #x2501
        org-element-use-cache                                   t
        org-noter-auto-save-last-location                       t
        org-startup-folded                                      'show2levels
        org-image-actual-width                                  600
        org-link-file-path-type                                 'relative
        org-ellipsis                                            nil
        org-agenda-show-future-repeats                          nil
        org-clock-mode-line-total                               'current
        org-agenda-current-time-string
        "◀── now ─────────────────────────────────────────────────"
        ;; FIXME this is useful to hide the title name from the org clock,
        ;; however it might be inconsistent. Better should be redefining
        ;; `org-clock-get-clock-string'. I wouldn't overwrite it myself since it
        ;; might break things in the future, I might consider making PR to
        ;; org-mode making the string customizable.
        org-clock-string-limit                                  7
        org-agenda-dim-blocked-tasks                            'invisible
        ;; org-agenda-sticky                                       t
        org-crypt-key                                           user-mail-address
        org-todo-keywords
        '((sequence
           "TODO(t)" "DAILY(e)" "PROJ(p)"
           "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)"
           "IDEA(i)" "|" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
          (sequence "|" "OKAY(o)" "YES(y)" "NO(n)"))
        org-todo-keyword-faces
        '(("[-]"        . +org-todo-active)
          ("STRT"       . +org-todo-active)
          ("DAILY"      . +org-todo-project)
          ("[?]"        . +org-todo-onhold)
          ("WAIT"       . +org-todo-onhold)
          ("HOLD"       . +org-todo-onhold)
          ("PROJ"       . +org-todo-project)
          ("NO"         . +org-todo-cancel)
          ("KILL"       . +org-todo-cancel))
        org-hide-leading-stars            't
        org-tags-column                   70
        org-archive-location              "%s_archive.org::"
        org-agenda-start-on-weekday       nil
        org-agenda-start-day              "0d"
        org-agenda-start-with-log-mode    t
        org-roam-capture-templates
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
           (file+head "references/${citekey}.org" "#+title: ${title}\n")))

        org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %? :@general:" :prepend t)

          ("f" "Empty" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%U" :prepend t)

          ("p" "Project-local todo" entry ; {project-root}/todo.org
           (file +org-capture-project-todo-file)
           "* TODO %?\n%i\n%a" :prepend t)

          ("w" "WATCH" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %? :@watch:" :prepend t)

          ("r" "READ" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %? :@read:" :prepend t)


          ("c" "CHECK" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %? :@check:" :prepend t)

          ("n" "Now doing " entry (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?"  :clock-in t
           :clock-keep t)

          ("i" "Got a new idea?" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %? :@idea:" :prepend t)

          ("j" "Journal" entry
           (file+headline +org-capture-journal-file "Posts")
           "*** %?\n:DATE:\n%<[%Y-%m-%d %a %H:%M]>\n:END:"  :prepend t))



        org-agenda-custom-commands
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
                                                       idea?"))))))))


(after! org-roam
  (org-roam-db-autosync-mode)
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
  (use-package! websocket :after org-roam)
  (use-package! org-roam-ui
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))



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
    (add-to-list 'org-tags-exclude-from-inheritance tag)))



(provide '+sets-org)
