
(after! org-roam
  (org-roam-db-autosync-mode)
  (setq org-roam-dailies-capture-templates      '(("d" "default" entry "* %<%H:%M> \n %?"
                                                   :if-new
                                                   (file+head "%<%Y-%m-%d>.org.gpg"
                                                              "#+title: %^{daily-title}\n#+DATE: <%<%Y-%m-%d>>\n#+FILETAGS: journal\n- tags :: [[roam:Journaling]] \n")
                                                   :unnarrowed t))
        org-roam-database-connector             'sqlite
        org-roam-dailies-directory              "journal/"))


(use-package! websocket
  :after org-roam)


(use-package! org-roam-ui
  :after org-roam ;; or :after org
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


(after! org
  (setq org-hide-leading-stars            't
        org-tags-column                   -80
        org-archive-location              "%s_archive.org::"
        org-agenda-start-on-weekday       nil
        org-agenda-start-day              "0d"
        org-agenda-start-with-log-mode    t
        org-roam-capture-templates        '(("k" "knowledge" plain "%?"
                                             :if-new
                                             (file+head "main/${slug}.org" "#+title: ${title}\n#+FILETAGS: permanent")
                                             :immediate-finish t
                                             :unnarrowed t)

                                            ("e" "encrypted knowledge" plain "%?"
                                             :if-new
                                             (file+head "main/${slug}.org.gpg" "#+title: ${title}\n#+FILETAGS: permanent")
                                             :immediate-finish t
                                             :unnarrowed t)


                                            ("l" "links" plain "%?"
                                             :if-new
                                             (file+head "main/${slug}.org" "#+title: ${title}\n#+FILETAGS: link\n")
                                             :immediate-finish t
                                             :unnarrowed t)

                                            ("f" "fleeting" plain "%?"
                                             :target
                                             (file+olp "main/fleet.org" ("${title}"))
                                             :immediate-finish t
                                             :unnarrowed nil)

                                            ("r" "bibliography reference" plain
                                             (file "~/configs/~s/orb")
                                             :target
                                             (file+head "references/${citekey}.org" "#+title: ${title}\n"))

                                            ("v"
                                             "video ref"
                                             entry
                                             "** ${body}"
                                             :target
                                             (file+olp
                                              "webnotes/yt.org"
                                              ("yt" "${title}"))
                                             :immediate-finish t
                                             :jump-to-captured t
                                             :unnarrowed t))
        org-capture-templates             '(("t" "Personal todo" entry
                                             (file+headline +org-capture-todo-file "Inbox")
                                             "* TODO %? :@general:" :prepend t)


                                            ("f" "Empty" entry
                                             (file+headline +org-capture-todo-file "Inbox")
                                             "* TODO %?" :prepend t)

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


                                            ("i" "Got a new idea?" entry
                                             (file+headline +org-capture-todo-file "Inbox")
                                             "* TODO %? :@idea:" :prepend t)

                                            ("j" "Journal" entry
                                             (file+headline +org-capture-journal-file "Posts")
                                             "*** %?\n:DATE:\n%<[%Y-%m-%d %a
                                            %H:%M]>\n:END:"  :prepend t))



        org-agenda-custom-commands '(("f" "Today Tasks"
                                      ((org-ql-block '(and
                                                       (priority "A")
                                                       (not (deadline))
                                                       (not (scheduled)))
                                                     ((org-ql-block-header "High-priority tasks")))

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
                                                       (todo "DAILY")
                                                       (or (and (not (deadline))
                                                                (not (scheduled)))
                                                           (tags "@general")))
                                                     ((org-ql-block-header "Daily Task")))




                                       (org-ql-block '(and
                                                       (todo "TODO")
                                                       (or (scheduled)
                                                           (deadline)))
                                                     ((org-ql-block-header "Soon")))))


                                     ("v" "General Tasks"
                                      ((org-ql-block '(and
                                                       (priority "A")
                                                       (not (deadline))
                                                       (not (scheduled)))
                                                     ((org-ql-block-header "High-priority tasks")))

                                       (org-ql-block '(and
                                                       (todo "TODO")
                                                       (tags "@current"))
                                                     ((org-ql-block-header "Current:")))


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
                                                     ((org-ql-block-header "Looking for an idea?")))))
                                     ("l" "General Later Tasks"
                                      ((org-ql-block '(and
                                                       (todo "TODO")
                                                       (not (tags))
                                                       (not (deadline))
                                                       (not (scheduled)))
                                                     ((org-ql-block-header "Tag:")))

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
                                                     ((org-ql-block-header "Looking for an idea?"))))))))


(provide '+sets-org)
