;;; mine/genorg/config.el -*- lexical-binding: t; -*-

(after! org
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"                    ; A task that needs doing & is ready to do
           "APPOINT(a)"
           "PROJ(p)"                    ; A project, which usually contains other tasks
           "RUNNING(r)"             ; A recurring task
           "STUDY(s)"                   ; A task that is in progress
           "WATCH(w)"                   ; Something external is holding up this task
           "HOLD(h)"                    ; This task is paused/on hold because of me
           "IDEA(i)"                    ; An unconfirmed and unapproved task or notion
           "READ(b)"                    ; To Read
           "CHECK(c)"                   ; To Read
           "|"
           "DONE(d)"                ; Task successfully completed
           "FAIL(f)"                ; Task is not successfully completed
           "KILL(k)")               ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"        ; A task that needs doing
           ;; "[-](S)"                     ; Task is in progress
           ;; "[?](W)"                     ; Task is being held up or paused
           "|"
           "[X](x)")                    ; Task was completed
          (sequence
           "DAILY(D)"        ; A task that needs doing
           "|"
           "DONE(d)")                    ; Task was completed

          (sequence
           "RUNNING(r)"        ; A task that needs doing
           "|"
           "DONE(d)")                    ; Task was completed
          (sequence
           "|"
           ;; "OKAY(o)"
           ;; "YES(y)"
           ;; "NO(n)"
           )
          )
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STUDY" . "DarkBlue")
          ;; ("[?]"  . +org-todo-onhold)
          ("WATCH" . "#86ba96")
          ("DAILY" . "#708090")
          ("Hold" . +Org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("FAIL"   . +org-todo-cancel)
          ("READ" . "#98be65")

          ("RUNNING" . "#d4cecd")
          ("APPOINT" . "#0a66c2")
          ("CHECK" . "#fc791c")
          ("KILL" . +org-todo-cancel)))

  (setq org-hide-leading-stars 't)
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?" :prepend t)


          ("i" "Templates for the internet")

          ("ic" "Nice computer science post" item
           (file+headline +org-capture-changelog-file "Computers for Love, SWE for money")
           "+ %?" :prepend t)

          ("ia" "Nice theory post" item
           (file+headline +org-capture-changelog-file "In Theory")
           "+ %?" :prepend t)

          ("F" "TODO with file" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)



          ("iv" "Nice computer science videos" item
           (file+headline +org-capture-changelog-file "Computers")
           "+ %?" :prepend t)

          ("it" "Nice videos" item
           (file+headline +org-capture-changelog-file "Other")
           "+ %?" :prepend t)



          ("s" "Personal to study" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* STUDY %?" :prepend t)


          ("w" "Watch" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* WATCH %?" :prepend t)

          ("r" "Read" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* READ %?" :prepend t)


          ("c" "Check" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* CHECK %?" :prepend t)


          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)
          ;; ("j" "Journal" entry
          ;;  (file+olp+datetree +org-capture-journal-file)
          ;;  "* %U %?\n%i\n%a" :prepend t)
          ;;              "* %<%A, %d %B %Y>" :prepend t
          ("j" "Journal" entry
           (file+headline +org-capture-journal-file "Posts")
           "*** %<%A, %d %B %Y>\n%?"  :prepend t)

          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t)
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
          ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
          ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)))

  (require 'ox))




(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(setq org-archive-location "%s_archive.org::")
