;;; mine/genorg/config.el -*- lexical-binding: t; -*-

(after! org
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"                    ; A task that needs doing & is ready to do
           "APPOINT(a)"
           "PROJ(p)"                    ; A project, which usually contains other tasks
           "HOLD(h)"                    ; This task is paused/on hold because of me
           "IDEA(i)"                    ; An unconfirmed and unapproved task or notion
           "READ(b)"                    ; To Read
           "CHECK(c)"                   ; To Read
           "|"
           "DONE(d)"                ; Task successfully completed
           "KILL(k)")               ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "DAILY(D)"        ; A task that needs doing
           "|"
           "DONE(d)"))                    ; Task was completed

        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("DAILY" . "#708090")
          ("Hold" . +Org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("READ" . "#98be65")

          ("APPOINT" . "#0a66c2")
          ("CHECK" . "#fc791c")
          ("KILL" . +org-todo-cancel)))

  (setq org-hide-leading-stars 't)
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %? :personal:" :prepend t)


          ("w" "Watch" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %? :watch:" :prepend t)

          ("r" "Read" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %? :read:" :prepend t)


          ("c" "Check" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* CHECK %?" :prepend t)


          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)
          ("j" "Journal" entry
           (file+headline +org-capture-journal-file "Posts")
           "*** %<%A, %d %B %Y>\n%?"  :prepend t)))
  (require 'ox))



(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(setq org-archive-location "%s_archive.org::"
      org-agenda-start-with-log-mode t)




(advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)
