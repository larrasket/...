;;; +l-org-capture.el -*- lexical-binding: t; -*-

;; Org-capture configuration
(use-package org
  :custom
  (org-capture-templates
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
           "*** %?\n:DATE:\n%<[%Y-%m-%d %a %H:%M]>\n:END:"  :prepend t))))

(provide '+l-org-capture)
