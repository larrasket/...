;;; +l-org-capture.el -*- lexical-binding: t; -*-

;; Org-capture configuration
(after! org
  (use-package org
    :custom
    (org-capture-templates
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

       ("p" "Project-local todo" entry ; {project-root}/todo.org
        (file +org-capture-project-todo-file)
        "* TODO %?\n%i\n%a" :prepend t)


       ("i" "Got a new idea?" entry
        (file+headline +org-capture-todo-file "Inbox")
        "* TODO %? :@idea:\n%<[%Y-%m-%d %a %H:%M]>\n" :prepend t)

       ("n" "Now doing " entry (file+headline +org-capture-todo-file "Inbox")
        "* TODO %?"  :clock-in t
        :clock-keep t)
       ("j" "Journal" entry
        (file+headline +org-capture-journal-file "Posts")
        "*** %?\n:DATE:\n%<[%Y-%m-%d %a %H:%M]>\n:END:"  :prepend t)))))

(provide '+l-org-capture)
