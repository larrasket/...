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
     ("p" "Project-local todo" entry
      (file +org-capture-project-todo-file)
      "* TODO %?\n%i\n%a" :prepend t)
     ("w" "WATCH" entry
      (file+headline +org-capture-todo-file "Inbox")
      "* WATCH %? :@watch:" :prepend t)
     ("r" "READ" entry
      (file+headline +org-capture-todo-file "Inbox")
      "* READ %? :@read:" :prepend t)
     ("m" "MEETING" entry
      (file+headline +org-capture-todo-file "Inbox")
      "* MEETING %? :@meeting:" :prepend t)
     ("c" "CALL" entry
      (file+headline +org-capture-todo-file "Inbox")
      "* CALL %? :@call:" :prepend t)
     ("h" "HABIT" entry
      (file+headline +org-capture-todo-file "Inbox")
      "* HABIT %? :@habit:" :prepend t)
     ("i" "IDEA" entry
      (file+headline +org-capture-todo-file "Inbox")
      "* IDEA %? :@idea:" :prepend t)
     ("n" "NOTE" entry
      (file+headline +org-capture-todo-file "Inbox")
      "* NOTE %? :@note:" :prepend t)
     ("s" "STUDY" entry
      (file+headline +org-capture-todo-file "Inbox")
      "* STUDY %? :@study:" :prepend t)
     ("l" "LINK" entry
      (file+headline +org-capture-todo-file "Inbox")
      "* LINK %? :@link:" :prepend t)
     ("a" "ARCHIVE" entry
      (file+headline +org-capture-todo-file "Inbox")
      "* ARCHIVE %? :@archive:" :prepend t)
     ("o" "OTHER" entry
      (file+headline +org-capture-todo-file "Inbox")
      "* OTHER %? :@other:" :prepend t))))

(provide '+l-org-capture) 