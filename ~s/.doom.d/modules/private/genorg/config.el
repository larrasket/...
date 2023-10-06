;;; mine/genorg/config.el -*- lexical-binding: t; -*-

(after! org





  (setq org-tags-column -80)
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"                  ; A task that needs doing & is ready to do
           "APPOINT(a)"
           "PROJ(p)"             ; A project, which usually contains other tasks
           "HOLD(h)"             ; This task is paused/on hold because of me
           "|"
           "DONE(d)"    ; Task successfully completed
           "KILL(k)")   ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "DAILY(D)"                   ; A task that needs doing
           "|"
           "DONE(d)"))                  ; Task was completed

        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("DAILY" . "#708090")
          ("Hold" . +Org-todo-onhold)
          ("PROJ" . +org-todo-project)

          ("APPOINT" . "#0a66c2")
          ("KILL" . +org-todo-cancel)))

  (setq org-hide-leading-stars 't)
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %? :@general:" :prepend t)


          ("f" "Empty" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?" :prepend t)

          ("p" "Project-local todo" entry  ; {project-root}/todo.org
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
           "*** %?\n:DATE:\n%<[%Y-%m-%d %a %H:%M]>\n:END:"  :prepend t)))

  (require 'ox))



(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(setq org-archive-location "%s_archive.org::"
      org-agenda-start-with-log-mode t)




(advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)


(setq org-agenda-start-on-weekday nil
      org-agenda-start-day "0d")

(setq org-agenda-custom-commands
      '(("f" "Today Tasks"
         ((agenda ""
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
                        ((org-ql-block-header "Looking for an idea?")))))))



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

(advice-add 'org-ql-view--format-element :around #'salih/org-ql-view--format-element)
