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

  (require 'ox)
  (defun org-add-my-extra-markup ()
    "Add highlight emphasis."
    (add-to-list 'org-font-lock-extra-keywords
                 '("[^\\w]\\(:\\[^\n\r\t]+:\\)[^\\w]"
                   (1 '(face highlight invisible nil)))))

  (add-hook 'org-font-lock-set-keywords-hook #'org-add-my-extra-markup)


  (defun my-html-mark-tag (text backend info)
    "Transcode :blah: into <mark>blah</mark> in body text."
    (when (org-export-derived-backend-p backend 'html)
      (let ((text (replace-regexp-in-string "[^\\w]\\(:\\)[^\n\t\r]+\\(:\\)[^\\w]" "<mark>"  text nil nil 1 nil)))
        (replace-regexp-in-string "[^\\w]\\(<mark>\\)[^\n\t\r]+\\(:\\)[^\\w]" "</mark>" text nil nil 2 nil))))

  (add-to-list 'org-export-filter-plain-text-functions
               'my-html-mark-tag))



(with-eval-after-load 'org
  ;; Allow multiple line Org emphasis markup.
  ;; http://emacs.stackexchange.com/a/13828/115
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
  ;; Below is needed to apply the modified `org-emphasis-regexp-components'
  ;; settings from above.
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))


(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)

(add-hook 'org-mode-hook 'turn-on-auto-fill)





;; Store Org-link in eww-mode buffer
(add-hook 'org-store-link-functions 'org-eww-store-link)
(defun org-eww-store-link ()
  "Store a link to the url of a eww buffer."
  (when (eq major-mode 'eww-mode)
    (org-store-link-props
     :type "eww"
     :link (if (< emacs-major-version 25)
	       eww-current-url
	     (eww-current-url))
     :url (url-view-url t)
     :description (if (< emacs-major-version 25)
		      (or eww-current-title eww-current-url)
		    (or (plist-get eww-data :title)
			  (eww-current-url))))))

;; Some auxiliary functions concerning links in eww buffers
(defun org-eww-goto-next-url-property-change ()
  "Move cursor to the start of next link if exists.  Else no
move.  Return point."
  (goto-char
   (or (next-single-property-change (point) 'shr-url)
       (point))))

(defun org-eww-has-further-url-property-change-p ()
  "Return t if there is a next url property change else nil."
  (save-excursion
    (not (eq (point) (org-eww-goto-next-url-property-change)))))

(defun org-eww-url-below-point ()
  "Return the url below point if there is an url; otherwise, return nil."
  (get-text-property (point) 'shr-url))

(defun org-eww-copy-for-org-mode ()
  "Copy current buffer content or active region with `org-mode' style links.
This will encode `link-title' and `link-location' with
`org-make-link-string', and insert the transformed test into the kill ring,
so that it can be yanked into an Org-mode buffer with links working correctly.

Further lines starting with a star get quoted with a comma to keep
the structure of the org file."
  (interactive)
  (let* ((regionp (org-region-active-p))
         (transform-start (point-min))
         (transform-end (point-max))
         return-content
         link-location link-title
         temp-position out-bound)
    (when regionp
      (setq transform-start (region-beginning))
      (setq transform-end (region-end))
      ;; Deactivate mark if current mark is activate.
      (if (fboundp 'deactivate-mark) (deactivate-mark)))
    (message "Transforming links...")
    (save-excursion
      (goto-char transform-start)
      (while (and (not out-bound)                 ; still inside region to copy
                  (org-eww-has-further-url-property-change-p)) ; there is a next link
        ;; store current point before jump next anchor
        (setq temp-position (point))
        ;; move to next anchor when current point is not at anchor
        (or (org-eww-url-below-point)
	    (org-eww-goto-next-url-property-change))
	(assert (org-eww-url-below-point) t
                "program logic error: point must have an url below but it hasn't")
	(if (<= (point) transform-end)  ; if point is inside transform bound
	    (progn
	      ;; get content between two links.
	      (if (< temp-position (point))
		  (setq return-content (concat return-content
					       (buffer-substring
						temp-position (point)))))
	      ;; get link location at current point.
	      (setq link-location (org-eww-url-below-point))
	      ;; get link title at current point.
	      (setq link-title
		    (buffer-substring
		     (point)
		     (org-eww-goto-next-url-property-change)))
              ;; concat `org-mode' style url to `return-content'.
              (setq return-content (concat return-content
                                           (org-make-link-string
                                            link-location link-title))))
	  (goto-char temp-position)     ; reset point before jump next anchor
	  (setq out-bound t)            ; for break out `while' loop
	  ))
      ;; add the rest until end of the region to be copied
      (if (< (point) transform-end)
          (setq return-content
                (concat return-content
                        (buffer-substring (point) transform-end))))
      ;; quote lines starting with *
      (org-kill-new
       (with-temp-buffer
	 (insert return-content)
	 (goto-char 0)
	 (while (re-search-forward "^\*" nil t)
	   (replace-match ",*"))
	 (buffer-string)))
      (message "Transforming links...done, use C-y to insert text into Org-mode file"))))

;; Additional keys for eww-mode

(defun org-eww-extend-eww-keymap ()
  (define-key eww-mode-map "\C-c\C-x\M-w" 'org-eww-copy-for-org-mode)
  (define-key eww-mode-map "\C-c\C-x\C-w" 'org-eww-copy-for-org-mode))

(when (and (boundp 'eww-mode-map)
           (keymapp eww-mode-map)) ; eww is already up.
  (org-eww-extend-eww-keymap))

(add-hook
 'eww-mode-hook
 (lambda () (org-eww-extend-eww-keymap)))

(provide 'org-eww)








(setq ob-mermaid-cli-path "/usr/bin/mmdc")

;; (setq org-extend-today-until 1)

(add-hook 'auto-save-hook 'org-save-all-org-buffers)

(setq org-archive-location "%s_archive.org::")

 (setq org-cycle-separator-lines -1)

(setq org-startup-folded t)




(setq org-plantuml-jar-path
      (expand-file-name "~/.doom.d/bin/plantuml.jar"))

(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
    (setq-default org-download-image-dir "./org-media")























(add-to-list 'org-agenda-custom-commands
             '("gs" "Super View"
               ((agenda "" (
                            (org-agenda-skip-scheduled-if-done t)
                            (org-agenda-skip-deadline-if-done t)
                            ;; (org-agenda-span 10)
                            ;; (org-agenda-start-day "-3d")
                            (org-super-agenda-groups
                             '(
                               (:discard (:and (:tag "routine" :tag ("@errand" "daily" "weekly"))))
                               (
                                :name none
                                :time-grid t
                                :date today
                                ;; :todo t
                                :scheduled today
                                :deadline today
                                :order 1)
                               (:name none
                                :deadline t
                                :scheduled t
                                :order 2)
                               (:habit t)
                               (:discard (:anything t))
                               ))
                            ))
                (alltodo "" ((org-agenda-overriding-header "")
                             (org-super-agenda-groups
                              '(
                                (:discard (:and (:tag "routine" :tag ("@errand" "daily" "weekly"))))
                                (:name "In Progress"
                                 :todo "GO"
                                 :order 10)
                                (:name "Next action items"
                                 :todo "NEXT"
                                 :order 20)
                                ;; (:name "Needs Review"
                                ;;  :todo "REVIEW"
                                ;;  :order 20)
                                (:name "Important"
                                 :tag "important"
                                 :priority "A"
                                 :order 30)
                                (:name "Due Today"
                                 :deadline today
                                 :order 40)
                                (:name "Due Soon"
                                 :deadline future
                                 :order 50)
                                (:name "Overdue"
                                 :deadline past
                                 :order 45)
                                ;; (:name "Assignments"
                                ;; :tag "Assignment"
                                ;; :order 10)
                                ;; (:name "Issues"
                                ;; :tag "Issue"
                                ;; :order 12)
                                (:name "Projects in todo-file"
                                 :tag "project"
                                 ;; :children todo
                                 :order 110)
                                (:name "Emacs"
                                 :tag "emacs"
                                 :order 200)
                                (:name "Research"
                                 :tag "research"
                                 :order 110)
                                (:name "To read"
                                 :tag "literature"
                                 :order 150)
                                (:name "Waiting"
                                 :todo ("WAIT" "HOLD")
                                 :order 95)
                                (:name "Reminder"
                                 :and (:tag "reminder")
                                 ;; :scheduled (before ,target-date))
                                 :order 95)
                                (:name "Trivial"
                                 :priority<= "C"
                                 :tag ("trivial" "unimportant")
                                 :todo ("IDEA" )
                                 :order 1000)
                                (:name "Not today"
                                 :and (:scheduled future :not (:tag "routine"))
                                 :order 60)
                                ;; (:name "Projects"
                                ;; :file-path "projects")
                                (:name "Unscheduled"
                                 :scheduled nil
                                 :order 100)
                                (:discard (:scheduled today))
                                (:discard (:scheduled past))
                                ;; (:name "Routine"
                                ;; :discard (:tag ("chore" "daily" "weekly"))
                                ;; :tag ("routine")
                                ;; :order 5000)
                                (:discard (:tag ("routine")))
                                ))))
                (alltodo ""
                         ((org-agenda-files '("~/org/someday.org"))
                          (org-agenda-overriding-header "")
                          (org-super-agenda-groups
                           '(
                             (:name "Someday / Maybe" ; Disable super group header
                              :anything t
                              )
                             ))
                          ))
                ))
             )

(setq ;; spacemacs-theme-org-agenda-height t
 ;; org-agenda-time-grid '((daily today require-timed) "----------------------" nil)
 ;; org-agenda-skip-scheduled-if-done t
 ;; org-agenda-skip-deadline-if-done t
 ;; org-agenda-include-deadlines t
 ;; org-agenda-include-diary t
 ;; org-agenda-block-separator nil
 ;; org-agenda-compact-blocks t
 org-agenda-start-with-log-mode t)

;;; print agenda
(add-to-list 'org-agenda-custom-commands
             '("gP" "Printed agenda"
               (
                ;; (agenda "" ((org-agenda-span 7)                      ;; overview of appointments
                ;;             (org-agenda-start-on-weekday nil)         ;; calendar begins today
                ;;             (org-agenda-repeating-timestamp-show-all t)
                ;;             (org-agenda-entry-types '(:timestamp :sexp))))
                (agenda "" ((org-agenda-span 1) ; daily agenda
                            (org-deadline-warning-days 7) ; 7 day advanced warning for deadlines
                            (org-agenda-todo-keyword-format "[ ]")
                            (org-agenda-scheduled-leaders '("" ""))
                            (org-agenda-prefix-format "%t%s")))
                (todo "TODO" ;; todos sorted by context
                      ((org-agenda-prefix-format "[ ] %T: ")
                       (org-agenda-sorting-strategy '(tag-up priority-down))
                       (org-agenda-todo-keyword-format "")
                       (org-agenda-overriding-header "\nTasks by Context\n------------------\n"))))
               ((org-agenda-with-colors nil)
                (org-agenda-compact-blocks t)
                (org-agenda-remove-tags t)
                (ps-number-of-columns 2)
                (ps-landscape-mode t))
               ("~/agenda.ps"))
             ;; other commands go here
             )



;;;; jobs
(add-to-list 'org-agenda-custom-commands
             '("gj" "Jobs"
               ((alltodo ""
                         ((org-agenda-files '("~/org/jobs.org"))
                          (org-agenda-overriding-header "Jobs")
                          (org-super-agenda-groups
                           '(
                             (:auto-ts t)
                             )))))))

;;;; projects
(add-to-list 'org-agenda-custom-commands
             '("gc" "Current project"
               (
                (search (concat "+" (projectile-project-name))
                        ((org-agenda-files '("~/org/projects.org"))
                         (org-agenda-overriding-header (concat "Current project: " (projectile-project-name)))
                         (org-super-agenda-groups
                          '(
                            (:name none ; Disable super group header
                             :auto-outline-path t)
                            (:discard (:anything t))
                            ))
                         ))
                )))

(add-to-list 'org-agenda-custom-commands
             '("go" "All projects"
               (
                (alltodo ""
                         ((org-agenda-files '("~/org/projects.org"))
                          (org-agenda-overriding-header "All projects")
                          (org-super-agenda-groups
                           '(
                             (:name none ; Disable super group header
                              :auto-outline-path t)
                             (:discard (:anything t))
                             ))
                          ))
                (alltodo ""
                         ((org-agenda-files '("~/org/todo.org"))
                          (org-agenda-overriding-header "Projects in todo-file")
                          (org-super-agenda-groups
                           '(
                             (:name none ; Disable super group header
                              :tag "project")
                             (:discard (:anything t))
                             ))
                          ))
                )))

;;;; inbox
(add-to-list 'org-agenda-custom-commands
             '("gi" "Inbox"
               ((alltodo ""
                         ((org-agenda-files '("~/org/inbox.org"))
                          (org-agenda-overriding-header "Inbox")
                          (org-super-agenda-groups
                           '(
                             ;; (:auto-parent t)
                             (:auto-ts t)
                             ;;(:anything t)
                             )))))))

;;;; misc
(add-to-list 'org-agenda-custom-commands
             '("gx" "Get to someday"
               ((todo ""
                      ((org-agenda-overriding-header "Someday / Maybe")
                       (org-agenda-files '("~/org/someday.org"))
                       (org-super-agenda-groups
                        '((:auto-ts t))))))))

(add-to-list 'org-agenda-custom-commands
             '("gu" "Unscheduled TODOs"
               ((todo ""
                      ((org-agenda-overriding-header "\nUnscheduled TODO")
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp 'todo '("DONE" "STOP" "IDEA" "WAIT" "HOLD"))))))) t)

(add-to-list 'org-agenda-custom-commands
             '("gr" "Routines"
               ((alltodo ""
                         ((org-agenda-files '("~/org/routines.org"))
                          (org-agenda-overriding-header "Routines")
                          (org-super-agenda-groups
                           '(
                             ;; (:auto-parent t)
                             (:auto-ts t)
                             ;;(:anything t)
                             )))))))

;;;; not using super agenda
(setq org-agenda-custom-commands
      (append org-agenda-custom-commands
              '(
                ;; open loops
                ("lr" "Recent open loops" agenda ""
                 ((org-agenda-start-day "-2d")
                  (org-agenda-span 4)
                  (org-agenda-start-with-log-mode t)))
                ("ll" "Longer open loops" agenda ""
                 ((org-agenda-start-day "-14d")
                  (org-agenda-span 28)
                  (org-agenda-start-with-log-mode t)))
                ;; search
                ("Qh" "Archive search" search ""
                 ((org-agenda-files (file-expand-wildcards "~/org/*.org_archive"))))
                ("Qw" "Website search" search ""
                 ((org-agenda-files (file-expand-wildcards "~/website/*.org"))))
                ("Qb" "Projects and Archive" search ""
                 ((org-agenda-text-search-extra-files (file-expand-wildcards "~/archive/*.org_archive"))))
                ("QA" "Archive tags search" org-tags-view ""
                 ((org-agenda-files (file-expand-wildcards "~/org/*.org_archive"))))
                ;; priority
                ("pa" "A items" tags-todo "+PRIORITY=\"A\"")
                ("pb" "B items" tags-todo "+PRIORITY=\"B\"")
                ("pc" "C items" tags-todo "+PRIORITY=\"C\"")
                )))

(provide 'super-agenda-custom-commands)

;;; super-agenda-custom-commands.el ends here
