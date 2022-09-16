;;; orgset.el -*- lexical-binding: t; -*-
(provide 'orgset)

(after! org
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"                    ; A task that needs doing & is ready to do
           "APPOINT(a)"
           "PROJ(p)"                    ; A project, which usually contains other tasks
           ;; "LOOP(r)"             ; A recurring task
           "STUDY(s)"                   ; A task that is in progress
           "WATCH(w)"                   ; Something external is holding up this task
           ;; "HOLD(h)"                    ; This task is paused/on hold because of me
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
          ;; ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("FAIL"   . +org-todo-cancel)
          ("READ" . "#98be65")
          ("APPOINT" . "#0a66c2")
          ("CHECK" . "#fc791c")
          ("KILL" . +org-todo-cancel)))

  (setq org-hide-leading-stars 't)
  (setq +org-capture-journal-file "~/blog/content/stack.org")
  (setq +org-capture-changelog-file "~/blog/content/nice.org")
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




          ("iv" "Nice computer science videos" item
           (file+headline +org-capture-changelog-file "computers")
           "+ %?" :prepend t)

          ("it" "Nice videos" item
           (file+headline +org-capture-changelog-file "other")
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


