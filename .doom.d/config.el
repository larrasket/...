(add-to-list 'load-path "~/.doom.d/")
(require '+roam)
(require 'leader)
(require 'keys)
(require '+handy)
(add-to-list 'org-agenda-files "~/roam/journal/agenda/todo.org")
(add-to-list 'org-agenda-files "~/roam/journal/agenda/birthday.org")
(setq +org-capture-journal-file "~/blog/content/stack.org")
(setq +org-capture-changelog-file "~/blog/content/nice.org")
(setq +org-capture-todo-file "~/roam/main/life.org")
(setq neo-autorefresh 't)
(setq neo-mode-line-type 'default)
(setq load-prefer-newer t)

(custom-set-variables
 '(highlight-indent-guides-method 'bitmap))


(defun highltier ()
  (require 'highlight-indent-guides)
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (highlight-indent-guides-mode))
(add-hook 'org-mode-hook 'highltier)
(add-hook 'prog-mode-hook 'highltier)

(define-key evil-normal-state-map (kbd "C-g") 'evil-escape)
(define-key evil-visual-state-map (kbd "C-g") 'evil-escape)
(define-key evil-insert-state-map (kbd "C-g") 'evil-escape)
(define-key evil-replace-state-map (kbd "C-g") 'evil-escape)
(define-key evil-operator-state-map (kbd "C-g") 'evil-escape)
(defun my/evil-escape-and-abort-company ()
  (interactive)
  (company-abort)
  (evil-escape))
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-g") 'my/evil-escape-and-abort-company)
  (define-key company-search-map (kbd "C-g") 'my/evil-escape-and-abort-company))




(setq bibtex-completion-bibliography "~/configs/ref.bib")
(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "C-j") 'bibtex-next-field)
  (define-key evil-insert-state-map (kbd "C-k") 'bibtex-previous-entry))

(with-eval-after-load 'vertigo
  (vertigo-mode 1)
  (setq vertigo-completing-read-function 'ivy-completing-read))


(setq bibtex-completion-bibliography '("~/configs/ref.bib" )
	bibtex-completion-library-path '("~/source/")
	bibtex-completion-notes-path "~/roam/reference/"
	bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
	bibtex-completion-additional-search-fields '(keywords)
	bibtex-completion-display-formats
	'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}")))


(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

(define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)


(use-package citar
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  (citar-bibliography '("~/configs/ref.bib")))

(setq citar-templates
      '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
        (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
        (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
        (note . "Notes on ${author editor}, ${title}")))


(setq citar-symbols
      `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
        (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
        (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
(setq citar-symbol-separator "  ")



(use-package citar-org-roam
  :after citar org-roam
  :no-require
  :config (citar-org-roam-mode))
(setq citar-org-roam-note-title-template "${author} - ${title}\n- tags :: [[roam:Book]]\n")


(add-hook 'helm-minibuffer-set-up-hook
          (lambda ()
            (advice-add #'doom-modeline--active :override (lambda () t))))
(add-hook 'helm-cleanup-hook
          (lambda ()
            (advice-remove #'doom-modeline--active (lambda () t))))


(require 'citar-org-roam)






(use-package! org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref))




(setq orb-preformat-keywords
      '("citekey" "title" "url" "author-or-editor" "keywords" "file")
      orb-process-file-keyword t
      orb-attached-file-extensions '("pdf"))

(define-key org-roam-bibtex-mode-map (kbd "C-c n a") #'orb-note-actions)




(defun org-babel-execute:chess (body params)
  "Execute a block of Chess code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((output-dir (expand-file-name "chessimages" (file-name-directory (buffer-file-name))))
         (output-file (concat (format "_%s_chess_output.svg" (format-time-string "%Y-%m-%d_%H-%M-%S")) ))
         (output-path (expand-file-name output-file output-dir))
         (notation (cdr (assq :notation params)))
         (extension (if (equal notation "fen") ".fen" ".pgn"))
         (notation-file (make-temp-file "chess-notation" nil extension))
         (cmd (format "python ~/configs/elchess.py %s %s %s" notation-file output-path notation)))
    (with-temp-buffer
      (insert body)
      (write-file notation-file))
    (shell-command cmd)
    (org-babel-result-to-file (file-relative-name output-path))))

(setq org-babel-default-header-args:chess
      '((:results . "raw")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((chess . t)))
(add-hook 'csv-mode-hook 'csv-align-mode)



(require 'saveplace-pdf-view)
(save-place-mode 1)

(setq org-crypt-key "ghd@keemail.me")



(require 'epa-file)
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(setq epa-file-select-keys 'silent)
(setq epa-file-encrypt-to "ghd@keemail.me")





(setq mu4e-maildir (expand-file-name "~/mail/salih"))

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

(setq mu4e-sent-messages-behavior 'delete)

(setq mu4e-maildir-shortcuts
      '(("/INBOX"             . ?i)
        ("/[Gmail].Sent Mail" . ?s)
        ("/[Gmail].Trash"     . ?t)))
(setq
 user-mail-address "salhghd7@gmail.com"
 user-full-name  "Salih Muhammed"
 message-signature
  (concat
    "Regards,\n"
    "Salih Muhammed\n"))

(set-email-account!
 "gmail"
 '((smtpmail-smtp-user     . "salhghd7@gmail.com")) t)







(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo.gpg")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

(setq +mu4e-backend 'offlineimap)


(after! mu4e
(setq mu4e-get-mail-command "offlineimap"
      mu4e-update-interval 100
      mu4e-compose-format-flowed t
      mu4e-index-cleanup nil
      mu4e-index-lazy-check t
      mu4e-headers-date-format "%d.%m.%y"))




(setq mail-user-agent 'gnus-user-agent)
(require 'org-msg)
(setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
	org-msg-startup "hidestars indent inlineimages"
	org-msg-greeting-name-limit 3
	org-msg-default-alternatives '((new		. (text html))
				       (reply-to-html	. (text html))
				       (reply-to-text	. (text)))
	org-msg-convert-citation t
	org-msg-signature "
Regards,

#+begin_signature
Salih Muhammed
#+end_signature")









(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(use-package nov-xwidget
  :demand t
  :after nov
  :config
  (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
  (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files))

(defun insert-formatted-citation (entry page)
  "Insert a formatted citation for the bibliographic entry with ENTRY at PAGE into the current buffer."
  (interactive
   (list (completing-read "Select a bibliographic entry: " (org-ref-get-bibtex-keys))
         (read-string "Page: ")))
  (let* ((bibtex-entry (bibtex-completion-get-entry entry))
         (author (cdr (assoc "author" bibtex-entry)))
         (year (cdr (assoc "year" bibtex-entry)))
         (title (cdr (assoc "title" bibtex-entry)))
         (publisher (cdr (assoc "publisher" bibtex-entry)))
         (translator (cdr (assoc "translator" bibtex-entry)))
         (editor (cdr (assoc "editor" bibtex-entry)))
         (url (cdr (assoc "url" bibtex-entry))))
    (insert (format "%s, %s. %s. %s%s,%s,%s. %s, p. %s."
                    (car (split-string author " and "))
                    (substring (car (split-string author ",")) 0 1)
                    year
                    title
                    (if editor (concat " Edited by " editor) "")
                    (if translator (concat " Translated by " translator) "")
                    (if publisher (concat " " publisher) "")
                    (if url (concat ", " url) "")
                    page))))



(defun insert-apa-citation ()
  "Insert APA citation from BibTeX entry selected with `citar-insert-citation`."
  (interactive)
  (let ((citekeys (citar-select-refs)))
    (citar-insert-keys citekeys)
    (let ((output-buffer (generate-new-buffer "*Pandoc Output*")))
      (shell-command (concat "pandoc -F pandoc-citeproc --bibliography=/home/ghd/configs/ref.bib --csl=/home/ghd/configs/aaps.csl -t plain " (mapconcat 'identity citekeys " ") " 2>&1") output-buffer output-buffer)
      (with-current-buffer output-buffer
        (let ((inhibit-read-only t))
          (erase-buffer))
        (insert-buffer-substring output-buffer)))))














;;; super-agenda-custom-commands.el

;;;; super
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
