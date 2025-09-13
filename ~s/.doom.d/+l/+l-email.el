;;; +l/email.el -*- lexical-binding: t; -*-

;; Email directory helper function
(defun email-dir (folder)
  "Construct email directory path."
  (concat "/" user-mail-address "/" folder))

;; Mu4e configuration

(use-package mu4e)
(after! mu4e
  (use-package mu4e
    :custom
    (message-send-mail-function 'smtpmail-send-it)
    (starttls-use-gnutls t)
    (mu4e-compose-reply-ignore-address `("no-?reply" ,user-mail-address))
    (mu4e-headers-visible-lines 10)
    (mu4e-update-interval 500)
    (mu4e-compose-signature (format "Regards\n%s" user-first-name))
    (smtpmail-default-smtp-server user-stmp-server)
    (smtpmail-smtp-server smtpmail-default-smtp-server)
    (smtpmail-smtp-service user-stmp-port)
    (smtpmail-stream-type 'starttls)
    (mu4e-modeline-show-global nil)
    (mu4e-headers-fields '((:fast-folding . 2)
                           (:human-date . 12)
                           (:flags . 6)
                           (:mailing-list . 10)
                           (:from . 22)
                           (:subject)))
    :config
    (defun salih/get-mail-password ()
      (interactive)
      (let* ((auth-info (auth-source-search :host "mail.gmx.com"
                                            :require '(:user :secret)))
             (password (funcall (plist-get (car auth-info) :secret))))
        password))

    (setq mu4e-drafts-folder (email-dir "Drafts")
          mu4e-refile-folder (email-dir "Archive")
          mu4e-sent-folder (email-dir "Sent")
          mu4e-trash-folder (email-dir "Trash")
          mu4e-rss-folder (email-dir "rss")
          mu4e-read-folder (email-dir "read"))


    (defun salih/feeds-- ()
      (if (featurep 'mu4e)
          (progn
            (setq mu4e-search-threads nil)
            (mu4e-search "maildir:\"/lr0@gmx.com/rss\" flag:unread")
            (mu4e-search-change-sorting :from 'descending))
        (progn
          (setq mu4e-search-threads t)
          (mu4e))))


    ;; Set alert query
    (setq mu4e-alert-interesting-mail-query
          (concat
           "flag:unread"
           " AND NOT flag:trashed"
           " AND NOT maildir:"
           "\"" mu4e-rss-folder "\""
           " AND NOT maildir:"
           "\"" mu4e-read-folder "\""
           " AND NOT maildir:"
           "\"" mu4e-refile-folder "\""))

    ;; Remove M-<down> binding
    (define-key mu4e-view-mode-map (kbd "M-<down>")
                nil)

    (defun salih/open-inbox ()
      (interactive)
      (setq mu4e-search-threads t)
      (if (featurep 'mu4e)
          (progn
            (mu4e~headers-jump-to-maildir "/lr0@gmx.com/Inbox")
            (mu4e-search-change-sorting :date 'descending))
        (mu4e)))


    (defun salih/insert-current-date ()
      (interactive)
      (if (eq major-mode 'org-mode)
          (progn
            (insert "- " (format-time-string "[%Y-%m-%d %a %H:%M]") " "))
        (let ((current-prefix-arg '(16)))
          (call-interactively 'org-time-stamp-inactive)
          (insert " "))))


    (defun salih/org-roam-dailies-capture-today ()
      (interactive)
      (setq salih/org-roam-dailies-capture-p t)
      (call-interactively #'org-roam-dailies-capture-today))

    (defun salih/org-roam-buffer ()
      "Display the Org Roam buffer for the node at point."
      (interactive)
      (let ((node (org-roam-node-at-point)))
        (when node
          (org-roam-buffer-display-dedicated node))))

    (defun salih/consult-org-roam-search-org-only ()
      (interactive)
      (let ((consult-ripgrep-args
             (concat
              consult-ripgrep-args
              " -g *.org")))
        (consult-org-roam-search)))


    (defun salih/open-rss (readanywayg)
      "Open RSS using mu4e, only callable once per hour within the same day."
      ;; [2024-10-30 Wed 22:41] Currently, Just run it
      (if readanywayg (salih/feeds--)
        (let* ((now (current-time))
               (last-open-time (salih/load-last-open-rss-time)))
          (if (or (not last-open-time)
                  (salih/within-hour-window-p last-open-time now))
              (progn
                ;; Save only the first time within the hour window, not on
                ;; subsequent calls
                (when (salih/different-day-p last-open-time now)
                  (salih/save-last-open-rss-time now))
                ;; Execute the main command
                (salih/feeds--))
            (message
             "This command can only be called once within the same hour of a day.")))))

    (defun salih/load-last-open-rss-time ()
      "Load the last execution time from the cache file."
      (when (f-exists? salih/open-rss-lock-file)
        (with-temp-buffer
          (insert-file-contents salih/open-rss-lock-file)
          (read (current-buffer)))))


    (defun salih/read-feeds-anyway () (interactive) (salih/open-rss t))

    (defun salih/read-feeds () (interactive) (salih/open-rss t))


    (defun salih/mu4e-go-to-url ()
      (interactive)
      (let ((browse-url-browser-function 'salih/open-url-in-chrome-cross-platform))
        (call-interactively #'mu4e-view-go-to-url)))


    (defun salih/mu4e-view-and-copy-html ()
      "View message as HTML in temp browser and copy to clipboard."
      (interactive)
      (let* ((msg (mu4e-message-at-point))
             (html-temp-file (make-temp-file "mu4e-html-" nil ".html")))
        (mu4e-action-view-in-browser msg)))

    (defun salih/mu4e-org-store-and-capture ()
      "Store a link to the current message or query.
\(depending on `mu4e-org-link-query-in-headers-mode', and capture
it with org)."
      (interactive)
      (call-interactively 'org-store-link)
      (org-capture nil "f"))





    ;; Custom fast folding function
    (defun mu4e-fast-folding-info (msg)
      (let* ((thread (mu4e-message-field msg :thread))
             (prefix (mu4e~headers-thread-prefix thread))
             (unread (memq 'unread (mu4e-message-field msg :flags))))
        (concat
         (if (= (length prefix)
                0)
             " " " ")
         (if unread "•" " "))))

    ;; XWidget action
    (defun mu4e-action-view-in-xwidget (msg)
      (unless (fboundp 'xwidget-webkit-browse-url)
        (mu4e-error "No xwidget support available"))
      (let ((browse-url-handlers nil)
            (browse-url-browser-function (lambda (url &optional _rest)
                                           (with-output-to-string
                                             (call-process
                                              "tidy" nil nil nil "-m"
                                              "--numeric-entities"
                                              "yes"
                                              (replace-regexp-in-string
                                               "^file://" "" url)))
                                           (xwidget-webkit-browse-url url))))
        (mu4e-action-view-in-browser msg)))

    ;; Add custom header info
    (add-to-list 'mu4e-header-info-custom '(:fast-folding .
                                            (:name "fast-folding"
                                             :shortname ""
                                             :function mu4e-fast-folding-info)))

    ;; Update bookmark query
    (let ((bookmark (seq-find (lambda (b)
                                (string= (plist-get b :name)
                                         "Unread messages"))
                              mu4e-bookmarks)))
      (when bookmark
        (plist-put bookmark :query (concat
                                    "flag:unread AND NOT flag:trashed"
                                    " AND NOT maildir:"
                                    "\"" mu4e-rss-folder "\""
                                    " AND NOT maildir:"
                                    "\"" mu4e-refile-folder "\""))))

    )
  (add-hook! 'mu4e-headers-mode-hook
  (visual-line-mode -1)))


;; Mu4e headers mode hooks
(provide '+l-email)
