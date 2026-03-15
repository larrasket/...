;;; +l/email.el -*- lexical-binding: t; -*-

;; Email directory helper function

(use-package mu4e)
(after! mu4e
  (use-package mu4e
    :custom
    (set-email-account! "icloud"
                        '((mu4e-sent-folder       .)
                          (mu4e-drafts-folder     .)
                          (mu4e-trash-folder      .)
                          (mu4e-refile-folder     .))
                        t)

    (message-send-mail-function 'smtpmail-send-it)
    (starttls-use-gnutls t)
    (mu4e-headers-visible-lines 10)
    (mu4e-update-interval 500)
    (mu4e-compose-signature (format "Regards\n%s" user-first-name))
    (smtpmail-default-smtp-server user-stmp-server)
    (smtpmail-smtp-server smtpmail-default-smtp-server)
    (smtpmail-smtp-service user-stmp-port)
    (smtpmail-stream-type 'starttls)
    (mu4e-modeline-show-global nil)
    (mu4e-headers-fields '((:fast-folding . 2))
                         (:human-date . 12)
                         (:flags . 6)
                         (:mailing-list . 10)
                         (:from . 22)
                         (:subject))
    :config
    (defun salih/get-mail-password ()
      (interactive)
      (let* ((auth-info (auth-source-search :host "imap.mail.me.com"
                                            :require '(:user :secret))))
        (password (funcall (plist-get (car auth-info) :secret)))
        password))

    (setq mu4e-drafts-folder "/icloud/Drafts"
          mu4e-refile-folder "/icloud/Archive"
          mu4e-sent-folder "/icloud/Sent Messages"
          mu4e-trash-folder "/icloud/Junk")

    (define-key mu4e-view-mode-map (kbd "M-<down>")
                nil)

    
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

    (add-to-list 'mu4e-header-info-custom '(:fast-folding .
                                            (:name "fast-folding"
                                             :shortname ""
                                             :function mu4e-fast-folding-info)))

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
                                    "\"" mu4e-refile-folder "\"")))))


  (add-hook! 'mu4e-headers-mode-hook
    (visual-line-mode -1)))

(after! mu4e (setq mu4e-get-mail-command "mbsync --verbose --all"
                   mu4e-update-interval 300)




  (after! mu4e
    (add-to-list 'mu4e-bookmarks
                 '(:name "Personal"
                   :query "maildir:/icloud/Personal"
                   :key ?p))

    (add-to-list 'mu4e-bookmarks
                 '(:name "Automated"
                   :query "maildir:/icloud/Automated"
                   :key ?a))

    (add-to-list 'mu4e-bookmarks
                 '(:name "Burner"
                   :query "maildir:/icloud/Burner"
                   :key ?b))

    (add-to-list 'mu4e-bookmarks
                 '(:name "Old"
                   :query "maildir:/icloud/Old"
                   :key ?o))

    (add-to-list 'mu4e-bookmarks
                 '(:name "Gmail"
                   :query "maildir:/icloud/Gmail"
                   :key ?g))))

;; Mu4e headers mode hooks




(provide '+l-email)



