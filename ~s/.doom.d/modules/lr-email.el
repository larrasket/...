;;; lr-email.el --- mu4e configuration -*- lexical-binding: t; -*-

;;; --- Mu4e (fully deferred) ---
(after! mu4e
  (setq mu4e-get-mail-command "mbsync --verbose --all"
        mu4e-update-interval 300
        mu4e-headers-visible-lines 10
        mu4e-modeline-show-global nil
        mu4e-compose-signature (format "Regards\n%s" user-first-name)
        mu4e-headers-fields '((:human-date   . 12)
                               (:flags        . 6)
                               (:mailing-list . 10)
                               (:from         . 22)
                               (:subject      . nil)))

  ;; Folders
  (setq mu4e-drafts-folder "/icloud/Drafts"
        mu4e-refile-folder "/icloud/Archive"
        mu4e-sent-folder   "/icloud/Sent Messages"
        mu4e-trash-folder  "/icloud/Junk")

  ;; SMTP via smtpmail (Emacs built-in) — avoids the msmtp subprocess deadlock.
  ;; The deadlock: Emacs blocks on msmtp → msmtp calls getmupassword.sh →
  ;; getmupassword.sh calls emacsclient → Emacs is blocked → hang.
  ;; smtpmail reads auth-source directly (no subprocess, no roundtrip).
  (setq send-mail-function    'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server  "smtp.mail.me.com"
        smtpmail-smtp-service 465
        smtpmail-stream-type  'ssl
        smtpmail-smtp-user    "root@lr0.org")

  ;; If authinfo.gpg only has imap.mail.me.com, copy those creds into the
  ;; auth-source memory cache under the SMTP host before each send.
  (defun salih/prefetch-smtp-password-h ()
    (require 'auth-source)
    (unless (car (auth-source-search :host "smtp.mail.me.com" :require '(:secret)))
      (when-let* ((entry (car (auth-source-search :host "imap.mail.me.com"
                                                   :require '(:user :secret))))
                  (user (plist-get entry :user))
                  (pass (funcall (plist-get entry :secret))))
        (auth-source-remember `(:host "smtp.mail.me.com" :port "465" :user ,user)
                               (list (list :host "smtp.mail.me.com"
                                           :port "465"
                                           :user user
                                           :secret (lambda () pass)))))))

  (add-hook 'message-send-hook #'salih/prefetch-smtp-password-h)

  ;; Bookmarks
  (dolist (bm '((:name "Personal"  :query "maildir:/icloud/Personal"  :key ?p)
                (:name "Automated" :query "maildir:/icloud/Automated" :key ?a)
                (:name "Burner"    :query "maildir:/icloud/Burner"    :key ?b)
                (:name "Old"       :query "maildir:/icloud/Old"       :key ?o)
                (:name "Gmail"     :query "maildir:/icloud/Gmail"     :key ?g)))
    (add-to-list 'mu4e-bookmarks bm))

  ;; Unbind conflicting key
  (define-key mu4e-view-mode-map (kbd "M-<down>") nil)

  ;; Helper functions
  (defun salih/get-mail-password ()
    (interactive)
    (let ((auth-info (auth-source-search :host "imap.mail.me.com"
                                         :require '(:user :secret))))
      (funcall (plist-get (car auth-info) :secret))))

  (defun salih/mu4e-go-to-url ()
    (interactive)
    (let ((browse-url-browser-function 'salih/open-url-in-chrome-cross-platform))
      (call-interactively #'mu4e-view-go-to-url)))

  (defun salih/mu4e-view-and-copy-html ()
    "View message as HTML."
    (interactive)
    (mu4e-action-view-in-browser (mu4e-message-at-point)))

  (defun salih/mu4e-org-store-and-capture ()
    "Store link to current message and capture."
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "f"))

  ;; Disable visual line in headers
  (add-hook! 'mu4e-headers-mode-hook (visual-line-mode -1)))

;;; --- Mu4e-alert (lazy) ---
(after! mu4e-alert
  (setq mu4e-alert-interesting-mail-query
        (concat "flag:unread"
                " AND NOT flag:trashed"
                " AND NOT maildir:"
                "\"/[Gmail].All Mail\"")))

(add-hook 'doom-first-buffer-hook
          (lambda () (when (featurep 'mu4e) (mu4e-alert-enable-mode-line-display))))

(provide 'lr-email)
