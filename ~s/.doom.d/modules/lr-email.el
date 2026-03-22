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

  ;; Send mail via msmtp asynchronously — never blocks the main thread.
  ;; message-send-mail-with-sendmail uses call-process-region (synchronous).
  ;; We replace it with start-process + process-send-region (non-blocking).
  (defun salih/send-mail-with-msmtp ()
    "Send mail via msmtp without blocking Emacs."
    (let* ((to (message-field-value "To"))
           (buf (generate-new-buffer " *msmtp*"))
           (proc (start-process "msmtp" buf
                                (executable-find "msmtp")
                                "--read-envelope-from" "-t")))
      (set-process-sentinel
       proc
       (lambda (p event)
         (cond
          ((string-match-p "finished" event)
           (message "✓ Mail to %s delivered." to)
           (kill-buffer (process-buffer p)))
          (t
           (message "✗ Mail to %s failed — see buffer %s"
                    to (buffer-name (process-buffer p)))
           (display-buffer (process-buffer p))))))
      (process-send-region proc (point-min) (point-max))
      (process-send-eof proc)
      (message "Sending to %s..." to)))

  (setq send-mail-function       #'salih/send-mail-with-msmtp
        message-send-mail-function #'salih/send-mail-with-msmtp)

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
