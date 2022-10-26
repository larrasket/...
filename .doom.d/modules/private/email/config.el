;;; mine/email/config.el -*- lexical-binding: t; -*-


(after! mu4e
  ;; load package to be able to capture emails for GTD
  (require 'org-mu4e)
  ;; do not use rich text emails
  (remove-hook! 'mu4e-compose-mode-hook #'org-mu4e-compose-org-mode)
  ;; ensure viewing messages and queries in mu4e workspace
  ;; instead of displaying the fallback buffer (dashboard) after quitting mu4e, switch to last active buffer in workspace
  ;; attach files to messages by marking them in dired buffer
  (require 'gnus-dired)
  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook! 'dired-mode-hook #'turn-on-gnus-dired-mode)
  ;; disable line wrapping when viewing headers
  ;; configure mu4e options
  (setq mu4e-confirm-quit nil ; quit without asking
        mu4e-attachment-dir "~/Downloads"
        mu4e-maildir (expand-file-name "~/email/mbsyncmail")
        mu4e-get-mail-command "mbsync nameaccount"
        mu4e-user-mail-address-list '("jabrr7@outlook.com" "jabrr7@outlook.com")
	    user-mail-address "jabrr7@outlook.com"
	    user-full-name "Salh Jabr")
  (setq message-send-mail-function 'smtpmail-send-it
	smtpmail-stream-type 'starttls
	smtpmail-default-smtp-server "smtp-mail.outlook.com"
	smtpmail-smtp-server "smtp-mail.outlook.com"
	smtpmail-smtp-service 587)
  ;; add custom actions for messages
  (add-to-list 'mu4e-view-actions
	       '("View in browser" . mu4e-action-view-in-browser) t))




(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)



(setq mu4e-html2text-command "w3m -T text/html" ; how to hanfle html-formatted emails
      mu4e-update-interval 300                  ; seconds between each mail retrieval
      mu4e-headers-auto-update t                ; avoid to type `g' to update
      mu4e-view-show-images t                   ; show images in the view buffer
      mu4e-compose-signature-auto-include nil   ; I don't want a message signature
      mu4e-use-fancy-chars t)                   ; allow fancy icons for mail threads



;; Revert Doom-emacs fast index settings back to default
(with-eval-after-load 'mu4e
  (setq
    mu4e-index-cleanup t      ;; do a full cleanup check
    mu4e-index-lazy-check nil))    ;; consider up-to-date dirs
