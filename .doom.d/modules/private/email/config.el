;;; private/email/config.el -*- lexical-binding: t; -*-


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

(after! mu4e)
(setq mu4e-get-mail-command "offlineimap"
      mu4e-update-interval 100
      mu4e-compose-format-flowed t
      mu4e-index-cleanup nil
      mu4e-index-lazy-check t
      mu4e-headers-date-format "%d.%m.%y")



(setq mail-user-agent 'gnus-user-agent)
(require 'org-msg)






(setq mail-user-agent 'gnus-user-agent)
(require 'org-msg)
(setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
	org-msg-startup "hidestars indent inlineimages"
	org-msg-greeting-fmt "\nHi%s,\n\n"
	org-msg-greeting-name-limit 3
	org-msg-default-alternatives '((new		. (text html))
				       (reply-to-html	. (text html))
				       (reply-to-text	. (text)))
	org-msg-convert-citation t
	org-msg-signature "

Regards,

#+begin_signature
--
*Salih Muhammed*
#+end_signature")
(org-msg-mode)
