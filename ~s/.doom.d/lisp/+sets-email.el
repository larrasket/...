;;; ../../../../:/home/l/configs/~s/.doom.d/lisp/+sets-email.el -*- lexical-binding: t; -*-


;; setup email
;; don't forget to setup authinfo
;; https://www.emacswiki.org/emacs/GnusAuthinfo
;; and of course mu, isync
;; tidy is required to use with xwidget:
;; pacman -S tidy
(after! mu4e
  (setq message-send-mail-function              'smtpmail-send-it
        starttls-use-gnutls                     t
        mu4e-compose-reply-ignore-address       `("no-?reply"
                                                  ,user-mail-address)
        mu4e-headers-visible-lines              10
        mu4e-update-interval                    500
        mu4e-compose-signature                  (format "Regards,\n%s"
                                                        user-first-name)
        smtpmail-default-smtp-server
        smtpmail-smtp-server                    smtpmail-default-smtp-server
        smtpmail-smtp-service                   user-stmp-port
        smtpmail-stream-type                    'starttls
        mu4e-alert-interesting-mail-query       (concat "flag:unread"
                                                        " AND NOT flag:trashed"
                                                        " AND NOT maildir:" "\"/rss\""
                                                        " AND NOT maildir:" "\"/read\""
                                                        " AND NOT maildir:" "\"/archive\"")
        mu4e-modeline-show-global               nil)


  (defun mu4e-action-view-in-xwidget (msg)
    (unless (fboundp 'xwidget-webkit-browse-url)
      (mu4e-error "No xwidget support available"))
    (let ((browse-url-handlers nil)
          (browse-url-browser-function (lambda (url &optional _rest)
                                         (with-output-to-string
                                           (call-process "tidy" nil nil nil "-m"
                                                         "--numeric-entities"
                                                         "yes"
                                                         (replace-regexp-in-string
                                                          "^file://" "" url)))
                                         (xwidget-webkit-browse-url url))))
      (mu4e-action-view-in-browser msg))))

(provide '+sets-email)
