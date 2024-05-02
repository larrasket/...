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
        smtpmail-default-smtp-server            user-stmp-server
        smtpmail-smtp-server                    smtpmail-default-smtp-server
        smtpmail-smtp-service                   user-stmp-port
        smtpmail-stream-type                    'starttls
        mu4e-drafts-folder                      (s/cm mu4e-drafts-folder)
        mu4e-refile-folder                      (s/cm mu4e-refile-folder)
        mu4e-sent-folder                        (s/cm mu4e-sent-folder)
        mu4e-trash-folder                       (s/cm mu4e-trash-folder)
        mu4e-rss-folder                         (s/cm "/rss")
        mu4e-read-folder                        (s/cm "/read")
        mu4e-alert-interesting-mail-query       (concat "flag:unread"
                                                        " AND NOT flag:trashed"
                                                        " AND NOT maildir:"
                                                        "\"" mu4e-rss-folder "\""
                                                        " AND NOT maildir:"
                                                        "\"" mu4e-read-folder "\""
                                                        " AND NOT maildir:"
                                                        "\"" mu4e-refile-folder "\"")
        mu4e-modeline-show-global               nil
        mu4e-headers-fields                     '((:fast-folding . 2)
                                                  (:human-date . 12)
                                                  (:flags . 6)
                                                  (:mailing-list . 10)
                                                  (:from . 22)
                                                  (:subject)))


  (defun mu4e-fast-folding-info (msg)
   (let* ((thread (mu4e-message-field msg :thread))
          (prefix (mu4e~headers-thread-prefix thread))
          (unread (memq 'unread (mu4e-message-field msg :flags))))
     (concat
      (if (= (length prefix) 0) " " " ")
      (if unread "•" " "))))

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
      (mu4e-action-view-in-browser msg)))

  (add-to-list 'mu4e-header-info-custom '(:fast-folding . (:name "fast-folding"
                                                           :shortname ""
                                                           :function mu4e-fast-folding-info)))

  ;; [2024-05-02 Thu 07:00] I find the look with doom theme quite annoying.
  (when (doom-theme?)
    (custom-set-faces
     '(mu4e-highlight-face ((t (:inherit mu4e-header-face :background nil)))))))



  


   ;; Specific character to later detect unread






(provide '+sets-email)
