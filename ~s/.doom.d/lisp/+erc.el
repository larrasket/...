(require 'erc-services)
(require 'erc-sasl)
(require '+erc-sasl)


(setq erc-nick                                          user-short-username
      erc-user-full-name                                user-full-name
      erc-prompt-for-password                           nil
      erc-prompt-for-nickserv-password                  nil
      erc-enable-logging                                t
      erc-log-channels-directory                        "~/me/irc-log"
      erc-quit-reason                                   'salih/quit-erc
      erc-hide-list                                     '("JOIN" "PART" "MODE" "QUIT"
                                                          "324"  ; modes
                                                          "329"  ; channel creation date
                                                          "332"  ; topic notice
                                                          "333"  ; who set the topic
                                                          "353") ; Names notice

      erc-autojoin-channels-alist                       '((Libera.Chat
                                                           "#org-mode"
                                                           "#go-nuts"
                                                           "#emacs"
                                                           "##arabic")))


(defun salih/erc-connect ()
    "Connect to Ement with credentials from Authinfo."
  (interactive)
  (let* ((auth-info (auth-source-search :host "irc.libera.chat"
                                        :require '(:user :secret)))
         (user-id (plist-get (car auth-info) :user))
         (password (funcall (plist-get (car auth-info) :secret))))
    (erc-tls       :user     user-id
                   :password password)))

(defun salih/quit-erc (s)
  "quit cute quote"
  "I seek refuge in God, from Satan the rejected")


(after! erc
  (erc-spelling-mode)
  (erc-log-mode))


(add-to-list 'erc-sasl-server-regexp-list "irc\\.freenode\\.net")
(add-to-list 'erc-sasl-server-regexp-list "irc\\.libra\\.chat")
(add-to-list 'erc-sasl-server-regexp-list "irc\\.oftc\\.net")


(defun erc-login ()
  "Perform user authentication at the IRC server."
  (erc-log (format "login: nick: %s, user: %s %s %s :%s"
                   (erc-current-nick)
                   (user-login-name)
                   (or erc-system-name (system-name))
                   erc-session-server
                   erc-session-user-full-name))
  (if erc-session-password
      (erc-server-send (concat "PASS :" (erc--unfun erc-session-password)))
    (message "Logging in without password"))
  (when (and (featurep 'erc-sasl) (erc-sasl-use-sasl-p))
    (erc-server-send "CAP REQ :sasl"))
  (erc-server-send (format "NICK %s" (erc-current-nick)))
  (erc-server-send
   (format "USER %s %s %s :%s"
           erc-session-username
           "0" "*"
           erc-session-user-full-name))
  (erc-update-mode-line))



(provide '+erc)
