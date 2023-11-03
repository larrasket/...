(require 'erc-services)
(require 'erc-sasl)



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


(provide '+erc)
