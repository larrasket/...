(require 'erc-services)
(require 'erc-sasl)

(setq erc-nick                                          user-short-username
      erc-user-full-name                                user-full-name
      erc-prompt-for-password                           nil
      erc-prompt-for-nickserv-password                  nil
      erc-enable-logging                                t
      erc-log-channels-directory                        "~/me/irc-log"
      erc-quit-reason                                   'salih/quit-erc
      erc-sasl-mechanism                                'scram
      erc-hide-list                                     '("JOIN"
                                                          "PART"
                                                          "MODE"
                                                          "QUIT"
                                                          "324"  ; modes
                                                          "329"  ; channel creation date
                                                          "332"  ; topic notice
                                                          "333"  ; who set the topic
                                                          "353") ; Names notice

      erc-autojoin-channels-alist                       '((Libera.Chat
                                                           "#org-mode"
                                                           "#emacs"
                                                           "##arabic")))

(defun salih/quit-erc (s)
  "quit cute quote"
  "I seek refuge in God, from Satan the rejected")

(defun salih/lookup-password (host user port)
  (let ((auth (auth-source-search :host host :user user :port port)))
    (if auth
        (let ((secretf (plist-get (car auth) :secret)))
          (if secretf
              (funcall secretf)
            (error "Auth entry for %s@%s:%s has no secret!"
                   user host port)))
      (error "No auth entry found for %s@%s:%s" user host port))))

(defun irc (&optional arg)
    (interactive "P")
    (let ((pass (salih/lookup-password "irc.libera.chat" "lr0" 80)))
      (erc
       :server "irc.libera.chat"
       :nick user-short-username
       :password pass)))

(provide '+erc)
