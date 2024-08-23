(require 'erc-services)

(setq erc-nick                                          user-short-username
      erc-user-full-name                                user-full-name
      erc-prompt-for-password                           nil
      erc-prompt-for-nickserv-password                  nil
      erc-enable-logging                                t
      erc-log-channels-directory                        "~/me/irc-log"
      erc-quit-reason                                   'salih/quit-erc
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
                                                           "#go-nuts"
                                                           "#emacs"
                                                           "##arabic")))


(defun salih/quit-erc (s)
  "quit cute quote"
  "I seek refuge in God, from Satan the rejected")

(provide '+erc)
