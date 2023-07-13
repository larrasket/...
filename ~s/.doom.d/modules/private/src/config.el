;;; private/src/config.el -*- lexical-binding: t; -*-


(require 'erc-services)

(setq-default erc-nick                                "lr0"
    erc-user-full-name                                user-full-name
    erc-prompt-for-password                           nil
    erc-prompt-for-nickserv-password                  nil
    erc-autojoin-channels-alist                       '(("irc.libera.chat"
                                                         "##arabic")))
 

(erc-spelling-mode)
