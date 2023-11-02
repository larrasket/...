(require 'erc-services)

(setq erc-nick                                          user-short-username
      erc-user-full-name                                user-full-name
      erc-prompt-for-password                           nil
      erc-prompt-for-nickserv-password                  nil
      erc-enable-logging                                t
      erc-log-channels-directory                        "~/log"
      erc-hide-list                                     '("JOIN" "PART"
                                                          "QUIT" "MODE")
      erc-autojoin-channels-alist                       '((Libera.Chat
                                                           "##tea"
                                                           "#erc"
                                                           "#org-mode"
                                                           "#go-nuts"
                                                           "#archlinux-offtopic"
                                                           "##chat"
                                                           "#emacs"
                                                           "##arabic")))

(after! erc (erc-spelling-mode)
  (erc-log-mode))

(provide '+erc)
