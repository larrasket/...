;;; ../configs/.doom.d/chatconf.el -*- lexical-binding: t; -*-

(map! :leader
      (:prefix ("e". "evaluate/ERC/EWW")
       :desc "Launch ERC with TLS connection" "E" #'erc-tls))

(setq erc-prompt (lambda () (concat "[" (buffer-name) "]"))
      erc-server "irc.libera.chat"
      erc-nick "wqopgkpopokomng"
      ;; erc-user-full-name "Derek Taylor"
      erc-track-shorten-start 24
      erc-autojoin-channels-alist '(("irc.libera.chat" "#ghdghd2"))
      erc-kill-buffer-on-part t
      erc-fill-column 100
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 20)
