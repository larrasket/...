(after! circe
  (defun salih/nickserv-password (server)
    (salih/fetch-password :user user-short-username :host "irc.libera.chat"))
  (set-irc-server! "irc.libera.chat"
    '(:tls t
      :port 6697
      :nick "lr0"
      :sasl-strict t
      :sasl-username "lr0"
      :sasl-password salih/nickserv-password
      :channels ("##arabic" "#emacs")))

  (circe-set-display-handler "JOIN" (lambda (&rest ignored) nil))
  (circe-set-display-handler "PART" (lambda (&rest ignored) nil))
  (circe-set-display-handler "MODE" (lambda (&rest ignored) nil))
  (circe-set-display-handler "QUIT" (lambda (&rest ignored) nil))
  (circe-set-display-handler "324"  (lambda (&rest ignored) nil))
  (circe-set-display-handler "329"  (lambda (&rest ignored) nil))
  (circe-set-display-handler "332"  (lambda (&rest ignored) nil))
  (circe-set-display-handler "333"  (lambda (&rest ignored) nil))
  (circe-set-display-handler "353"  (lambda (&rest ignored) nil))
  (require 'lui-autopaste)
  (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)

  (setq lui-flyspell-p                                    t
        circe-default-quit-message
        "I seek refuge in God, from Satan the rejected"
        lui-logging-directory                             "~/me/irc-logs"
        circe-reduce-lurker-spam                          t)

  (load "lui-logging" nil t)

  (enable-lui-logging-globally)

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
    (let ((pass (salih/lookup-password "irc.libera.chat"
                                       user-short-username 80)))
      (erc
       :server "irc.libera.chat"
       :nick user-short-username
       :password pass))))

(provide '+erc)
