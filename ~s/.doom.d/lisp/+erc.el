(require 'erc-services)
;; (require '+erc-sasl)

(setq erc-nick                                          user-short-username
      erc-user-full-name                                user-full-name
      erc-prompt-for-password                           nil
      erc-prompt-for-nickserv-password                  nil
      erc-enable-logging                                t
      erc-log-channels-directory                        "~/log"
      erc-hide-list                                     '("JOIN" "PART" "MODE"
                                                          "324"   ; modes
                                                          "329"   ; channel creation date
                                                          "332"   ; topic notice
                                                          "333"   ; who set the topic
                                                          "353")  ; Names notice

      erc-autojoin-channels-alist                       '((Libera.Chat
                                                           "##tea"
                                                           "#org-mode"
                                                           "#go-nuts"
                                                           "#archlinux-offtopic"
                                                           "#emacs"
                                                           "##arabic")))

;; (add-to-list 'erc-sasl-server-regexp-list "irc\\.libera\\.chat")



;; (defun erc-login ()
;;   "Perform user authentication at the IRC server. (PATCHED)"
;;   (erc-log (format "login: nick: %s, user: %s %s %s :%s"
;;                    (erc-current-nick)
;;                    (user-login-name)
;;                    (or erc-system-name (system-name))
;;                    erc-session-server
;;                    erc-session-user-full-name))
;;   (if erc-session-password
;;       (erc-server-send (concat "PASS :" (erc--unfun erc-session-password)))
;;     (message "Logging in without password"))
;;   (when (and (featurep '+erc-sasl)
;;              (erc-sasl-use-sasl-p))
;;     (erc-server-send "CAP REQ :sasl"))
;;   (erc-server-send (format "NICK %s" (erc-current-nick)))
;;   (erc-server-send
;;    (format "USER %s %s %s :%s"
;;            ;; hacked - S.B.
;;            erc-session-username
;;            "0" "*"
;;            erc-session-user-full-name))
;;   (erc-update-mode-line))




(after! erc
  (erc-spelling-mode)
  (erc-log-mode))


(provide '+erc)
