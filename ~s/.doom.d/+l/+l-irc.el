;;; +l/irc.el -*- lexical-binding: t; -*-

;; Circe configuration
(use-package circe
  :custom
  (circe-default-quit-message "I seek refuge in God, from Satan the rejected")
  (circe-reduce-lurker-spam t)
  :config
  ;; Password function
  (defun salih/nickserv-password (server)
    (salih/fetch-password :user user-short-username :host "irc.libera.chat"))
  
  ;; Server configuration
  (set-irc-server! "irc.libera.chat"
    '(:tls t
      :port 6697
      :nick "lr0"
      :sasl-strict t
      :sasl-username "lr0"
      :sasl-password salih/nickserv-password
      :channels ("##arabic" "##philosophy")))
  
  ;; Display handlers
  (circe-set-display-handler "324"  (lambda (&rest ignored) nil))
  (circe-set-display-handler "329"  (lambda (&rest ignored) nil))
  (circe-set-display-handler "333"  (lambda (&rest ignored) nil))
  (circe-set-display-handler "353"  (lambda (&rest ignored) nil))
  
  ;; LUI configuration
  (require 'lui-autopaste)
  (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)
  
  ;; Logging configuration
  (setq lui-flyspell-p t
        lui-logging-directory "~/me/irc-logs")
  (load "lui-logging" nil t)
  (enable-lui-logging-globally)
  
  ;; Quit function
  (defun salih/quit-erc (s)
    "quit cute quote"
    "I seek refuge in God, from Satan the rejected"))
  
  ;; Password lookup function
  (defun salih/lookup-password (host user port)
    (let ((auth (auth-source-search :host host :user user :port port)))
      (if auth
          (let ((secretf (plist-get (car auth) :secret)))
            (if secretf
                (funcall secretf)
              (error "Auth entry for %s@%s:%s has no secret!"
                     user host port)))
        (error "No auth entry found for %s@%s:%s" user host port))))
  
  ;; IRC function
  (defun irc (&optional arg)
    (interactive "P")
    (let ((pass (salih/lookup-password "irc.libera.chat"
                                       user-short-username 80)))
      (erc
       :server "irc.libera.chat"
       :nick user-short-username
       :password pass)))

;; ERC SASL configuration
(use-package erc-sasl
  :custom
  (erc-sasl-use-sasl t)
  (erc-sasl-server-regexp-list '())
  :config
  (defun erc-sasl-use-sasl-p ()
    "Used internally to decide whether SASL should be used in the
current session"
    (and erc-sasl-use-sasl
         (boundp 'erc-session-server)
         (loop for re in erc-sasl-server-regexp-list
               thereis (integerp (string-match re erc-session-server)))))
  
  ;; Response handlers
  (define-erc-response-handler (CAP)
    "Client capability framework is used to request SASL auth, need
  to wait for ACK to begin" nil
    (let ((msg (erc-response.contents parsed)))
      (when (string-match " *sasl" msg)
        (erc-server-send "AUTHENTICATE PLAIN"))))
  
  (define-erc-response-handler (AUTHENTICATE)
    "Handling empty server response indicating ready to receive
  authentication." nil
    (if erc-session-password
        (let ((msg (erc-response.contents parsed)))
          (when (string= "+" msg)
            ;; plain auth
            (erc-server-send
             (format "AUTHENTICATE %s"
                     (base64-encode-string
                      (concat "\0" (erc-current-nick)
                              "\0" erc-session-password) t)))))
      (progn
        (erc-display-message
         parsed 'error
         (if erc-server-connected 'active proc)
         "You must set a password in order to use SASL authentication.")
        ;; aborting SASL auth
        (erc-server-send (erc-server-send "AUTHENTICATE *")))))
  
  (define-erc-response-handler (903)
    "Handling a successful SASL authentication." nil
    (erc-server-send "CAP END")))

;; Modeline configuration
(setq doom-modeline-irc-stylize #'ignore)

;; IRC advice
(advice-add '+irc/tracking-next-buffer
            :override #'salih/tracking-next-buffer--always-switch)

(provide '+l-irc) 