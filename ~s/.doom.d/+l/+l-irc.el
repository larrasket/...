;;; +l/irc.el -*- lexical-binding: t; -*-

;; Circe configuration
(use-package circe
  :custom
  (circe-default-quit-message "I seek refuge in God, from Satan the rejected")
  (circe-reduce-lurker-spam t)
  :config
  ;; Password function
  (defun salih/tracking-next-buffer--always-switch (&rest _args)
    "Advice to always switch to the next unread buffer, bypassing the `circe-mode`
check."
    (tracking-next-buffer))

  (defun salih/fetch-password (&rest params)
    (let ((match (car (apply #'auth-source-search params))))
      (if match (let ((secret (plist-get match :secret)))
                  (if (functionp secret)
                      (funcall secret)
                    secret))
        (error "Password not found for %S" params))))


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
  ;; [2025-08-02 Sat 03:36] I no longer use erc, but I love this quote so much.
  ;; I guess I will just keep it in the codebase :)
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
  


;; Modeline configuration
(setq doom-modeline-irc-stylize #'ignore)

;; IRC advice
(advice-add '+irc/tracking-next-buffer
            :override #'salih/tracking-next-buffer--always-switch)

(provide '+l-irc)
