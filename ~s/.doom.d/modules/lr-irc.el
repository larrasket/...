;;; lr-irc.el --- Circe IRC configuration -*- lexical-binding: t; -*-

(after! circe
  (setq circe-default-quit-message "I seek refuge in God, from Satan the rejected"
        circe-reduce-lurker-spam t)

  ;; Password
  (defun salih/fetch-password (&rest params)
    (let ((match (car (apply #'auth-source-search params))))
      (if match
          (let ((secret (plist-get match :secret)))
            (if (functionp secret) (funcall secret) secret))
        (error "Password not found for %S" params))))

  (defun salih/nickserv-password (_server)
    (salih/fetch-password :user user-short-username :host "irc.libera.chat"))

  ;; Server
  (set-irc-server! "irc.libera.chat"
    '(:tls t :port 6697
      :nick "larrasket"
      :sasl-strict t :sasl-username "larrasket"
      :sasl-password salih/nickserv-password
      :channels ("##arabic" "##philosophy")))

  ;; Suppress noisy handlers
  (dolist (code '("324" "329" "333" "353"))
    (circe-set-display-handler code (lambda (&rest _) nil)))

  ;; LUI
  (require 'lui-autopaste)
  (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)
  (setq lui-flyspell-p t
        lui-logging-directory "~/me/irc-logs")
  (load "lui-logging" nil t)
  (enable-lui-logging-globally)

  ;; Tracking
  (defun salih/tracking-next-buffer--always-switch (&rest _)
    (tracking-next-buffer))

  (advice-add '+irc/tracking-next-buffer
              :override #'salih/tracking-next-buffer--always-switch))

;; Modeline
(setq doom-modeline-irc-stylize #'ignore)

(provide 'lr-irc)
