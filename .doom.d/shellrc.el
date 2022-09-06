;;; shellrc.el -*- lexical-binding: t; -*-

(provide 'shellrc)

    (defun eshell-load-bash-aliases ()
           "Reads bash aliases from Bash and inserts
    them into the list of eshell aliases."
           (interactive)
           (progn
                   (message "Parsing aliases")
                   (shell-command "alias" "bash-aliases" "bash-errors")
                   (switch-to-buffer "bash-aliases")
                   (replace-string "alias " "")
                   (goto-char 1)
                   (replace-string "='" " ")
                   (goto-char 1)
                   (replace-string "'\n" "\n")
                   (goto-char 1)
                   (let ((alias-name) (command-string) (alias-list))
                        (while (not (eobp))
                           (while (not (char-equal (char-after) 32))
                                  (forward-char 1))
                               (setq alias-name
                                       (buffer-substring-no-properties (line-beginning-position) (point)))
                               (forward-char 1)
                               (setq command-string
                                       (buffer-substring-no-properties (point) (line-end-position)))
                               (setq alias-list (cons (list alias-name command-string) alias-list))
                               (forward-line 1))
                        (setq eshell-command-aliases-list alias-list))
               (if (get-buffer "bash-aliases")(kill-buffer "bash-aliases"))
               (if (get-buffer "bash-errors")(kill-buffer "bash-errors"))))
    (add-hook 'eshell-mode-hook 'eshell-load-bash-aliases)
(set-variable 'shell-command-switch "-c")
