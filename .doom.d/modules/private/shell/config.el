;;; mine/shell/config.el -*- lexical-binding: t; -*-


    ;; (defun eshell-load-bash-aliases ()
    ;;        "Reads bash aliases from Bash and inserts
    ;; them into the list of eshell aliases."
    ;;        (interactive)
    ;;        (progn
    ;;                (message "Parsing aliases")
    ;;                (shell-command "alias" "bash-aliases" "bash-errors")
    ;;                (switch-to-buffer "bash-aliases")
    ;;                (replace-string "alias " "")
    ;;                (goto-char 1)
    ;;                (replace-string "='" " ")
    ;;                (goto-char 1)
    ;;                (replace-string "'\n" "\n")
    ;;                (goto-char 1)
    ;;                (let ((alias-name) (command-string) (alias-list))
    ;;                     (while (not (eobp))
    ;;                        (while (not (char-equal (char-after) 32))
    ;;                               (forward-char 1)
    ;;                            (setq alias-name
    ;;                                    (buffer-substring-no-properties (line-beginning-position) (point)))
    ;;                            (forward-char 1)
    ;;                            (setq command-string
    ;;                                    (buffer-substring-no-properties (point) (line-end-position)))
    ;;                            (setq alias-list (cons (list alias-name command-string) alias-list))
    ;;                            (forward-line 1)))
    ;;                     (setq eshell-command-aliases-list alias-list))
    ;;            (if (get-buffer "bash-aliases")(kill-buffer "bash-aliases"))
    ;;            (if (get-buffer "bash-errors")(kill-buffer "bash-errors"))))
    ;; (add-hook 'eshell-mode-hook 'eshell-load-bash-aliases)
;; (set-variable 'shell-command-switch "-c")

;; (remove-hook 'eshell-mode-hook 'hide-mode-line-mode t)
;;
  (defun eshell-load-bash-aliases ()
    "Read Bash aliases and add them to the list of eshell aliases."
    ;; Bash needs to be run - temporarily - interactively
    ;; in order to get the list of aliases.
      (with-temp-buffer
        (call-process "bash" nil '(t nil) nil "-ci" "alias")
        (goto-char (point-min))
        (while (re-search-forward "alias \\(.+\\)='\\(.+\\)'$" nil t)
          (eshell/alias (match-string 1) (match-string 2)))))

  ;; We only want Bash aliases to be loaded when Eshell loads its own aliases,
  ;; rather than every time `eshell-mode' is enabled.
  (add-hook 'eshell-alias-load-hook 'eshell-load-bash-aliases)

(after! eshell (remove-hook 'eshell-mode-hook 'hide-mode-line-mode))
