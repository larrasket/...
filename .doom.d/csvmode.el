;;; ../configs/.doom.d/csvmode.el -*- lexical-binding: t; -*-

(add-hook 'csv-mode-hook
          (lambda ()
            (define-key csv-mode-map (kbd "C-c C-M-a")
              (defun csv-align-visible (&optional arg)
                "Align visible fields"
                (interactive "P")
                (csv-align-fields nil (window-start) (window-end))))
            (csv-align-mode))
          )

(provide 'csvmode)
