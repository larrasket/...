;;; private/chess/config.el -*- lexical-binding: t; -*-


;; FIXME the path is not being set correctly.

(defun org-babel-execute:chess (body params)
  "Execute a block of Chess code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((output-dir (expand-file-name "chessimages" (file-name-directory (buffer-file-name))))
         (output-file (concat (format "_%s_chess_output.svg" (format-time-string "%Y-%m-%d_%H-%M-%S"))))
         (output-path (expand-file-name output-file output-dir))
         (notation (cdr (assq :notation params)))
         (extension (if (equal notation "fen") ".fen" ".pgn"))
         (notation-file (make-temp-file "chess-notation" nil extension))
         (cmd (format "python ~/configs/elchess.py %s %s %s" notation-file output-path notation)))
    (with-temp-buffer
      (insert body)
      (write-file notation-file))
    (shell-command cmd)
    (org-babel-result-to-file (file-relative-name output-path))))

(setq org-babel-default-header-args:chess
      '((:results . "raw")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((chess . t)))
