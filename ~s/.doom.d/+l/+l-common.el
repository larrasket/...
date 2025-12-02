;;; +l/common.el -*- lexical-binding: t; -*-

;; Theme-specific customizations

(defun salih/tmp-buffer ()
  "Open a new temporary buffer with a random name to play in."
  (interactive)
  (let ((bufname (generate-new-buffer-name
                  (format "*scratch-%x*" (random most-positive-fixnum)))))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)
    (message "Opened temporary buffer: %s" bufname)))

(provide '+l-common)
