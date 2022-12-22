;;; private/caption/config.el -*- lexical-binding: t; -*-

;; This configuration adds capitalized words of .aspell.en.pws
;; (aspell's user dictionary)
(require 'auto-capitalize)
(setq auto-capitalize-words `("I" "English"))
(setq auto-capitalize-aspell-file "path/to/.aspell.en.pws")
(auto-capitalize-setup)



(defun org-auto-capitalize-headings-and-lists ()
  "Create a buffer-local binding of sentence-end to auto-capitalize
section headings and list items."
  (make-local-variable 'sentence-end)
  (setq sentence-end (concat (rx (or
                                  ;; headings
                                  (seq line-start (1+ "*") (1+ space))
                                  ;; list and checklist items
                                  (seq line-start (0+ space) "-" (1+ space) (? (or "[ ]" "[X]") (1+ space)))))
                             "\\|" (sentence-end))))
(add-hook 'org-mode-hook #'org-auto-capitalize-headings-and-lists)
