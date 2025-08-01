;;; +l-prog-sly.el -*- lexical-binding: t; -*-

;; SLY advice
(advice-add 'sly-compile-string
            :before #'salih/sly--compile-eval-begin-print)
(advice-add 'sly-compile-file
            :before #'salih/sly--compile-eval-begin-print)
(advice-add 'sly-eval-print-last-expression
            :before   #'salih/sly--compile-eval-begin-print)
(advice-add 'sly-eval-with-transcript
            :before   #'salih/sly--compile-eval-begin-print)

;; Go macro advice
(advice-add 'gomacro--sanitize-string :override 'salih/gomacro--sanitize-string)

;; Deft advice
(advice-add 'deft-parse-title  :override #'cm/deft-parse-title)

;; Doom advice
(advice-add  #'doom-highlight-non-default-indentation-h :override #'ignore)

(provide '+l-prog-sly) 