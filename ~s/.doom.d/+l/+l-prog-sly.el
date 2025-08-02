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

;; Doom advice
(advice-add  #'doom-highlight-non-default-indentation-h :override #'ignore)

(provide '+l-prog-sly)
