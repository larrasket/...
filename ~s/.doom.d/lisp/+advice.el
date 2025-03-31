;;; ../configs/~s/.doom.d/lisp/+advice.el -*- lexical-binding: t; -*-

(setq salih/adding-note? nil)

(defun salih/start-note () (setq salih/adding-note? t))

(advice-add 'org-agenda          :before #'vulpea-agenda-files-update)
(advice-add 'org-clock-in        :before #'salih/toggle-logbook-on)
(advice-add 'org-clock-in        :after  #'salih/toggle-stats-on)
(advice-add 'org-todo-list       :before #'vulpea-agenda-files-update)
(advice-add 'org-agenda-quit     :before #'org-save-all-org-buffers)
(advice-add 'org-log-beginning   :before #'salih/toggle-log-int-drawer-off)
(advice-add 'org-log-beginning   :after  #'salih/toggle-stats-on)
(advice-add 'org-add-note        :before #'salih/start-note)
(advice-add 'org-add-note        :before #'salih/toggle-log-int-drawer-off)
(advice-add 'org-add-note        :after  #'salih/toggle-stats-on)
(advice-add 'org-agenda-add-note :before #'salih/start-note)
(advice-add 'org-agenda-add-note :before #'salih/toggle-log-int-drawer-off)
(advice-add 'org-agenda-add-note :after  #'salih/toggle-stats-on)
(advice-add 'org-media-note-insert-link
            :around #'salih/org-media-note-insert-link)
(advice-add '+lookup/documentation :around #'salih/ensure-eww-in-search)

(advice-add 'org-ql-view--format-element
            :around #'salih/org-ql-view--format-element)

(advice-add 'sly-compile-string
            :before #'salih/sly--compile-eval-begin-print)
(advice-add 'sly-compile-file
            :before #'salih/sly--compile-eval-begin-print)
;; `C-j' in `sly-scratch' buffer
(advice-add 'sly-eval-print-last-expression
            :before   #'salih/sly--compile-eval-begin-print)
(advice-add 'sly-eval-with-transcript
            :before   #'salih/sly--compile-eval-begin-print)

(advice-add 'lsp-resolve-final-command
            :around   #'lsp-booster--advice-final-command)

(advice-add '+irc/tracking-next-buffer
            :override #'salih/tracking-next-buffer--always-switch)

(advice-add 'deft-parse-title  :override #'cm/deft-parse-title)
(advice-add 'org-id-get-create :after    #'salih/set-custom-id-to-id)


;; `sly-compile-region' already done by `sly-compile-string'
;; (advice-add 'sly-compile-region
;;             :before 'salih/sly--compile-eval-begin-print)

(advice-add 'gomacro--sanitize-string :override 'salih/gomacro--sanitize-string)

(advice-add 'org-noter-pdf--pdf-view-get-precise-info
            :override 'salih/org-noter-pdf--pdf-view-get-precise-info)

(advice-add  #'doom-highlight-non-default-indentation-h :override #'ignore)

;; Disables creating org-id in the case of `org-download-clipboard'
(advice-add 'org-download-clipboard :around
            (lambda (orig-fun &rest args)
              (cl-letf (((symbol-function 'org-id-get-create) #'ignore))
                (apply orig-fun args))))

(provide '+advice)
