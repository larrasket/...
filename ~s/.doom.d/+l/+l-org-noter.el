;;; +l-org-noter.el -*- lexical-binding: t; -*-

;; Org-noter configuration
(use-package org-noter
  :config

  (defun salih/org-noter--try-add-highlight-before-note (&rest args)
    "Try to add a highlight annotation if there's a selection, but don't fail if
it doesn't work.  ARGS are ignored but accepted to work with advice system."
    (when (pdf-view-active-region-p)
      (condition-case err
          (progn
            (call-interactively #'pdf-annot-add-highlight-markup-annotation)
            (save-buffer))
        (error
         (message "Failed to add highlight annotation: %s" (error-message-string
                                                            err))))))


  ;; Org-noter utilities
  (defun salih/org-noter-pdf--pdf-view-get-precise-info (orig-fun &rest args)
    "Custom wrapper for org-noter PDF view."
    (apply orig-fun args))


  (defun salih/org-noter-open-in-zathura ()
    "Get the value of a PROPERTY from the current Org heading."
    (interactive)
    (let ((path (org-entry-get nil "NOTER_DOCUMENT"))
          (page (org-entry-get nil "NOTER_PAGE")))
      (if page
          (start-process "" nil "zathura" "-P" page path)
        (start-process "" nil "zathura" path))))
  :custom
  (org-noter-always-create-frame nil)
  (org-noter-kill-frame-at-session-end nil)
  (org-noter-swap-window nil))


(after! org-noter
  (defun salih/org-noter-sync-current-note-and-switch-window ()
    (interactive)
    (let ((prev-window (selected-window)))
      (org-noter-sync-current-note)
      (select-window prev-window)))
  (define-key org-noter-notes-mode-map (salih/mode "C-j")
              #'salih/org-noter-sync-current-note-and-switch-window)
  (define-key org-noter-doc-mode-map
              (salih/mode "C-c") #'org-noter-insert-precise-note))

;; Org-noter advice
(advice-add 'org-noter-pdf--pdf-view-get-precise-info
            :override 'salih/org-noter-pdf--pdf-view-get-precise-info)

(advice-add 'org-noter-insert-precise-note
            :before #'salih/org-noter--try-add-highlight-before-note)

(provide '+l-org-noter)
