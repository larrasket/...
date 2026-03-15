;;; lr-org-noter.el --- PDF annotation with org-noter -*- lexical-binding: t; -*-

(after! org-noter
  (setq org-noter-always-create-frame nil
        org-noter-kill-frame-at-session-end nil
        org-noter-swap-window nil)

  (defun salih/pdf-occure ()
    (interactive)
    (save-window-excursion (pdf-occur-goto-occurrence)))

  (defun salih/org-noter--try-add-highlight-before-note (&rest _)
    "Add highlight annotation if selection exists."
    (when (pdf-view-active-region-p)
      (ignore-errors
        (call-interactively #'pdf-annot-add-highlight-markup-annotation)
        (save-buffer))))

  (defun salih/org-noter-pdf--pdf-view-get-precise-info (mode window)
    (when (eq mode 'pdf-view-mode)
      (let (v-position h-position)
        (if (pdf-view-active-region-p)
            (let ((edges (car (pdf-view-active-region))))
              (setq v-position (min (nth 1 edges) (nth 3 edges))
                    h-position (min (nth 0 edges) (nth 2 edges))))
          (let ((event nil))
            (while (not (and (eq 'mouse-1 (car event))
                             (eq window (posn-window (event-start event)))))
              (setq event (read-event "Click where you want the note to be!")))
            (let* ((col-row (posn-col-row (event-start event)))
                   (click-position (org-noter--conv-page-scroll-percentage
                                   (+ (window-vscroll) (cdr col-row))
                                   (+ (window-hscroll) (car col-row)))))
              (setq v-position (car click-position)
                    h-position (cdr click-position)))))
        v-position)))

  (defun salih/org-noter-open-in-zathura ()
    "Open noter document in external viewer."
    (interactive)
    (let ((path (org-entry-get nil "NOTER_DOCUMENT")))
      (when path (start-process "" nil "open" path))))

  (defun salih/org-noter-sync-current-note-and-switch-window ()
    (interactive)
    (let ((prev-window (selected-window)))
      (org-noter-sync-current-note)
      (select-window prev-window)))

  ;; Advice
  (advice-add 'org-noter-pdf--pdf-view-get-precise-info
              :override 'salih/org-noter-pdf--pdf-view-get-precise-info)
  (advice-add 'org-noter-insert-precise-note
              :before #'salih/org-noter--try-add-highlight-before-note))

;; Nov-consult for epub search
(require 'nov-consult)

(provide 'lr-org-noter)
