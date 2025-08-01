;;; +l-org-core.el -*- lexical-binding: t; -*-

;; Org-mode core configuration
(use-package org
  :custom
  (org-id-method 'org)
  (org-log-into-drawer "STATS")
  (org-log-done nil)
  (org-agenda-skip-scheduled-if-done nil)
  (org-use-tag-inheritance t)
  (org-log-reschedule 'note)
  (org-agenda-block-separator #x2501)
  (org-element-use-cache t)
  (org-noter-auto-save-last-location t)
  (org-startup-folded 'show2levels)
  (org-image-actual-width 600)
  (org-link-file-path-type 'relative)
  (org-ellipsis nil)
  (org-agenda-show-future-repeats nil)
  (org-clock-mode-line-total 'current)
  (org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")
  (org-clock-string-limit 7)
  (org-agenda-dim-blocked-tasks 'invisible)
  (org-crypt-key user-mail-address)
  (org-todo-keywords
   '((sequence
      "TODO(t)" "DAILY(e)" "PROJ(p)"
      "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)"
      "IDEA(i)" "|" "DONE(d)" "KILL(k)")
     (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
     (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))
  (org-todo-keyword-faces
   '(("[-]"        . +org-todo-active)
     ("STRT"       . +org-todo-active)
     ("DAILY"      . +org-todo-project)
     ("[?]"        . +org-todo-onhold)
     ("WAIT"       . +org-todo-onhold)
     ("HOLD"       . +org-todo-onhold)
     ("PROJ"       . +org-todo-project)
     ("NO"         . +org-todo-cancel)
     ("KILL"       . +org-todo-cancel)))
  (org-hide-leading-stars t)
  (org-tags-column 70)
  (org-archive-location "%s_archive.org::")
  (org-agenda-start-on-weekday nil)
  (org-agenda-start-day "0d")
  (org-agenda-start-with-log-mode t)
  :config
  (custom-set-faces!
   '(org-agenda-done :strike-through nil)
   '(org-document-title :height 2.0)
   '(org-list-dt :inherit default)
   `(jinx-misspelled
     :underline (:style wave :color ,(face-foreground 'error)))))

;; Org archive functions
(defun salih/org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(defun salih/org-capture-general ()
  (interactive)
  (salih/capture-- 'org-capture "f"))

(defun salih/org-capture-log ()
  (interactive)
  (salih/capture-- 'org-capture "n"))

(defun salih/org-roam-capture-fleet ()
  (interactive)
  (salih/capture-- 'org-roam-capture "f"))

(defun salih/org-agenda-no-full-f ()
  (interactive)
  (setq salih/vulpea-show-full nil)
  (org-agenda nil "f"))

(defun salih/org-agenda-no-full-l ()
  (interactive)
  (setq salih/vulpea-show-full nil)
  (org-agenda nil "l"))

(defun salih/open-agenda ()
  (interactive)
  (org-agenda-remove-restriction-lock)
  (org-agenda nil "v"))





(defun salih/org-id-get-create-with-custom-id ()
  (interactive)
  (when (org-before-first-heading-p)
    (user-error "Not inside a heading"))
  (let* ((org-id (org-id-get))
         (custom-id-property "CUSTOM_ID"))

    (unless org-id
      (setq org-id (org-id-new))
      (org-entry-put nil "ID" org-id)
      (org-entry-put nil custom-id-property org-id))
    org-id))

(defun salih/org-archive-killed-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/KILL" 'file))

;; Org link configuration
(use-package org
  :config
  (org-link-set-parameters
   "eww"
   :follow (lambda (link) (eww link))
   :store (lambda ()
            (when (eq major-mode 'eww-mode)
              (let ((url (eww-current-url))
                    (title (or (plist-get eww-data :title) "No title")))
                (unless url
                  (error "No URL found in the current eww buffer"))
                (org-store-link-props
                 :type "eww"
                 :link url
                 :description title))))))

;; Org log note configuration
(use-package org
  :config
  (setf (cdr (assoc 'note org-log-note-headings)) "%t")
  (custom-set-faces! '(org-done :strike-through nil :weight bold)))

;; Org-mode advice
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
(advice-add 'org-id-get-create :after    #'salih/set-custom-id-to-id)

;; Org-download advice
(advice-add 'org-download-clipboard :around
            (lambda (orig-fun &rest args)
              (cl-letf (((symbol-function 'org-id-get-create) #'ignore))
                (apply orig-fun args))))

;; Org-mode hooks
(add-hook! 'org-mode-hook
  (add-hook 'before-save-hook  #'vulpea-project-update-tag nil 'local)
  (add-hook 'find-file-hook    #'vulpea-project-update-tag nil 'local)
  (setq org-hide-leading-stars t
        fill-column 80
        display-line-numbers-width 3)
  (setq-local truncate-lines t)
  (display-line-numbers-mode -1))

;; Auto-fill mode hooks for org
(add-hook! '(org-mode-hook
             markdown-mode-hook)
           #'auto-fill-mode)






(provide '+l-org-core)
