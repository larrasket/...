;;; lr-completion.el --- Corfu, Vertico, Consult -*- lexical-binding: t; -*-

;;; Text-mode completion
;; jinx owns spelling here, so keep the Emacs 30 ispell capf out of
;; `completion-at-point-functions'.  Doom only disables it AFTER it first
;; errors; turning it off up front stops corfu from ever invoking a slow or
;; erroring ispell subprocess in org/text/markdown buffers.
(setq text-mode-ispell-word-completion nil)

;;; Corfu
(after! corfu
  (setf (alist-get 'border-width          corfu--frame-parameters) 3
        (alist-get 'internal-border-width  corfu--frame-parameters) 2
        (alist-get 'child-frame-border-width corfu--frame-parameters) 2)
  (setq kind-icon-blend-background t
        kind-icon-default-face     'corfu-default
        global-corfu-minibuffer    nil
        corfu-preselect            'directory
        corfu-auto-delay           0.3
        corfu-min-width            30
        corfu-max-width            80))

;;; Consult
(after! consult
  (setq consult-preview-excluded-buffers t))

;;; Consult-org-roam: narrow with "r" in consult-buffer to list all roam nodes
;; Hidden by default so `org-roam-node-list' (a full DB scan, slow on a large
;; corpus) runs only when you actually narrow to ?r, not on every consult-buffer
;; call.  The :items guard also returns nothing until org-roam has loaded.
(defvar salih/consult-org-roam-node-source
  `(:name     "Roam"
    :narrow   ?r
    :hidden   t
    :category org-roam-node
    :items    ,(lambda ()
                 (when (featurep 'org-roam)
                   (mapcar #'org-roam-node-title (org-roam-node-list))))
    :annotate ,(lambda (title)
                 (when-let* ((node (and (featurep 'org-roam)
                                        (org-roam-node-from-title-or-alias title))))
                   (file-relative-name (org-roam-node-file node)
                                       org-roam-directory)))
    :action   ,(lambda (title)
                 (when-let* ((node (and (featurep 'org-roam)
                                        (org-roam-node-from-title-or-alias title))))
                   (org-roam-node-visit node)))))

(after! consult
  (add-to-list 'consult-buffer-sources 'salih/consult-org-roam-node-source 'append))

;;; Vertico multiform
(after! vertico-multiform
  (add-to-list 'vertico-multiform-categories
               '(jinx (vertico-grid-annotate . 25)))
  (vertico-multiform-mode 1))

(provide 'lr-completion)
