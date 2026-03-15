;;; lr-completion.el --- Corfu, Vertico, Consult -*- lexical-binding: t; -*-

;;; --- Corfu ---
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

;;; --- Consult ---
(after! consult
  (setq consult-preview-excluded-buffers t))

;;; --- Consult-org-roam: "r" in consult-buffer shows all roam nodes ---
;; Defined at startup; :items guard means it silently returns nothing
;; until org-roam is loaded (on first org file open).
(defvar salih/consult-org-roam-node-source
  `(:name     "Roam"
    :narrow   ?r
    :hidden   nil
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

;;; --- Vertico multiform ---
(after! vertico-multiform
  (add-to-list 'vertico-multiform-categories
               '(jinx (vertico-grid-annotate . 25)))
  (vertico-multiform-mode 1))

(provide 'lr-completion)
