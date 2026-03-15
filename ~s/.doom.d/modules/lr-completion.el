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

;;; --- Vertico multiform ---
(after! vertico-multiform
  (add-to-list 'vertico-multiform-categories
               '(jinx (vertico-grid-annotate . 25)))
  (vertico-multiform-mode 1))

(provide 'lr-completion)
