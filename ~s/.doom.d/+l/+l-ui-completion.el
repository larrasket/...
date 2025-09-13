;;; +l-ui-completion.el -*- lexical-binding: t; -*-

;; Corfu configuration
(use-package corfu
  :config
  (setf (alist-get 'border-width             corfu--frame-parameters) 3
        (alist-get 'internal-border-width    corfu--frame-parameters) 2
        (alist-get 'child-frame-border-width corfu--frame-parameters) 2)
  (setq kind-icon-blend-background t
        kind-icon-default-face     'corfu-default
        global-corfu-minibuffer     nil
        corfu-preselect            'directory))

(use-package consult
  :config
  (setq consult-preview-excluded-buffers t))

;; Corfu popupinfo configuration
(use-package corfu-popupinfo
  :custom
  (corfu-auto-delay 0.3)
  (corfu-min-width 30)
  (corfu-max-width 80))

(remove-hook! 'doom-first-input-hook #'global-company-mode)

(provide '+l-ui-completion)
