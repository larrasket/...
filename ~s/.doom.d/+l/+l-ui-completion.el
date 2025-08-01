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

;; Company configuration
;; (use-package company
;;   :custom
;;   (company-idle-delay 0.3)
;;   :config
;;   )

(remove-hook! 'doom-first-input-hook #'global-company-mode)
;; Mixed pitch configuration
(use-package mixed-pitch
  :config
  (dolist (face '(org-special-keyword org-drawer org-date))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

;; Popup rules
(set-popup-rules!
  '(("^\\*cider-doc" :slot -1 :size 0.3 :select t)))

;; Lookup and documentation advice
(advice-add '+lookup/documentation             :around #'salih/ensure-eww-in-search)
(advice-add 'salih/mu4e-action-view-in-browser :around #'salih/ensure-eww-in-search)

(provide '+l-ui-completion)
