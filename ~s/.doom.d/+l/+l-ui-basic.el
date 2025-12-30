;;; +l-ui-basic.el -*- lexical-binding: t; -*-



(use-package breadcrumb
  :custom
  (breadcrumb-project-max-length 0.5)
  (breadcrumb-project-crumb-separator "/")
  (breadcrumb-imenu-max-length 1.0)
  (breadcrumb-imenu-crumb-separator " > ")
  (breadcrumb-idle-time 20)
  :config
  (add-hook! 'doom-docs-org-mode-hook (breadcrumb-local-mode -1)))


(add-to-list 'doom-emoji-fallback-font-families "Symbola")


(setq +doom-dashboard-menu-sections
      '(("Recently opened files"
         :action recentf-open-files)))
(setq +doom-dashboard-ascii-banner-fn 'salih/banner)


(add-hook! 'pdf-view-mode-hook
  (setq-local evil-normal-state-cursor (list nil)
              browse-url-browser-function 'salih/open-url-in-chrome-cross-platform))



(add-hook!
 '+doom-dashboard-functions :append
 (insert "\n"
         (+doom-dashboard--center
          +doom-dashboard--width
          (concat
           "The fear of the Lord is the beginning"
           " of wisdom; all those who practice it have\na good understanding."
           " His praise endures forever."))))


(provide '+l-ui-basic)
