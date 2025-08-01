;;; +l-ui-basic.el -*- lexical-binding: t; -*-



;; Theme configuration
(use-package modus-themes
  :custom
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-common-palette-overrides
   '((bg-mode-line-active          bg-inactive)
     (fg-mode-line-active          fg-main)
     (bg-mode-line-inactive        bg-inactive)
     (fg-mode-line-active          fg-dim)
     (bg-line-number-active        unspecified)
     (bg-line-number-inactive      unspecified)
     (border-mode-line-active      bg-main)
     (border-mode-line-inactive    bg-inactive)))
  :config
  (setq modus-themes-bold-constructs t
        modus-themes-italic-constructs nil))

;; Indent bars configuration
(use-package indent-bars
  :custom
  (indent-bars-highlight-current-depth nil)
  (indent-bars-treesit-support t)
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.25))

;; Breadcrumb configuration
(use-package breadcrumb
  :custom
  (breadcrumb-project-max-length 0.5)
  (breadcrumb-project-crumb-separator "/")
  (breadcrumb-imenu-max-length 1.0)
  (breadcrumb-imenu-crumb-separator " > ")
  (breadcrumb-idle-time 20))

;; Treemacs configuration
(use-package treemacs
  :custom
  (treemacs-position 'right))

;; Add emoji fallback font
(add-to-list 'doom-emoji-fallback-font-families "Symbola")

;; Frame configuration
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Auto-mode configuration
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . html-mode))

;; Popup configuration
(plist-put +popup-defaults :modeline t)

;; Dashboard configuration
(setq +doom-dashboard-menu-sections
      '(("Recently opened files"
         :action recentf-open-files)
        ("Open project"
         :action projectile-switch-project)
        ("Jump to bookmark"
         :action bookmark-jump)
        ("Open documentation"
         :action doom/help)))
(setq +doom-dashboard-ascii-banner-fn 'salih/banner)



;; Remove dashboard menu items
(assoc-delete-all "Open project" +doom-dashboard-menu-sections)
(assoc-delete-all "Open documentation" +doom-dashboard-menu-sections)
(assoc-delete-all "Jump to bookmark" +doom-dashboard-menu-sections)

;; Face customizations
(custom-set-faces!
 '(font-lock-keyword-face :weight bold :slant normal)
 '(line-number              :slant normal)
 '(line-number-current-line :slant normal))

(when (doom-theme-p?)
  (custom-set-faces! '(fill-column-indicator :height 1.0)))

(if (kaolin-theme-p?)
    (custom-set-faces! '(fill-column-indicator :height 0.28))
  (custom-set-faces! '(fill-column-indicator :height 0.1)))

(when (eq doom-theme 'doom-monokai-machine)
  (custom-set-faces!
   '(font-lock-variable-name-face :weight normal :foreground "#b0e0e6")))

;; Variable declaration
(put 'salih/modeline-major-mode 'risky-local-variable t)

;; Indent bars hooks
(add-hook! 'prog-mode-hook :append #'indent-bars-mode)
(add-hook! 'prog-mode-hook :append #'display-fill-column-indicator-mode)

;; Disable indent bars for specific modes
(add-hook! '(emacs-lisp-mode-hook
             sql-mode-hook
             TeX-mode-hook
             clojure-mode-hook
             LaTeX-mode-hook)
           (indent-bars-mode -1))

;; Enable indent bars for specific modes
(add-hook! '(julia-mode-hook
             java-mode-hook
             c-mode-hook
             go-mode-hook
             yaml-mode-hook)
           #'indent-bars-mode)

;; Eglot managed mode hooks
(add-hook 'eglot-managed-mode-hook 'indent-bars-mode)

;; Disable bright mode for various modes
(add-hook! '(maxima-inferior-mode-hook
             neotree-mode-hook
             sly-mrepl-mode-hook
             circe-mode-hook
             circe-server-mode-hook
             circe-chat-mode-hook
             circe-channel-mode-hook
             circe-query-mode-hook
             vterm-mode-hook
             dired-mode-hook
             mu4e-headers-mode-hook
             mu4e-view-mode-hook
             mu4e-main-mode-hook) #'salih/disable-bright)

;; Nov-mode hooks
(add-hook! 'nov-mode-hook
  (defface tmp-buffer-local-face
    '((t :family "Roboto Condensed" :height 1.0)) "")
  (buffer-face-set 'tmp-buffer-local-face)
  (setq-local right-fringe-width 0)
  (setq-local left-margin-width  4)
  (setq-local left-fringe-width  0)
  (text-scale-set 1))

;; PDF view mode hooks
(add-hook! 'pdf-view-mode-hook
  (setq-local evil-normal-state-cursor (list nil)
               browse-url-browser-function 'salih/open-url-in-chrome))





(add-hook!
 '+doom-dashboard-functions :append
 (insert "\n"
         (+doom-dashboard--center
          +doom-dashboard--width
          (concat
           "The fear of the Lord is the beginning"
           " of wisdom; all those who practice it have\na good understanding."
           " His praise endures forever."))))


(remove-hook! 'doom-after-init-hook #'doom-modeline-mode)
(add-hook! 'doom-after-init-hook #'cocaine-line-mode)
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)


(remove-hook! '+popup-buffer-mode-hook #'+popup-set-modeline-on-enable-h)
(remove-hook! '+popup-buffer-mode-hook #'+popup-unset-modeline-on-enable-h)



(remove-hook 'doom-first-input-hook             #'evil-snipe-mode)
(remove-hook '+doom-dashboard-functions         #'doom-dashboard-widget-footer)
(remove-hook 'after-change-major-mode-hook
             #'doom-highlight-non-default-indentation-h)



(provide '+l-ui-basic)
