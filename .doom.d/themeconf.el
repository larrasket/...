;;; ../configs/.doom.d/theme.el -*- lexical-binding: t; -*-


(custom-theme-set-faces! 'doom-homage-black
  '(default :background "#121212"))
(custom-theme-set-faces! 'doom-one
  '(default :background "#121212"))
(custom-theme-set-faces! 'doom-one-light
  '(default :background "#ffffff"))

;; (setq doom-theme 'kaolin-dark)
;; (setq doom-theme 'leuven)
;; (setq doom-theme 'doom-acario-light)
;; (setq doom-theme 'doom-badger)
;; (setq doom-theme 'doom-gruvbox)
;; (setq doom-theme 'default)
;; (setq doom-theme 'kaolin-galaxy)
;; (setq doom-theme 'habamax)
;; (setq doom-theme 'humanoid-dark)
;; (setq doom-theme 'doom-acario-light)
;; (setq doom-theme 'mine)
(setq doom-theme 'tsdh-dark)
(defvar company-new-bg "gray26") ; v1: gray30
(defvar company-new-selection-bg "gray21"); v1: gray24
(defvar company-new-common-fg "pale turquoise"); v1: light blue
(defvar company-new-annotation-fg "light slate gray"); v1: "light blue"
(defvar company-new-scrollbar-fg "gray23"); v1: gray26
(defvar company-new-scrollbar-bg "gray28"); v1: gray32
(defvar company-new-preview-fg (face-attribute 'default :foreground))
(defvar company-new-preview-bg "gray21")

(custom-set-faces
 `(company-tooltip ((t (:background ,company-new-bg))))
 `(company-tooltip-selection ((t (:background ,company-new-selection-bg))))
 `(company-tooltip-common ((t (:foreground ,company-new-common-fg))))
 `(company-tooltip-annotation ((t (:foreground ,company-new-annotation-fg))))
 `(company-scrollbar-fg ((t (:background ,company-new-scrollbar-fg))))
 `(company-scrollbar-bg ((t (:background ,company-new-scrollbar-bg))))
 `(company-preview ((t (:foreground ,company-new-preview-fg :background ,company-new-preview-bg))))
 `(company-preview-common ((t (:foreground ,company-new-common-fg :background ,company-new-preview-bg))))
`(company-preview-search ((t (:foreground ,company-new-common-fg :background ,company-new-preview-bg)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "unspecified-fg" :background "gray21"))))
 '(company-preview-common ((t (:foreground "pale turquoise" :background "gray21"))))
 '(company-preview-search ((t (:foreground "pale turquoise" :background "gray21"))))
 '(company-scrollbar-bg ((t (:background "gray28"))) t)
 '(company-scrollbar-fg ((t (:background "gray23"))) t)
 '(company-tooltip ((t (:background "gray26"))))
 '(company-tooltip-annotation ((t (:foreground "light slate gray"))))
 '(company-tooltip-common ((t (:foreground "pale turquoise"))))
 '(company-tooltip-selection ((t (:background "gray21"))))
 '(mode-line ((t (:background "gray30" :box nil :family "JetBrainsMono Nerd Font")))))

(provide 'themeconf)
