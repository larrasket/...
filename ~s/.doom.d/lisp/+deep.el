


(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(plist-put +popup-defaults :modeline t)
;; (assoc-delete-all "Open org-agenda"             +doom-dashboard-menu-sections)
;; (assoc-delete-all "Recently opened files"       +doom-dashboard-menu-sections)
;; (assoc-delete-all "Open project"                +doom-dashboard-menu-sections)
;; (assoc-delete-all "Jump to bookmark"            +doom-dashboard-menu-sections)
(custom-set-variables '(all-the-icons-completion-mode nil))

(when (or (eq doom-theme 'modus-vivendi-tritanopia)
          (eq doom-theme 'modus-vivendi-deuteranopia)
          (eq doom-theme 'modus-vivendi))
  (set-frame-parameter nil 'alpha-background 95)
  (add-to-list 'default-frame-alist '(alpha-background . 95)))

(provide '+deep)
