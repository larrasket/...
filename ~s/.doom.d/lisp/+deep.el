


(add-to-list 'doom-emoji-fallback-font-families "Symbola")
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . html-mode))
(plist-put +popup-defaults :modeline t)


(provide '+deep)

(assoc-delete-all "Open project"                +doom-dashboard-menu-sections)
(assoc-delete-all "Open documentation"          +doom-dashboard-menu-sections)
(assoc-delete-all "Jump to bookmark"            +doom-dashboard-menu-sections)
