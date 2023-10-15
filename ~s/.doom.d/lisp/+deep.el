


(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(plist-put +popup-defaults :modeline t)
(assoc-delete-all "Open org-agenda"             +doom-dashboard-menu-sections)
(assoc-delete-all "Recently opened files"       +doom-dashboard-menu-sections)
(assoc-delete-all "Open project"                +doom-dashboard-menu-sections)
(assoc-delete-all "Jump to bookmark"            +doom-dashboard-menu-sections)

(custom-set-variables '(all-the-icons-completion-mode nil))
(custom-set-faces
 '(doom-modeline-buffer-modified ((t (:inherit (doom-modeline-urgent))))))

(set-face-background 'highlight-indent-guides-odd-face "darkgray")
(set-face-background 'highlight-indent-guides-odd-face "darkgray")
(set-face-background 'highlight-indent-guides-even-face "dimgray")
(set-face-foreground 'highlight-indent-guides-character-face "dimgray")

(provide '+deep)
