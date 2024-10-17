


(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . html-mode))
(plist-put +popup-defaults :modeline t)
(custom-set-variables '(all-the-icons-completion-mode nil))

(setq lsp-use-plists t)



(provide '+deep)
