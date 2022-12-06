(add-to-list 'load-path "~/.doom.d/")
(require '+roam)
(require 'leader)
(require 'keys)
(setq org-agenda-files (directory-files-recursively "~/roam/journal" "\\.org$"))
(setq +org-capture-journal-file "~/blog/content/stack.org")
(setq +org-capture-changelog-file "~/blog/content/nice.org")
(setq +org-capture-todo-file "~/roam/journal/todo.org")
(setq neo-autorefresh 't)
(setq neo-mode-line-type 'default)
;; (after! highlight-indent-guides
;;   (highlight-indent-guides-auto-set-faces))
;;
;; Die, Doc-View-mode! die!
;; (defalias 'doc-view-mode #'doc-view-fallback-mode) ;Or fundamental-mode, ...
(setq load-prefer-newer t)


(require 'highlight-indent-guides)
(custom-set-variables
 '(highlight-indent-guides-method 'bitmap))
(set-face-background 'highlight-indent-guides-odd-face "darkgray")
(set-face-background 'highlight-indent-guides-even-face "dimgray")
(set-face-foreground 'highlight-indent-guides-character-face "dimgray")

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(add-hook 'org-mode-hook 'highlight-indent-guides-mode)
