(add-to-list 'load-path "~/.doom.d/")
(require '+roam)
(require 'themeconf)
(require 'leader)
(require 'keys)
;; (use-package org-upcoming-modeline
;;   :after org
;;   :load-path "~/gits/org-upcoming-modeline"
;;   :config
;;   (org-upcoming-modeline-mode))
(setq org-agenda-files (directory-files-recursively "~/roam/journal" "\\.org$"))
(setq +org-capture-journal-file "~/blog/content/stack.org")
(setq +org-capture-changelog-file "~/blog/content/nice.org")
(setq +org-capture-todo-file "~/roam/journal/todo.org")
