;; -*- no-byte-compile: t; -*-
;;; private/roam/packages.el



(package! org-ql :recipe
  (:host github
   :repo "larrasket/org-ql"
   :branch "priority"))


(unpin! org-roam)
(package! org-roam-ui)
(package! emacsql-sqlite3)
(package! emacsql-sqlite-module)
