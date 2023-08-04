;;; configs/~s/.doom.d/org-tags.el -*- lexical-binding: t; -*-

(setq org-tag-alist   '((:startgroup)
                        ("@personal" . nil)
                        (:grouptags)
                        ("@read" . ?r)
                        ("@idea" . ?i)
                        ("@write" . ?w)
                        ("@check" . ?c)
                        ("@watch" . ?W)
                        (:endgroup)


                        (:startgroup)
                        ("@nothing" . ?N)
                        (:grouptags)
                        ("@people" . ?p)
                        (:endgroup)
                        ("noexport" . ?n)))

(add-to-list 'org-tags-exclude-from-inheritance "@read")
(add-to-list 'org-tags-exclude-from-inheritance "noexport")
(add-to-list 'org-tags-exclude-from-inheritance "project")
(add-to-list 'org-tags-exclude-from-inheritance "permanent")
(add-to-list 'org-tags-exclude-from-inheritance "link")
