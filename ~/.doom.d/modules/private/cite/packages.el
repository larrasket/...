;; -*- no-byte-compile: t; -*-
;;; private/cite/packages.el


(package! org-ref)
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))
(unpin! org-roam)

(unpin! bibtex-completion helm-bibtex ivy-bibtex)

(package! ivy-bibtex)

(package! citar)
(package! citar-org-roam)
