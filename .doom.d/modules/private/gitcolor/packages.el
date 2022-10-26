;; -*- no-byte-compile: t; -*-
;;; mine/gitcolor/packages.el


(package! git-gutter)
(package! git-gutter-fringe)

(package! gitconfig-mode
	  :recipe (:host github :repo "magit/git-modes"
			 :files ("gitconfig-mode.el")))
(package! gitignore-mode
	  :recipe (:host github :repo "magit/git-modes"
			 :files ("gitignore-mode.el")))
