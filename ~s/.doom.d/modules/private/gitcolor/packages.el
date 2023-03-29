;; -*- no-byte-compile: t; -*-
;;; mine/gitcolor/packages.el


(package! git-gutter)
(package! git-gutter-fringe)

(package! gitconfig-mode
	  :recipe (:host github :repo "luggages/git-modes"
			 :files ("gitconfig-mode.el")))
