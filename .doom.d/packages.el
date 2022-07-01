;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
;;(package! dap-mode)
(package! imenu-list)
(package! easy-hugo)
(package! flymd)
(package! ox-hugo)
(package! org-journal)
(package! objed)
(package! nov)
(package! auto-complete-auctex)
(package! latex-preview-pane)
(package! auctex)
(package! telega)
(package! markdown-preview-mode)
(package! org-pdfview)
(package! dirtree)
;; (package! google-translate)
(package! vimish-fold)
(package! org-download)
(package! iedit)
(package! languagetool)
(package! powerline)
;; (package! airline-themes)
(package! org-download)
(package! math-preview)
;; (package! org-bullets)
(package! gitconfig-mode
	  :recipe (:host github :repo "magit/git-modes"
			 :files ("gitconfig-mode.el")))
(package! gitignore-mode
	  :recipe (:host github :repo "magit/git-modes"
			 :files ("gitignore-mode.el")))




(package! sage-shell-mode)
(package! dir-treeview)
(package! base16-theme)
(package! real-auto-save)
(package! sr-speedbar)
(package! twittering-mode)
(package! ejc-sql)
(package! company-quickhelp)
(package! dired-sidebar)
(package! leetcode)
(package! pandoc)
(package! quickrun)
(package! simple-httpd)
(package! omnisharp)
(package! white-theme)
(package! lsp-treemacs)
(package! org-tree-slide)
(package! projectile)
(package! irony)
(package! habamax-theme)
;;(package! highlight-indent-guides)
(package! highlight-indentation)
(package! solaire-mode :disable t)
(package! dired-sidebar)
(package! highlight-indentation)
(package! ivy-posframe)
(package! sql-indent)
(package! smart-compile)
(package! ob-csharp :recipe (:host github :repo "samwdp/ob-csharp"))
;(package! centaur-tabs)
(package! org-superstar)
(package! vimrc-mode)
(package! company-tabnine)
(package! dotnet)
(package! multi-term)
(package! vscode-dark-plus-theme)
(package! distinguished-theme)
(package! kaolin-themes)
(package! badger-theme)
(package! cil-mode)
(package! leuven-theme)
(package! eziam-theme)
(package! tao-theme)
(package! wakatime-mode)

(package! awqat
  :recipe (:host github
           :repo "zkry/awqat"))

(package! toc-org)
