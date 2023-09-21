;;; ../configs/~s/.doom.d/lisp/+custom.el -*- lexical-binding: t; -*-


(set-face-background 'highlight-indent-guides-odd-face "darkgray")
(set-face-background 'highlight-indent-guides-odd-face "darkgray")
(set-face-background 'highlight-indent-guides-even-face "dimgray")
(set-face-foreground 'highlight-indent-guides-character-face "dimgray")


(after! julia-repl
  (set-popup-rule! "^\\*julia:*.*\\*$" :quit nil :side 'right :width .5))


(after! org-roam
  (setq org-roam-list-files-commands '(find fd fdfind rg)))


(after! git-gutter-fringe
  (setq-default fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom))


(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(assoc-delete-all "Open org-agenda"             +doom-dashboard-menu-sections)
(assoc-delete-all "Recently opened files"       +doom-dashboard-menu-sections)
(assoc-delete-all "Open project"                +doom-dashboard-menu-sections)
(assoc-delete-all "Jump to bookmark"            +doom-dashboard-menu-sections)

(custom-set-variables '(all-the-icons-completion-mode nil))



(after! solaire-mode
  (setq solaire-mode-real-buffer-fn #'salih/solaire-mode-real-buffer-custom-p))

(after! sly
  (setq sly-complete-symbol-function 'sly-flex-completions))

(plist-put +popup-defaults :modeline t)


(provide '+custom)
