;;; ../configs/~s/.doom.d/lisp/+custom.el -*- lexical-binding: t; -*-


(set-face-background 'highlight-indent-guides-odd-face "darkgray")
(set-face-background 'highlight-indent-guides-odd-face "darkgray")
(set-face-background 'highlight-indent-guides-even-face "dimgray")
(set-face-foreground 'highlight-indent-guides-character-face "dimgray")



(after! org
  (custom-set-faces
   '(org-link ((t (:inherit link :underline nil :foreground "#79b58f" :slant normal :weight bold :family "Pragmata Pro"))))
   '(org-roam-dailies-calendar-note ((t (:inherit link :underline nil))))
   '(variable-pitch ((t (:family "DejaVu Sans"))))
   '(org-level-1 ((t (:inherit outline-1 :height 1.5 :weight bold :family "Pragmata Pro"))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.2 :weight bold :family "Pragmata Pro"))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.2 :weight bold :family "Pragmata Pro"))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0 :weight bold :family "Pragmata Pro"))))
   '(org-level-5 ((t (:inherit outline-5 :height 0.9 :weight bold :family "Pragmata Pro"))))
   '(org-level-6 ((t (:inherit outline-6 :height 0.8 :weight bold :family "Pragmata Pro"))))
   '(org-level-7 ((t (:inherit outline-7 :height 0.7 :weight bold :family "Pragmata Pro"))))
   '(org-level-8 ((t (:inherit outline-8 :height 0.6 :weight bold :family "Pragmata Pro"))))
   '(org-document-title ((t (:inherit outline-8 :height 1.8 :weight bold :family "Pragmata Pro"))))))

(require 'ob-julia)
(unless (featurep 'tadwin)
  (progn
    (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
    (defalias 'org-babel-variable-assignments:julia 'org-babel-variable-assignments:julia-vterm)))

(setq org-babel-default-header-args:julia    (list '(:results . "value")
                                                   '(:cache   . "yes")
                                                   '(:exports . "both")))

(defun advise-once (symbol where function &optional props)
  (advice-add symbol :after (lambda (&rest _) (advice-remove symbol function)))
  (advice-add symbol where function props))


(custom-set-faces
 '(doom-modeline-buffer-modified ((t (:inherit (doom-modeline-urgent))))))

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

(setf elfeed-search-sort-function #'salih/elfeed-tag-sort)


(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(provide '+custom)
