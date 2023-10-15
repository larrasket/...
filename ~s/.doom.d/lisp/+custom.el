;;; ../configs/~s/.doom.d/lisp/+custom.el -*- lexical-binding: t; -*-


(after! org
  (require 'org-inlinetask)
  (require 'org-media-note)
  (require 'org-roam-protocol)
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
   '(org-document-title ((t (:inherit outline-8 :height 1.8 :weight bold))))))


(after! ob-julia
  (unless (featurep 'tadwin)
    (progn
      (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
      (defalias 'org-babel-variable-assignments:julia 'org-babel-variable-assignments:julia-vterm)))

  (setq org-babel-default-header-args:julia    (list '(:results . "value")
                                                     '(:cache   . "yes")
                                                     '(:exports . "both"))))
(after! julia-repl
  (set-popup-rule! "^\\*julia:*.*\\*$" :quit nil :side 'right :width .5))

(after! org-roam
  (setq org-roam-list-files-commands '(find fd fdfind rg)))


(after! flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))


(after! neotree
  (setq neo-theme               (if (display-graphic-p) 'icons 'arrow)
        neo-autorefresh         t
        neo-smart-open          t
        neo-window-fixed-size   nil
        neo-window-width        35))

(after! solaire-mode
  (setq solaire-mode-real-buffer-fn #'salih/solaire-mode-real-buffer-custom-p))

(after! sly
  (setq sly-complete-symbol-function 'sly-flex-completions))


(after! git-gutter-fringe
  (setq-default fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))


(after! elfeed
  (setf elfeed-search-sort-function #'salih/elfeed-tag-sort))

(after! consult
  (add-to-list 'consult-buffer-sources 'salih/consult--source-books 'append))

(after! embark
  (define-key embark-url-map            (kbd "c") 'salih/open-url-in-chrome)
  (define-key embark-org-link-map       (kbd "RET") 'org-web-tools-read-url-as-org))

(after! elfeed
  (require 'elfeed-tube)
  (elfeed-tube-setup))
 
 



(provide '+custom)
