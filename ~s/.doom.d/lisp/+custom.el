;;; ../configs/~s/.doom.d/lisp/+custom.el -*- lexical-binding: t; -*-


(after! org
  (require 'org-inlinetask)
  (require 'org-roam-protocol)
  (require 'org-download)
  (custom-set-faces
   '(org-document-title ((t (:inherit outline-8 :height 2.0 :weight normal))))
   '(org-tag ((t (:weight normal))))
   '(org-todo ((t (:weight bold))))
   '(org-link ((t (:weight normal))))
   '(org-done ((t (:weight bold))))
   '(org-headline-done ((t (:weight normal))))
   ;; '(org-link ((t (:weight bold)))), but not when it's on titles too.
   '(org-level-1 ((t (:inherit outline-1 :height 1.45 :weight normal :family
                               "JetBrains Mono"))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.2  :weight normal :family
                               "JetBrains Mono"))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.05 :weight normal :family
                               "JetBrains Mono"))))
   '(org-level-4 ((t (:inherit outline-4 :height 0.9  :weight normal :family
                               "JetBrains Mono"))))
   '(org-level-5 ((t (:inherit outline-5 :height 0.7  :weight normal :family
                               "JetBrains Mono"))))
   '(org-level-6 ((t (:inherit outline-6 :height 0.6  :weight normal :family
                               "JetBrains Mono"))))
   '(org-level-7 ((t (:inherit outline-7 :height 0.5  :weight normal :family
                               "JetBrains Mono"))))))




(after! ob-julia
  (unless (featurep 'tadwin)
    (progn
      (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
      (defalias 'org-babel-variable-assignments:julia
        'org-babel-variable-assignments:julia-vterm)))

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
  (define-fringe-bitmap
    'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap
    'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(after! consult
  (add-to-list 'consult-buffer-sources 'salih/consult--source-books 'append))

(after! consult-org-roam
  (setq consult-org-roam-grep-func #'consult-ripgrep))

(after! embark
  (add-to-list 'embark-keymap-alist '(org-timestamp embark-org-timestamp-map))
  (defvar-keymap embark-org-timestamp-map
    :doc "Keymap for actions on an org timestamp."
    :parent embark-general-map
    "t" #'salih/org-add-week-to-timestamp)
  (define-key embark-url-map (kbd "c") 'salih/open-url-in-chrome)
  (define-key embark-org-link-map (kbd "RET") 'org-web-tools-read-url-as-org))

(after! edebug
  (setcdr emacs-lisp-mode-map nil))

(after! gud
  (salih/set-convenient-keys))

(after! org-drill
  (setq org-drill-scope (let ((nodes
                               (salih/get-org-roam-nodes-with-tag "drill")))
                          (delete-dups (mapcar 'car nodes))))
  (setq org-drill-maximum-duration 100))

(after! eshell
  (remove-hook 'eshell-mode-hook 'hide-mode-line-mode))

(after! modus-themes
  (setq modus-themes-bold-constructs                      t
        modus-themes-italic-constructs                    nil))

(after! epg
  (fset 'epg-wait-for-status 'ignore))

(after! projectile
  (setq projectile-switch-project-action 'projectile-dired))


(after! git-gutter
  (and (not (featurep 'tadwin))
       (featurep 'modus-themes)
       (modus-themes--modus-p doom-theme)
       (modus-themes-with-colors
        (custom-set-faces
         ;; Replace green with blue if you use `modus-themes-deuteranopia'.
         `(git-gutter-fr:added ((,c :foreground ,bg-added-fringe)))
         ;; `(git-gutter-fr:deleted ((,class :foreground ,red-fringe-bg)))
         `(git-gutter-fr:modified ((,c :foreground ,bg-changed-fringe)))))))


;; other handy stuff
(with-eval-after-load 'embark
  (add-hook 'embark-collect-mode-hook  #'salih/consult-preview-at-point-mode))


(set-popup-rules!
  '(("^\\*cider-doc" :slot -1 :size 0.3 :select t)))

(after! clojure-mode
  (set-lookup-handlers! 'cider-mode nil)
  (set-lookup-handlers! 'clj-refactor-mode nil))

(after! mixed-pitch
  (dolist (face '(org-special-keyword org-document-title org-drawer org-date))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))






(after! corfu
  (setf (alist-get 'border-width corfu--frame-parameters) 3
        (alist-get 'internal-border-width corfu--frame-parameters) 2
        (alist-get 'child-frame-border-width corfu--frame-parameters) 2)
  (custom-set-faces
   '(corfu-bar ((t (:background "#a8a8a8" :height 1.0 :box nil))))
   '(corfu-border ((t (:background "#323232" :height 2.0 :box nil)))))


  (setq kind-icon-blend-background t)
  (setq kind-icon-default-face 'corfu-default)
  (setq corfu-preselect 'directory))

(after! corfu-popupinfo
  (setq corfu-popupinfo-delay '(0.2 . 0.2)
        corfu-min-width 30
        corfu-max-width 80))

(provide '+custom)
