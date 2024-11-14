;;; ../configs/~s/.doom.d/lisp/+custom.el -*- lexical-binding: t; -*-
(after! org
  (require 'org-inlinetask)
  (require 'org-roam-protocol)
  (require 'org-download)
  (custom-set-faces!
    '(font-lock-keyword-face  :weight bold :slant normal)
    '(font-lock-constant-face :weight bold :slant normal))
  (custom-set-faces
   ;; '(org-tag ((t (:weight bold))))
   '(org-todo ((t (:weight bold))))
   '(org-done ((t (:weight bold))))
   '(org-agenda-done ((t (:strike-through nil))))
   '(org-document-title ((t (:weight bold :height 2.0
                             :family "JetBrains Mono"))))
   '(org-level-1 ((t (:inherit outline-1 :height 1.45
                      :weight bold :family "JetBrains Mono"))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.2  :weight bold :family
                               "JetBrains Mono"))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.05 :weight bold :family
                               "JetBrains Mono"))))
   '(org-level-4 ((t (:inherit outline-4 :height 0.9  :weight bold :family
                               "JetBrains Mono"))))
   '(org-level-5 ((t (:inherit outline-5 :height 0.7  :weight bold :family
                               "JetBrains Mono"))))
   '(org-level-6 ((t (:inherit outline-6 :height 0.6  :weight bold :family
                               "JetBrains Mono"))))
   '(org-level-7 ((t (:inherit outline-7 :height 0.5  :weight bold :family
                               "JetBrains Mono"))))))

(after! doom-modeline
 (doom-modeline-def-segment salih/selection-info
   "Information about the current selection.
Such as how many characters and lines are selected, or the NxM dimensions of a
block selection."
   (when (and (or mark-active (and (bound-and-true-p evil-local-mode)
                                   (eq evil-state 'visual)))
              (doom-modeline--active))
     (cl-destructuring-bind (beg . end)
         (if (and (bound-and-true-p evil-local-mode) (eq evil-state 'visual))
             (cons evil-visual-beginning evil-visual-end)
           (cons (region-beginning) (region-end)))
       (propertize
        (let ((lines (count-lines beg (min end (point-max)))))
          (concat (doom-modeline-spc)
                  (cond ((or (bound-and-true-p rectangle-mark-mode)
                             (and (bound-and-true-p evil-visual-selection)
                                  (eq 'block evil-visual-selection)))
                         (let ((cols (abs (- (doom-modeline-column end)
                                             (doom-modeline-column beg)))))
                           (format "%dx%dB" lines cols)))
                        ((and (bound-and-true-p evil-visual-selection)
                              (eq evil-visual-selection 'line))
                         (format "%dL" lines))
                        ((> lines 1)
                         (format "%dC %dL" (- end beg) lines))
                        (t
                         (format "%dC" (- end beg))))
                  (doom-modeline-spc)))
        'face 'doom-modeline-emphasis))))

 (doom-modeline-def-segment salih/word-count
   "The buffer word count.
Displayed when in a major mode in `doom-modeline-continuous-word-count-modes'.
Respects `doom-modeline-enable-word-count'."
   (when (and doom-modeline-enable-word-count
              (member major-mode doom-modeline-continuous-word-count-modes)
              (derived-mode-p 'org-mode))
     (propertize (format " %dW" (count-words (point-min) (point-max)))
                 'face (doom-modeline-face))))

 (doom-modeline-def-modeline 'salih-line
   '(eldoc
     workspace-name
     window-number
     follow remote-host
     salih/word-count
     parrot)
   '(salih/selection-info matches
     buffer-position compilation
     objed-state misc-info persp-name
     battery grip irc
     mu4e gnus
     github debug repl lsp minor-modes
     input-method indent-info
     buffer-encoding
     process vcs check time)))


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

(after! edebug (setcdr emacs-lisp-mode-map nil))

(after! gud (salih/set-convenient-keys))

(after! org-drill
  (setq org-drill-scope (let ((nodes
                               (salih/get-org-roam-nodes-with-tag "drill")))
                          (delete-dups (mapcar 'car nodes))))
  (setq org-drill-maximum-duration 100))

(after! eshell (remove-hook 'eshell-mode-hook 'hide-mode-line-mode))

(after! modus-themes
  (setq modus-themes-bold-constructs                      t
        modus-themes-italic-constructs                    nil))

(after! epg (fset 'epg-wait-for-status 'ignore))

(after! projectile (setq projectile-switch-project-action 'projectile-dired))

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
  (dolist (face '(org-special-keyword org-drawer org-date))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

(after! corfu
  (setf (alist-get 'border-width             corfu--frame-parameters) 3
        (alist-get 'internal-border-width    corfu--frame-parameters) 2
        (alist-get 'child-frame-border-width corfu--frame-parameters) 2)

  (setq kind-icon-blend-background t
        kind-icon-default-face     'corfu-default
        corfu-preselect            'directory))

(after! corfu-popupinfo
  (setq corfu-popupinfo-delay      '(0.2 . 0.2)
        corfu-min-width            30
        corfu-max-width            80))

(after! company
  (remove-hook! 'doom-first-input-hook #'global-company-mode))

(after! org
  (custom-set-faces! '(org-done :strike-through nil)))

(when (eq doom-theme 'doom-monokai-machine)
 (set-face-attribute 'fill-column-indicator nil :height 1.1)
 (custom-set-faces!
   '(font-lock-keyword-face :weight bold :slant normal)
   ;; '(font-lock-variable-name-face :weight normal :foreground "#a3d8ff")
   ;; '(font-lock-variable-name-face :weight normal :foreground "#9be2ef")
   '(font-lock-variable-name-face :weight normal :foreground "#b0e0e6")
   ;; '(font-lock-variable-name-face :weight normal :foreground "#b3e5fc")
   ;; '(font-lock-variable-name-face :weight normal :foreground "#61cdff")
  (custom-set-faces!
    '(line-number              :slant normal)
    '(line-number-current-line :slant normal))))


(put 'salih/modeline-major-mode 'risky-local-variable t)

(provide '+custom)
