;;; configs/~s/.doom.d/+hooks.el -*- lexical-binding: t; -*-


(add-hook! '(maxima-inferior-mode-hook
             neotree-mode-hook
             sly-mrepl-mode-hook
             circe-mode-hook
             circe-server-mode-hook
             circe-chat-mode-hook
             circe-channel-mode-hook
             circe-query-mode-hook
             vterm-mode-hook
             dired-mode-hook
             mu4e-headers-mode-hook
             mu4e-view-mode-hook
             mu4e-main-mode-hook)               #'salih/disable-bright)

(when (featurep 'centaur-tabs-mode)
  (add-hook! '(org-agenda-mode-hook)
             org-mode-hook
             dired-mode-hook
             native-comp-limple-mode-hook)      #'centaur-tabs-local-mode)

(add-hook! 'org-mode-hook
  (add-hook 'before-save-hook  #'vulpea-project-update-tag nil 'local)
  (add-hook 'find-file-hook    #'vulpea-project-update-tag nil 'local)
  (setq org-hide-leading-stars t
        fill-column 90
        display-line-numbers-width 3)
  (setq-local truncate-lines t)
  (display-line-numbers-mode -1)
  (mixed-pitch-mode)
  (git-gutter-mode))


(add-hook! 'nov-mode-hook
  (defface tmp-buffer-local-face
    '((t :family "Roboto Condensed" :height 1.0)) "")
  (buffer-face-set 'tmp-buffer-local-face)
  (setq-local right-fringe-width 0)
  (setq-local left-margin-width  4)
  (setq-local left-fringe-width  0)
  (text-scale-set 1))


(add-hook!
 'prog-mode-hook      (setq prettify-symbols-alist '(("lambda" . 923))))
(add-hook! 'python-mode-hook    (flycheck-mode -1))
(add-hook!
 'pdf-view-mode-hook  (setq-local evil-normal-state-cursor (list nil)))
(add-hook! 'mu4e-headers-mode-hook (visual-line-mode -1))
(add-hook!
 'org-roam-capture-new-node-hook (setq roam-titles
                                       (salih/org-roam-get-node-titles
                                        (org-roam-node-read--completions))))

(add-hook! '(org-mode-hook
             markdown-mode-hook
             prog-mode-hook)
           #'auto-fill-mode)

(add-hook! '(prog-mode-hook)
  (smartparens-mode 1))

(add-hook! '(bibtex-mode-hook
             prog-mode-hook)
           #'format-all-mode
           #'salih/format-all-ensure-formatter)


(add-hook! 'prog-mode-hook :append #'indent-bars-mode)
(add-hook! 'prog-mode-hook :append #'display-fill-column-indicator-mode)

(add-hook! '(emacs-lisp-mode-hook
             sql-mode-hook
             TeX-mode-hook
             clojure-mode-hook
             LaTeX-mode-hook)             (indent-bars-mode -1))

(add-hook! '(emacs-lisp-mode-hook
             sql-mode-hook
             TeX-mode-hook
             clojure-mode-hook
             LaTeX-mode-hook)             (format-all-mode -1))


(add-hook! '(julia-mode-hook
             java-mode-hook
             c-mode-hook
             go-mode-hook
             yaml-mode-hook)              #'indent-bars-mode)

(add-hook 'eglot-managed-mode-hook 'indent-bars-mode)

(add-hook! '(clojure-mode-hook)
  (flycheck-mode -1)
  (flymake-mode 1)
  (add-hook! 'eglot-managed-mode-hook :local
    (setq completion-at-point-functions
          (list #'cider-complete-at-point
              #'eglot-completion-at-point
              #'lispy-clojure-complete-at-point
              #'yasnippet-capf))))





(add-hook! '(html-mode-hook)             (sgml-electric-tag-pair-mode)
  (flycheck-mode -1))


(add-hook 'csv-mode-hook                #'csv-align-mode)
(add-hook 'after-init-hook              #'global-flycheck-mode)
(add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)
(add-hook 'lisp-mode-hook               #'rainbow-delimiters-mode)

(add-hook 'eshell-alias-load-hook       #'salih/eshell-load-bash-aliases)
;; (add-hook 'dired-after-readin-hook      #'salih/dired-git-info-auto-enable)
(add-hook 'org-roam-find-file-hook      #'git-auto-commit-mode)
(add-hook 'after-make-frame-functions   (lambda (frame)
                                          (with-selected-frame frame
                                            (if (doom-theme?)
                                                (set-fringe-style '(8 . 8))
                                              (set-fringe-style '(3 . 1)))
                                            (salih/keyboard-config))))


(add-hook 'org-capture-prepare-finalize-hook 'salih/org-roam-capture-create-id)

(if (featurep 'nov-xwidget)
    (add-hook 'nov-mode-hook            #'nov-xwidget-inject-all-files))



;; TODO: what about adding some verious quotes here? I can imaging having an RPC
;; call to some external program to get awsome quotes from there.
(add-hook!
 '+doom-dashboard-functions :append
 (insert "\n"
         (+doom-dashboard--center
          +doom-dashboard--width
           "Love is not boastful or self-seeking. It does not insist on its own
way; it is not irritable or resentful; it does not rejoice in wrongdoing, but
rejoices in the truth. It bears all things, hopes all things, endures all
things. Corinthians 13:4-7.")))

(setq fancy-splash-image "/home/l/ner.png")


;; Activate the advice
(ad-activate 'org-agenda-get-some-entry-text)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(add-to-list 'org-babel-load-languages '(julia-vterm . t))

(remove-hook 'vterm-mode-hook                   #'hide-mode-line-mode)
(remove-hook 'doom-first-input-hook             #'evil-snipe-mode)
(remove-hook '+doom-dashboard-functions         #'doom-dashboard-widget-footer)
(remove-hook 'after-change-major-mode-hook
             #'doom-highlight-non-default-indentation-h)






;; init
;; (centaur-tabs-mode)
(breadcrumb-mode)
(cocaine-line-mode)
(yas-global-mode 1)
;; (vertico-buffer-mode)
(global-wakatime-mode)
(salih/keyboard-config)

;; (if (eq (cdr (salih/get-random-theme 0)) 'nour)
;;     (spacious-padding-mode 1))

(consult-org-roam-mode 1)
(global-visual-line-mode 1)
(salih/consult-preview-at-point)
(when salih/awqat-show-mode-line (awqat-display-prayer-time-mode))

(if (or (eq doom-theme 'modus-vivendi-tritanopia)
        (eq doom-theme 'modus-vivendi-deuteranopia)
        (eq doom-theme 'modus-vivendi))
    (progn
      (set-frame-parameter nil 'alpha-background 98)
      (add-to-list 'default-frame-alist '(alpha-background . 98)))
  (progn
    (set-frame-parameter nil 'alpha-background 100)
    (add-to-list 'default-frame-alist '(alpha-background . 100))))

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)



(add-hook! 'doom-init-ui-hook :append
  (when
      t
    ;; usually I disable it only w/ doom
    ;; themes.
    ;; (doom-theme?)
    ;; (spacious-padding-mode -1)
    (remove-hook! 'dired-mode-hook #'all-the-icons-dired-mode)))


(provide '+hooks)
