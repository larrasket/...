;;; configs/~s/.doom.d/+hooks.el -*- lexical-binding: t; -*-





(add-hook 'prog-mode-hook 'column-enforce-mode)
(add-hook 'prog-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(add-hook 'csv-mode-hook          #'csv-align-mode)



(add-hook 'lisp-mode-hook               #'rainbow-delimiters-mode)
(add-hook 'maxima-inferior-mode-hook    #'salih/disable-bright)
(add-hook 'neotree-mode-hook            #'salih/disable-bright)
(add-hook 'sly-mrepl-mode-hook          #'salih/disable-bright)
(add-hook 'dired-mode-hook              #'salih/disable-bright)
(add-hook 'mu4e-headers-mode-hook       #'salih/disable-bright)
(add-hook 'mu4e-view-mode-hook          #'salih/disable-bright)
(add-hook 'mu4e-main-mode-hook          #'salih/disable-bright)
(add-hook 'yas-minor-mode               (lambda () (yas-activate-extra-mode 'fundamental-mode)))
(add-hook 'org-mode-hook                (lambda ()
                                          (display-line-numbers-mode -1)
                                          (setq truncate-lines 1)
                                          (add-hook 'before-save-hook #'vulpea-project-update-tag nil 'local)
                                          (add-hook 'find-file-hook #'vulpea-project-update-tag nil 'local)
                                          (git-gutter-mode -1)
                                          (setq org-hide-leading-stars t)))


(advice-add 'org-agenda :before #'vulpea-agenda-files-update)
(advice-add 'org-todo-list :before #'vulpea-agenda-files-update)
(add-to-list 'org-tags-exclude-from-inheritance "@read")
(add-to-list 'org-tags-exclude-from-inheritance "noexport")
(add-to-list 'org-tags-exclude-from-inheritance "project")


;; Activate the advice
(ad-activate 'org-agenda-get-some-entry-text)


(add-hook 'python-mode-hook (lambda () (flycheck-mode -1)))


(add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)



(add-hook 'pdf-view-mode-hook
          (lambda ()
            (set (make-local-variable 'evil-normal-state-cursor) (list nil))
            (pdf-view-midnight-minor-mode)))

(add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))






(add-hook 'bibtex-mode-hook (lambda ()
                              (add-hook 'after-save-hook '+format/buffer)))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)


(add-hook 'nov-mode-hook (lambda ()
                           (defface tmp-buffer-local-face
                             '((t :family "Roboto Condensed" :height 1.0)) "")
                           (buffer-face-set 'tmp-buffer-local-face)
                           (setq left-margin-width 4)
                           (setq left-fringe-width 0)
                           (setq right-fringe-width 0)
                           (text-scale-set 1)))




;; ;; make evil treat "-" and "_" as parts of words when using w or e
;; (with-eval-after-load 'evil
;;     (defalias #'forward-evil-word #'forward-evil-symbol)
;;     (setq-default evil-symbol-word-search t))




(add-hook 'org-agenda-mode-hook         #'centaur-tabs-local-mode)
(add-hook 'treemacs-mode-hook           #'centaur-tabs-local-mode)
(add-hook 'org-mode-hook                #'centaur-tabs-local-mode)
(add-hook 'dired-mode-hook              #'centaur-tabs-local-mode)
(add-hook 'native-comp-limple-mode-hook #'centaur-tabs-local-mode)
(centaur-tabs-mode)


(advice-add 'sly-compile-string :before 'salih/sly--compile-eval-begin-print)
(advice-add 'sly-compile-file :before 'salih/sly--compile-eval-begin-print)
;;(advice-add 'sly-compile-region :before 'salih/sly--compile-eval-begin-print) ;; `sly-compile-region' already done by `sly-compile-string'
(advice-add 'sly-eval-print-last-expression :before 'salih/sly--compile-eval-begin-print) ;; `C-j' in `sly-scratch' buffer
(advice-add 'sly-eval-with-transcript :before 'salih/sly--compile-eval-begin-print)

(add-hook 'elfeed-show-mode-hook 'visual-line-mode)
(add-hook 'eshell-alias-load-hook 'salih/eshell-load-bash-aliases)
(run-at-time nil (* 30 60) #'elfeed-update)


(add-to-list 'consult-buffer-sources 'salih/consult--source-books 'append)



;; TODO this does not belong here.
(setq org-roam-buffer-source
      `(:name     "Org-roam"
        :hidden   nil
        :narrow   ,consult-org-roam-buffer-narrow-key
        :annotate ,(lambda (cand)
                     (let* ((name (org-roam-node-from-title-or-alias cand)))
                       (if name (file-name-nondirectory (org-roam-node-file name))
                         "")))

        :action ,(lambda (name)
                   (if salih/temp-roam-insert
                       (progn
                         (setq salih/temp-roam-insert nil)
                         (let* ((node (org-roam-node-from-title-or-alias name))
                                (description (org-roam-node-title node))
                                (id (org-roam-node-id node)))
                           (insert (org-link-make-string
                                    (concat "id:" id)
                                    description))
                           (run-hook-with-args 'org-roam-post-node-insert-hook
                                               id
                                               description)))
                     (find-file (org-roam-node-file (org-roam-node-from-title-or-alias name)))))

        :new ,(lambda (name)
                (let* ((n (org-roam-node-create :title name)))
                  (org-roam-capture- :node n)
                  (when salih/temp-roam-insert
                    (progn
                      (setq salih/temp-roam-insert nil)
                      (let* ((node (org-roam-node-from-title-or-alias name))
                             (description (org-roam-node-title node))
                             (id (org-roam-node-id node)))
                        (insert (org-link-make-string
                                 (concat "id:" id)
                                 description))
                        (run-hook-with-args 'org-roam-post-node-insert-hook
                                            id
                                            description)))))


                (setq roam-titles (salih/org-roam-get-node-files (org-roam-node-read--completions))))

        :items    ,#'salih/get-org-roam-titles))





(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook) #'vi-tilde-fringe-mode)



(add-hook 'org-roam-capture-new-node-hook (lambda  ()
                                            (setq roam-titles
                                                  (salih/org-roam-get-node-files (org-roam-node-read--completions)))))



(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook #'salih/fix-tag-alignment)


;; init

(add-hook 'after-make-frame-functions (lambda (frame) (with-selected-frame frame (salih/keyboard-config))))

(epa-file-enable)
(elfeed-tube-setup)
(yas-global-mode 1)
(vertico-buffer-mode)
(global-wakatime-mode)
(salih/consult-preview-at-point)
(add-hook 'after-init-hook        #'global-flycheck-mode)
(consult-org-roam-mode 1)


;; see https://github.com/emacs-lsp/lsp-mode/issues/3577#issuecomment-1709232622
(after! lsp
  (delete 'lsp-terraform lsp-client-packages))


(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width
                                        "The fear of the Lord is the beginning of wisdom; all those who practice it have
a good understanding. His praise endures forever. ")))



(provide '+hooks)
