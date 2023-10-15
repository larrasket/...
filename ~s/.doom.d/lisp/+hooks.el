;;; configs/~s/.doom.d/+hooks.el -*- lexical-binding: t; -*-



(add-hook 'prog-mode-hook               #'auto-fill-mode)
(add-hook 'csv-mode-hook                #'csv-align-mode)
(add-hook 'html-mode-hook               #'format-all-mode)
(add-hook 'elfeed-show-mode-hook        #'visual-line-mode)
(add-hook 'prog-mode-hook               #'column-enforce-mode)
(add-hook 'after-init-hook              #'global-flycheck-mode)
(add-hook 'maxima-inferior-mode-hook    #'salih/disable-bright)
(add-hook 'neotree-mode-hook            #'salih/disable-bright)
(add-hook 'sly-mrepl-mode-hook          #'salih/disable-bright)
(add-hook 'dired-mode-hook              #'salih/disable-bright)
(add-hook 'mu4e-headers-mode-hook       #'salih/disable-bright)
(add-hook 'mu4e-view-mode-hook          #'salih/disable-bright)
(add-hook 'mu4e-main-mode-hook          #'salih/disable-bright)
(add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)
(add-hook 'org-agenda-mode-hook         #'centaur-tabs-local-mode)
(add-hook 'treemacs-mode-hook           #'centaur-tabs-local-mode)
(add-hook 'org-mode-hook                #'centaur-tabs-local-mode)
(add-hook 'dired-mode-hook              #'centaur-tabs-local-mode)
(add-hook 'native-comp-limple-mode-hook #'centaur-tabs-local-mode)
(add-hook 'lisp-mode-hook               #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook               #'highlight-indent-guides-mode)
(add-hook 'nov-mode-hook                #'nov-xwidget-inject-all-files)
(add-hook 'eshell-alias-load-hook       'salih/eshell-load-bash-aliases)

(add-hook 'yas-minor-mode               (lambda () (yas-activate-extra-mode 'fundamental-mode)))
(add-hook 'after-make-frame-functions   (lambda (frame) (with-selected-frame frame (salih/keyboard-config))))
(add-hook 'python-mode-hook             (lambda () (flycheck-mode -1)))
(add-hook 'bibtex-mode-hook             (lambda () (add-hook 'after-save-hook '+format/buffer)))
(add-hook 'org-mode-hook                (lambda () (org-bullets-mode 1)))
(add-hook 'pdf-view-mode-hook           (lambda ()
                                          (set (make-local-variable 'evil-normal-state-cursor) (list nil))))
;; (add-hook 'pdf-view-mode-hook           #'pdf-view-midnight-minor-mode)
(add-hook 'org-mode-hook                (lambda ()
                                          (display-line-numbers-mode -1)
                                          (setq truncate-lines 1)
                                          (add-hook 'before-save-hook #'vulpea-project-update-tag nil 'local)
                                          (add-hook 'find-file-hook #'vulpea-project-update-tag nil 'local)
                                          (git-gutter-mode -1)
                                          (setq org-hide-leading-stars t)))

(add-hook 'nov-mode-hook                (lambda ()
                                          (defface tmp-buffer-local-face
                                            '((t :family "Roboto Condensed" :height 1.0)) "")
                                          (buffer-face-set 'tmp-buffer-local-face)
                                          (setq left-margin-width 4)
                                          (setq left-fringe-width 0)
                                          (setq right-fringe-width 0)
                                          (text-scale-set 1)))

(add-hook 'org-roam-capture-new-node-hook (lambda  ()
                                            (setq roam-titles
                                                  (salih/org-roam-get-node-files (org-roam-node-read--completions)))))

;; TODO: what about adding some verious quotes here? I can imaging having an RPC
;; call to some external program to get awsome quotes from there.
(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width
                                        "The fear of the Lord is the beginning of wisdom; all those who practice it have
a good understanding. His praise endures forever. ")))


;; Activate the advice
(ad-activate 'org-agenda-get-some-entry-text)

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


(add-to-list 'org-babel-load-languages '(julia-vterm . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)


(run-at-time nil (* 30 60) #'elfeed-update)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)
(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook) #'vi-tilde-fringe-mode)

;; ;; make evil treat "-" and "_" as parts of words when using w or e
;; (with-eval-after-load 'evil
;;     (defalias #'forward-evil-word #'forward-evil-symbol)
;;     (setq-default evil-symbol-word-search t))


;; init
(epa-file-enable)
(centaur-tabs-mode)
(yas-global-mode 1)
(vertico-buffer-mode)
(global-wakatime-mode)
(salih/keyboard-config)
(consult-org-roam-mode 1)
(salih/consult-preview-at-point)
(when salih/awqat-show-mode-line (awqat-display-prayer-time-mode))



;; see https://github.com/emacs-lsp/lsp-mode/issues/3577#issuecomment-1709232622
(after! lsp
  (delete 'lsp-terraform lsp-client-packages))


(provide '+hooks)
