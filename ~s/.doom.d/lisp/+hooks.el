;;; configs/~s/.doom.d/+hooks.el -*- lexical-binding: t; -*-

(add-hook 'prog-mode-hook (lambda ()
                            (highltier)
                            (column-enforce-mode)
                            (auto-fill-mode)
                            (setq-default indent-tabs-mode nil)))

(add-hook 'csv-mode-hook          #'csv-align-mode)
(plist-put +popup-defaults :modeline t)


(defun salih/disable-bright ()
  (solaire-mode -1))

(add-hook 'lisp-mode-hook         #'rainbow-delimiters-mode)
(add-hook 'neotree-mode-hook      #'salih/disable-bright)
(add-hook 'sly-mrepl-mode-hook    #'salih/disable-bright)
(add-hook 'dired-mode-hook        #'salih/disable-bright)
(add-hook 'mu4e-headers-mode-hook #'salih/disable-bright)
(add-hook 'mu4e-view-mode-hook    #'salih/disable-bright)
(add-hook 'mu4e-main-mode-hook    #'salih/disable-bright)
(add-hook 'yas-minor-mode         (lambda () (yas-activate-extra-mode 'fundamental-mode)))
(add-hook 'org-mode-hook          (lambda ()
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


(add-hook 'python-mode-hook (lambda ()
                              (flycheck-mode -1)))


(add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)



(add-hook 'pdf-view-mode-hook
          (lambda ()
            (set (make-local-variable 'evil-normal-state-cursor) (list nil))
            (pdf-view-midnight-minor-mode)))

(add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


(after! sly
  (setq sly-complete-symbol-function 'sly-flex-completions))



;; init
(epa-file-enable)
(vertico-buffer-mode)
(yas-global-mode 1)
(global-wakatime-mode)
(salih/consult-preview-at-point)
(add-hook 'after-init-hook        #'global-flycheck-mode)
;; (add-hook 'after-init-hook #'mu4e)
(consult-org-roam-mode 1)
(add-hook 'after-make-frame-functions (lambda (frame) (with-selected-frame frame (salih/keyboard-config))))

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



(after! solaire-mode
  (setq solaire-mode-real-buffer-fn #'salih/solaire-mode-real-buffer-custom-p))


(add-hook 'org-agenda-mode-hook         #'centaur-tabs-local-mode)
(add-hook 'treemacs-mode-hook           #'centaur-tabs-local-mode)
(add-hook 'org-mode-hook                #'centaur-tabs-local-mode)
(add-hook 'dired-mode-hook              #'centaur-tabs-local-mode)
(add-hook 'native-comp-limple-mode-hook #'centaur-tabs-local-mode)
(centaur-tabs-mode)
(defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Helm" name)
     (string-prefix-p "*Org Agenda*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*LSP" name)
     (string-prefix-p "*company" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*tramp" name)
     (string-prefix-p " *Mini" name)
     (string-prefix-p "*help" name)
     (string-prefix-p "*straight" name)
     (string-prefix-p " *temp" name)
     (string-prefix-p "*Help" name)
     (string-prefix-p "*Compile-Log*" name)

     (string-prefix-p "*doom*" name)
     (string-prefix-p "*Org tags*" name)
     (string-prefix-p "*scratch*" name)
     (string-prefix-p "*Semantic" name)
     (string-prefix-p "*mu4e-headers*" name)
     (string-prefix-p "*mu4e-main*" name)
     (string-prefix-p "*mu4e-update" name)
     (string-prefix-p "*julia" name)
     (string-prefix-p "*sly-mrepl" name)


     (string-prefix-p "*Messages*" name)
     (string-prefix-p "*Warnings*" name)
     (string-prefix-p "*httpd*" name)
     (string-prefix-p "*gopls*" name)
     (string-prefix-p "*Async-native-compile-log*" name)
     (string-prefix-p "*Native-compile-Log" name)


     (string-prefix-p "*elfeed-log*" name)
     (string-prefix-p "*Org Clock*" name)


     (string-prefix-p "*flycheck" name)
     (string-prefix-p "*nov" name)
     (string-prefix-p "*format" name)
     (string-prefix-p "*Pandoc" name)


     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name))))))

;; lisp

(defvar salih/sly--compile-eval-begin-print-counter 0 "a counter to distinguish compile/eval cycles")
(defun salih/sly--compile-eval-begin-print (&rest _)
  "print the counter value into REPL to distinguish compile/eval cycles."
  ;;(sly-eval-async `(cl:format t "~&----- my advice called from: ~a" (quote ,real-this-command))) ;; debug-code
  (sly-eval-async `(cl:format t "" ,(cl-incf salih/sly--compile-eval-begin-print-counter))))
(advice-add 'sly-compile-string :before 'salih/sly--compile-eval-begin-print)
(advice-add 'sly-compile-file :before 'salih/sly--compile-eval-begin-print)
;;(advice-add 'sly-compile-region :before 'salih/sly--compile-eval-begin-print) ;; `sly-compile-region' already done by `sly-compile-string'
(advice-add 'sly-eval-print-last-expression :before 'salih/sly--compile-eval-begin-print) ;; `C-j' in `sly-scratch' buffer
(advice-add 'sly-eval-with-transcript :before 'salih/sly--compile-eval-begin-print)

(defun salih/sly-eval-with-print (form)
  "Evaluate FORM in the SLY REPL, wrapping it with a (print ...) form."
  (interactive "sForm: ")
  (let* ((form-with-print (format "(print %s)" form))
         (sly-command (sly-interactive-eval form-with-print)))
    (sly-eval-last-expression)
    (message "Evaluated: %s" form-with-print)))

(defun salih/sly-compile-defun-with-print ()
  "Compile the current toplevel form in SLY, wrapping it with a (print ...) form."
  (interactive)
  (let* ((form (sly-sexp-at-point))
         (form-with-print (format "(print %s)" form))
         (sly-command (sly-interactive-eval form-with-print)))
    (sly-compile-defun)
    (message "Compiled: %s" form-with-print)))

(add-hook 'elfeed-show-mode-hook 'visual-line-mode)
;; (add-hook 'elfeed-show-mode-hook (lambda () (set-fontset-font "fontset-default" 'arabic (font-spec :family "Arial" :size 16))))

(add-hook 'eshell-alias-load-hook 'salih/eshell-load-bash-aliases)

;; call elfeed-update every 30 minutes
(run-at-time nil (* 30 60) #'elfeed-update)


(defvar salih/consult--source-books
  `(:name     "File"
    :narrow   ?f
    :category file
    :face     consult-file
    :history  file-name-history
    :state    ,#'consult--file-state
    :new      ,#'consult--file-action
    :items
    ,(lambda ()
       (let ((ht (consult--buffer-file-hash))
             items)
         (dolist (file (bound-and-true-p salih/books) (nreverse items))
           (unless (eq (aref file 0) ?/)
             (let (file-name-handler-alist)
               (setq file (expand-file-name file))))
           (unless (gethash file ht)
             (push (consult--fast-abbreviate-file-name file) items)))))))
(add-to-list 'consult-buffer-sources 'salih/consult--source-books 'append)


(defun salih/org-roam-get-node-files (node-list)
  "Applies `org-roam-node-file' function to the cdr of each element in NODE-LIST."
  (mapcar (lambda (node) (org-roam-node-title (cdr node)))
          node-list))
(setq roam-titles (salih/org-roam-get-node-files (org-roam-node-read--completions)))
(defun salih/get-org-roam-titles ()
  roam-titles)
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


(after! julia-repl
  (set-popup-rule! "^\\*julia:*.*\\*$" :quit nil :side 'right :width .5))


(after! org-roam
  (setq org-roam-list-files-commands '(find fd fdfind rg)))



(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
  #'vi-tilde-fringe-mode)


(after! git-gutter-fringe
  (setq-default fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom))

(add-hook 'org-roam-capture-new-node-hook (lambda  ()
                                            (setq roam-titles
                                                  (salih/org-roam-get-node-files (org-roam-node-read--completions)))))



(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook #'salih/fix-tag-alignment)



(custom-set-variables
 '(all-the-icons-completion-mode nil))

(provide '+hooks)
