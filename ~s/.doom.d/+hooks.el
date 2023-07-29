;;; configs/~s/.doom.d/+hooks.el -*- lexical-binding: t; -*-

(add-hook 'prog-mode-hook (lambda ()
                            (highltier)
                            (column-enforce-mode)
                            (auto-fill-mode)
                            (setq-default indent-tabs-mode nil)))

(add-hook 'after-init-hook        #'global-flycheck-mode)
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
                                    (setq org-hide-leading-stars t)))


(advice-add 'org-agenda :before #'vulpea-agenda-files-update)
(advice-add 'org-todo-list :before #'vulpea-agenda-files-update)
(add-to-list 'org-tags-exclude-from-inheritance "@read")
(add-to-list 'org-tags-exclude-from-inheritance "noexport")
(add-to-list 'org-tags-exclude-from-inheritance "project")


(add-hook 'python-mode-hook (lambda ()
                              (flycheck-mode -1)))


(add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)


(add-hook 'xwidget-webkit-mode-hook (lambda ()
                                      (evil-collection-define-key 'normal 'xwidget-webkit-mode-map "y" 'xwidget-webkit-copy-selection-as-kill)
                                      (evil-collection-define-key 'normal 'xwidget-webkit-mode-map "SPC" 'xwidget-webkit-scroll-up)))

(add-hook 'pdf-view-mode-hook
          (lambda ()
            (set (make-local-variable 'evil-normal-state-cursor) (list nil))))

(add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


(after! sly
  (setq sly-complete-symbol-function 'sly-flex-completions))



(epa-file-enable)
(yas-global-mode 1)
(global-wakatime-mode)
(awqat-display-prayer-time-mode)
(salih/consult-preview-at-point)




(add-hook 'bibtex-mode-hook (lambda ()
                              (add-hook 'after-save-hook '+format/buffer)))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(add-hook 'after-init-hook #'mu4e)




(with-eval-after-load 'org-agenda
  (defun my/org-has-children ()
    (if (save-excursion (org-goto-first-child)) "▶" " "))
  (add-to-list 'org-agenda-prefix-format '(
                                           agenda  . "%i%-3:(my/org-has-children) %-12:c%?-12t% s ")))



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
     (string-prefix-p "*Async-native-compile-log*" name)
     (string-prefix-p "*Native-compile-Log" name)


     (string-prefix-p "*elfeed-log*" name)
     (string-prefix-p "*Org Clock*" name)


     (string-prefix-p "*flycheck" name)
     (string-prefix-p "*nov" name)
     (string-prefix-p "*format" name)


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

(add-hook 'eshell-alias-load-hook 'salih/eshell-load-bash-aliases)



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
