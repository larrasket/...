;;; configs/~s/.doom.d/+hooks.el -*- lexical-binding: t; -*-

(add-hook 'prog-mode-hook (lambda ()
                            (highltier)
                            (column-enforce-mode)
                            (auto-fill-mode)
                            (setq-default indent-tabs-mode nil)))

(add-hook 'after-init-hook        #'global-flycheck-mode)
(add-hook 'csv-mode-hook          #'csv-align-mode)

(defun salih/disable-bright ()
  (doom-modeline 1)
  (solaire-mode -1))

(add-hook 'lisp-mode-hook         #'rainbow-delimiters-mode)
(add-hook 'neotree-mode-hook      #'salih/disable-bright)
(add-hook 'sly-mrepl-mode         #'salih/disable-bright)
(add-hook 'dired-mode-hook        #'salih/disable-bright)
(add-hook 'mu4e-headers-mode-hook #'salih/disable-bright)
(add-hook 'mu4e-view-mode-hook    #'salih/disable-bright)
(add-hook 'mu4e-main-mode-hook    #'salih/disable-bright)
(add-hook 'yas-minor-mode         (lambda () (yas-activate-extra-mode 'fundamental-mode)))
(add-hook 'org-mode-hook          (lambda ()
                                    (display-line-numbers-mode -1)
                                    (setq truncate-lines 1)
                                    (setq org-hide-leading-stars t)))

(add-hook 'python-mode-hook (lambda ()
                              (flycheck-mode -1)))


(add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)


(add-hook 'xwidget-webkit-mode-hook (lambda ()
                                      (evil-collection-define-key 'normal 'xwidget-webkit-mode-map "y" 'xwidget-webkit-copy-selection-as-kill)
                                      (evil-collection-define-key 'normal 'xwidget-webkit-mode-map "SPC" 'xwidget-webkit-scroll-up)))
                                     

                                     


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




;; make evil treat "-" and "_" as parts of words when using w or e
(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    (setq-default evil-symbol-word-search t))



(after! solaire-mode
  (setq solaire-mode-real-buffer-fn #'salih/solaire-mode-real-buffer-custom-p))


(add-hook 'org-agenda-mode-hook 'centaur-tabs-local-mode)
(add-hook 'native-comp-limple-mode-hook 'centaur-tabs-local-mode)
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
     (string-prefix-p "*scratch*" name)
     (string-prefix-p "*Semantic" name)
     (string-prefix-p "*mu4e-headers*" name)
     (string-prefix-p "*mu4e-main*" name)
     (string-prefix-p "*Messages*" name)
     (string-prefix-p "*Async-native-compile-log*" name)
     (string-prefix-p "*Native-compile-Log" name)


     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name))))))
