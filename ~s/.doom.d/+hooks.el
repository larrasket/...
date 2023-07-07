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
(if (not IS-PLASMA)
    (vertico-buffer-mode))
(global-wakatime-mode)
;; (global-org-modern-mode)
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
                             '((t :family "Noto Sans" :height 1.0)) "")
                           (buffer-face-set 'tmp-buffer-local-face)
                           (setq left-margin-width 4)
                           (setq left-fringe-width 0)
                          (setq right-fringe-width 0)))
