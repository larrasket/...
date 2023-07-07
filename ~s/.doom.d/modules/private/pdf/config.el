;;; private/pdf/config.el -*- lexical-binding: t; -*-


(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))



(add-hook 'pdf-view-mode-hook
  (lambda ()
    (set (make-local-variable 'evil-normal-state-cursor) (list nil))))



(add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))




(add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)




(use-package nov-xwidget
  :demand t
  :after nov
  :config
  (define-key nov-mode-map (kbd "&") 'nov-xwidget-view)
  (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files))


(require 'nov-xwidget)
(setq nov-xwidget-style-dark nov-xwidget-style-light)
