;;; private/pdf/config.el -*- lexical-binding: t; -*-


(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))



(add-hook 'pdf-view-mode-hook
  (lambda ()
    (set (make-local-variable 'evil-normal-state-cursor) (list nil))))




(save-place-mode 1)


(add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
