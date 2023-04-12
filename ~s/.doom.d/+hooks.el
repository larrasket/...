;;; ../../../:/home/ghd/gits/configs/~s/.doom.d/+hooks.el -*- lexical-binding: t; -*-

(provide '+hooks)
(add-hook 'prog-mode-hook (lambda ()
                            (highltier)
                            (column-enforce-mode)
                            (auto-fill-mode)
                            (setq-default indent-tabs-mode nil)))

(add-hook 'after-init-hook   #'global-flycheck-mode)
(add-hook 'nov-mode-hook     #'salih/make-buffer-white)
(add-hook 'csv-mode-hook     #'csv-align-mode)
(add-hook 'neotree-mode-hook (lambda () (doom-modeline 1) (solaire-mode -1)))
(add-hook 'sly-mrepl-mode (lambda () (doom-modeline-mode 1)))
(add-hook 'lisp-mode-hook    #'rainbow-delimiters-mode)
(add-hook 'yas-minor-mode(lambda() (yas-activate-extra-mode 'fundamental-mode)))
(add-hook 'dired-mode-hook(lambda () (solaire-mode -1) (org-download-enable)))

(add-hook 'org-mode-hook     (lambda ()
                               (display-line-numbers-mode -1)
                               (setq truncate-lines 1)))

(add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)
(add-hook 'after-init-hook #'mu4e)


(after! solaire-mode
  (setq solaire-mode-real-buffer-fn #'salih/solaire-mode-real-buffer-custom-p))


(after! sly
  (setq sly-complete-symbol-function 'sly-flex-completions))



(epa-file-enable)
(yas-global-mode 1)
(erc-spelling-mode)
(global-wakatime-mode)
(vertico-buffer-mode)
(salih/consult-preview-at-point)
