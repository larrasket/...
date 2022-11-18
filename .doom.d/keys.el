;;; ../configs/.doom.d/keys.el -*- lexical-binding: t; -*-


(global-set-key [f3] 'toggle-maximize-buffer)
(global-set-key (kbd "C-<=>") 'text-scale-increase)
(global-set-key (kbd "C-<->") 'text-scale-decrease)
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<up>") 'enlarge-windor)
(global-set-key (kbd "M-n") 'make-frame)
(global-set-key (kbd "M-f") 'org-footnote-action)
(global-set-key (kbd "C-M-g") 'lsp-find-definition)
(global-set-key (kbd "C-M-r") 'lsp-find-references)
(global-set-key (kbd "M-RET") 'lsp-execute-code-action)
(require 'evil)
(evil-global-set-key 'normal (kbd "/") 'swiper)




(global-set-key (kbd "C-x C-d") 'find-file)
(global-set-key (kbd "C-x C-r") 'recentf)
(global-set-key (kbd "C-x C-t") 'gts-do-translate)
(global-set-key (kbd "C-x C-t") 'gts-do-translate)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'bookmark-jump)
(global-set-key (kbd "C-x C-b") 'bookmark-jump)
(global-set-key (kbd "C-h C-f") 'mark-defun)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-+") 'doom/reset-font-size)


(global-set-key (kbd "M-1") '+workspace/switch-to-0 )
(global-set-key (kbd "M-2") '+workspace/switch-to-1 )
(global-set-key (kbd "M-3") '+workspace/switch-to-2 )
(global-set-key (kbd "M-4") '+workspace/switch-to-3 )
(global-set-key (kbd "M-5") '+workspace/switch-to-4 )
(global-set-key (kbd "M-6") '+workspace/switch-to-5 )
(global-set-key (kbd "M-7") '+workspace/switch-to-6 )
(global-set-key (kbd "M-8") '+workspace/switch-to-7 )
(global-set-key (kbd "M-9") '+workspace/switch-to-8 )



(global-set-key (kbd "C-c a") 'org-agenda)





(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))


(global-set-key [f6] (lambda () (interactive) (neotree-project-dir) (lsp-treemacs-symbols) (evil-window-next) ))

(global-set-key (kbd "C-;") 'iedit-mode)


(add-hook 'pdf-view-mode-hook
          (lambda () (local-set-key (kbd "<f2>") #'pdf-annot-add-text-annotation)))

(add-hook 'pdf-view-mode-hook
          (lambda () (local-set-key (kbd "<f3>") #'pdf-annot-add-underline-markup-annotation)))

(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "<f8>") #'org-tree-slide-mode)))
(add-hook 'calc-mode-hook (lambda () (local-set-key (kbd "r r") #'calc-reset)))



(add-hook 'after-init-hook #'global-flycheck-mode) (add-to-list 'display-buffer-alist
                                                                `(,(rx bos "*Flycheck errors*" eos)
                                                                  (display-buffer-reuse-window
                                                                   display-buffer-in-side-window)
                                                                  (side            . bottom)
                                                                  (reusable-frames . visible)
                                                                  (window-height   . 0.18)))



 (defun my-flymd-browser-function (url)
   (let ((browse-url-browser-function 'browse-url-firefox))
     (browse-url url)))
 (setq flymd-browser-open-function 'my-flymd-browser-function)


(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)
(provide 'keys)
