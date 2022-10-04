;;; keys.el -*- lexical-binding: t; -*-

(provide 'keys)

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
(global-set-key (kbd "M-RET") 'lsp-execute-code-action)
(evil-global-set-key 'normal (kbd "/") 'swiper)




(global-set-key (kbd "C-x C-d") 'find-file)
(global-set-key (kbd "C-x C-r") 'recentf)
(global-set-key (kbd "C-x C-t") 'gts-do-translate)
(global-set-key (kbd "C-x C-t") 'gts-do-translate)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'bookmark-jump)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-+") 'doom/reset-font-size)


(global-set-key (kbd "C-c e p") 'emms-play-playlist)
(global-set-key (kbd "C-c e d") 'emms-play-directory)
;; (global-set-key (kbd "C-c e <up>") 'emms-start)
;; (global-set-key (kbd "C-c e <down>") 'emms-stop)
(global-set-key (kbd "C-c e <left>") 'emms-seek-forward)
(global-set-key (kbd "C-c e <right>") 'emms-seek-backward)

(global-set-key (kbd "M-1") '+workspace/switch-to-0 )
(global-set-key (kbd "M-2") '+workspace/switch-to-1 )
(global-set-key (kbd "M-3") '+workspace/switch-to-2 )
(global-set-key (kbd "M-4") '+workspace/switch-to-3 )
(global-set-key (kbd "M-5") '+workspace/switch-to-4 )
(global-set-key (kbd "M-6") '+workspace/switch-to-5 )
(global-set-key (kbd "M-7") '+workspace/switch-to-6 )
(global-set-key (kbd "M-8") '+workspace/switch-to-7 )
(global-set-key (kbd "M-9") '+workspace/switch-to-8 )


(use-package org-roam
  :bind (("C-c r l" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r g" . org-roam-graph)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r c" . org-roam-capture)
         ;; Dailies
         ("C-c r j" . org-roam-dailies-capture-today)))


;; '+workspace/switch-to-0
;; '+workspace/switch-to-1
;; '+workspace/switch-to-2
;; '+workspace/switch-to-3
;; '+workspace/switch-to-4
;; '+workspace/switch-to-5
;; '+workspace/switch-to-6
;; '+workspace/switch-to-7
;; '+workspace/switch-to-8



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

;; need another one for python stuff, since this gets re-bound


(global-set-key [f6] (lambda () (interactive) (neotree-project-dir) (lsp-treemacs-symbols) (evil-window-next) ))
;; (global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)
;; (global-set-key (kbd "<f11>") 'org-tree-slide-move-next-tree)
;; (global-set-key (kbd "<f12>") 'org-tree-slide-move-previous-tree)


;; debug

;; (global-set-key [f5] '+debugger/start)
;; (global-set-key (kbd "C-<f5>")'+debugger/quit)
;; (global-set-key [f11] 'dap-step-in)
;; (global-set-key [f12] 'lsp-goto-implementation)
;; (global-set-key [f9] 'dap-breakpoint-toggle)










;; language tool
(setq languagetool-language-tool-jar
      "~/.languagetool/languagetool-commandline.jar")
(setq languagetool-default-language "en-GB")
;; (global-set-key (kbd "C-c l c") 'languagetool-check)
;; (global-set-key (kbd "C-q") 'org-agenda-open-link)
(global-set-key (kbd "C-;") 'iedit-mode)
;; (global-set-key (kbd "C-c l d") 'langrrgetool-clear-buffer)
;; (global-set-key (kbd "C-c l p") 'languagetool-correct-at-point)
;; (global-set-key (kbd "C-c l b") 'languagetool-correct-buffer)
;; (global-set-key (kbd "C-c l l") 'languagetool-set-language)
;; (global-set-key (kbd "C-c l l") 'languagetool-set-language)
;; (local-set-key (kbd "C-c x") 'org-latex-preview)



(add-hook 'pdf-view-mode-hook
          (lambda () (local-set-key (kbd "<f2>") #'pdf-annot-add-text-annotation)))

(add-hook 'pdf-view-mode-hook
          (lambda () (local-set-key (kbd "<f3>") #'pdf-annot-add-underline-markup-annotation)))

;; (add-hook 'prog-mode-hook (lambda)  () (local-set-key (kbd "M-RET") #'lsp-execute-code-action))

(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "<f8>") #'org-tree-slide-mode)))


(add-hook 'calc-mode-hook (lambda () (local-set-key (kbd "r r") #'calc-reset)))



(defun EmacsAnyWhere ()
  (when (string= (buffer-name) "*Emacs Anywhere*")
    (local-set-key (kbd "C-c C-c") #'evil-quit)
    (auto-fill-mode)
    (flyspell-mode)
    (markdown-mode)))

(add-hook 'buffer-list-update-hook 'EmacsAnyWhere)


;; comment


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



;; (global-set-key (kbd "M-[")  'centaur-tabs-backward)
;; (global-set-key (kbd "M-]") 'centaur-tabs-forward)
;; (global-set-key (kbd "M-t") 'centaur-tabs--create-new-tab)


;; TODO use C-'
