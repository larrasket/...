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



(map! :leader
      :desc "move to jorunal"
      "j" #'org-journal-new-entry)


(map! :leader
      :desc "play playlist"
      "m p p" #'emms-play-playlist)


(map! :leader
      :desc "play directory"
      "m p d" #'emms-play-directory)


(map! :leader
      :desc "save play directory"
      "m p d" #'emms-play-directory)


(map! :leader
      :desc "save current playlist"
      "m p s" #'emms-playlist-save)


(map! :leader
      :desc "shuffle playlist"
      "m p c" #'emms-shuffle)


(map! :leader
      :desc "repeat"
      "m p r" #'emms-toggle-repeat-track)


(map! :leader
      :desc "run vterm"
      "t t" 'vterm)
(map! :leader
      :desc "run mail"
      "m m" 'mu4e)
(map! :leader
      :desc "watch var"
      "o w" 'dap-ui-expressions-add)
(map! :leader
      :desc "open-ajenda"
      "a" #'org-agenda)

(map! :leader
      :desc "insert date"
      "d" #'org-schedule)


(map! :leader
      :desc "show errors"
      "e e" #'flycheck-list-errors)



(map! :leader
      :desc "emacs shell"
      "e s" #'eshell)

(map! :leader
      :desc "show errors"
      "e l" #'lsp-treemacs-errors-list)

(map! :leader
      :desc "evaluate latex"
      "l e" #'TeX-command-master "LaTex")




(map! :leader
      :desc "open leetcode"
      "l l" #'leetcode)



(map! :leader
      :desc "roam graph"
      "r g" #'org-roam-graph)


(map! :leader
      :desc "add tag"
      "r t" #'org-roam-tag-add)



(map! :leader
      :desc "switch to raom buffer"
      "r b" #'org-roam-buffer-toggle)


(map! :leader
      :desc "capture"
      "r c" #'org-roam-capture)


(map! :leader
      :desc "insert"
      "r i" #'org-roam-node-insert)


(map! :leader
      :desc "find file"
      "r f" #'org-roam-node-find)

(map! :leader
      :desc "roam"
      "r r" #'org-roam-buffer-display-dedicated)


(map! :leader
      :desc "org caputer"
      "x" #'org-capture)

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
