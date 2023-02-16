(add-to-list 'load-path "~/.doom.d/")
(require '+roam)
(require '+handy)



(add-to-list 'org-agenda-files "~/roam/journal/agenda/todo.org")
(add-to-list 'org-agenda-files "~/roam/journal/agenda/births.org")


;; set org files
(setq +org-capture-journal-file "~/blog/content/stack.org"
 +org-capture-changelog-file "~/blog/content/nice.org"
 +org-capture-todo-file "~/roam/main/life.org")

(setq org-preview-html-viewer 'xwidget)

;; avoid warrnings of old compiled files
(setq load-prefer-newer t)
(setq highlight-indent-guides-method 'bitmap)

(defun highltier ()
  (require 'highlight-indent-guides)
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (highlight-indent-guides-mode))



(add-hook 'org-mode-hook 'highltier)
(add-hook 'prog-mode-hook 'highltier)


;; modern org mode style
(setq highlight-indent-guides-method 'bitmap
 org-modern-block-name '(("" "" ""))
 org-modern-checkbox nil
 org-modern-keyword '(("" . ""))
 org-modern-list nil
 org-modern-priority nil
 org-modern-star nil
 org-modern-tag nil
 org-modern-timestamp nil
 org-modern-todo nil)
(global-org-modern-mode)




(setq browse-url-browser-function 'xwidget-webkit-browse-url)




;; get red column indecator
(add-hook 'prog-mode-hook 'column-enforce-mode)




;; setup lisp
(setq inferior-lisp-program "sbcl")
(set-repl-handler! 'lisp-mode #'sly-mrepl)
(set-eval-handler! 'lisp-mode #'sly-eval-region)
(set-lookup-handlers! 'lisp-mode
  :definition #'sly-edit-definition
  :documentation #'sly-describe-symbol)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))


(defun insert-now-timestamp()
  (interactive)
  (org-insert-time-stamp (current-time) t))

(setq doom-theme 'distinguished)












(global-set-key (kbd "C-g") #'evil-keyboard-quit)
(global-set-key (kbd "C-g") #'evil-keyboard-quit)
(global-set-key (kbd "C-g") #'evil-keyboard-quit)

(global-set-key (kbd "C-g") #'evil-keyboard-quit)
(global-set-key (kbd "C-g") #'evil-keyboard-quit)





(general-auto-unbind-keys)


(general-define-key
 :prefix "C-x"
 "c" 'org-capture
 "a" 'org-agenda
 "." 'find-file
 "f" 'find-file
 "," 'persp-switch-to-buffer
 "<" 'switch-to-buffer
 "RET" 'bookmark-jump
 "C-x" 'projectile-find-file
 "[" 'previous-buffer
 "]" 'next-buffer
 "d" 'kill-current-buffer
 "k" 'kill-current-buffer
 "D" 'doom/kill-all-buffers
 "K" 'doom/kill-other-buffers
 "C-f" 'recentf-open-files)



(general-define-key
 :prefix "C-c c"
 "d" '+lookup/definition
 "r" '+lookup/references
 "t" '+lookup/type-definition
 "l" '+default/diagnostics)



(general-define-key
 :prefix "C-x s"
 "d" '+default/search-cwd
 "r" '+lookup/references
 "t" '+lookup/type-definition
 "l" '+default/diagnostics
 "f" '+lookup/file
 "o" '+lookup/online
 "p" '+default/search-project
 "b" '+default/search-buffer)

(general-define-key
 :prefix "C-x i"
 "u" 'insert-char
 "n" 'org-noter-insert-note
 "t" 'insert-now-timestamp)



(general-define-key
 :prefix "C-x n"
 "b" 'citar-open-notes
 "n" 'org-noter)




(general-define-key
 :prefix "C-c r"
 "b" 'org-roam-buffer-toggle
 "c" 'org-roam-capture
 "f" 'org-roam-find-node
 "i" 'org-roam-insert
 "I" 'org-roam-insert-immediate
 "t" 'org-roam-tag-add
 "a" 'org-roam-alias-add
 "j" 'org-roam-dailies-capture-today)


(general-define-key
 :prefix "C-x g"
 "g"   'magit-status
 "G"   'magit-status-here
 "C"   'magit-clone
 "L"   'magit-log-buffer-file
 "."   '+vc/browse-at-remote
 "t"   'magit-todos-list)



(general-define-key
 :prefix "C-x t"
 "t"   'vterm
 "e"   'eshell)

(global-set-key (kbd "M-RET") 'lsp-execute-code-action)

(add-hook 'neotree-mode-hook #'hide-mode-line-mode)
