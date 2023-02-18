;;; ../configs/.doom.d/keys.el -*- lexical-binding: t; -*-

(provide 'keys)

(setq salih/prefix-global "C-x"
      salih/prefix-mode "C-c")


;; resolve evil issue with C-g

(define-key evil-normal-state-map (kbd "C-g") 'evil-escape)
(define-key evil-visual-state-map (kbd "C-g") 'evil-escape)
(define-key evil-insert-state-map (kbd "C-g") 'evil-escape)
(define-key evil-replace-state-map (kbd "C-g") 'evil-escape)
(define-key evil-operator-state-map (kbd "C-g") 'evil-escape)


(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-g") 'salih/evil-escape-and-abort-company)
  (define-key company-search-map (kbd "C-g") 'salih/evil-escape-and-abort-company))


;; set key definition

(general-auto-unbind-keys)



(add-hook 'c++-mode-hook
          (lambda () (local-set-key (salih/mode "C-c") 'compileandrun)))

(add-hook 'csharp-mode-hook
          (lambda () (local-set-key (salih/mode "C-c") 'sharprun)))

(add-hook 'go-mode-hook
          (lambda () (local-set-key (salih/mode "C-c") 'gorun)
                (local-set-key (kbd "<f2>") 'rungo)))

(add-hook 'dired-mode-hook
          (lambda () (local-set-key (salih/mode "C-c") #'xah-open-in-external-app)))

(add-hook 'org-mode-hook
          (lambda () (local-set-key (salih/mode "C-f") #'org-footnote-action)))

(global-unset-key (kbd "C-f"))
(define-key org-mode-map (kbd "C-c C-f") nil)




(general-define-key
 :prefix salih/prefix-global
 "c" 'org-capture
 "a" 'org-agenda
 "." 'find-file
 "," 'persp-switch-to-buffer
 "<" 'switch-to-buffer
 "RET" 'bookmark-jump
 "[" 'previous-buffer
 "]" 'next-buffer
 "d" 'kill-current-buffer
 "k" 'kill-current-buffer
 "D" 'doom/kill-all-buffers
 "K" 'doom/kill-other-buffers
 "C-t" 'gts-do-translate)

;; file keys
(general-define-key
 :prefix (concat salih/prefix-global " f")
 "r" 'recentf-open-files
 "g" 'magit-find-file
 "L" 'magit-log-buffer-file
 "l" #'locate)



;; code keys
(general-define-key
 :prefix (concat salih/prefix-mode " c")
 "d" '+lookup/definition
 "r" '+lookup/references
 "t" '+lookup/type-definition
 "e" '+default/diagnostics
 "g" 'salih/find-definition-or-lookup)



;; search keys
(general-define-key
 :prefix (concat salih/prefix-global " s")
 "d" '+default/search-cwd
 "p" '+default/search-project
 "b" '+default/search-buffer
 "w" '+lookup/dictionary-definition)


;; insertion
(general-define-key
 :prefix (concat salih/prefix-global " i")
 "u" 'insert-char
 "n" 'org-noter-insert-note
 "t" 'insert-now-timestamp)


;; notes
(general-define-key
 :prefix (concat salih/prefix-global " n")
 "b" 'citar-open-notes
 "n" 'org-noter)



;; roam
(general-define-key
 :prefix (concat salih/prefix-global " r")
 "b" 'org-roam-buffer-toggle
 "c" 'org-roam-capture
 "f" 'org-roam-node-find
 "i" 'org-roam-insert
 "I" 'org-roam-insert-immediate
 "t" 'org-roam-tag-add
 "a" 'org-roam-alias-add
 "j" 'org-roam-dailies-capture-today)

;; magit
(general-define-key
 :prefix (concat salih/prefix-global " g")
 "g"   'magit-status
 "G"   'magit-status-here
 "C"   'magit-clone
 "L"   'magit-log-buffer-file
 "."   '+vc/browse-at-remote
 "t"   'magit-todos-list)


;; terminals
(general-define-key
 :prefix (concat salih/prefix-global " t")
 "t"   'vterm
 "e"   'eshell)


;; code editing
(general-define-key
 :prefix (concat salih/prefix-mode)
 "r" #'salih/rename-or-iedit)




(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)


(global-set-key (kbd "M-RET") 'lsp-execute-code-action)
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") 'enlarge-window)
(global-set-key (kbd "C-<up>") 'shrink-window)
