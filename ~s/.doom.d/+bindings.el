;;; ../configs/.doom.d/keys.el -*- lexical-binding: t; -*-



;; unbinding
(define-key org-mode-map (kbd "C-c C-f") nil)
(global-unset-key        (kbd "C-f"))
(define-key org-mode-map (salih/mode "]") nil)
(define-key org-mode-map (salih/mode "[") nil)
(general-auto-unbind-keys)



;; FIXME this is to make it easier with C-g instead of ESC while using evil
;; mode. It is not prefect yet, I will tree to reach better solutions in the
;; future
(define-key evil-normal-state-map       (kbd "C-g") #'evil-escape)
(define-key evil-visual-state-map       (kbd "C-g") #'evil-escape)
(define-key evil-insert-state-map       (kbd "C-g") #'evil-escape)
(define-key evil-replace-state-map      (kbd "C-g") #'evil-escape)
(define-key evil-operator-state-map     (kbd "C-g") #'evil-escape)
(define-key evil-insert-state-map       (salih/global "C-s") #'save-buffer)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-g") #'salih/evil-escape-and-abort-company)
  (define-key company-search-map (kbd "C-g") #'salih/evil-escape-and-abort-company))



(general-define-key
 :keymaps 'prog-mode-map
 :prefix salih/prefix-mode
 "c d" #'+lookup/definition
 "c r" #'+lookup/references
 "c t" #'+lookup/type-definition
 "c e" #'+default/diagnostics
 "c g" #'salih/find-definition-or-lookup
 ";"   #'salih/rename-or-iedit)


(general-define-key
 :keymaps 'flycheck-mode-map
 :prefix salih/prefix-mode
 "e l" #'flycheck-list-errors)


(general-define-key
 :keymaps 'nov-mode-map
 :prefix salih/prefix-mode
 "t" #'gts-do-translate)


(add-hook 'pdf-view-mode-hook (lambda ()
                                (local-set-key (salih/mode "t") #'gts-do-translate)
                                (evil-local-set-key 'normal (kbd "J") #' pdf-view-next-page-command)
                                (evil-local-set-key 'normal (kbd "K") #' pdf-view-previous-page-command)))



(add-hook 'lsp-mode-hook (lambda () (local-set-key (kbd "M-RET") #'lsp-execute-code-action)))
;; Run project

;; TODO Refactor this. I think this should be rewritten as an only one function
;; that is "Run file", which check the mode and map it to the matching running
;; method.

;; C++
(add-hook 'c++-mode-hook
          (lambda () (local-set-key (salih/mode "C-c") #'salih/compile-and-run-cpp)))

;; C#
(add-hook 'csharp-mode-hook
          (lambda () (local-set-key (salih/mode "C-c") #'salih/compile-and-run-csharp)))

;; Go
(add-hook 'go-mode-hook
          (lambda () (local-set-key (salih/mode "C-c") #'salih/compile-and-run-go-file)
            (local-set-key (kbd "<f2>") #'salih/compile-and-run-go-project)))


;; Dired
(general-define-key
 :keymaps 'dired-mode-map
 :prefix salih/prefix-mode
 "C-c" #'salih/open-in-external-app
 "C-e" #'salih/epa-dired-do-encrypt
 "C-d" #'epa-dired-do-encrypt)



;; Org-mode
(general-define-key
 :keymaps 'org-mode-map
 :prefix salih/prefix-mode
 "C-f" #'org-footnote-action
 "c i" #'org-clock-in
 "c o" #'org-clock-out
 "C-i" #'org-id-get-create
 "i l" #'org-web-tools-insert-link-for-url
 "i d" #'org-download-clipboard
 "i c" #'salih/org-id-get-create-with-custom-id
 "i k" #'citar-insert-citation
 "i n" #'orb-insert--link
 "n n" #'org-noter
 "n k" #'org-noter-kill-session
 "e p" #'org-pandoc-export-to-latex-pdf

 ;; roam
 "i r" #'org-roam-node-insert
 "r i" #'org-roam-node-insert
 "r t" #'org-roam-tag-add
 "r a" #'org-roam-alias-add
 "i b" #'orb-insert-link
 "f b" #'consult-org-roam-backlinks
 "f f" #'consult-org-roam-forward-links
 "TAB" #'consult-org-heading)



;; Lisp

(eval-after-load 'sly
  `(define-key sly-mode-map (salih/mode "C-e") 'sly-eval-region))


(eval-after-load 'sly
  `(define-key sly-mode-map (salih/mode "C-f") 'sly-eval-buffer))

(eval-after-load 'sly
  `(define-key sly-mode-map (salih/mode "C-j") 'salih/sly-compile-defun-with-print))



;; convenient
(general-define-key
 :prefix salih/prefix-global
 "c"     #'org-capture
 "a"     #'org-agenda
 "."     #'find-file
 ","     #'persp-switch-to-buffer
 "<"     #'switch-to-buffer
 "RET"   #'switch-to-buffer
 "["     #'previous-buffer
 "]"     #'next-buffer
 "d"     #'kill-current-buffer
 "k"     #'kill-current-buffer
 "D"     #'doom/kill-all-buffers
 "K"     #'doom/kill-other-buffers
 "l l"   #'leetcode
 "s s"   #'doom/sudo-this-file
 "TAB d" #'+workspace/delete
 "SPC"   #'projectile-find-file
 "/"     #'swiper)

;; file keys
(general-define-key
 :prefix (concat salih/prefix-global "f")
 "r" #'recentf-open-files
 "g" #'magit-find-file
 "l" #'projectile-find-file)


;; search global
(general-define-key
 :prefix (concat salih/prefix-global "s")
 "d" #'+default/search-cwd
 "b" #'+default/search-buffer
 "p" #'+default/search-project
 "g" #'rgrep)


;; notes
(general-define-key
 :prefix (concat salih/prefix-global "n")
 "f" #'citar-open-notes
 "b" #'citar-open-notes
 "o" #'salih/open-book)

;; roam
(general-define-key
 :prefix (concat salih/prefix-global "r")
 "b" #'org-roam-buffer-toggle
 "c" #'org-roam-capture
 "f" #'org-roam-node-find
 "j" #'org-roam-dailies-capture-today
 "t" #'org-roam-dailies-goto-today
 "s" #'consult-org-roam-search)



;; magit and vc
(general-define-key
 :prefix (concat salih/prefix-global "g")
 "g"   #'magit-status
 "G"   #'magit-status-here
 "C"   #'magit-clone
 "L"   #'magit-log-buffer-file
 "."   #'+vc/browse-at-remote
 "t"   #'magit-todos-list
 "D"   #'magit-file-delete)




;; other
(general-define-key
 :prefix (concat salih/prefix-global "e")
 "e" #'eshell
 "f" #'elfeed)

;; projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-x p")   #'projectile-command-map)
(define-key projectile-mode-map (kbd "C-x p a") #'projectile-add-known-project)

;; convenient

(global-set-key (kbd "C-M-g")      #'+lookup/definition)


(general-define-key
 :prefix salih/prefix-mode
 "i u" #'insert-char
 "s w" #'+lookup/dictionary-definition
 "C-t" #'gts-do-translate
 "C-s" #'centaur-tabs-ace-jump
 "]"   #'centaur-tabs-forward
 "["   #'centaur-tabs-backward)


;; resize windows
(global-set-key (kbd "C-<left>")  #'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") #'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>")  #'enlarge-window)
(global-set-key (kbd "C-<up>")    #'shrink-window)

;; maximize buffer
(global-set-key (kbd "<f3>") 'salih/toggle-maximize-buffer)

;; commenting lines
(global-set-key (kbd "M-;") 'salih/comment-or-uncomment-region-or-line)

(global-set-key [f6]
                (lambda ()
                  (interactive)
                  (neotree-project-dir)
                  (lsp-treemacs-symbols)))


(after! org-noter
  (defun salih/org-noter-sync-current-note-and-switch-window ()
    (interactive)
    (let ((prev-window (selected-window)))
      (org-noter-sync-current-note)
      (select-window prev-window)))
  (define-key org-noter-notes-mode-map (salih/mode "C-j")
              #'salih/org-noter-sync-current-note-and-switch-window)
  (define-key org-noter-doc-mode-map (salih/mode "C-c") #'org-noter-insert-precise-note))




(general-define-key
 :prefix (concat salih/prefix-global "m")
 "m" #'(lambda () (interactive) (mu4e~headers-jump-to-maildir "/Inbox"))
 "i" #'mu4e)


(evil-define-key 'normal elfeed-show-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)
(evil-define-key 'normal elfeed-search-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)
