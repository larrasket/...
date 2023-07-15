;;; ../configs/.doom.d/keys.el -*- lexical-binding: t; -*-



;; unbinding
(define-key org-mode-map (kbd "C-c C-f") nil)
(global-unset-key        (kbd "C-f"))
(general-auto-unbind-keys)



(define-key evil-normal-state-map       (kbd "C-g") #'evil-escape)
(define-key evil-visual-state-map       (kbd "C-g") #'evil-escape)
(define-key evil-insert-state-map       (kbd "C-g") #'evil-escape)
(define-key evil-replace-state-map      (kbd "C-g") #'evil-escape)
(define-key evil-operator-state-map     (kbd "C-g") #'evil-escape)


(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-g") #'salih/evil-escape-and-abort-company)
  (define-key company-search-map (kbd "C-g") #'salih/evil-escape-and-abort-company))





;; general programming
(add-hook 'prog-mode-hook (lambda ()
                            (local-set-key (salih/mode "c d") #'+lookup/definition)
                            (local-set-key (salih/mode "c r") #'+lookup/references)
                            (local-set-key (salih/mode "c t") #'+lookup/type-definition)
                            (local-set-key (salih/mode "c e") #'+default/diagnostics)
                            (local-set-key (salih/mode "c g") #'salih/find-definition-or-lookup)
                            (local-set-key (salih/mode ";")   #'salih/rename-or-iedit)))

(add-hook 'flycheck-mode-hook (lambda ()
                                (local-set-key (salih/mode "e l") #'flycheck-list-errors)))

(add-hook 'nov-mode-hook (lambda ()
                           (local-set-key (salih/mode "t") #'gts-do-translate)))


(add-hook 'pdf-view-mode-hook (lambda ()
                                (local-set-key (salih/mode "t") #'gts-do-translate)
                                (evil-local-set-key 'normal (kbd "J") #' pdf-view-next-page-command)
                                (evil-local-set-key 'normal (kbd "K") #' pdf-view-previous-page-command)))


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
(add-hook 'dired-mode-hook
          (lambda () (local-set-key (salih/mode "C-c") 'salih/open-in-external-app)
            (local-set-key (salih/mode "C-e") 'salih/epa-dired-do-encrypt)
            (local-set-key (salih/mode "C-d") 'epa-dired-do-decrypt)))


;; Org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (salih/mode "C-f") #'org-footnote-action)
            (local-set-key (salih/mode "C-i") #'org-id-get-create)
            (local-set-key (salih/mode "i l") #'org-web-tools-insert-link-for-url)
            (local-set-key (salih/mode "i d") #'org-download-clipboard)
            (local-set-key (salih/mode "i k") #'citar-insert-citation)
            (local-set-key (salih/mode "i n") #'orb-insert-link)
            (local-set-key (salih/mode "n n") #'org-noter)
            (local-set-key (salih/mode "n k") #'org-noter-kill-session)
            (local-set-key (salih/mode "e p") #'org-pandoc-export-to-latex-pdf)
            (define-key org-mode-map (kbd "C-c /") nil)


            ;; roam
            (local-set-key (salih/mode "i r") #'org-roam-node-insert)
            (local-set-key (salih/mode "r i") #'org-roam-node-insert)
            (local-set-key (salih/mode "r t") #'org-roam-tag-add)
            (local-set-key (salih/mode "r a") #'org-roam-alias-add)
            (local-set-key (salih/mode "i b") #'orb-insert-link)
            (local-set-key (salih/mode "f b") #'consult-org-roam-backlinks)
            (local-set-key (salih/mode "f f") #'consult-org-roam-forward-links)
            (local-set-key (salih/mode "f n") #'consult-org-roam-search)

            (local-set-key (salih/global "TAB") #'consult-org-heading)))


;; ement.el
(add-hook 'ement-room-mode-hook
          (lambda ()
            (evil-local-set-key 'normal (kbd "RET") #'ement-room-send-message)
            (evil-local-set-key 'normal (kbd "C-k") #'ement-room-delete-message)
            (evil-local-set-key 'normal (kbd "s f") #'ement-room-send-file)
            (evil-local-set-key 'normal (kbd "s i") #'ement-room-send-image)
            (evil-local-set-key 'normal (kbd "q") #'quit-window)
            (evil-local-set-key 'normal (kbd "r j") #'ement-room-join)
            (evil-local-set-key 'normal (kbd "r l") #'ement-room-leave)
            (evil-local-set-key 'normal (kbd "R") #'ement-room-write-reply)
            (evil-local-set-key 'normal (kbd "E") #'ement-room-edit-message)))

(add-hook 'ement-room-list-mode-hook (lambda ()
                                       (evil-local-set-key 'normal (kbd "RET") #'ement-room-list-RET)))



;; Lisp

(eval-after-load 'sly
  `(define-key sly-mode-map (salih/mode "C-e") 'sly-eval-region))


(eval-after-load 'sly
  `(define-key sly-mode-map (salih/mode "C-f") 'sly-eval-buffer))


;; convenient
(general-define-key
 :prefix salih/prefix-global
 "c"     #'org-capture
 "a"     #'org-agenda
 "."     #'find-file
 ","     #'persp-switch-to-buffer
 "<"     #'switch-to-buffer
 "RET"   #'bookmark-jump
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
 "t" #'org-roam-dailies-goto-today)



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
 :prefix (concat salih/prefix-global)
 "e e" #'eshell)

;; projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-x p")   #'projectile-command-map)
(define-key projectile-mode-map (kbd "C-x p a") #'projectile-add-known-project)

;; convenient
(global-set-key (kbd "M-RET")      #'lsp-execute-code-action)
(global-set-key (kbd "C-M-g")      #'+lookup/definition)



(general-define-key
 :prefix salih/prefix-mode
 "i w" #'insert-char
 "s w" #'+lookup/dictionary-definition
 "C-t" #'gts-do-translate
 "C-s" #'centaur-tabs-ace-jump)

(global-set-key (salih/mode "i u") #'insert-char)
(global-set-key (salih/mode "s w") #'+lookup/dictionary-definition)
(global-set-key (salih/mode "C-t") #'gts-do-translate)

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
  (define-key org-noter-doc-mode-map (salih/mode "i i")
              #'org-noter-insert-precise-note))



(general-define-key
 :prefix (concat salih/prefix-global "m")
 "m" #'(lambda () (interactive) (mu4e~headers-jump-to-maildir "/Inbox"))
 "i" #'mu4e)



;; tabs
(define-key evil-normal-state-map (kbd "g t") 'centaur-tabs-forward)
(define-key evil-normal-state-map (kbd "g T") 'centaur-tabs-backward)
