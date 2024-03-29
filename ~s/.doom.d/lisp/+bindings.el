;;; ../configs/.doom.d/lisp/+bindings.el -*- lexical-binding: t; -*-


;; unbinding
(define-key org-mode-map (kbd "C-c C-f") nil)
(global-unset-key        (kbd "C-f"))
(define-key org-mode-map (salih/mode "]") nil)
(define-key org-mode-map (salih/mode "[") nil)
(general-auto-unbind-keys)





(define-key flyspell-mode-map (kbd "C-;") nil)
(define-key evil-visual-state-map       (kbd "C-g") #'evil-normal-state)
(define-key evil-insert-state-map       (kbd "C-g") #'evil-normal-state)
(define-key evil-replace-state-map      (kbd "C-g") #'evil-normal-state)
(define-key evil-operator-state-map     (kbd "C-g") #'evil-normal-state)
(define-key evil-insert-state-map       (salih/global "C-s") #'save-buffer)
(define-key evil-normal-state-map       (kbd "C-g") (lambda ()
                                                      (interactive)
                                                      (evil-normal-state)
                                                      (evil-ex-nohighlight)))


(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-g") #'salih/evil-escape-and-abort-company)
  (define-key company-search-map (kbd "C-g") #'salih/evil-escape-and-abort-company))



(general-define-key
 :keymaps 'prog-mode-map
 :prefix salih/prefix-mode
 "C-c C-d" #'+lookup/definition
 "C-c C-r" #'+lookup/references
 "C-c C-t" #'+lookup/type-definition
 "C-c C-e" #'+default/diagnostics
 "C-c C-g" #'salih/find-definition-or-lookup
 "C-;"   #'salih/rename-or-iedit)


(general-define-key
 :keymaps 'flycheck-mode-map
 :prefix salih/prefix-mode
 "C-e C-l" #'flycheck-list-errors
 "C-e C-e" #'flycheck-list-errors)


(general-define-key
 :keymaps 'nov-mode-map
 :prefix salih/prefix-mode
 "C-t" #'gts-do-translate)



(add-hook 'pdf-view-mode-hook (lambda ()
                                (define-key pdf-view-mode-map (salih/mode "C-c") #'org-noter-insert-precise-note)
                                (define-key pdf-view-mode-map (salih/mode "C-d") #'pdf-view-themed-minor-mode)
                                (evil-local-set-key 'normal (salih/mode "C-c") #'org-noter-insert-precise-note)
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
          (lambda ()
            (gomacro-mode)
            (local-set-key (salih/mode "C-c") #'salih/compile-and-run-go-project)
            (local-set-key (salih/mode "C-v") #'gomacro-eval-region)
            (local-set-key (salih/mode "C-f") #'gomacro-eval-file)
            (local-set-key (salih/mode "C-b") #'gomacro-eval-buffer)
            (local-set-key (kbd "<f2>") #'salih/compile-and-run-go-project)))


;; Dired
(general-define-key
 :keymaps 'dired-mode-map
 :prefix salih/prefix-mode
 "C-c" #'salih/open-in-external-app
 "C-e" #'salih/epa-dired-do-encrypt
 "C-d" #'epa-dired-do-decrypt)


(general-auto-unbind-keys)
;; Org-mode
(map!
 :map org-mode-map
 :after org
 :prefix salih/prefix-mode
 "C-f"     #'org-footnote-action
 "c i"     #'org-clock-in
 "c o"     #'org-clock-out
 "H-i H-i" #'org-id-get-create
 "H-i C-l" #'org-web-tools-insert-link-for-url
 "H-i C-d" #'org-download-clipboard
 "H-i C-c" #'salih/org-id-get-create-with-custom-id
 "H-i C-k" #'citar-insert-citation
 "H-i C-t" #'org-inlinetask-insert-task
 "C-b"     #'citar-insert-citation
 "H-i C-b" #'orb-insert-link
 "C-n C-n" #'org-noter
 "C-n C-k" #'org-noter-kill-session
 "C-e"     nil
 "C-e C-p" #'org-pandoc-export-to-latex-pdf
 "C-e C-t" #'salih/get-file-todos
 "H-i C-r" #'salih/org-roam-node-insert
 "C-r"     nil
 "C-r H-i" #'org-roam-node-insert
 "C-r C-t" #'org-roam-tag-add
 "C-r C-a" #'org-roam-alias-add
 "C-r C-b" #'consult-org-roam-backlinks
 "C-;"     #'salih/rename-or-iedit
 "C-r C-f" #'consult-org-roam-forward-links)

(general-auto-unbind-keys)

;; Lisp

(eval-after-load 'sly
  `(define-key sly-mode-map (salih/mode "C-e") 'sly-eval-region))


(eval-after-load 'sly
  `(define-key sly-mode-map (salih/mode "C-f") 'sly-eval-buffer))

(eval-after-load 'sly
  `(define-key sly-mode-map (salih/mode "C-j") 'salih/sly-compile-defun-with-print))


;; (global-set-key (salih/global "C-a") #'org-agenda)

(add-hook 'xwidget-webkit-mode-hook (lambda ()
                                      (evil-define-key 'nomral xwidget-webkit-mode-map (kbd "O") 'salih/elfeed-open-url)
                                      (evil-collection-define-key 'normal 'xwidget-webkit-mode-map "y" 'xwidget-webkit-copy-selection-as-kill)
                                      (evil-collection-define-key 'normal 'xwidget-webkit-mode-map "C" 'salih/open-current-url-in-chrome)
                                      (evil-collection-define-key 'normal 'xwidget-webkit-mode-map "c" 'xwidget-webkit-current-url)
                                      (evil-collection-define-key 'normal 'xwidget-webkit-mode-map "SPC" 'xwidget-webkit-scroll-up)))


;; convenient
(map!
 :prefix salih/prefix-global
 "C-c"          (lambda () (interactive) (org-capture nil "f"))
 "C-a"          nil
 "C-a C-a"      (lambda () (interactive (org-agenda nil "f")))

 "C-a C-v"      #'salih/open-agenda
 "C-."          #'find-file
 "."            #'find-file
 "C-,"          #'persp-switch-to-buffer
 ","            #'persp-switch-to-buffer
 "C-<"          #'switch-to-buffer
 "<"            #'switch-to-buffer
 "RET"          #'switch-to-buffer
 "C-<return>"   #'switch-to-buffer
 "["            #'previous-buffer
 "]"            #'next-buffer
 "C-d"          #'calendar
 "C-k"          #'kill-current-buffer
 "C-l"          nil
 "C-l C-l"      #'leetcode
 "C-r"          nil
 "C-r C-r"      #'doom/sudo-this-file
 "TAB"          nil
 "TAB d"        #'+workspace/delete
 "SPC"          #'projectile-find-file
 "H-i"          #'(lambda ()
                    (interactive)
                    (if (featurep 'mu4e)
                        (mu4e~headers-jump-to-maildir "/Inbox")
                      (mu4e)))
 "/"            #'swiper)

;; file keys
(general-define-key
 :prefix (concat salih/prefix-global "C-f")
 "" nil
 "C-r" #'recentf-open-files
 "C-g" #'magit-find-file
 "C-l" #'projectile-find-file)


;; search global
(map!
 :prefix "C-s"
 :map 'override
 "C-d" #'+default/search-cwd
 "C-w" #'+lookup/dictionary-definition
 "C-b" #'+default/search-buffer
 "C-p" #'+default/search-project
 "C-g" #'rgrep
 "C-r" #'consult-org-roam-search)



;; notes
(general-define-key
 :prefix (concat salih/prefix-global "C-n")
 "" nil
 "C-f" #'citar-open-notes
 "C-b" #'citar-open-notes
 "C-o" #'salih/open-book)

;; roam
(general-define-key
 :prefix (concat salih/prefix-global "C-r")
 ""  nil
 "C-b"  #'org-roam-buffer-toggle
 "c"    #'org-roam-capture
 "C-f"  #'org-roam-node-find
 "C-j"  #'org-roam-dailies-capture-today
 "C-t"  #'org-roam-dailies-goto-today)



;; magit and vc
;; TODO refactor if possible
(general-define-key
 :prefix (concat salih/prefix-global "g")
 "" nil
 "g"   #'magit-status
 "G"   #'magit-status-here
 "C"   #'magit-clone
 "L"   #'magit-log-buffer-file
 "."   #'+vc/browse-at-remote
 "t"   #'magit-todos-list
 "D"   #'magit-file-delete)




;; other
(general-define-key
 :prefix (concat salih/prefix-global "C-e")
 "" nil
 "C-e" #'eshell
 "C-f" #'elfeed)

;; projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-x p")   #'projectile-command-map)
(define-key projectile-mode-map (kbd "C-x p a") #'projectile-add-known-project)

;; convenient

(global-set-key (kbd "C-M-g")      #'+lookup/definition)


(map!
 :prefix salih/prefix-mode
 "H-i C-u" #'insert-char
 "C-s" nil
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
 :prefix salih/prefix-mode
 :keymaps 'elfeed-search-mode-map
 "C-u" #'elfeed-update)

(evil-define-key 'normal elfeed-show-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "c") 'salih/elfeed-copy-url
  (kbd "O") 'salih/elfeed-open-url
  (kbd "C") 'salih/elfeed-open-url-in-chrome
  (kbd "K") 'elfeed-goodies/split-show-prev)
(evil-define-key 'normal elfeed-search-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)

(evil-define-key 'normal calendar-mode-map (kbd "RET") 'salih/org-calendar-goto-agenda)

(add-hook 'nov-mode-hook (lambda ()
                           (evil-collection-define-key 'normal 'nov-mode-map "t"  nil)
                           (evil-collection-define-key 'normal 'nov-mode-map "h"  nil)
                           (define-key nov-mode-map        (kbd "l")              nil)
                           (define-key nov-button-map      (kbd "l")              nil)
                           (define-key shr-map             (kbd "u")              nil)
                           (define-key shr-map             (kbd "w")              nil)))

(provide '+bindings)
