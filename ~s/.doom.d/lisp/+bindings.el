;;; ../configs/.doom.d/lisp/+bindings.el -*- lexical-binding: t; -*-

;; TODO refactor `evil-define-key`-like code to use either `general-define-key`
;; or `map!`.

;; unbinding
(define-key org-mode-map (kbd "C-c C-f") nil)
(define-key org-mode-map (kbd "C-c C-j") nil)
(global-unset-key        (kbd "C-f"))
(define-key org-mode-map (salih/mode "]") nil)
(define-key org-mode-map (salih/mode "[") nil)
(general-auto-unbind-keys)



(after! flyspell
  (define-key flyspell-mode-map (kbd "C-;") nil))



(define-key evil-normal-state-map       (kbd "g w") #'evil-avy-goto-char-2)
(define-key evil-insert-state-map       (salih/global "C-s") #'save-buffer)

(map!
 :mode wordnut-mode
 :n
 "q" #'+workspace/close-window-or-workspace)

;; abusing evil mode
(map! :i "C-v" #'yank)
(define-key evil-normal-state-map        (kbd "]") #'centaur-tabs-forward)
(define-key evil-normal-state-map        (kbd "[") #'centaur-tabs-backward)
(define-key evil-visual-state-map        (kbd "]") #'centaur-tabs-forward)
(define-key evil-visual-state-map        (kbd "[") #'centaur-tabs-backward)


(map!
 :map eww-mode-map
 :n
 "C" #'eww-browse-with-external-browser)


(general-define-key
 :keymaps 'prog-mode-map
 :prefix  salih/prefix-mode
 "C-d"    #'+lookup/definition
 "C-r"    #'+lookup/references
 "C-t"    #'+lookup/type-definition
 "C-e"    #'+default/diagnostics
 "C-;"    #'salih/rename-or-iedit)

(general-define-key
 :keymaps 'projectile-mode-map
 :prefix  salih/prefix-mode
 "C-o"    (lambda () (interactive) (org-capture nil "p")))



(general-define-key
 :keymaps 'nov-mode-map
 :prefix  salih/prefix-mode
 "C-t"    #'gts-do-translate)


(general-define-key
 :keymaps 'pdf-view-mode-map
 :prefix  salih/prefix-mode
 "C-c"    #'org-noter-insert-precise-note
 "H-i"    #'org-noter-insert-note
 "C-f"    #'salih/zathura-open
 "C-d"    #'pdf-view-themed-minor-mode)




(general-define-key
 :keymaps 'pdf-view-mode-map
 :prefix  salih/prefix-mode
 "C-c"    #'org-noter-insert-precise-note
 "H-i"    #'org-noter-insert-note
 "C-f"    #'salih/zathura-open
 "C-d"    #'pdf-view-themed-minor-mode)


(with-eval-after-load 'pdf-view
  (evil-define-key 'normal pdf-view-mode-map (kbd "<right>") #'ignore)
  (evil-define-key 'motion pdf-view-mode-map (kbd "<right>") #'ignore)
  (evil-define-key 'nroaml pdf-view-mode-map (kbd "<left>") #'ignore)
  (evil-define-key 'motion pdf-view-mode-map (kbd "<left>") #'ignore))

(add-hook! 'pdf-view-mode-hook
  (evil-local-set-key 'normal (salih/mode "C-c") #'org-noter-insert-precise-note)
  (evil-local-set-key 'normal (kbd "J") #'pdf-view-next-page-command)
  (evil-local-set-key 'normal (kbd "<right>") nil)
  (evil-local-set-key 'normal (kbd "<left>") nil)
  (evil-local-set-key 'motion (kbd "<right>") nil)
  (evil-local-set-key 'motion (kbd "<left>") nil)
  (evil-local-set-key 'normal (kbd "K") #'pdf-view-previous-page-command))



(general-define-key
 :keymaps 'lsp-mode-map
 "M-RET" #'lsp-execute-code-action)


;; Run project

;; TODO Refactor this. I think this should be rewritten as an only one function
;; that is "Run file", which check the mode and map it to the matching running
;; method.

(general-define-key
 :keymaps 'c++-mode-map
 :prefix salih/prefix-mode
 "C-c" #'salih/compile-and-run-cpp)

(general-define-key
 :keymaps 'csharp-mode-map
 :prefix salih/prefix-mode
 "C-c" #'salih/compile-and-run-csharp)

(general-define-key
 :keymaps 'go-mode-map
 :prefix salih/prefix-mode
 "C-c"  #'salih/compile-and-run-go-project
 "<f2>" #'salih/compile-and-run-go-project)

;; Dired
(general-define-key
 :keymaps 'dired-mode-map
 :prefix salih/prefix-mode
 "F"   #'magit-pull
 "C-c" #'salih/open-in-external-app
 "C-e" #'salih/epa-dired-do-encrypt
 "C-s" #'salih/dired-sort
 "C-d" #'epa-dired-do-decrypt)

(evil-define-key 'nomral dired-mode-map (kbd "F") 'magit-pull)
(evil-define-key 'motion dired-mode-map (kbd "F") 'magit-pull)

(after! cc-mode
  (setq c-mode-map (make-sparse-keymap))
  (map!
   :map c-mode-map
   :prefix salih/prefix-mode
   "C-c"  #'salih/make-c
   "C-b"  #'salih/compile-and-run-c))


;; Org-mode
(map!
 :map org-mode-map
 :after org
 :prefix salih/prefix-mode
 "["       #'previous-buffer
 "]"       #'next-buffer
 "C-f"     #'org-footnote-action
 "c i"     #'org-clock-in
 "c o"     #'org-clock-out
 "H-m"     #'org-media-note-hydra/body
 "H-i H-i" #'org-id-get-create
 "H-i C-l" #'org-web-tools-insert-link-for-url
 "H-i C-c" #'salih/org-id-get-create-with-custom-id
 "H-i C-k" #'citar-insert-citation
 "H-i C-t" #'org-inlinetask-insert-task
 "C-b"     #'citar-insert-citation
 "H-i C-b" #'orb-insert-link
 "C-n C-n" #'org-noter
 "C-n C-k" #'org-noter-kill-session
 "C-e"     nil
 "C-e C-p" #'org-pandoc-export-to-latex-pdf
 "C-r"     nil
 "C-r H-i" #'org-roam-node-insert
 "C-r C-t" #'org-roam-tag-add
 "C-r C-a" #'org-roam-alias-add
 "C-r C-b" #'consult-org-roam-backlinks
 "C-;"     #'salih/rename-or-iedit
 "C-r C-f" #'consult-org-roam-forward-links)


(map!
 :map org-mode-map
 :after org
 :i
 "H-i H-i" #'org-id-get-create
 "H-i C-b" #'orb-insert-link
 "H-i C-l" #'org-web-tools-insert-link-for-url
 "H-i C-c" #'salih/org-id-get-create-with-custom-id
 "H-i C-k" #'citar-insert-citation
 "H-i C-t" #'org-inlinetask-insert-task

 "H-i C-r" #'org-roam-node-insert
 "C-c C-d" #'org-download-clipboard
 "C-r C-t" #'org-roam-tag-add
 "C-r C-a" #'org-roam-alias-add)


(map!
 :map org-agenda-mode-map
 "C-=" #'text-scale-increase
 "C--" #'text-scale-decrease
 "C-+" #'doom/reset-font-size)



;; Lisp

(eval-after-load 'sly
  `(define-key sly-mode-map (salih/mode "C-e") 'sly-eval-region))

(eval-after-load 'sly
  `(define-key sly-mode-map (salih/mode "C-f") 'sly-eval-buffer))

(eval-after-load 'sly
  `(define-key sly-mode-map (salih/mode "C-j") 'salih/sly-compile-defun-with-print))

(add-hook 'xwidget-webkit-mode-hook (lambda ()
                                      (evil-collection-define-key 'normal 'xwidget-webkit-mode-map "y" 'xwidget-webkit-copy-selection-as-kill)
                                      (evil-collection-define-key 'normal 'xwidget-webkit-mode-map "C" 'salih/open-current-url-in-chrome)
                                      (evil-collection-define-key 'normal 'xwidget-webkit-mode-map "c" 'xwidget-webkit-current-url)
                                      (evil-collection-define-key 'normal 'xwidget-webkit-mode-map "SPC" 'xwidget-webkit-scroll-up)))


;; convenient
(defun salih/set-convenient-keys ()
  (map!
   :prefix salih/prefix-global
   "C-u"          nil
   "C-n"          nil
   "C-t"          #'+vterm/here
   "C-j"          #'centaur-tabs-ace-jump
   "C-c"          (lambda () (interactive) (org-capture nil "f"))
   "C-a"          nil
   "C-a C-a"      (lambda () (interactive (org-agenda nil "f")))
   "C-a C-l"      (lambda () (interactive (org-agenda nil "l")))
   "C-a C-v"      #'salih/open-agenda
   "C-f"          #'salih/open-rss
   "C-e"          #'salih/eshell
   "C-."          #'find-file
   "."            #'find-file
   "C-,"          #'persp-switch-to-buffer
   ","            #'persp-switch-to-buffer
   "C-<"          #'switch-to-buffer
   "<"            #'switch-to-buffer
   "H-m"          #'magit-status
   "RET"          #'switch-to-buffer
   "C-<return>"   #'switch-to-buffer
   "["            #'previous-buffer
   "]"            #'next-buffer
   "C-d"          #'calendar
   "C-k"          #'kill-current-buffer
   "C-l"          nil
   "C-r"          nil
   "TAB"          nil
   "TAB d"        #'+workspace/delete
   "SPC"          #'projectile-find-file
   "C-SPC"        #'projectile-find-file
   "H-i"          #'salih/open-inbox
   "/"            #'swiper))
(salih/set-convenient-keys)

(map! :i "H-i C-d" #'salih/insert-current-date)
(map! :n ";"       #'embark-act)
(map!    "C-SPC"   #'projectile-find-file)


;; files and roam
(general-define-key
 :prefix "C-f"
 :states 'normal
 :keymaps 'override
 "C-f"  #'org-roam-node-find
 "C-p"  #'projectile-switch-project
 "C-j"  #'org-roam-dailies-capture-today
 "C-b"  #'org-roam-buffer-toggle
 "C-r"  #'recentf-open-files)

;; search global
(map!
 :prefix "C-s"
 :map 'override
 "C-d" #'+default/search-cwd
 "C-w" #'+lookup/dictionary-definition
 "C-b" #'+default/search-buffer
 "C-p" #'+default/search-project
 "<escape>" #'rgrep
 "C-r" #'consult-org-roam-search)



;; notes
(general-define-key
 :states 'normal
 :keymaps 'override
 "C-n C-f" #'citar-open-notes
 "C-n C-b" #'citar-open-notes
 "C-n C-o" #'salih/open-book)



;; magit and vc
;; TODO refactor if possible
(general-define-key
 :prefix "H-m"
 :states 'normal
 :keymaps 'override
 "" nil
 "H-m" #'magit-status
 "C-c"   #'magit-clone
 "C-l"   #'magit-log-buffer-file
 "C-d"   #'magit-file-delete)

(general-define-key
 :keymap 'magit-mode-map
 "C-c C-d" #'magit-file-delete)



(global-set-key (kbd "C-M-g")      #'+lookup/definition)
(global-set-key (kbd "<f12>")      #'keyboard-quit)

(map!
 :prefix salih/prefix-mode
 "C-k"     #'kill-current-buffer
 "C-w"     #'write-file
 "C-s"     nil
 "C-t"     #'gts-do-translate
 "]"       #'centaur-tabs-forward
 "["       #'centaur-tabs-backward
 "C-v"     #'magit-log-buffer-file)

(map!
 :i
  "H-i C-u" #'insert-char)




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
 :keymaps 'elfeed-search-mode-map
 :states 'normal
 "C-c C-u"  #'elfeed-update
 "J"    #'elfeed-goodies/split-show-next
 "m"    #'salih/elfeed-toggle-star
 "C"    #'salih/elfeed-search-open-in-chrome
 "K"    #'elfeed-goodies/split-show-prev)


(general-define-key
 :keymaps 'elfeed-show-mode-map
 :states 'normal
 "J" #'elfeed-goodies/split-show-next
 "c" #'salih/elfeed-copy-url
 "O" #'salih/elfeed-open-url
 "C" #'salih/elfeed-open-url-in-chrome
 "K" #'elfeed-goodies/split-show-prev)



(evil-define-key 'normal calendar-mode-map (kbd "RET") 'salih/org-calendar-goto-agenda)

(add-hook 'nov-mode-hook (lambda ()
                           (evil-collection-define-key 'normal 'nov-mode-map "t"  nil)
                           (evil-collection-define-key 'normal 'nov-mode-map "h"  nil)))



(general-define-key
 :keymaps 'nov-mode-map
 "l" nil)

(general-define-key
 :keymaps 'nov-button-map
 "l" nil)

(general-define-key
 :keymaps 'shr-map
 "u" nil)

(general-define-key
 :keymaps 'shr-map
 "w" nil)

(map!
 :map eshell-mode-map
 "C-c C-f" #'salih/open-kitty-in-current-directory)


(map!
 :map mu4e-headers-mode-map
 :n
 "r" #'mu4e-headers-mark-for-refile)

(map!
 :map mu4e-view-mode-map
 :n
 ";" #'salih/mu4e-go-to-url)

(define-key ctl-x-map (kbd "C-z") nil)
(define-key global-map (kbd "C-x C-z") nil)
(define-key global-map (kbd "C-x s") #'save-buffer)
(define-key evil-motion-state-map (kbd "H-i") 'evil-jump-backward)
(define-key evil-motion-state-map (kbd "C-o") 'evil-jump-forward)
(define-key evil-motion-state-map "-" 'er/expand-region)
(provide '+bindings)
