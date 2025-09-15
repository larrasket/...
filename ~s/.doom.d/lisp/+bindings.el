;;; ../configs/.doom.d/lisp/+bindings.el -*- lexical-binding: t; -*-
(define-key evil-motion-state-map "-" 'er/expand-region)
(define-key evil-motion-state-map (kbd "H-i") 'evil-jump-backward)
(define-key evil-motion-state-map (kbd "C-o") 'evil-jump-forward)



(map!
 :leader
 :prefix "m"
 "m"    #'magit-status
 "c"    #'magit-checkout
 "l"    #'magit-log-buffer-file
 "d"    #'magit-file-delete)



(evil-local-set-key 'insert (kbd "C-SPC") nil)

;; TODO refactor `evil-define-key`-like code to use either `general-define-key`
;; or `map!`.

;; unbinding

(general-auto-unbind-keys)


(map!
 :i
 "C-x C-s" #'save-buffer)

(after! flyspell
  (define-key flyspell-mode-map (kbd "C-;") nil))


(map!
 :mode wordnut-mode
 :n
 "q" #'+workspace/close-window-or-workspace)

;; ;; abusing evil mode
(map! :i "M-v" #'yank)
(map! :i "C-v" #'yank)


(map!
 :map eww-mode-map
 "C" #'eww-browse-with-external-browser
 "SPC c c" #' salih/eww-org-store-and-capture)


(map!
 :leader
 "c r" #'salih/rename-or-iedit)

(map!
 :leader
 :v
 "w t"    #'gt-translate)


(general-define-key
 :keymaps 'pdf-view-mode-map
 :prefix  "SPC n"
 "i"    #'org-noter-insert-precise-note
 "o"    #'salih/zathura-open
 "d"    #'pdf-view-themed-minor-mode)

(map!
 :map pdf-occur-buffer-mode-map
 :n
 "RET" #'salih/pdf-occure)

(with-eval-after-load 'pdf-view
 (evil-define-key 'normal pdf-view-mode-map (kbd "<right>") #'ignore)
 (evil-define-key 'motion pdf-view-mode-map (kbd "<right>") #'ignore)
 (evil-define-key 'nroaml pdf-view-mode-map (kbd "<left>") #'ignore)
 (evil-define-key 'motion pdf-view-mode-map (kbd "<left>") #'ignore))

(add-hook! 'pdf-view-mode-hook
  (evil-local-set-key 'normal (kbd "J") #'pdf-view-next-page-command)
  (evil-local-set-key 'normal (kbd "<right>") nil)
  (evil-local-set-key 'normal (kbd "<left>") nil)
  (evil-local-set-key 'motion (kbd "<right>") nil)
  (evil-local-set-key 'motion (kbd "<left>") nil)
  (evil-local-set-key 'normal (kbd "K") #'pdf-view-previous-page-command))

(general-define-key
 :keymaps 'lsp-mode-map
 "M-RET" #'lsp-execute-code-action)


;; ;; TODO Refactor this. I think this should be rewritten as an only one function
;; ;; that is "Run file", which check the mode and map it to the matching running
;; ;; method.
;; [2025-09-13 Sat 02:04] RIP.

;; (general-define-key
;;  :keymaps 'c++-mode-map
;;  :prefix salih/prefix-mode
;;  "C-c" #'salih/compile-and-run-cpp)

;; ;; Dired
(map!
 :map 'dired-mode-map
 :leader
 "o o" #'salih/open-in-external-app)


(map!
 :leader
 "[" #'previous-buffer
 "]" #'next-buffer)


(map!
 :leader
 "e e" #'eshell)

;; ;; Org-mode
(map!
 :map org-mode-map
 :after org
 :leader
 "c i"     #'org-clock-in
 "c o"     #'org-clock-out
 "i i"     #'salih/org-id-get-create-with-custom-id
 "i c"     #'citar-insert-citation
 "i b"     #'orb-insert-link
 "b b"     #'org-noter
 "b k"     #'org-noter-kill-session
 "b o"     #'salih/org-noter-open-in-zathura
 "r i"     #'org-roam-node-insert
 "r t"     #'org-roam-tag-add
 "r f"     #'org-fc-type-normal-init
 "r l f"   #'consult-org-roam-backlinks
 "r l f"   #'consult-org-roam-forward-links
 "C-;"     #'salih/rename-or-iedit)

(setq  doom-leader-alt-key  "M-m")




;; ;; convenient
(map!
 :leader
 "0"            #'+workspace/close-window-or-workspace
 "t t"          #'salih/vterm
 "o a"          #'salih/org-agenda-no-full-f
 "o l"          #'salih/org-agenda-no-full-l
 "o f"          #'salih/org-agenda-full-f
 "o v"          #'salih/open-agenda
 "f f"          #'salih/read-feeds
 "o c"          #'calendar
 "n z"          #'salih/open-book
 "TAB d"        #'+workspace:delete
 "/"            #'swiper
 "f p"          #'projectile-switch-project
 "l e"          #'salih/list-errors)

(map!
 :map doom-leader-map
 "RET" #'consult-buffer)

;; ;; files and roam
(map!
 :map 'override
 :leader
 "r f"  #'org-roam-node-find
 "r c"  #'salih/org-roam-capture-fleet
 "r j"  #'salih/org-roam-dailies-capture-today
 "r b"  #'salih/org-roam-buffer)


;; ;; notes
;; (general-define-key
;;  :states 'normal
;;  :keymaps 'override
;;  "C-n C-f"     #'citar-open-notes
;;  "C-n C-b"     #'citar-open-notes
;;  "C-n C-o"     #'salih/open-book
;;  "C-n C-c C-o" #'salih/open-book-zathura)


;; (general-define-key
;;  :keymaps 'override
;;  "M-n" #'make-frame)



(global-set-key (kbd "C-M-g")      #'+lookup/definition)



;; resize windows
(global-set-key (kbd "M-<left>")  #'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") #'enlarge-window-horizontally)
(global-set-key (kbd "M-<down>")  #'enlarge-window)
(global-set-key (kbd "M-<up>")    #'shrink-window)


;; commenting lines
(global-set-key (kbd "M-;") 'salih/comment-or-uncomment-region-or-line)

;; ;; AuthorDate: Sat Jan 13 03:09:16 2024 +0200
;; ;; this was one of my first written elisp. I will comment it only for historical
;; ;; reasons :)

;; ;; (global-set-key [f6]
;; ;;                 (lambda ()
;; ;;                   (interactive)
;; ;;                   (neotree-project-dir)
;; ;;                   (lsp-treemacs-symbols)))

;; ;; [2024-10-24 Thu 22:29] I miss this time.
;; ;; [2025-08-01 Fri 21:18] refactoring my config rn. A lot of time passed. :)-:(






(evil-define-key 'normal calendar-mode-map (kbd "RET")
  'salih/org-calendar-goto-agenda)



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
 :map mu4e-headers-mode-map
 :n
 "r" #'mu4e-headers-mark-for-refile)

(map!
 :map mu4e-view-mode-map
 :n
 ";"       #'salih/mu4e-go-to-url
 "C-c C-b" #'salih/mu4e-view-and-copy-html
 "C-c C-c" #'salih/mu4e-org-store-and-capture)


(map!
 :map text-mode-map
 :n
 ";" #'embark-act)


(evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-flip-mode
  (kbd "r") 'org-fc-review-flip
  (kbd "n") 'org-fc-review-flip
  (kbd "s") 'org-fc-review-suspend-card
  (kbd "q") 'org-fc-review-quit)

(evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-rate-mode
  (kbd "a") 'org-fc-review-rate-again
  (kbd "h") 'org-fc-review-rate-hard
  (kbd "g") 'org-fc-review-rate-good
  (kbd "e") 'org-fc-review-rate-easy
  (kbd "s") 'org-fc-review-suspend-card
  (kbd "q") 'org-fc-review-quit)

(map!
 :map flyspell-mouse-map
 "RET"    'jinx-correct
 [return] 'jinx-correct
 [mouse-1] #'flyspell-correct-at-point)

(map!
 :map jinx-overlay-map
 :nvim
 "RET"    'jinx-correct
 [return] 'jinx-correct)


(map!
 :map mu4e-view-active-urls-keymap
 "RET"    'mu4e--view-browse-url-from-binding
 [return] 'mu4e--view-browse-url-from-binding)

(provide '+bindings)
