;;; packconf.el -*- lexical-binding: t; -*-

(provide 'packconf)


;; hugo
(setq easy-hugo-basedir "~/blog/")
(setq HUGO_BASE_DIR "~/blog/")
(setq org-hugo-base-dir "~/blog/")
(setq easy-hugo-url "/")
(setq easy-hugo-default-ext ".org")
(setq org-startup-folded t)



(use-package! awqat
  :commands (awqat-display-prayer-time-mode
             awqat-times-for-day)
  :config
  (setq calendar-latitude 30.0
        calendar-longitude 31.2
        awqat-mode-line-format " 🕌 ${prayer} (${hours}h${minutes}m) "))
(awqat-display-prayer-time-mode)
(global-wakatime-mode)

(setq neo-smart-open t)
(setq neo-theme (if (display-graphic-p) 'icons 'icons))
(setq doom-themes-neotree-file-icons 'nil)


;; jorunal

(setq org-journal-date-format "%A, %d %B %Y")
(setq org-journal-file-format "%Y%m%d.org")
(setq org-journal-enable-agenda-integration t)
(setq org-directory "/mnt/disk/leet")
(setq org-agenda-files '("~/leet/"))


(setq org-journal-dir "/mnt/disk/leet/journal")
(add-to-list 'org-agenda-files org-journal-dir)



;; leeet


(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/roam"))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))
    (defun cm/deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
      (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
	(if begin
	    (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
	  (deft-base-filename file))))

    (advice-add 'deft-parse-title :override #'cm/deft-parse-title)

    (setq deft-strip-summary-regexp
	  (concat "\\("
		  "[\n\t]" ;; blank
		  "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
		  "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
		  "\\)"))











(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))



;; leetcode

(setq leetcode-prefer-language "cpp")
(setq leetcode-prefer-sql "mysql")
(setq leetcode-save-solutions t)
(setq leetcode-directory "/home/ghd/leet/ps/lc")




(setq pdf-view-midnight-colors '("#ABB2BF" . "#282C35"))
(setq sr-speedbar-right-side nil)
(setq speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'")
;; (setq sr-speedbar-max-width 40)
(setq imenu-list-size 43)
(setq twittering-allow-insecure-server-cert t)
(icomplete-mode)
(setq latex-run-command "pdflatex")





(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new
         (file+head "main/${slug}.org" "#+title: ${title}\n#+filetags: \n- tags :: ")
         :immediate-finish t
         :unnarrowed t)
        ("p" "People" plain
         "%?"
         :if-new
         (file+head "main/${slug}.org" "#+title: ${title}\n#+filetags: People\n- tags :: ")
         :immediate-finish t
         :unnarrowed t)
        ("s" "saved" plain "%?"
         :if-new
         (file+head "saved/${slug}.org" "#+title: ${title}\n#+filetags: \n- tags :: [[roam:saved things]]")
         :immediate-finish t
         :unnarrowed t)
        ("c" "contemplations" plain "%?"
         :if-new
         (file+head "contemplations/${slug}.org" "#+title: ${title}\n#+filetags: \n- tags :: [[roam:Contemplation]]")
         :immediate-finish t
         :unnarrowed t)
        ("q" "quotes" plain "%?"
         :if-new
         (file+head "quotes/${slug}.org" "#+title: ${title}\n#+filetags: \n- tags :: [[roam:Quotes]]")
         :immediate-finish t
         :unnarrowed t)
        ("l" "literature" plain "%?"
         :if-new
         (file+head "literature/${slug}.org" "#+title: ${title}\n#+filetags: \n")
         :immediate-finish t
         :unnarrowed t)
        ("h" "history" plain "%?"
         :if-new
         (file+head "everything/${slug}.org" "#+title: ${title}\n#+filetags: History \n- tags :: ")
         :immediate-finish t
         :unnarrowed t)

        ("k" "knowledge" plain "%?"
         :if-new
         (file+head "everything/${slug}.org" "#+title: ${title}\n#+filetags: \n- tags :: ")
         :immediate-finish t
         :unnarrowed t)))



(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))

(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))




(use-package org-upcoming-modeline
  :after org
  :load-path "~/gits/org-upcoming-modeline"
  :config
  (org-upcoming-modeline-mode))

;; (use-package org-wild-notifier
;;   :ensure t
;;   :custom
;;   (alert-default-style 'notifications)
;;   (org-wild-notifier-alert-time '(1 10 30))
;;   :config
;;   (org-wild-notifier-mode 1))



(add-hook 'neotree-mode-hook(lambda () (solaire-mode -1)))
;; (add-hook 'doom-modeline-hook (lambda () (solitaire-mode -1)))
(add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)
(require 'highlight-indent-guides)


;; hmbx


(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)


;; (set-face-background 'highlight-indent-guides-odd-face "#2f3337")
;; (set-face-background 'highlight-indent-guides-even-face "#2f3337")
;; (set-face-foreground 'highlight-indent-guides-character-face "#2f3337")

;; leavun



(set-face-background 'highlight-indent-guides-odd-face "#9B9C97")
(set-face-background 'highlight-indent-guides-even-face "#9B9C97")
(set-face-foreground 'highlight-indent-guides-character-face "#9B9C97")

(use-package! org-wild-notifier
  ;; :hook (with-eval-after-load . org-wild-notifier-mode)
  :config
  (setq org-wild-notifier-alert-time '(60 30 5 1)))

(setq alert-default-style 'libnotify)
(org-wild-notifier-mode)

;; (use-package mu4e-views
;;   :after mu4e
;;   :defer nil
;;   :bind (:map mu4e-headers-mode-map
;; 	    ("v" . mu4e-views-mu4e-select-view-msg-method) ;; select viewing method
;; 	    ("M-d" . mu4e-views-cursor-msg-view-window-down) ;; from headers window scroll the email view
;; 	    ("M-u" . mu4e-views-cursor-msg-view-window-up) ;; from headers window scroll the email view
;;         ("f" . mu4e-views-toggle-auto-view-selected-message) ;; toggle opening messages automatically when moving in the headers view
;;         ("i" . mu4e-views-mu4e-view-as-nonblocked-html) ;; show currently selected email with all remote content
;; 	    )
;;   :config
;;   (setq mu4e-views-completion-method 'ivy) ;; use ivy for completion
;;   (setq mu4e-views-default-view-method "html") ;; make xwidgets default
;;   (mu4e-views-mu4e-use-view-msg-method "html") ;; select the default
;;   (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window) ;; when pressing n and p stay in the current window
;;   (setq mu4e-views-auto-view-selected-message t)) ;; automatically open messages when moving in the headers view



(define-minor-mode lordpretzel-elfeed-xwidgets-mode
  "Minor mode for setting up keys when viewing elfeed entry in xwidgets."
  :init-value nil
  :lighter "elfeed-browsing"
  :keymap
  `((,(kbd "q")
     . lordpretzel/elfeed-search-window-only))
  :global nil)

;; register minor mode with xwidgets-reuse to turn it on or off
(xwidgets-reuse-register-minor-mode 'lordpretzel-elfeed-xwidgets-mode)

(defun lordpretzel/elfeed-search-window-only ()
  "Show only the search window of elfeed."
  (interactive)
  (switch-to-buffer (elfeed-search-buffer))
  (delete-other-windows)
  )

(defun lordpretzel/elfeed-open-entry-in-xwidgets
    (entry)
  (interactive
   (list
    (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (when
      (elfeed-entry-p entry)
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (forward-line)
    (let
        ((link
          (elfeed-entry-link entry)))
      (when link
        (let
            ((window
              (selected-window))
             newwindow)
          (delete-other-windows)
          (setq newwindow
                (split-window-right))
          (select-window newwindow)
          (webkit-browse-url link)
          (select-window window))))))
;; (add-hook 'elfeed-search-mode-hook (lambda () (local-set-key (kbd "<RET>") #'lordpretzel/elfeed-open-entry-in-xwidgets)))



(with-eval-after-load "elfeed-search"
  (define-key elfeed-search-mode-map (kbd "<RET>") 'lordpretzel/elfeed-open-entry-in-xwidgets))


(require 'webkit-ace) ;; If you want link hinting
(use-package evil-collection-webkit
  :config
  (evil-collection-xwidget-setup))
(use-package ox-reveal
:ensure ox-reveal)

(setq org-reveal-root "/home/ghd/me/other/rv/reveal.js-master/")
(setq org-reveal-mathjax t)

(defun my/center (width)
  (interactive "nBuffer width: ")
  (let* ((adj          (- (window-text-width)
                          width))
         (total-margin (+ adj
                          left-margin-width
                          right-margin-width)))
    (setq left-margin-width  (/ total-margin 2))
    (setq right-margin-width (- total-margin left-margin-width)))
  (set-window-buffer (selected-window) (current-buffer)))



(defun startpresent ()

  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)

  (setq header-line-format " ")
  (setq header-line-format nil)
  (display-line-numbers-mode 0)





  (require 'org-faces)

  ;; Hide emphasis markers on formatted text
  (setq org-hide-emphasis-markers t)

  ;; Resize Org headings
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))

  ;; Make the document title a bit bigger
  (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.3)

  ;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)





  (set-face-attribute 'default nil :font "JetBrains Mono" :weight 'light :height 110)
  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'light :height 100)
  (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'light :height 1.3)


  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (my/center 110)
  (vi-tilde-fringe-mode 0)
  ;; (centered-window-mode)
  ;; (visual-fill-column-mode 1)
  ) ;; (visual-line-mode 1)

(add-hook 'org-present-mode-hook 'startpresent)



(defun endpresent ()
  (interactive)
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (org-present-end)
  (visual-fill-column-mode 0)
  (visual-line-mode 0))
