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
(setq org-agenda-files '("/mnt/disk/leet"))
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
(setq sr-speedbar-max-width 40)
(setq imenu-list-size 43)
(setq twittering-allow-insecure-server-cert t)
(icomplete-mode)
(setq latex-run-command "pdflatex")


(defun enable-doom-modeline-icons (_frame)
  (setq doom-modeline-icon t))

(add-hook 'after-make-frame-functions
          #'enable-doom-modeline-icons)



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

(use-package org-wild-notifier
  :ensure t
  :custom
  (alert-default-style 'notifications)
  (org-wild-notifier-alert-time '(1 10 30))
  :config
  (org-wild-notifier-mode 1))



(add-hook 'neotree-mode-hook(lambda () (solaire-mode -1)))
(add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)
(require 'highlight-indent-guides)


;; hmbx


(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)


(set-face-background 'highlight-indent-guides-odd-face "#2f3337")
(set-face-background 'highlight-indent-guides-even-face "#2f3337")
(set-face-foreground 'highlight-indent-guides-character-face "#2f3337")

;; leavun



;; (set-face-background 'highlight-indent-guides-odd-face "#9B9C97")
;; (set-face-background 'highlight-indent-guides-even-face "#9B9C97")
;; (set-face-foreground 'highlight-indent-guides-character-face "#9B9C97")
