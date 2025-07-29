;;; config.el -*- lexical-binding: t; -*-
(require 'awqat)                        ; for prayer support in the agenda
(require 'vulpea)                       ; org-roam project tasks in org-agenda
(require '+early)                       ; personal utilities
(require 'go-translate)                 ; define translation engine in config.el
(require 'doom-modeline)                ; I use it for segment definition only
(require 'cocaine-line)                 ; my modeline.
(setq user-full-name                                    "Salih Muhammed"
      user-mail-address                                 "lr0@gmx.com"
      user-first-name                                   "Salih"
      user-stmp-server                                  "mail.gmx.com"
      user-stmp-port                                    587
      user-short-username                               "lr0"
      user-config-repo-path                             "~/configs/~s"
      salih/blog-content-path                           "~/blog/content"
      org-roam-directory                                (file-truename "~/roam")
      srht-username                                     user-short-username
      ;; appearance
      ;; [2024-08-06 Tue 06:33] `ef-deuteranopia-light' is amazing light theme.
      ;; [2024-09-01 Sun 00:43] `doom-rouge' is an amazing dark theme
      ;; [2024-09-02 Mon 03:01] and `ef-elea-dark' too.
      ;; [2024-09-04 Wed 02:03] `ef-maris-dark' too.
      ;; [2025-02-19 Wed 01:12] `doom-bluloco-dark' is lovely.
      ;; [2025-02-23 Sun 12:19] `doom-earl-grey' is nice as a light theme.
      ;; [2025-03-08 Sat 16:58] `doom-ephemeral' is acceptable.
      ;; [2025-03-10 Mon 06:42] `doom-gruvbox-light' is nice, light.
      ;; [2025-03-17 Mon 01:33] `doom-gruvbox' is nice, dark.
      ;; [2025-03-27 Thu 01:22] `doom-ir-black' is nice too, dark.
      ;; [2025-03-31 Mon 05:38] `doom-material-dark' is nice, dark.
      ;; [2025-04-15 Tue 11:17] `doom-miramare' is beautiful, dark.
      ;; [2025-04-22 Tue 11:48] `doom-molokai' is nice, dark.
      ;; [2025-05-12 Mon 01:34] `doom-outrun-electric' is great.
      ;; [2025-05-28 Wed 16:55] `doom-solarized-light' is nice, light
      ;; [2025-06-25 Wed 15:30] `modus-vivendi-tinted' is cozy, dark
      ;; [2025-07-05 Sat 18:28] `modus-vivendi-tinted' is okay, dark
      ;; [2025-07-05 Sat 18:43] Just found out that my feeling towards the same
      ;; theme changed over couple of days.
      ;; [2025-07-06 Sun 02:39] in fact, now I don't even like it.
      ;; [2025-07-23 Wed 23:54] `ef-bio' is good, dark.
      ;; [2025-07-29 Tue 21:54] `ef-cherie' is likeable, dark.
      doom-theme                                        (salih/get-random-theme-full 1)
      doom-modeline-icon                                t
      doom-modeline-height                              32
      display-line-numbers-type                         'relative

      ;; prayer time
      ;; not my real coordinates, just to save you time.
      calendar-latitude                                 29.392691
      calendar-longitude                                30.828360
      salih/awqat-show-mode-line                        t

      ;; org
      org-directory                                     org-roam-directory
      org-id-locations-file                             "~/roam/.orgids"
      +org-capture-changelog-file                       "~/blog/content/nice.org"
      +org-capture-journal-file                         "~/blog/content/stack.org"
      salih/org-roam-fleet-file                         "~/roam/main/lr.org"
      salih/org-vocal-store                             "~/roam/media/vocal"
      +org-capture-todo-file                            "~/roam/main/life.org"

      ;; this option is useful when you are up after 00:00. set 0 to the value
      ;; you sleep at. if you sleep at 02:00 it should be 2, if you sleep at
      ;; 02:30 it should be 3 and so on. Org agenda for the day will not overlap
      ;; until your day is done.
      ;; [2024-08-07 Wed 19:43] currently I sleep at 07:00.
      ;; [2024-08-08 Wed 23:41] Not anymore.
      org-extend-today-until                            3

      ;; other
      auto-save-no-message                              t
      dired-preview-delay                               0.1
      safe-local-variable-values
      '((org-download-image-dir
         . "../i")
        (salih/rebuild . t))
      ;; currently org causes some annoying warnings because of org-element
      ;; breaking API updates.
      ;; [2024-04-26 Fri 02:01] I wrote "currently" above a long time ago
      ;; (perhaps can be detected from the git history, too lazy tho). Not sure
      ;; if it is still the case
      ;; [2024-11-20 Wed 11:45] Let's try without it!
      ;; [2024-11-22 Fri 12:07] Works fine so far.
      ;; [2025-06-08 Sun 12:20] It's back!
      ;; [2025-06-27 Fri 20:41] https://github.com/org-noter/org-noter/issues/111
      ;; [2025-06-27 Fri 20:42] https://list.orgmode.org/87qzzfd7bf.fsf@localhost/T/#t
      warning-minimum-level                             :error)


(s/require
 '+sets-email                           ; mu4e
 '+sets-school                          ; school settings (TeX & BibTeX)
 '+sets-org                             ; org mode settings
 '+sets-inhibit                         ; other settings
 '+helper                               ; functions
 '+advice                               ; advice
 '+bindings                             ; personal key bindings
 '+custom                               ; specials
 '+erc                                  ; erc
 '+deep                                 ; other
 (unless (featurep 'tadwin) '+hooks))   ; hooks

;; Experimental. I just copied it from the internet.
(setq jit-lock-stealth-time 1.25
      jit-lock-chunk-size 4096
      jit-lock-defer-time 0)

(with-eval-after-load 'evil
  (add-hook 'evil-insert-state-entry-hook
            (lambda ()
              (setq jit-lock-defer-time 0.25)) nil t)
  (add-hook 'evil-insert-state-exit-hook
    (lambda ()
      (setq jit-lock-defer-time 0)) nil t))




(when (eq system-type 'darwin)
  (menu-bar-mode -1)
  (require 'ls-lisp)
  (setq mac-function-modifier 'control)
  (setq mac-option-key-is-meta               nil
        mac-command-key-is-meta              t
        mac-command-modifier                 'meta
        mac-option-modifier                  'none
        frame-title-format                   nil
        org-download-screenshot-method       "/usr/local/bin/pngpaste %s"
        ls-lisp-dirs-first                   t
        ls-lisp-use-insert-directory-program nil
        epg-pinentry-mode                    'loopback)
;;; Transparent titlebar
;; https://github.com/d12frosted/homebrew-emacs-plus/blob/master/Formula/emacs-plus.rb#L98
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/55
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Properties-in-Mode.html#Properties-in-Mode
;; (modify-all-frames-parameters '((inhibit-double-buffering . t)))
  (add-to-list 'default-frame-alist    '(ns-appearance . 'dark))
  (add-to-list 'default-frame-alist    '(ns-transparent-titlebar . t)))


;; TODO this was merged to Awqat, we need to move to them
(awqat-notification-mode 1)
;; (spacious-padding-mode)


(add-hook! 'flycheck-mode-hook #'flyover-mode)
(setq flyover-show-at-eol t)
(setq flyover-hide-when-cursor-is-on-same-line nil)
(setq flyover-virtual-line-icon "──►") ;;; default its nil

(remove-hook 'flycheck-mode-hook '+syntax-init-popups-h)


(defun salih/org-noter--try-add-highlight-before-note (&rest args)
  "Try to add a highlight annotation if there's a selection, but don't fail if
it doesn't work.  ARGS are ignored but accepted to work with advice system."
  (when (pdf-view-active-region-p)
    (condition-case err
        (progn
          (call-interactively #'pdf-annot-add-highlight-markup-annotation)
          (save-buffer))
      (error
       (message "Failed to add highlight annotation: %s" (error-message-string err))))))

(advice-add 'org-noter-insert-precise-note :before #'salih/org-noter--try-add-highlight-before-note)
