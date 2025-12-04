;;; config.el -*- lexical-binding: t; -*-
(require '+early)
(setq salih/temp-roam-insert nil)
(setq user-full-name                                    "Salih Muhammed"
      user-mail-address                                 "l@larr.net"
      user-first-name                                   "Salih"
      ;; Commenting these temporarily until I get back to mu4e with iCloud.
      ;; user-stmp-server                                  "mail.gmx.com"
      ;; user-stmp-port                                    587
      user-short-username                               "lr0"
      user-config-repo-path                             "~/configs/~s"
      salih/blog-content-path                           "~/blog/content"
      org-roam-directory                                (file-truename "~/roam")
      doom-font                                         (font-spec :family "Pragmasevka" :size 16)
      ;; kaolin-dark
      ;; doom-badger
      ;; kaolin-temple
      doom-theme                                        'kaolin-dark
      doom-modeline-icon                                t
      doom-modeline-height                              32
      display-line-numbers-type                         'relative
      ;; org
      org-directory                                     org-roam-directory
      org-id-locations-file                             "~/roam/.orgids"
      +org-capture-changelog-file                       "~/blog/content/nice.org"
      +org-capture-journal-file                         "~/blog/content/stack.org"
      salih/org-roam-fleet-file                         "~/roam/main/lr.org"
      salih/org-vocal-store                             "~/roam/media/vocal"
      +org-capture-todo-file                            "~/roam/main/life.org"
      salih/vulpea-show-full                            nil
      salih/adding-note?                                nil
      salih/org-agenda-full-f                           nil
      ;; this option is useful when you are up after 00:00. set 0 to the value
      ;; you sleep at. if you sleep at 02:00 it should be 2, if you sleep at
      ;; 02:30 it should be 3 and so on. Org agenda for the day will not overlap
      ;; until your day is done.
      ;; [2024-08-07 Wed 19:43] currently I sleep at 07:00.
      ;; [2024-08-08 Wed 23:41] Not anymore.
      ;; [2025-09-20 Sat 00:02] Quite outdated information huh :)
      org-extend-today-until                            4

      ;; other
      auto-save-no-message                              t
      dired-preview-delay                               0.1
      safe-local-variable-values
      '((org-download-image-dir
         . "../i")
        (salih/rebuild . t)))

(setq epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-select-keys                               'silent
      epa-file-encrypt-to                                user-mail-address)

(setq doom-modeline-icon nil)
(setq doom-modeline-height 30)
(setq doom-modeline-unicode-fallback t)
(setq doom-modeline-height 30)
(setq doom-modeline-bar-width 1)
(setq doom-modeline-major-mode-icon t
      doom-modeline-icon t
      doom-modeline-buffer-state-icon nil)


(setq gac-debounce-interval                             200
      gac-silent-message-p                              t)



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
;; [2025-09-20 Sat 00:02] I cleaned my org config. Let's give that a try again.
;; warning-minimum-level                             :error)


(require '+l-init)

(s/require
 '+bindings
 '+early)

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








(breadcrumb-mode)
(global-jinx-mode)
(yas-global-mode 1)
(display-battery-mode)
(salih/keyboard-config)
(consult-org-roam-mode 1)
(global-visual-line-mode 1)
(awqat-notification-mode 1)
(awqat-display-prayer-time-mode)
;; (global-display-line-numbers-mode)

(custom-set-faces
 '(mode-line ((t (:family "Pragmasevka"))))
 '(mode-line-active ((t (:family "Pragmasevka"))))
 '(mode-line-inactive ((t (:family "Pragmasevka")))))

(set-popup-rules! '(("^\\*Project errors\\*" :size 0.25)))
(set-face-attribute 'shr-text nil :family "Arial" :height 180)


;; (setq modus-themes-common-palette-overrides
;;       '((fg-line-number-inactive bg-alt)
;;         (fg-line-number-active bg-alt)
;;         (bg-line-number-inactive unspecified)
;;         (bg-line-number-active unspecified)))

;; (custom-set-faces
;;  '(line-number ((t (:slant normal))))
;;  '(line-number-current-line ((t (:slant normal)))))

;; (setq modus-themes-italic-constructs t)
;; (setq modus-themes-bold-constructs nil)

(set-fringe-style '(2 . 0))

(require 'spacious-padding)

