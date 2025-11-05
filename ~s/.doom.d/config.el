;;; config.el -*- lexical-binding: t; -*-
(require '+early)
(setq salih/temp-roam-insert nil)
(setq user-full-name                                    "Salih Muhammed"
      user-mail-address                                 "lr0@gmx.com"
      user-first-name                                   "Salih"
      user-stmp-server                                  "mail.gmx.com"
      user-stmp-port                                    587
      user-short-username                               "lr0"
      user-config-repo-path                             "~/configs/~s"
      salih/blog-content-path                           "~/blog/content"
      org-roam-directory                                (file-truename "~/roam")
      doom-font                                         (font-spec :family "PragmataPro" :size 16)

      doom-theme                                        'ef-elea-dark
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




(after! embark
  (add-to-list 'embark-keymap-alist '(org-timestamp embark-org-timestamp-map))
  (defvar-keymap embark-org-timestamp-map
    :doc "Keymap for actions on an org timestamp."
    :parent embark-general-map
    "t" #'salih/org-add-week-to-timestamp)
  (define-key embark-url-map (kbd "c") 'salih/open-url-in-chrome-cross-platform)
  (define-key embark-org-link-map (kbd "RET") 'org-web-tools-read-url-as-org))




(breadcrumb-mode)
(yas-global-mode 1)
(salih/keyboard-config)
(consult-org-roam-mode 1)
(global-visual-line-mode 1)
(awqat-notification-mode 1)
(awqat-display-prayer-time-mode)

(setq epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-select-keys                               'silent
      epa-file-encrypt-to                                user-mail-address)

(setq doom-modeline-icon nil)
(setq doom-modeline-height 30)
(setq doom-modeline-unicode-fallback t)


(setq doom-modeline-height 30)
(custom-set-faces
 '(mode-line ((t (:family "Iosevka"))))
 '(mode-line-active ((t (:family "Iosevka"))))
 '(mode-line-inactive ((t (:family "Iosevka")))))
(setq doom-modeline-bar-width 1)




(set-popup-rules!
  '(("^\\*Project errors\\*" :size 0.25)))


(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)


(setq lsp-enable-symbol-highlighting nil)
(setq lsp-ui-doc-show-with-cursor nil)
(setq lsp-ui-doc-show-with-mouse nil)
(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-code-actions t)
(setq lsp-ui-sideline-enable nil)
(setq lsp-modeline-code-actions-enable t)
(setq lsp-ui-sideline-enable nil)
(setq lsp-eldoc-enable-hover t)
(setq lsp-signature-auto-activate t) ;; you could manually request them via `lsp-signature-activate`
(setq lsp-signature-render-documentation nil)
(setq lsp-headerline-breadcrumb-enable t)
(add-hook 'org-roam-find-file-hook      #'git-auto-commit-mode)

(setq       gac-debounce-interval                             200
            gac-silent-message-p                              t)

(display-battery-mode)
;; nice themes
;; [ ] doom-bluloco-light -- (no documentation available)
;; [X] doom-fairy-floss -- (no documentation available)
;; [ ] kaolin-breeze -- (no documentation available)


(setq doom-modeline-major-mode-icon t
      doom-modeline-icon t
      doom-modeline-buffer-state-icon nil)

(global-jinx-mode)
(set-face-attribute 'shr-text nil :family "Arial" :height 180)

(after! vertico-multiform ;; if using vertico
  (add-to-list 'vertico-multiform-categories
               '(jinx (vertico-grid-annotate . 25)))

  (vertico-multiform-mode 1))


(setq org-modern-tag nil
      org-modern-timestamp nil
      org-modern-todo nil)

(add-hook 'doom-docs-org-mode-hook (lambda () (breadcrumb-local-mode -1)))


(defun salih/pdf-occure ()
  (interactive)
  (save-window-excursion
    (pdf-occur-goto-occurrence)))


(defun salih/tmp-buffer ()
  "Open a new temporary buffer with a random name to play in."
  (interactive)
  (let ((bufname (generate-new-buffer-name
                  (format "*scratch-%x*" (random most-positive-fixnum)))))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)
    (message "Opened temporary buffer: %s" bufname)))
