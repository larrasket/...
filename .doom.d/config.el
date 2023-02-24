(add-to-list 'load-path "~/.doom.d/")
(add-to-list 'org-agenda-files "~/roam/journal/agenda/todo.org")
(add-to-list 'org-agenda-files "~/roam/journal/agenda/births.org")
(require 'epa-file)

(setq load-prefer-newer t ;; avoid warnings
      ;; set org files
      +org-capture-journal-file                         "~/blog/content/stack.org"
      +org-capture-changelog-file                       "~/blog/content/nice.org"
      +org-capture-todo-file                            "~/roam/main/life.org"
      org-preview-html-viewer                           'xwidget
      org-roam-directory                                "~/roam"

      ;; I've no idea of any of this.
      org-crypt-key                                     "ghd@keemail.me"
      epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-select-keys                              'silent
      epa-file-encrypt-to                               "ghd@keemail.me"

      ;; please don't stalk me
      user-full-name                                    "Salih Muhammed"
      user-mail-address                                 "ghd@keemail.me"

      ;; skull welecome in emacs
      fancy-splash-image                                "~/.doom.d/pan.png"

      ;; modern org mode style
      highlight-indent-guides-method                    'bitmap
      org-modern-block-name                             '(("" "" ""))
      org-modern-checkbox                               nil
      org-modern-keyword                                '(("" . ""))
      org-modern-list                                   nil
      org-modern-priority                               nil
      org-modern-star                                   nil
      org-modern-tag                                    nil
      org-modern-timestamp                              nil
      org-modern-todo                                   nil

      ;; theme
      doom-theme                                        'distinguished
      highlight-indent-guides-method                    'bitmap

      ;; prayer time
      calendar-latitude                                 30.0
      calendar-longitude                                31.2

      ;; school
      bibtex-completion-bibliography                    "~/configs/ref.bib"

      ;; translate
      gts-translate-list                                '(("en" "ar"))

      ;; keyboard
      salih/prefix-global                               "C-x "
      salih/prefix-mode                                 "C-c "

      ;; other
      browse-url-generic-program                        "chromium"
      large-file-warning-threshold                      nil
      inferior-lisp-program                             "sbcl"
      neo-mode-line-type                                'default
      org-annotate-file-storage-file                    "~/configs/annotated.org")

;; this should be called after defining salih/prefix-global
(require '+handy)
(require 'keys)

(add-hook 'org-mode-hook 'highltier)
(add-hook 'prog-mode-hook 'highltier)
(add-hook 'prog-mode-hook 'column-enforce-mode)
(add-hook 'prog-mode-hook 'auto-fill-mode)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'neotree-mode-hook #'hide-mode-line-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

(epa-file-enable)
(global-wakatime-mode)
(global-org-modern-mode)
