(add-to-list 'load-path "~/.doom.d/")
(require '+handy)
(require 'keys)
(require 'epa-file)


(add-to-list 'org-agenda-files "~/roam/journal/agenda/todo.org")
(add-to-list 'org-agenda-files "~/roam/journal/agenda/births.org")

(setq load-prefer-newer t ;; avoid warnings
;; set org files
      +org-capture-journal-file "~/blog/content/stack.org"
      +org-capture-changelog-file "~/blog/content/nice.org"
      +org-capture-todo-file "~/roam/main/life.org"
      org-preview-html-viewer 'xwidget
      org-roam-directory "~/roam"

      highlight-indent-guides-method 'bitmap
      browse-url-generic-program "chromium"
      inferior-lisp-program "sbcl"
      large-file-warning-threshold nil

      org-crypt-key "ghd@keemail.me"
      epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-select-keys 'silent
      epa-file-encrypt-to "ghd@keemail.me"


      user-full-name "Salih Muhammed"
      user-mail-address "ghd@keemail.me"

      fancy-splash-image "~/.doom.d/pan.png"

;; modern org mode style
      highlight-indent-guides-method 'bitmap
      org-modern-block-name '(("" "" ""))
      org-modern-checkbox nil
      org-modern-keyword '(("" . ""))
      org-modern-list nil
      org-modern-priority nil
      org-modern-star nil
      org-modern-tag nil
      org-modern-timestamp nil
      org-modern-todo nil

      doom-theme 'distinguished


      ;; prayer time
      calendar-latitude 30.0
      calendar-longitude 31.2


      bibtex-completion-bibliography "~/configs/ref.bib"

      gts-translate-list '(("en" "ar")))


(add-hook 'org-mode-hook 'highltier)
(add-hook 'prog-mode-hook 'highltier)
(add-hook 'prog-mode-hook 'column-enforce-mode)
(add-hook 'prog-mode-hook 'auto-fill-mode)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'neotree-mode-hook #'hide-mode-line-mode)

(epa-file-enable)
(global-wakatime-mode)
(global-org-modern-mode)

(after! eshell (remove-hook 'eshell-mode-hook 'hide-mode-line-mode))
(add-hook 'after-init-hook #'global-flycheck-mode)



(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side            . bottom)
               (reusable-frames . visible)
               (window-height   . 0.18)))
