;; settings that i'm unlikely to change

(setq inhibit-automatic-native-compilation              t
      package-native-compile                            t
      completion-ignore-case                            t
      load-prefer-newer                                 t
      bidi-paragraph-direction                          'left-to-right
      scroll-conservatively                             101
      read-process-output-max                           1000000
      +doom-dashboard-menu-sections                     '(("Recently opened files"
                                                           :action
                                                           recentf-open-files)
                                                          ("Open project"
                                                           :action
                                                           projectile-switch-project)
                                                          ("Jump to bookmark"
                                                           :action bookmark-jump)
                                                          ("Open documentation"
                                                           :action doom/help))

      ;; fonts
      doom-font
      (concat
       "Pragmasevka:pixelsize=16"
       ":antialias=true:hinting=true:autohint=false:hintstyle=3")

      doom-symbol-font
      (concat
       "Amiri UI"
       ":pixelsize=16:antialias=true:hinting=true:autohint=false:hintstyle=3")
      doom-variable-pitch-font
      (font-spec :family "Arimo")

      ;; modeline
      doom-modeline-enable-word-count                   t
      doom-modeline-buffer-state-icon                   nil
      doom-modeline-icon                                t
      all-the-icons-completion-mode                     nil
      global-hl-line-modes                              nil
      scroll-margin                                     4
      doom-modeline-mode-alist                          nil
      doom-modeline-modal-icon                          nil
      doom-modeline-lsp-icon                            nil

      ;; awqat
      awqat-mode-line-format
      " ${prayer} (${hours}h${minutes}m) "
      awqat-update-interval                             (* 60 5)

      ;; consult
      consult-preview-key                               nil
      consult-org-roam-buffer-narrow-key                ?r

      ;; tabs
      centaur-tabs-enable-key-bindings                  t
      centaur-tabs-close-button                         "✕"
      centaur-tabs-modified-marker                      "•"
      centaur-tabs-cycle-scope                          'tabs
      centaur-tabs-set-icons                            nil

      company-idle-delay                                0.3
      format-all-show-errors                            'never
      proced-auto-update-flag                           t
      salih/temp-roam-insert                            nil
      large-file-warning-threshold                      nil

      ;; school
      salih/source-directory                            "~/roam/source"

      salih/books
      (mapcar 'file-truename
              (directory-files-recursively salih/source-directory "" nil t))

      bibtex-completion-bibliography                    "~/configs/~s/ref.bib"
      bibtex-completion-notes-path                      "~/roam/references"
      org-cite-csl-styles-dir                           "~/configs/~s/assets/csl"
      bibtex-completion-library-path
      (l salih/source-directory)
      org-cite-global-bibliography
      (l bibtex-completion-bibliography)
      citar-bibliography
      bibtex-completion-bibliography
      org-cite-csl--fallback-style-file
      (f-join org-cite-csl-styles-dir "chicago-ibid.csl")
      org-fc-flashcard-tag                              "drill"
      org-fc-directories                                (l "~/roam/main"
                                                           "~/roam/other"
                                                           "~/roam/references")



      save-place-ignore-files-regexp
      (concat
       "\\(?:COMMIT_EDITMSG\\|hg-editor-[[:alnum:]]"
       "+\\.txt\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\|\\.pdf\\)$")
      inferior-lisp-program                             "sbcl"
      neo-mode-line-type                                'default

      ;; org-noter
      org-noter-always-create-frame                     nil
      org-noter-kill-frame-at-session-end               nil
      org-noter-swap-window                             nil
      nov-text-width                                    140

      ;; TODO update julia conf with eglot
      ;; julia

      ;; git-auto-commit-mode
      gac-debounce-interval                             200
      gac-silent-message-p                              t

      enable-recursive-minibuffers                      nil
      vertico-buffer-display-action
      '(display-buffer-at-bottom (window-height . 20))

      salih/prefix-global                               "C-x "
      salih/prefix-mode                                 "C-c "
      shr-inhibit-images                                nil

      ;; epa? what does that mean. I've no idea about this.
      salih/vulpea-show-full                            nil
      epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-select-keys                              'silent
      epa-file-encrypt-to                               user-mail-address

      ;; modus
      modus-themes-org-blocks                           'gray-background
      modus-themes-common-palette-overrides
      '((bg-mode-line-active          bg-inactive)
        (fg-mode-line-active          fg-main)
        (bg-mode-line-inactive        bg-inactive)
        (fg-mode-line-active          fg-dim)
        (bg-line-number-active        unspecified)
        (bg-line-number-inactive      unspecified)
        (border-mode-line-active      bg-main)
        (border-mode-line-inactive    bg-inactive))

      centaur-tabs-height                               22
      +doom-dashboard-ascii-banner-fn                   'salih/banner
      all-the-icons-color-icons                         nil
      treemacs-position                                 'right

      ;; indent highlight
      indent-bars-highlight-current-depth               nil
      indent-bars-treesit-support                       t
      indent-bars-pattern                               "."
      indent-bars-width-frac                            0.25

      breadcrumb-project-max-length                     0.5
      breadcrumb-project-crumb-separator                "/"
      breadcrumb-imenu-max-length                       1.0
      breadcrumb-imenu-crumb-separator                  " > "
      helm-ag-show-status-function
      #'doom-modeline-set-helm-modeline
      mode-line-format                                  nil

      cocaine-show-buffer-position                      nil
      cocaine-show-column-info                          nil
      cocaine-show-misc-info                            t
      cocaine-show-processes-info                       t

      ;; translation
      gt-langs                                          `("en" "ar")
      gt-default-translator
      (gt-translator
       :taker   (gt-taker :text 'buffer :pick 'paragraph)
       :engines (list (gt-google-engine))
       :render        (gt-buffer-render)))

(provide '+sets-inhibit)
