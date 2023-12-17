;; settings that i'm unlikely to change


(setq inhibit-automatic-native-compilation              t
      package-native-compile                            t
      completion-ignore-case                            t
      load-prefer-newer                                 t
      bidi-paragraph-direction                          'left-to-right
      ;; gcmh-high-cons-threshold                          1073741824
      scroll-conservatively                             101
      jit-lock-defer-time                               0
      read-process-output-max                           1000000

      doom-modeline-height                              19
      doom-modeline-buffer-state-icon                   nil
      doom-modeline-icon                                nil


      ;; consult
      consult-preview-key                               nil
      consult-org-roam-buffer-narrow-key                ?r

      ;; tabs
      centaur-tabs-enable-key-bindings                  t
      centaur-tabs-close-button                         "✕"
      centaur-tabs-modified-marker                      "•"
      centaur-tabs-cycle-scope                          'tabs
      centaur-tabs-height                               20
      centaur-tabs-set-icons                            nil

      company-idle-delay                                0.3
      format-all-show-errors                            'never
      proced-auto-update-flag                           t
      salih/temp-roam-insert                            nil
      large-file-warning-threshold                      nil


      save-place-ignore-files-regexp                    "\\(?:COMMIT_EDITMSG\\|hg-editor-[[:alnum:]]+\\.txt\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\|\\.pdf\\)$"
      inferior-lisp-program                             "sbcl"
      neo-mode-line-type                                'default





      ;; org-noter
      org-noter-always-create-frame                     nil
      org-noter-kill-frame-at-session-end               nil
      org-noter-swap-window                             nil
      nov-text-width                                    140


      ;; julia
      lsp-julia-default-environment                     "~/.julia/environments/v1.9"
      lsp-julia-package-dir                             nil
      lsp-julia-flags                                   `("-J/home/l/configs/~s/assets/languageserver.so")

      ;; git-auto-commit-mode
      gac-debounce-interval                             200
      gac-silent-message-p                              t


      vertico-buffer-display-action                     '(display-buffer-at-bottom (window-height . 20))
      enable-recursive-minibuffers                      nil


      salih/prefix-global                               "C-x "
      salih/prefix-mode                                 "C-c "
      shr-inhibit-images                                nil

      lsp-use-plists                                    nil
      lsp-ui-doc-enable                                 nil
      lsp-headerline-breadcrumb-enable                  nil
      lsp-enable-symbol-highlighting                    t
      lsp-ui-sideline-enable                            t
      lsp-modeline-code-actions-enable                  t
      lsp-eldoc-enable-hover                            t
      lsp-modeline-diagnostics-enable                   t
      lsp-signature-auto-activate                       t
      lsp-signature-render-documentation                t
      lsp-completion-show-detail                        t
      lsp-lens-enable                                   t
      lsp-ui-sideline-enable                            t
      lsp-ui-sideline-show-diagnostics                  t
      lsp-completion-show-kind                          t


      org-bullets-bullet-list                           '("◉" "✸" "✿" "♥" "●")
      org-id-method                                     'org
      org-roam-ui-open-on-start                         nil
      org-agenda-skip-scheduled-if-done                 nil
      org-use-tag-inheritance                           t
      org-agenda-block-separator                        9472
      org-clock-mode-line-total                         'today
      org-element-use-cache                             t
      org-noter-auto-save-last-location                 t
      org-startup-folded                                'show2levels
      org-image-actual-width                            600
      org-link-file-path-type                           'relative
      org-agenda-show-future-repeats                    nil
      org-clock-mode-line-total                         'current
      ;; FIXME this is useful to hide the title name from the org clock, however
      ;; it might be inconsistent. Better should be redefining
      ;; `org-clock-get-clock-string'. I wouldn't overwrite it myself since it
      ;; might break things in the future, I might consider making PR to
      ;; org-mode making the string customizable.
      org-clock-string-limit                            8
      org-agenda-dim-blocked-tasks                      'invisible
      org-tags-column                                   70
      org-agenda-sticky                                 t
      salih/vulpea-show-full                            nil
      org-crypt-key                                     user-mail-address
      epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-select-keys                              'silent
      epa-file-encrypt-to                               user-mail-address



      ;; translation
      gts-translate-list                                '(("en" "ar"))
      gts-default-translator                            (gts-translator
                                                         :picker        (gts-prompt-picker)
                                                         :engines (list (gts-google-engine))
                                                         :render        (gts-buffer-render)))

(provide '+sets-inhibit)
