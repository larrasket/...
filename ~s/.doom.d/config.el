(add-to-list 'load-path "~/.doom.d/lisp/")
(add-to-list 'org-agenda-files "~/roam/main/life.org")
(add-to-list 'doom-emoji-fallback-font-families "Symbola")

(require 'awqat)                        ; for prayer support in the agenda
(require 'vulpea)                       ; org-roam project tasks in org-agenda
(require '+early)                       ; personal utilities
(require 'go-translate)                 ; define trnaslation engine in config.el


(setq-default frame-title-format                        '("%b")
              bidi-paragraph-direction                  'left-to-right
              org-download-image-dir                    "~/roam/media"
              indent-tabs-mode                          nil
              pdf-view-display-size                     'fit-width)

(setq user-full-name                                    "Salih Muhammed"
      user-mail-address                                 "lr0@gmx.com"
      user-short-username                               "lr0"
      user-config-repo-path                             "/home/l/configs/~s"
      salih/blog-content-path                           "~/blog/content"
      user-first-name                                   (salih/user-first-name)
      org-roam-directory                                (file-truename "~/roam")
      srht-username                                     user-short-username

      ;; emacs settings
      inhibit-automatic-native-compilation              t
      package-native-compile                            t
      completion-ignore-case                            t
      load-prefer-newer                                 t
      bidi-paragraph-direction                          'left-to-right
      ;; gcmh-high-cons-threshold                          1073741824
      scroll-conservatively                             101
      jit-lock-defer-time                               0
      read-process-output-max                           1000000
      lsp-use-plists                                    nil
      lsp-ui-doc-enable                                 nil

      ;; appearance
      ;; font `:size` value of 29 is prefect for filming
      ;; with high dpi use (set-frame-font "PragmataPro Mono Liga")
      ;; or just remove `:size`.
      doom-font                                         "Pragmata Pro:pixelsize=13:antialias=true:hinting=true:autohint=false:hintstyle=3"
      doom-modeline-height                              19
      doom-modeline-buffer-state-icon                   nil
      doom-modeline-icon                                nil
      doom-theme                                        'modus-vivendi
      +doom-dashboard-ascii-banner-fn                   'salih/banner
      display-line-numbers-type                         'nil
      all-the-icons-color-icons                         nil
      treemacs-position                                 'right

      ;; set org files
      +org-capture-journal-file                         (salih/path-blog "stack.org")
      +org-capture-changelog-file                       (salih/path-blog "nice.org")
      +org-capture-todo-file                            (salih/path-roam "main" "life.org")
      org-bullets-bullet-list                           '("◉" "✸" "✿" "♥" "●")
      org-id-method                                     'org
      org-directory                                     org-roam-directory
      org-id-locations-file                             (salih/path-roam ".orgids")
      org-roam-ui-open-on-start                         nil
      org-agenda-skip-scheduled-if-done                 nil
      org-use-tag-inheritance                           t
      org-agenda-block-separator                        9472
      org-clock-mode-line-total                         'today
      ;; this option is useful when you are up after 00:00. set 0 to the value
      ;; yoe sleep at. if you sleep at 02:00 it should be 2, if you sleep at
      ;; 02:30 it should be 3 and so on. Org agenda for the day will not overlap
      ;; until your day is done.
      org-extend-today-until                            0
      org-element-use-cache                             t
      org-noter-auto-save-last-location                 t
      org-startup-folded                                'show2levels
      org-image-actual-width                            600
      org-link-file-path-type                           'relative
      org-agenda-entry-text-exclude-regexps             '("- State \"\\S-+\"\\s-+from\\s-+\"\\S-+\"\\s-+\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^]]*\\)\\]")
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

      ;; I've no idea of any of this.
      org-crypt-key                                     user-mail-address
      epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-select-keys                              'silent
      epa-file-encrypt-to                               user-mail-address

      ;; prayer time
      calendar-latitude                                 30.0
      calendar-longitude                                31.2
      awqat-mode-line-format                            " ${prayer} (${hours}h${minutes}m) "
      salih/awqat-show-mode-line                        t

      ;; school
      salih/source-directory                            (salih/path-roam "source")
      salih/books                                       (salih/path-list salih/source-directory)
      bibtex-completion-bibliography                    (salih/path-configs "ref.bib")
      bibtex-completion-notes-path                      (salih/path-roam "references")
      org-cite-csl-styles-dir                           (salih/path-configs "assets" "csl")

      bibtex-completion-library-path                    `(,salih/source-directory)
      org-cite-global-bibliography                      `(,bibtex-completion-bibliography)
      citar-bibliography                                bibtex-completion-bibliography
      org-cite-csl--fallback-style-file                 (f-join org-cite-csl-styles-dir "chicago-ibid.csl")
                                                         
      ;; translate
      gts-translate-list                                '(("en" "ar"))
      gts-default-translator                            (gts-translator
                                                         :picker        (gts-prompt-picker)
                                                         :engines (list (gts-google-engine))
                                                         :render        (gts-buffer-render))

      ;; keyboard
      salih/prefix-global                               "C-x "
      salih/prefix-mode                                 "C-c "

      ;; browser
      browse-url-browser-function                       'eww
      shr-inhibit-images                                nil

      ;; vertico
      vertico-buffer-display-action                     '(display-buffer-at-bottom (window-height . 20))
      enable-recursive-minibuffers                      nil

      ;; git-auto-commit-mode
      gac-debounce-interval                             200
      gac-silent-message-p                              t

      ;; julia
      lsp-julia-default-environment                     "~/.julia/environments/v1.9"
      lsp-julia-package-dir                             nil
      lsp-julia-flags                                   `("-J/home/l/configs/~s/assets/languageserver.so")

      ;; org-noter
      org-noter-always-create-frame                     nil
      org-noter-kill-frame-at-session-end               nil
      org-noter-swap-window                             nil
      nov-text-width                                    140

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

      ;; modus theme
      modus-themes-bold-constructs                      t
      modus-themes-fringes                              nil
      modus-themes-italic-constructs                    t
      modus-themes-org-blocks                           'gray-background
      modus-themes-common-palette-overrides             '((border-mode-line-active unspecified) (border-mode-line-inactive unspecified))

      ;; indent highlight
      indent-bars-highlight-current-depth               nil
      indent-bars-treesit-support                       t

      ;; other
      company-idle-delay                                0.3
      format-all-show-errors                            'never
      proced-auto-update-flag                           t
      salih/temp-roam-insert                            nil
      large-file-warning-threshold                      nil
      safe-local-variable-values                        '((org-download-image-dir
                                                           . "../i")
                                                          (salih/rebuild . t))
      save-place-ignore-files-regexp                    "\\(?:COMMIT_EDITMSG\\|hg-editor-[[:alnum:]]+\\.txt\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\|\\.pdf\\)$"
      inferior-lisp-program                             "sbcl"
      neo-mode-line-type                                'default
      bmkp-last-as-first-bookmark-file                  "/home/ghd/.emacs.d/.local/etc/bookmarks")
;; setup email
;; don't forget to setup authinfo
;; https://www.emacswiki.org/emacs/GnusAuthinfo
;; and of course mu, isync
;; tidy is required to use with xwidget:
;; pacman -S tidy
(after! mu4e
  (setq message-send-mail-function              'smtpmail-send-it
        starttls-use-gnutls                     t
        mu4e-compose-reply-ignore-address       `("no-?reply"
                                                  ,user-mail-address)
        mu4e-update-interval                    200
        mu4e-compose-signature                  (format "Regards,\n%s"
                                                        user-first-name)
        smtpmail-default-smtp-server            "mail.gmx.com"
        smtpmail-smtp-server                    smtpmail-default-smtp-server
        smtpmail-smtp-service                   587
        smtpmail-starttls-credentials           '(("mail.gmx.com" 465 nil nil))
        smtpmail-stream-type                    'starttls
        mu4e-alert-interesting-mail-query       (concat "flag:unread"
                                                        " AND NOT flag:trashed"
                                                        " AND NOT maildir:" "\"/rss\""
                                                        " AND NOT maildir:" "\"/read\""
                                                        " AND NOT maildir:" "\"/archive\"")
        mu4e-modeline-show-global               nil)


  (defun mu4e-action-view-in-xwidget (msg)
    (unless (fboundp 'xwidget-webkit-browse-url)
      (mu4e-error "No xwidget support available"))
    (let ((browse-url-handlers nil)
          (browse-url-browser-function (lambda (url &optional _rest)
                                         (with-output-to-string
                                           (call-process "tidy" nil nil nil "-m"
                                                         "--numeric-entities"
                                                         "yes"
                                                         (replace-regexp-in-string
                                                          "^file://" "" url)))
                                         (xwidget-webkit-browse-url url))))
      (mu4e-action-view-in-browser msg)))


  (defun salih/delete-citation ()
    (delete-region (point) (point-max)))

  (defun salih/mu4e-reply (prefix)
    (interactive "P")
    (setq mu4e-compose-cite-function #'salih/delete-citation)
    (mu4e-compose-reply))

  (define-key mu4e-view-mode-map    (salih/mode "C-r") #'salih/mu4e-reply)
  (define-key mu4e-headers-mode-map (salih/mode "C-r") #'salih/mu4e-reply))

(set-fontset-font "fontset-default" 'arabic (font-spec :family "SF Arabic"))


(require '+helper)
(require '+hooks)
(require '+advice)
(require '+bindings)
(require '+org-tags)
(require '+custom)
(require '+erc)
(require '+deep)
