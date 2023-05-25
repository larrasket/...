(add-to-list 'load-path "~/.doom.d/")
(add-to-list 'org-agenda-files "~/roam/main/life.org")
(add-to-list 'doom-emoji-fallback-font-families "Symbola")
(require 'epa-file)
(require 'go-translate)


(setq-default frame-title-format                        '("%b")
              bidi-paragraph-direction                  'left-to-right
              org-download-image-dir                    "~/roam/media")

(setq load-prefer-newer                                 t ;; avoid warnings
      ;; emacs settings
      completion-ignore-case                            t
      bidi-paragraph-direction                          'left-to-right

      ;; appearance
      doom-font                                         (font-spec :family "PragmataPro Mono Liga" :size 12)
      all-the-icons-color-icons                         nil
      neo-theme                                         'icons
      neo-window-width                                  35
      +doom-dashboard-ascii-banner-fn                   'salih/banner
      doom-theme                                        'distinguished
      highlight-indent-guides-method                    'bitmap

      ;; set org files
      +org-capture-journal-file                         "~/blog/content/stack.org"
      +org-capture-changelog-file                       "~/blog/content/nice.org"
      +org-capture-todo-file                            "~/roam/main/life.org"
      org-preview-html-viewer                           'xwidget
      org-directory                                     org-roam-directory
      org-id-locations-file                             "~/roam/.orgids"
      org-agenda-skip-scheduled-if-done                 nil
      org-use-tag-inheritance                           nil
      org-agenda-block-separator                        9472
      org-extend-today-until                            6
      org-element-use-cache                             nil


      ;; please don't stalk me
      user-full-name                                    "Salih Muhammed"
      user-mail-address                                 "larrasket@gmx.com"

      ;; I've no idea of any of this.
      org-crypt-key                                     user-mail-address
      epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-select-keys                              'silent
      epa-file-encrypt-to                               user-mail-address



      ;; prayer time
      calendar-latitude                                 30.0
      calendar-longitude                                31.2
      awqat-mode-line-format                            " 🕌 ${prayer} (${hours}h${minutes}m) "

      ;; school
      org-roam-directory                                (file-truename "~/roam")
      bibtex-completion-bibliography                    "~/configs/~s/ref.bib"
      bibtex-completion-library-path                    '("~/roam/source/")
      citar-bibliography                                bibtex-completion-bibliography
      bibtex-completion-notes-path                      "~/roam/reference/"

      ;; translate
      gts-translate-list                                '(("en" "ar"))
      gts-default-translator                            (gts-translator
                                                         :picker (gts-prompt-picker)
                                                         :engines (list (gts-google-engine))
                                                         :render (gts-buffer-render))

      ;; keyboard
      salih/prefix-global                               "C-x "
      salih/prefix-mode                                 "C-c "


      ;; browser
      browse-url-browser-function                       'eww-browse-url
      shr-inhibit-images                                nil

      ;; will be purged soon. "i don't want to play with you anymore"
      leetcode-prefer-language                          "cpp"
      leetcode-prefer-sql                               "mssql"
      leetcode-save-solutions                           t
      leetcode-directory                                "/home/l/gits/ps/lc"

      ;; other
      vertico-buffer-display-action                     '(display-buffer-same-window)
      large-file-warning-threshold                      nil
      inferior-lisp-program                             "sbcl"
      neo-mode-line-type                                'default
      consult-preview-key                               nil
      treemacs-position                                 'right
      dired-sidebar-refresh-on-special-commands         t
      display-line-numbers-type                         'visual
      doom-modeline-height                              17
      doom-modeline-buffer-state-icon                   nil
      doom-modeline-icon                                nil
      org-annotate-file-storage-file                    "~/configs/annotated.org"
      pdf-view-restore-filename                         "~/configs/~/.pdf-view-restore")


;; setup email
;; don't forget to setup authinfo
;; https://www.emacswiki.org/emacs/GnusAuthinfo
;; and of course mu, isync
(after! mu4e
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        mu4e-update-interval 200
        smtpmail-default-smtp-server "mail.gmx.com"
        smtpmail-smtp-server "mail.gmx.com"
        smtpmail-smtp-service 587
        smtpmail-starttls-credentials '(("mail.gmx.com" 465 nil nil))
        smtpmail-stream-type 'starttls
        mu4e-modeline-show-global nil))



;; this should be called after defining salih/prefix-global
(require '+handy)
(require '+bindings)
(require '+hooks)
