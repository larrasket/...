(add-to-list 'load-path "~/.doom.d/")
(add-to-list 'org-agenda-files "~/roam/main/life.org")
(add-to-list 'doom-emoji-fallback-font-families "Symbola")
(require 'epa-file)
(require 'erc-services)
(require 'go-translate)


(setq-default frame-title-format '("%b")
              bidi-paragraph-direction 'left-to-right)

(setq load-prefer-newer                                 t ;; avoid warnings
      ;; emacs settings
      completion-ignore-case                            t
      doom-font                                         (font-spec :family "PragmataPro Mono Liga" :size 12)
      all-the-icons-color-icons                         nil
      neo-theme                                         'icons
      neo-window-width                                  35
      bidi-paragraph-direction                          'left-to-right


      ;; set org files
      +org-capture-journal-file                         "~/blog/content/stack.org"
      +org-capture-changelog-file                       "~/blog/content/nice.org"
      +org-capture-todo-file                            "~/roam/main/life.org"
      org-preview-html-viewer                           'xwidget
      org-roam-directory                                "~/roam"
      org-directory                                     org-roam-directory
      org-id-locations-file                             "~/roam/.orgids"
      org-agenda-skip-scheduled-if-done                 t
      org-download-image-dir                            "~/roam/media"
      ;; org-extend-today-until                            2


      ;; please don't stalk me
      user-full-name                                    "Salih Muhammed"
      user-mail-address                                 "larrasket@bk.ru"

      ;; I've no idea of any of this.
      org-crypt-key                                     user-mail-address
      epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-select-keys                              'silent
      epa-file-encrypt-to                               user-mail-address


      ;; skull welecome in emacs
      fancy-splash-image                                "~/.doom.d/pan.png"

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


      ;; irc
      ;; for the options `erc-prompt-for-password' and
      ;; `erc-prompt-for-nickserv-password', you should have a .authinfo file
      ;; conatins your password for the nickname. For Example, using
      ;; yournickname and yourpassword as a NickName and password (repsectively
      ;; machine irc.libera.chat login yournickname password yourpassword
      ;; See. https://www.gnu.org/software/emacs/manual/html_node/emacs/Authentication.html
      erc-nick                                          "jahiz"
      erc-user-full-name                                user-full-name
      erc-prompt-for-password                           nil
      erc-prompt-for-nickserv-password                  nil
      erc-autojoin-channels-alist                       '(("irc.libera.chat"
                                                           "##arabic"))
      leetcode-prefer-language                          "cpp"
      leetcode-prefer-sql                               "mssql"
      leetcode-save-solutions                           t
      leetcode-directory                                "/home/l/gits/ps/lc"

      ;; fix time format
      system-time-locale                                "C"

      ;; other
      vertico-buffer-display-action                     '(display-buffer-same-window)
      browse-url-generic-program                        "chromium"
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
      gts-default-translator                            (gts-translator
                                                         :picker (gts-prompt-picker)
                                                         :engines (list (gts-google-engine))
                                                         :render (gts-buffer-render))
      org-annotate-file-storage-file                    "~/configs/annotated.org")


;; setup email
;; don't forget to setup authinfo
;; https://www.emacswiki.org/emacs/GnusAuthinfo
;; and of course mu, isync
(after! mu4e
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        mu4e-update-interval 200
        smtpmail-default-smtp-server "smtp.mail.ru"
        smtpmail-smtp-server "smtp.mail.ru"
        smtpmail-smtp-service 465
        smtpmail-starttls-credentials '(("smtp.mail.ru" 465 nil nil))
        smtpmail-stream-type 'ssl
        mu4e-modeline-show-global nil))



;; this should be called after defining salih/prefix-global
(require '+handy)
(require '+bindings)
(require '+hooks)

