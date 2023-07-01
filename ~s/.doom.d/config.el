(add-to-list 'load-path "~/.doom.d/")
(add-to-list 'org-agenda-files "~/roam/main/life.org")
(add-to-list 'doom-emoji-fallback-font-families "Symbola")
(require 'epa-file)
(require 'go-translate)


(setq-default frame-title-format                        '("%b")
              bidi-paragraph-direction                  'left-to-right
              org-download-image-dir                    "~/roam/media")

(defvar IS-PLASMA (let ((output (shell-command-to-string "pgrep -x plasmashell")))
                    (not (string-blank-p output))))

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
      doom-theme                                        (if IS-PLASMA
                                                            'doom-monokai-spectrum
                                                          'distinguished)
      highlight-indent-guides-method                    'bitmap
      display-line-numbers-type                         'visual
      doom-modeline-height                              17
      doom-modeline-buffer-state-icon                   nil
      doom-modeline-icon                                nil
      treemacs-position                                 'right



      ;; set org files
      +org-capture-journal-file                         "~/blog/content/stack.org"
      +org-capture-changelog-file                       "~/blog/content/nice.org"
      +org-capture-todo-file                            "~/roam/main/life.org"
      org-directory                                     org-roam-directory
      org-id-locations-file                             "~/roam/.orgids"
      org-agenda-skip-scheduled-if-done                 nil
      org-use-tag-inheritance                           t
      org-agenda-block-separator                        9472
      org-clock-mode-line-total                         'today
      ;; org-extend-today-until                            6
      org-element-use-cache                             t
      org-noter-auto-save-last-location                 t
      org-startup-folded                                t
      org-image-actual-width                            600
      org-link-file-path-type                           'relative
      org-tag-alist                                     '((:startgroup)
                                                          ("@personal" . nil)
                                                          (:grouptags)

                                                          ("@read" . ?r)
                                                          ("@school" . ?s)
                                                          ("@programming" . ?p)
                                                          ("@idea" . ?i)
                                                          ("@write" . ?w)
                                                          ("@check" . ?c)
                                                          ("@watch" . ?W)
                                                          (:endgroup)
                                                          (:startgroup)
                                                          ("@tinker" . nil)
                                                          (:grouptags)
                                                          ("@emacs" . ?e) ("@blog" . ?b)
                                                          (:endgroup)

                                                          (:startgroup)
                                                          ("@nothing" . nil)
                                                          (:grouptags)
                                                          ("@chill" . ?C)
                                                          ("@lost" . ?l)
                                                          ("@people" . nil)
                                                          (:endgroup)
                                                          ;; some topics off-groups
                                                          ("@politics" . nil)
                                                          ("@philosophy" . nil)
                                                          ("@history" . nil)
                                                          ("@science" . nil))

      ;; please don't stalk me
      user-full-name                                    "Salih Muhammed"
      user-mail-address                                 "lr0@gmx.com"

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
      browse-url-browser-function                       'xwidget-webkit-browse-url
      shr-inhibit-images                                nil

      ;; vertico
      vertico-buffer-display-action                     '(display-buffer-same-window)
      enable-recursive-minibuffers                      nil

      ;; git-auto-commit-mode
      gac-debounce-interval                             200
      gac-silent-message-p                              t

      ;; other
      large-file-warning-threshold                      nil
      inferior-lisp-program                             "sbcl"
      neo-mode-line-type                                'default
      consult-preview-key                               nil
      dired-sidebar-refresh-on-special-commands         t
      org-annotate-file-storage-file                    "~/configs/annotated.org"
      pdf-view-restore-filename                         "~/configs/~/.pdf-view-restore")


;; setup email
;; don't forget to setup authinfo
;; https://www.emacswiki.org/emacs/GnusAuthinfo
;; and of course mu, isync
;; tidy is required to use with xwidget:
;; pacman -S tidy
(after! mu4e
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        mu4e-update-interval 200
        smtpmail-default-smtp-server "mail.gmx.com"
        smtpmail-smtp-server "mail.gmx.com"
        smtpmail-smtp-service 587
        smtpmail-starttls-credentials '(("mail.gmx.com" 465 nil nil))
        smtpmail-stream-type 'starttls
        mu4e-modeline-show-global nil)

  (defun remove-file-prefix (url)
    (replace-regexp-in-string "^file://" "" url))

  (defun mu4e-action-view-in-xwidget (msg)
    (unless (fboundp 'xwidget-webkit-browse-url)
      (mu4e-error "No xwidget support available"))
    (let ((browse-url-handlers nil)
          (browse-url-browser-function (lambda (url &optional _rest)
                                         (with-output-to-string (call-process "tidy" nil nil nil "-m" "--numeric-entities" "yes" (remove-file-prefix url)))
                                         (xwidget-webkit-browse-url url))))
      (mu4e-action-view-in-browser msg))))





;; this should be called after defining salih/prefix-global
(load "+helper.el")
(load "+bindings.el")
(load "+hooks.el")
