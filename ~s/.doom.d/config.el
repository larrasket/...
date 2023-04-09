(add-to-list 'load-path "~/.doom.d/")
(add-to-list 'org-agenda-files "~/roam/journal/agenda/todo.org")
(add-to-list 'org-agenda-files "~/roam/journal/agenda/births.org")
(add-to-list 'doom-emoji-fallback-font-families "Symbola")
(require 'epa-file)
(require 'erc-services)
(require 'go-translate)


(setq-default frame-title-format '("%b")
              bidi-paragraph-direction 'left-to-right)

(setq load-prefer-newer                                 t ;; avoid warnings
      ;; emacs settings
      completion-ignore-case                            t
      doom-font                                         (font-spec :family "PragmataPro" :size 12)
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
      ;; org-extend-today-until                            2


      ;; please don't stalk me
      user-full-name                                    "Salih Muhammed"
      user-mail-address                                 "salhghd7@gmail.com"

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
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-starttls-credentials '(("smtp.gmail.com" "587" nil nil))
        smtpmail-stream-type 'starttls))


(after! solaire-mode
  (setq solaire-mode-real-buffer-fn #'salih/solaire-mode-real-buffer-custom-p))


(after! sly
  (setq sly-complete-symbol-function 'sly-flex-completions))

;; this should be called after defining salih/prefix-global
(require '+handy)
(require 'keys)

(add-hook 'prog-mode-hook (lambda ()
                            (highltier)
                            (column-enforce-mode)
                            (auto-fill-mode)
                            (setq-default indent-tabs-mode nil)))

(add-hook 'after-init-hook   #'global-flycheck-mode)
(add-hook 'nov-mode-hook     #'salih/make-buffer-white)
(add-hook 'csv-mode-hook     #'csv-align-mode)
(add-hook 'neotree-mode-hook (lambda () (doom-modeline 1) (solaire-mode -1)))
(add-hook 'sly-mrepl-mode (lambda () (doom-modeline-mode 1)))
(add-hook 'lisp-mode-hook    #'rainbow-delimiters-mode)
(add-hook 'yas-minor-mode(lambda() (yas-activate-extra-mode 'fundamental-mode)))
(add-hook 'dired-mode-hook(lambda () (solaire-mode -1) (org-download-enable)))

(add-hook 'org-mode-hook     (lambda ()
                               (display-line-numbers-mode -1)
                               (setq truncate-lines 1)))

(epa-file-enable)
(yas-global-mode 1)
(erc-spelling-mode)
(global-wakatime-mode)
(vertico-buffer-mode)
(salih/consult-preview-at-point)

(add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)


(add-hook 'after-init-hook #'mu4e)


(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 0.9))))
 '(org-level-6 ((t (:inherit outline-6 :height 0.8))))
 '(org-level-7 ((t (:inherit outline-7 :height 0.7))))
 '(org-level-8 ((t (:inherit outline-8 :height 0.6))))
 '(org-document-title ((t (:weight bold :height 1.5)))))
