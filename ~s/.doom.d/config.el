(add-to-list 'load-path "~/.doom.d/lisp/")
(add-to-list 'org-agenda-files "~/roam/main/life.org")
(add-to-list 'doom-emoji-fallback-font-families "Symbola")

(require 'awqat)                        ; for praer support in the agenda
(require 'embark)                       ; for embark action `+helper` specifications
(require 'vulpea)                       ; org-roam project tasks in org-agenda
(require 'epa-file)                     ; for encryption function in `+helper`
(require 'elfeed-tube)                  ; for reviewing youtube feeds in elfeed
(require 'auth-source)                  ; required for encryption support
(require 'go-translate)                 ; define trnaslation engine in config.el
(require 'org-inlinetask)               ; enable org inline tasks
(require 'org-roam-protocol)            ; enable org-roam note taking from the browser
(require 'highlight-indent-guides)      ; enables indent guide



(setq-default frame-title-format                        '("%b")
              bidi-paragraph-direction                  'left-to-right
              org-download-image-dir                    "~/roam/media"
              indent-tabs-mode                          nil
              highlight-indent-guides-auto-enabled      nil
              pdf-view-display-size                     'fit-width)

(defvar IS-PLASMA (let ((output (shell-command-to-string "pgrep -x plasmashell")))
                    (not (string-blank-p output))))

(setq load-prefer-newer                                 t ;; avoid warnings
      ;; emacs settings
      completion-ignore-case                            t
      bidi-paragraph-direction                          'left-to-right

      ;; appearance
      ;; font `:size` value of 29 is prefect for filming
      doom-font                                         (font-spec :family "PragmataPro Mono Liga" :size 12)
      all-the-icons-color-icons                         nil
      neo-theme                                         'icons
      neo-window-width                                  35
      +doom-dashboard-ascii-banner-fn                   'salih/banner
      doom-theme                                        (if IS-PLASMA
                                                            'doom-monokai-spectrum
                                                          'doom-ir-black)
      highlight-indent-guides-method                    'bitmap
      display-line-numbers-type                         'visual
      doom-modeline-height                              17
      doom-modeline-buffer-state-icon                   nil
      doom-modeline-icon                                nil
      treemacs-position                                 'right
      fancy-splash-image                                "~/configs/~s/assets/chomsky.png"



      ;; set org files
      +org-capture-journal-file                         "~/blog/content/stack.org"
      +org-capture-changelog-file                       "~/blog/content/nice.org"
      +org-capture-todo-file                            "~/roam/main/life.org"
      org-bullets-bullet-list                           '("◉" "✸" "✿" "♥" "●")
      org-id-method                                     'org
      org-directory                                     org-roam-directory
      org-id-locations-file                             "~/roam/.orgids"
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
      org-startup-folded                                t
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

      salih/source-directory                             "~/roam/references/source/"
      salih/books                                       (let (file-list)
                                                          (dolist (file (directory-files-recursively salih/source-directory "" nil t))
                                                            (push file file-list))
                                                          file-list)

      org-roam-directory                                (file-truename "~/roam")
      bibtex-completion-library-path                    (list salih/source-directory)
      bibtex-completion-notes-path                      "~/roam/reference/"
      bibtex-completion-bibliography                    "/home/l/configs/~s/ref.bib"
      org-cite-global-bibliography                      (list bibtex-completion-bibliography)
      org-cite-csl-styles-dir                           "/home/l/configs/~s/assets/csl"
      citar-bibliography                                bibtex-completion-bibliography
      org-cite-csl--fallback-style-file                 (expand-file-name "chicago-ibid.csl" org-cite-csl-styles-dir)

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
      centaur-tabs-set-icons                            t
      centaur-tabs-plain-icons                          t
      centaur-tabs-set-modified-marker                  t
      centaur-tabs-close-button                         "✕"
      centaur-tabs-modified-marker                      "•"
      centaur-tabs-cycle-scope                          'tabs

      ;; other
      company-idle-delay                                0.3
      salih/temp-roam-insert                            nil
      large-file-warning-threshold                      nil
      save-place-ignore-files-regexp                    "\\(?:COMMIT_EDITMSG\\|hg-editor-[[:alnum:]]+\\.txt\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\|\\.pdf\\)$"
      inferior-lisp-program                             "sbcl"
      neo-mode-line-type                                'default
      dired-sidebar-refresh-on-special-commands         t
      org-annotate-file-storage-file                    "~/configs/annotated.org"
      bmkp-last-as-first-bookmark-file                  "/home/ghd/.emacs.d/.local/etc/bookmarks"
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
        mu4e-compose-reply-ignore-address '("no-?reply" "lr0@gmx.com")
        mu4e-update-interval 200
        mu4e-compose-signature "Regards,\nSalih"
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
      (mu4e-action-view-in-browser msg)))


  (defun salih/delete-citation ()
   (delete-region (point) (point-max)))

  (defun salih/mu4e-reply (prefix)
    (interactive "P")
    (setq mu4e-compose-cite-function #'salih/delete-citation)
    (mu4e-compose-reply))

  (define-key mu4e-view-mode-map    (salih/mode "C-r") #'salih/mu4e-reply)
  (define-key mu4e-headers-mode-map (salih/mode "C-r") #'salih/mu4e-reply))
 


;; this should be called after defining salih/prefix-global

(require '+helper)
(require '+hooks)
(require '+feeds)
(require '+bindings)
(require '+org-tags)
