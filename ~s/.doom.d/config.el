;;; config.el -*- lexical-binding: t; -*-
(put 'org-download-image-dir 'safe-local-variable
     (lambda (val) (stringp val)))
(require '+early)
(setq salih/temp-roam-insert nil)
(setq user-full-name                                    "Salih Muhammed"
      user-mail-address                                 "root@lr0.org"
      user-first-name                                   "Salih"
      ;; TODO Commenting these temporarily until I get back to mu4e with iCloud.
      ;; user-stmp-server                                  "mail.gmx.com"
      ;; user-stmp-port                                    587
      user-short-username                               "lr0"
      user-config-repo-path                             "~/configs/~s"
      salih/blog-content-path                           "~/blog/content"
      salih/hugo-directory                              "~/roam/hugo/"
      org-roam-directory                                (file-truename "~/roam")
      doom-font                                         (font-spec
                                                         :family
                                                         "Pragmasevka"
                                                         :size 16)
      ;; kaolin-dark
      ;; doom-badger
      ;; kaolin-temple
      ;; [2026-01-25 Sun 00:45] If you go through the git blame, you will find
      ;; so many comments about choosing the right theme and me experiementing
      ;; with many themes. Analysising this now feels like reading a four
      ;; identity analysis of James Marcia. Anyway. I think I might have finally
      ;; passed all the stages, and finally became what I am. And I think I
      ;; found "the theme", it seems to be `ef-elea-dark'
      doom-theme                                        'ef-elea-dark
      doom-modeline-icon                                t
      doom-modeline-height                              32
      display-line-numbers-type                         'relative
      ;; org
      org-directory                                     org-roam-directory
      org-id-locations-file                             "~/roam/.orgids"
      +org-capture-changelog-file                       "~/blog/content/nice.org"
      +org-capture-journal-file                         "~/blog/content/stack.org"
      salih/org-roam-fleet-file                         "~/roam/main/lr.org"
      salih/org-vocal-store                             "~/roam/media/vocal"
      +org-capture-todo-file                            "~/roam/main/life.org"
      salih/vulpea-show-full                            nil
      salih/adding-note?                                nil
      salih/org-agenda-full-f                           nil
      ;; this option is useful when you are up after 00:00. set 0 to the value
      ;; you sleep at. if you sleep at 02:00 it should be 2, if you sleep at
      ;; 02:30 it should be 3 and so on. Org agenda for the day will not overlap
      ;; until your day is done.
      ;; [2024-08-07 Wed 19:43] currently I sleep at 07:00.
      ;; [2024-08-08 Wed 23:41] Not anymore.
      ;; [2025-09-20 Sat 00:02] Quite outdated information huh :)
      ;; [2026-01-01 Thu 03:01] Quite outdating outdated
      ;; org-extend-today-until                            2

      ;; other
      auto-save-no-message                              t
      dired-preview-delay                               0.1
      safe-local-variable-values
      '((org-download-image-dir
         . "../i")
        (salih/rebuild . t))


      ;; currently org causes some annoying warnings because of org-element
      ;; breaking API updates.
      ;; [2024-04-26 Fri 02:01] I wrote "currently" above a long time ago
      ;; (perhaps can be detected from the git history, too lazy tho). Not sure
      ;; if it is still the case
      ;; [2024-11-20 Wed 11:45] Let's try without it!
      ;; [2024-11-22 Fri 12:07] Works fine so far.
      ;; [2025-06-08 Sun 12:20] It's back!
      ;; [2025-06-27 Fri 20:41] https://github.com/org-noter/org-noter/issues/111
      ;; [2025-06-27 Fri 20:42] https://list.orgmode.org/87qzzfd7bf.fsf@localhost/T/#t
      ;; [2025-09-20 Sat 00:02] I cleaned my org config. Let's give that a try again.
      ;; [2026-02-19 Thu 20:03] apparently this will just keep happening as long as I use nov.el
      warning-minimum-level                             :error)






(setq epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-select-keys                               'silent
      epa-file-encrypt-to                                user-mail-address
      gac-debounce-interval                             200
      gac-silent-message-p                              t)



(require '+l-init)
(s/require
 '+bindings
 '+early)

(breadcrumb-mode)
(global-jinx-mode)
(yas-global-mode 1)
(display-battery-mode)
(salih/keyboard-config)
(consult-org-roam-mode 1)
(global-visual-line-mode 1)
(awqat-notification-mode 1)
(awqat-display-prayer-time-mode)

(custom-set-faces
 '(mode-line ((t (:family "Pragmasevka"))))
 '(mode-line-active ((t (:family "Pragmasevka"))))
 '(mode-line-inactive ((t (:family "Pragmasevka")))))

(set-popup-rules! '(("^\\*Project errors\\*" :size 0.25)))
(set-face-attribute 'shr-text nil :family "Optima" :height 180)

(setq modus-themes-common-palette-overrides
      '((fg-line-number-inactive bg-alt)
        (fg-line-number-active bg-alt)
        (bg-line-number-inactive unspecified)
        (bg-line-number-active unspecified)))

(custom-set-faces
 '(line-number ((t (:slant normal))))
 '(line-number-current-line ((t (:slant normal)))))

(setq modus-themes-italic-constructs t)
(setq modus-themes-bold-constructs nil)
(setq dired-preview-max-size (* 1024 1024 30))

(set-fringe-style '(2 . 0))


(setq org-hide-emphasis-markers t
      org-pretty-entities t
      org-agenda-tags-column 'auto
      org-ellipsis "…")


(setq evil-respect-visual-line-mode t)
(global-org-modern-mode -1)

(set-fringe-style '(1 . 1))
(setq evil-respect-visual-line-mode t)
;; Apheleia configuration for Scala with scalafmt

(defconst scalafmt-default-config
  "version = \"3.2.1\"
style = default
runner.dialect = scala213
docstrings.wrap = \"no\"
maxColumn = 140
rewrite.rules = [
  AvoidInfix
  RedundantBraces
  RedundantParens
  AsciiSortImports
  PreferCurlyFors
]
indent.main = 2
indent.defnSite = 2
indent.callSite = 2
indent.extendSite = 2
align.preset = more
rewrite.trailingCommas.style = keep
newlines.source = keep
"
  "Default scalafmt configuration for Scala 2.13 projects.")

(defun scalafmt-find-project-root ()
  "Find the project root directory for scalafmt."
  (or (locate-dominating-file default-directory ".scalafmt.conf")
      (locate-dominating-file default-directory "build.sbt")
      (locate-dominating-file default-directory ".git")
      (locate-dominating-file default-directory "project/build.properties")
      default-directory))

(defun scalafmt-ensure-config ()
  (let* ((project-root (scalafmt-find-project-root))
         (config-file (expand-file-name ".scalafmt.conf" project-root)))
    (unless (file-exists-p config-file)
      (with-temp-file config-file
        (insert scalafmt-default-config))
      (message "Created default .scalafmt.conf in %s" project-root))
    config-file))

;; Configure apheleia for Scala
(with-eval-after-load 'apheleia
  ;; Add scalafmt formatter - using a lambda to ensure config is created/found each time
  (setf (alist-get 'scalafmt apheleia-formatters)
        '("scalafmt" "--stdin" "--config" (eval (scalafmt-ensure-config))))
  
  ;; Register scalafmt for scala modes
  (setf (alist-get 'scala-mode apheleia-mode-alist) 'scalafmt)
  (setf (alist-get 'scala-ts-mode apheleia-mode-alist) 'scalafmt))

;; Enable apheleia-mode for Scala files
(add-hook 'scala-mode-hook #'apheleia-mode)
(add-hook 'scala-ts-mode-hook #'apheleia-mode)


;; Add this to your config BEFORE starting Eglot
;; (setq eglot-events-buffer-size 2000000)  ; Must be non-zero!
;; (setq eglot-events-buffer-config '(:size 2000000 :format full))

;; (add-to-list 'eglot-stay-out-of 'eldoc)

;; (defun my/eglot-scala-setup ()
;;   (setq-local eglot-ignored-server-capabilities
;;               '(:semanticTokensProvider  ; disable semantic tokens
;;                 :documentOnTypeFormattingProvider)))  ; disable format-on-type

;; (add-hook 'scala-ts-mode-hook #'my/eglot-scala-setup)
(defun metals-import-build ()
  "Import build by running sbt bloopInstall and reconnecting"
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (async-shell-command "sbt bloopInstall")
    (message "Running sbt bloopInstall... Reconnect Eglot when done.")))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


(setq doom-variable-pitch-font (font-spec :family "MS Gothic" :size 12))

;; TODO remove this when flycheck-golangci-lint is patched
(after! flycheck-golangci-lint
  (defun flycheck-golangci-lint--executable ()
    "Pick an executable to use"
    (or flycheck-golangci-lint-executable "golangci-lint"))


  (defun flycheck-golangci-lint--parse-version ()
    "Parse golangci-lint version from --version output.
Returns a list of (major minor patch) as integers, or nil if parsing fails."
    (unless flycheck-golangci-lint--version
      (let* ((output (ignore-errors
                       (with-temp-buffer
                         (call-process (flycheck-golangci-lint--executable) nil
                                       t nil "--version")
                         (buffer-string))))
             (version-regex
              "version \\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)"))
        (when (and output (string-match version-regex output))
          (setq flycheck-golangci-lint--version
                (list (string-to-number (match-string 1 output))
                      (string-to-number (match-string 2 output))
                      (string-to-number (match-string 3 output)))))))
    flycheck-golangci-lint--version)

  (defun flycheck-golangci-lint--output-format-flags ()
    "Return appropriate output format flags based on golangci-lint version.
v1.x uses --out-format=checkstyle
v2.x uses --output.checkstyle.path=stdout (without --output.text.path=stderr
which causes mixed output that breaks the checkstyle parser)."
    ;; v2.x: Use new format (without text output flag to avoid mixed output)
    (let ((version (flycheck-golangci-lint--parse-version)))
      (if (and version (>= (car version) 2))
          '("--output.checkstyle.path=stdout")
        "--path-mode=abs"
        ;; v1.x or fallback: Use legacy format
        '("--out-format=checkstyle")))))






(defun yaml-env-to-dotenv ()
  "Convert Helm-style YAML env entries into a .env file."
  (interactive)
  (save-excursion
    (goto-char (point-min))

    (unless (re-search-forward "^env:" nil t)
      (user-error "No env: section found"))

    (let ((env-vars '()))
      ;; Iterate over each env item
      (while (re-search-forward "^[[:space:]]*- name: \\(.+\\)$" nil t)
        (let* ((name (match-string 1))
               (start (point))
               (end (or (save-excursion
                          (when (re-search-forward "^[[:space:]]*- name:" nil t)
                            (match-beginning 0)))
                        (point-max)))
               value)

          (save-excursion
            (goto-char start)
            (cond
             ;; value: "foo"
             ((re-search-forward "^[[:space:]]+value: \\(\"?\\)\\(.+?\\)\\1$" end t)
              (setq value (match-string 2)))

             ;; valueFrom.secretKeyRef
             ((re-search-forward "^[[:space:]]+valueFrom:" end t)
              (let (secret key)
                (when (re-search-forward "^[[:space:]]+name: \\(.+\\)$" end t)
                  (setq secret (match-string 1)))
                (when (re-search-forward "^[[:space:]]+key: \\(.+\\)$" end t)
                  (setq key (match-string 1)))
                (when (and secret key)
                  (setq value (format "<secret:%s/%s>" secret key)))))))

          (when value
            (push (format "%s=%s" name value) env-vars))))

      ;; Save file
      (let ((file (read-file-name "Save .env file as: " nil nil nil ".env")))
        (with-temp-file file
          (insert (string-join (nreverse env-vars) "\n"))
          (insert "\n"))
        (message "Wrote %d env vars to %s"
                 (length env-vars) file)))))
































(add-hook! 'org-mode-hook (visual-fill-column-mode 1))




(defun salih/kill-all-org-buffers ()
  "Kill all buffers whose major mode is `org-mode`."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'org-mode)
        (kill-buffer buffer)))))

(add-hook! 'org-mode-hook (visual-line-fill-column-mode))

(defun salih/toggle-agenda-late ()
  (interactive)
  (if (eq org-extend-today-until 9)
      (setq org-extend-today-until 0)
    (setq org-extend-today-until 9))
  (salih/org-agenda-no-full-f))

(add-hook! 'org-noter-doc-mode-hook (breadcrumb-local-mode -1))
(add-hook! 'nov-mode-hook (breadcrumb-local-mode -1))


;; (add-hook! org-noter-doc-mode-hook (breadcrumb-local-mode -1))




(defun salih/org-open-file-link-in-macos ()
  "Open the Org file link at point using macOS `open`."
  (interactive)
  (let* ((context (org-element-context)))
    (if (and (eq (org-element-type context) 'link)
             (string= (org-element-property :type context) "file"))
        (let* ((raw-path (org-element-property :path context))
               (full-path (expand-file-name raw-path)))
          (start-process "macos-open" nil "open" full-path)
          (message "Opened: %s" full-path))
      (message "Not on a file link."))))

(require 'nov-consult)


