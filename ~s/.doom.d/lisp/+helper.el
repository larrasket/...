;;; configs/~s/.doom.d/+helper.el -*- lexical-binding: t; -*-

;; basic definiton for `+bindings`
(defun salih/global (key-sequence)
  (kbd (concat salih/prefix-global key-sequence)))
(defun salih/mode (key-sequence)
  (kbd (concat salih/prefix-mode   key-sequence)))


;; fix evil C-g methods
(defun salih/evil-escape-and-abort-company ()
  (interactive)
  (company-abort)
  (evil-escape))

(defun quit-it ()
  (if (and evil-mode (eq evil-state 'insert))
      (evil-force-normal-state)
    (keyboard-quit)))

(defun evil-keyboard-quit ()
  "Keyboard quit and force normal state."
  (and evil-mode (evil-force-normal-state))
  (keyboard-quit))

;; org archive
(defun salih/org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(defun salih/org-archive-killed-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/KILL" 'file))

;; chess
(defun salih/chess-notation-to-symbols ()
  (interactive)
  (let ((piece-symbols '((?K . "🨀")
                         (?Q . "🨁")
                         (?R . "🨂")
                         (?B . "🨃")
                         (?N . "🨄")
                         (?P . "🨅"))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(K\\|Q\\|R\\|B\\|N\\|P\\)[a-h][1-8]"
                                (point-max) t)
        (let ((piece (string-to-char (match-string 1)))
              (destination (match-string 0)))
          (replace-match (concat (cdr (assoc piece piece-symbols))
                                 (substring destination 1))
                         t t))))))

(defun chess-notation-to-symbols-region (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (salih/chess-notation-to-symbols))))

;; bidi support
(defun salih/bidi-direction-toggle ()
  (interactive "")
  (setq bidi-display-reordering t)
  (if (equal bidi-paragraph-direction 'right-to-left)
      (setq bidi-paragraph-direction 'left-to-right)
    (setq bidi-paragraph-direction 'right-to-left))
  (message "%s" bidi-paragraph-direction))

;; window management
(defun salih/toggle-maximize-buffer ()
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

;; neotree
(defun neotree-project-dir ()
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

;; editing
(defun salih/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active
region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (forward-line)))

(defun salih/rename-or-iedit ()
  "If current buffer is in lsp-mode, call lsp-rename. Otherwise, call
iedit-mode."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (call-interactively #'lsp-rename)
    (call-interactively #'iedit-mode)))

(defun salih/find-definition-or-lookup ()
  (interactive)
  "If current buffer is in lsp-mode, call lsp-find-definition. Otherwise, call
lookup."
  (if (bound-and-true-p lsp-mode)
      (call-interactively #'lsp-find-definition)
    (call-interactively #'+lookup/file)))

(defun salih/insert-now-timestamp()
  (interactive)
  (org-insert-time-stamp (current-time) t))

;; dired
(defun salih/open-in-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
When called in emacs lisp, if @fname is given, open that.
URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-04 2021-02-16"
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (shell-command (concat
                           "PowerShell -Command \"Invoke-Item -LiteralPath\" "
                           "'"
                           (shell-quote-argument
                            (expand-file-name $fpath )) "'")))
         $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                       (start-process "" nil "xdg-open" $fpath))) $file-list))))))

;; compile-and-run methods
(defun salih/compile-and-run-cpp ()
  (interactive)
  (save-buffer)
  (compile (concat "g++ "  (file-name-nondirectory (buffer-file-name)) " -o "
                   (file-name-sans-extension   (file-name-nondirectory
                                                (buffer-file-name))) " && ./"
                   (file-name-sans-extension  (file-name-nondirectory
                                               (buffer-file-name))) " && rm "
                   (file-name-sans-extension  (file-name-nondirectory
                                               (buffer-file-name)))) t  )
  (other-window t)
  (end-of-add-hook 'c++-mode))


(defun salih/compile-and-run-c ()
  (interactive)
  (save-buffer)
  (compile (concat "gcc "  (file-name-nondirectory (buffer-file-name)) " -o "
                   (file-name-sans-extension   (file-name-nondirectory
                                                (buffer-file-name))) " && ./"
                   (file-name-sans-extension  (file-name-nondirectory
                                               (buffer-file-name))) " && rm "
                   (file-name-sans-extension  (file-name-nondirectory
                                               (buffer-file-name)))) t  )
  (other-window t)
  (end-of-add-hook 'c-mode))

(defun salih/make-c ()
  (interactive)
  (save-buffer)
  (compile "make")
  (other-window t)
  (end-of-add-hook 'c-mode))

(defun salih/compile-and-run-csharp ()
  (interactive)
  (save-buffer)
  (compile (concat "dotnet run") t  ) (other-window t)
  (end-of-add-hook 'csharp-mode))

(defun salih/compile-and-run-go-project ()
  (interactive)
  (save-buffer)
  (compile
   (concat "go run .") t)
  (other-window t)
  (end-of-add-hook 'go-mode))

(defun salih/compile-and-run-go-file ()
  (interactive)
  (save-buffer)
  (compile (concat "go run "  (file-name-nondirectory (buffer-file-name))) t)
  (other-window t)
  (end-of-add-hook 'go-mode))

;; school
(defun salih/open-book ()
  "Search for a file in ~/me and open it."
  (interactive)
  (let ((default-directory (concat salih/source-directory "/")))
    (call-interactively 'find-file)))

;; let's hope for the best
(defun salih/epa-encrypt-file (recipients)
  "Encrypt the currently opened file for RECIPIENTS and delete the original."
  (interactive
   (list (epa-select-keys (epg-make-context epa-protocol)
                          "Select recipients for encryption. If no one is selected, symmetric encryption will be performed.")))
  (let* ((file (buffer-file-name))
         (cipher (concat file
                         (if (eq epa-protocol 'OpenPGP)
                             (if epa-armor ".asc" ".gpg")
                           ".p7m")))
         (context (epg-make-context epa-protocol)))
    (setf (epg-context-armor context) epa-armor)
    (setf (epg-context-textmode context) epa-textmode)
    (epg-context-set-passphrase-callback context
					 #'epa-passphrase-callback-function)
    (epg-context-set-progress-callback context
				       (cons
					#'epa-progress-callback-function
					(format "Encrypting %s..."
                                                (file-name-nondirectory file))))
    (message "Encrypting %s..." (file-name-nondirectory file))
    (condition-case error
	(epg-encrypt-file context file recipients cipher)
      (error
       (epa-display-error context)
       (signal (car error) (cdr error))))
    (delete-file file)
    (message "Encrypting %s...wrote %s and deleted original file"
	     (file-name-nondirectory file)
	     (file-name-nondirectory cipher))))

(defun salih/epa-dired-do-encrypt ()
  "Encrypt marked files and delete the originals."
  (interactive)
  (let ((recipients (epa-select-keys (epg-make-context) "Select recipients for encryption.
If no one is selected, symmetric encryption will be performed.  ")))
    (dolist (file (dired-get-marked-files))
      (with-current-buffer (find-file-noselect file)
	(salih/epa-encrypt-file recipients)))
    (revert-buffer)))

;; other handy stuff
(with-eval-after-load 'embark
  (add-hook 'embark-collect-mode-hook  #'salih/consult-preview-at-point-mode))

(define-minor-mode salih/consult-preview-at-point-mode
  "Preview minor mode for an *Embark Collect* buffer.
When moving around in the *Embark Collect* buffer, the candidate at point is
automatically previewed."
  :init-value nil :group 'consult
  (if salih/consult-preview-at-point-mode
      (add-hook 'post-command-hook #'salih/consult-preview-at-point nil 'local)
    (remove-hook 'post-command-hook #'salih/consult-preview-at-point 'local)))

(defun salih/consult-preview-at-point ()
  "Preview candidate at point in an *Embark Collect* buffer."
  (interactive)
  (let ((display-buffer-base-action '(display-buffer-pop-up-window))
        (cbuf (current-buffer))
        (node))
    ;; Avoid pushing the button created by Embark.  For some reason, some
    ;; candidates lead to a org-roam-node-find prompt and create a new frame.
    (if (setq node (get-text-property (point) 'node))
        ;; `org-roam-node-visit' does not return the buffer visited
        (progn
          (unless (featurep 'org-roam)(require 'org-roam))
          (org-roam-node-visit node :other-window)
          (switch-to-buffer-other-window cbuf))
      (push-button))))

(defun salih/xwidget-open-html ()
  "Open the current buffer's file path in an xwidget window."
  (interactive)
  (add-hook 'after-save-hook 'xwidget-webkit-reload nil t)
  (let ((file-path (buffer-file-name)))
    (when file-path
      (let ((xwidget (xwidget-webkit-browse-url (concat "file://" file-path))))
        (message "Opened file %s in an xwidget window." file-path)))))

(use-package! awqat
  :commands (awqat-display-prayer-time-mode
             awqat-times-for-day))

(defun salih/banner ()
  (let* ((banner '("       d8888                                     8888888888       888    d8b      "
                   "      d88888                                     888              888    Y8P      "
                   "     d88P888                                     888              888             "
                   "    d88P 888 88888b.d88b.   .d88b.  888d888      8888888  8888b.  888888 888      "
                   "   d88P  888 888 \"888 \"88b d88\"\"88b 888P\"        888         \"88b 888    888      "
                   "  d88P   888 888  888  888 888  888 888          888     .d888888 888    888      "
                   " d8888888888 888  888  888 Y88..88P 888          888     888  888 Y88b.  888      "
                   "d88P     888 888  888  888  \"Y88P\"  888          888     \"Y888888  \"Y888 888      "
                   ""
                   ""
                   ""
                   ""))

         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

;; disable spaces and icons in dashboard
(defun doom-dashboard-widget-shortmenu ()
  (let ((all-the-icons-scale-factor 1.45)
        (all-the-icons-default-adjust -0.02))
    (insert "\n")
    (dolist (section +doom-dashboard-menu-sections)
      (cl-destructuring-bind (label &key icon action when face key) section
        (when (and (fboundp action)
                   (or (null when)
                       (eval when t)))
          (insert
           (+doom-dashboard--center
            (- +doom-dashboard--width 1)
            (let ((icon (if (stringp icon) icon (eval icon t))))
              (format (format "%s%%s%%-10s" (if icon "%3s\t" "%3s"))
                      (or icon "")
                      (with-temp-buffer
                        (insert-text-button
                         label
                         'action
                         `(lambda (_)
                            (call-interactively (or (command-remapping #',action)
                                                    #',action)))
                         'face (or face 'doom-dashboard-menu-title)
                         'follow-link t
                         'help-echo
                         (format "%s (%s)" label
                                 (propertize (symbol-name action) 'face
                                             'doom-dashboard-menu-desc)))
                        (format "%-37s" (buffer-string)))
                      ;; Lookup command keys dynamically
                      (propertize
                       (or key
                           (when-let*
                               ((keymaps
                                 (delq
                                  nil (list (when
                                                (bound-and-true-p
                                                 evil-local-mode)
                                              (evil-get-auxiliary-keymap
                                               +doom-dashboard-mode-map 'normal))
                                            +doom-dashboard-mode-map)))
                                (key
                                 (or (when keymaps
                                       (where-is-internal action keymaps t))
                                     (where-is-internal action nil t))))
                             (with-temp-buffer
                               (save-excursion (insert (key-description key)))
                               (while (re-search-forward "<\\([^>]+\\)>" nil t)
                                 (let ((str (match-string 1)))
                                   (replace-match
                                    (upcase (if (< (length str) 3)
                                                str
                                              (substring str 0 3))))))
                               (buffer-string)))
                           "")
                       'face 'doom-dashboard-menu-desc))))
           "\n"))))))

(setq +doom-dashboard-menu-sections
      '(("Reload last session"
         :when (cond ((modulep! :ui workspaces)
                      (file-exists-p (expand-file-name persp-auto-save-fname
                                                       persp-save-dir)))
                     ((require 'desktop nil t)
                      (file-exists-p (desktop-full-file-name))))
         :face (:inherit (doom-dashboard-menu-title bold))
         :action doom/quickload-session)
        ("Open org-agenda"
         :when (fboundp 'org-agenda)
         :action org-agenda)
        ("Recently opened files"
         :action recentf-open-files)
        ("Open project"
         :action projectile-switch-project)
        ("Jump to bookmark"
         :action bookmark-jump)
        ("Open private configuration"
         :when (file-directory-p doom-user-dir)
         :action doom/open-private-config)
        ("Open documentation"
         :action doom/help)))

(defun salih/org-remove-all-tags ()
  "Remove all tags from all headlines in the current Org mode buffer."
  (interactive)
  (org-map-entries (lambda () (org-set-tags nil))))

(defun salih/make-nov-white ()
  (interactive)
  (setq buffer-face-mode-face `(:background "white"
                                :foreground "black"
                                :family "Roboto Condensed" :height 1.0))
  (face-remap-add-relative 'hl-line :background "#e6e6e6")
  (face-remap-add-relative 'link :foreground "blue")
  (buffer-face-mode t))

(defun salih/solaire-mode-real-buffer-custom-p ()
  "Return t if the current buffer is the dashboard or scratch, or is a real
(file-visiting) buffer."
  (cond ((string-prefix-p "*sly-mrepl for sbcl*" (buffer-name
                                                  (buffer-base-buffer)) ) t)
        ((string-prefix-p "*eshell*" (buffer-name (buffer-base-buffer)) ) t)
        ((string-prefix-p "*julia" (buffer-name (buffer-base-buffer)) ) t)
        ((string-prefix-p "*doom*" (buffer-name (buffer-base-buffer)) ) t)
        ((buffer-file-name (buffer-base-buffer)) t)
        (t nil)))

(defun centaur-tabs-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode'
`emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
  (list
   (cond
    ((or (string-equal "*" (substring (buffer-name) 0 1))
         (memq major-mode '(magit-process-mode
                            magit-status-mode
                            magit-diff-mode
                            magit-log-mode
                            magit-file-mode
                            magit-blob-mode
                            magit-blame-mode))) "Emacs")

    ((derived-mode-p 'prog-mode) "Editing")
    ((derived-mode-p 'dired-mode) "Dired")


    ((memq major-mode '(helpful-mode help-mode)) "Help")

    ((memq major-mode '(erc-mode)) "Erc")


    ((memq major-mode '(eshell-mode)) "eshell")



    ((memq major-mode '(pdf-view-mode nov-mode doc-view-mode)) "PDF")

    ((memq major-mode '(mu4e-view-mode
                        mu4e-headers-mode
                        mu4e-main-mode
                        mu4e-org-mode
                        mu4e-compose-mode
                        mu4e-loading-mode
                        mu4e-raw-view-mode)) "Mu4eMode")

    ((memq major-mode '(org-mode
                        org-agenda-clockreport-mode
                        org-src-mode
                        org-agenda-mode
                        org-beamer-mode
                        org-indent-mode
                        org-bullets-mode
                        org-cdlatex-mode
                        org-agenda-log-mode
                        diary-mode)) "OrgMode")
    (t (centaur-tabs-get-group-name (current-buffer))))))

(defun vulpea-project-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (seq-find                                 ; (3)
   (lambda (type)
     (or (eq type 'todo)))
   (org-element-map                         ; (2)
       (org-element-parse-buffer 'headline) ; (1)
       'headline
     (lambda (h)
       (org-element-property :todo-type h)))))

(defun vulpea-project-done-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (seq-find                                 ; (3)
   (lambda (type)
     (or (eq type 'done)))
   (org-element-map                         ; (2)
       (org-element-parse-buffer 'headline) ; (1)
       'headline
     (lambda (h)
       (org-element-property :todo-type h)))))


(defun vulpea-project-update-tag ()
  "Update PROJECT tag in the current buffer."
  (when (and (not (active-minibuffer-window))
             (vulpea-buffer-p))
    (save-excursion
      (goto-char (point-min))
      (let* ((tags (vulpea-buffer-tags-get))
             (original-tags tags))


        (cond ((vulpea-project-p) (and (or (setq tags (cons "project" tags)) t)
                                       (setq tags (remove "project_archived" tags))))

              ((vulpea-project-done-p) (and (or (setq tags (cons "project_archived" tags)) t)
                                       (setq tags (remove "project" tags))))
              (t (and (or (setq tags (remove "project" tags)) t)
                      (or (setq tags (remove "project_archived" tags)) t))))

        ;; cleanup duplicates
        (setq tags (seq-uniq tags))

        ;; update tags if changed
        (when (or (seq-difference tags original-tags)
                  (seq-difference original-tags tags))
          (apply #'vulpea-buffer-tags-set tags))))))

(defun vulpea-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(defun vulpea-project-files ()
  "Return a list of note files containing 'project' tag." ;
  (if salih/vulpea-show-full
      (vulpea-project-files-full)
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (like tag (quote "%\"project\"%"))])))))

(defun vulpea-project-files-full ()
  "Return a list of note files containing 'project' tag." ;
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (or (like tag (quote "%\"project\"%"))
                 (like tag (quote "%\"project_archived\"%")))]))))


(defun vulpea-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (vulpea-project-files)))

(defun salih/org-id-get-create-with-custom-id ()
  (interactive)
  (when (org-before-first-heading-p)
    (user-error "Not inside a heading"))
  (let* ((org-id (org-id-get))
         (custom-id-property "CUSTOM_ID"))

    (unless org-id
      (setq org-id (org-id-new))
      (org-entry-put nil "ID" org-id)
      (org-entry-put nil custom-id-property org-id))
    org-id))

(defun salih/eshell-load-bash-aliases ()
  "Read Bash aliases and add them to the list of eshell aliases."
  ;; Bash needs to be run - temporarily - interactively
  ;; in order to get the list of aliases.
  (with-temp-buffer
    (call-process "bash" nil '(t nil) nil "-ci" "alias")
    (goto-char (point-min))
    (while (re-search-forward "alias \\(.+\\)='\\(.+\\)'$" nil t)
      (eshell/alias (match-string 1) (match-string 2)))))

(defun salih/xwidget-open-with-clipboard ()
  (interactive)
  (xwidget-webkit-browse-url (current-kill 0 t)))

(defun salih/open-agenda ()
  (interactive)
  (org-agenda-remove-restriction-lock)
  (org-agenda nil "v"))

(defun salih/get-mail-password ()
  (interactive)
  (let* ((auth-info (auth-source-search :host "mail.gmx.com"
                                        :require '(:user :secret)))
         (password (funcall (plist-get (car auth-info) :secret))))
    password))

(defun salih/keyboard-config ()
  (when (display-graphic-p)
    (keyboard-translate ?\C-m ?\H-m)
    (keyboard-translate ?\C-i ?\H-i))
  (set-fontset-font "fontset-default" 'arabic (font-spec :family "SF Arabic"))
  (define-key key-translation-map (kbd "C-g") (kbd "<escape>")))

(defun salih/org-roam-node-insert ()
  (interactive)
  (setq salih/temp-roam-insert t)
  (consult-buffer (list org-roam-buffer-source)))

(defun salih/org-roam-node-open ()
  (interactive)
  (consult-buffer (list org-roam-buffer-source)))

(defun salih/open-current-url-in-chrome ()
  "Open the current URL (from kill-ring) in Chrome"
  (interactive)
  (let ((_ (xwidget-webkit-current-url)))
    (salih/open-url-in-chrome (car kill-ring))))

(defun salih/open-url-in-chrome (url &optional args)
  "Open the current URL in Chrome"
  (start-process "chromium" nil "chromium" url))

(defun salih/unescape-string (str)
  "Remove escape characters from a string."
  (replace-regexp-in-string "\\\\(.)" "\\1" str))

(defun salih/gomacro--sanitize-string (str)
  (salih/unescape-string str))

(defun salih/format (format-string arg)
  "Custom format function to replace all %s with the same argument."
  (replace-regexp-in-string "%s" arg format-string))

(defun salih/nov-search (pattern)
  (interactive "sEnter search pattern: ")
  (let ((version nov-epub-version)
        (index 1)
        results)
    (while (< index (1- (length nov-documents)))
      (seq-let (id &rest path) (aref nov-documents index)
        (let (;; HACK: this should be looked up in the manifest
              (imagep (seq-find (lambda (item) (string-match-p (car item) path))
                                image-type-file-name-regexps))
              ;; NOTE: allows resolving image references correctly
              (default-directory (file-name-directory path)))
          (unless imagep
            (with-temp-buffer
              (if (and (version< version "3.0") (eq id nov-toc-id))
                  (insert (nov-ncx-to-html path))
                (insert (nov-slurp path)))
              (goto-char (point-min))
              (when (search-forward pattern nil t)
                (nov-render-html)
                (goto-char (point-min))
                (while (search-forward pattern nil t)
                  (push (list (format "%d %s" index
                                      (replace-regexp-in-string "\n" " "
                                                                (thing-at-point
                                                                 'line)))
                              index (point))
                        results)))))
          (setq index (1+ index)))))
    ;; (print results)))
    (seq-let (index point) (alist-get (completing-read "Jump to: " (reverse
                                                                    results))
                                      results
                                      nil nil #'string=)
      (nov-goto-document index)
      (goto-char point))))



(defun salih/insert-relative-file-path ()
  "Insert a relative file path selected by the user."
  (interactive)
  (let* ((current-buffer-file (buffer-file-name))
         (file-path (read-file-name "Insert file path: ")))
    (if (file-exists-p file-path)
        (if current-buffer-file
            (let ((relative-path (file-relative-name file-path
                                                     (file-name-directory
                                                      current-buffer-file))))
              (insert relative-path))
          (insert file-path))
      (message "File does not exist: %s" file-path))))




(defadvice org-agenda-get-some-entry-text (after modify-agenda-entry-text activate)
  "Modify the text returned by org-agenda-get-some-entry-text."
  (setq ad-return-value (salih/modify-agenda-entry-text ad-return-value)))

(defun salih/modify-agenda-entry-text (text)
  "Customize the agenda entry text."
  ;; Modify the 'text' variable as needed here
  (replace-regexp-in-string "^[[:space:]]*$\\|^[[:space:]]*>[[:space:]]*$" ""
                            (replace-regexp-in-string "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" "\\2" text)))

(defun salih/get-file-list-todos ()
  (interactive)
  (org-agenda-set-restriction-lock)
  (org-agenda nil "t"))


(defun salih/polyphasic-sleep (start n)
  (if (or org-agenda-show-future-repeats (time-equal-p (awqat--today) date))
      (cond
       ((= n 1) (salih/polyphasic-sleep--1 start))
       ((= n 2) (salih/polyphasic-sleep--2 start))
       ((= n 3) (salih/polyphasic-sleep--3 start))
       ((= n 4) (salih/polyphasic-sleep--4 start)))))

(defun salih/polyphasic-sleep--1 (s)
  (format "Sleep (1h.30) %d:30 " (mod (+ s 5) 24)))

(defun salih/polyphasic-sleep--2 (s)
  (format "Sleep (30m) %d:00 " (mod (+ s 12) 24)))

(defun salih/polyphasic-sleep--3 (s)
  (format "Sleep (30m) %d:15 " (mod (+ s 17) 24)))

(defun salih/polyphasic-sleep--4 (s)
  (format "Sleep (1h.30) %d:30 " (mod (+ s 22) 24)))

(defun salih/org-calendar-goto-agenda ()
  (interactive)
  (let ((org-agenda-span 1))
    (org-calendar-goto-agenda)))

(defun salih/pacman-pkg-info ()
  (interactive)
  (let* ((completions (->> "pacman -Q"
                           (shell-command-to-string)
                           (s-trim)
                           (s-lines)
                           (--map (car (s-split " " it :no-nulls)))))
         (name (completing-read "Package: " completions)))
    (switch-to-buffer (get-buffer-create "*Package Info*"))
    (erase-buffer)
    (-> (format "pacman -Qi %s" name)
        (shell-command-to-string)
        (s-trim)
        (insert))
    (goto-char 0)
    (conf-mode)))

(defun salih/disable-bright ()
  (solaire-mode -1))

(after! centaur-tabs
  (defun centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       (window-dedicated-p (selected-window))

       ;; Buffer name not match below blacklist.
       (string-prefix-p "*epc" name)
       (string-prefix-p "*Org Agenda*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*LSP" name)
       (string-prefix-p "*company" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p "*tramp" name)
       (string-prefix-p " *Mini" name)
       (string-prefix-p "*help" name)
       (string-prefix-p "*straight" name)
       (string-prefix-p " *temp" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p "*Compile-Log*" name)

       (string-prefix-p "*doom*" name)
       (string-prefix-p "*Org tags*" name)
       (string-prefix-p "*scratch*" name)
       (string-prefix-p "*Semantic" name)
       (string-prefix-p "*mu4e-headers*" name)
       (string-prefix-p "*mu4e-main*" name)
       (string-prefix-p "*mu4e-update" name)
       (string-prefix-p "*julia" name)
       (string-prefix-p "*clangd" name)
       (string-prefix-p "*sly-mrepl" name)


       (string-prefix-p "*Messages*" name)
       (string-prefix-p "*Warnings*" name)
       (string-prefix-p "*httpd*" name)
       (string-prefix-p "*gopls*" name)
       (string-prefix-p "*Async-native-compile-log*" name)
       (string-prefix-p "*Native-compile-Log" name)


       (string-prefix-p "*Org Clock*" name)


       (string-prefix-p "*Ediff" name)
       (string-prefix-p "*ediff" name)


       (string-prefix-p "*Local Variables" name)
       (string-prefix-p "*Calc" name)
       (cl-search       "stderr" name)



       (string-prefix-p "*flycheck" name)
       (string-prefix-p "*nov" name)
       (string-prefix-p "*format" name)
       (string-prefix-p "*Pandoc" name)




       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name)))))))

(defun salih/org-media-note-insert-link (orgin)
  (let ((org-link-file-path-type 'absolute))
    (funcall orgin)))

;; lisp
(defvar salih/sly--compile-eval-begin-print-counter 0 "a counter to distinguish compile/eval cycles")
(defun salih/sly--compile-eval-begin-print (&rest _)
  "print the counter value into REPL to distinguish compile/eval cycles."
  ;;(sly-eval-async `(cl:format t "~&----- my advice called from: ~a" (quote ,real-this-command))) ;; debug-code
  (sly-eval-async `(cl:format t "" ,(cl-incf salih/sly--compile-eval-begin-print-counter))))

(defun salih/sly-eval-with-print (form)
  "Evaluate FORM in the SLY REPL, wrapping it with a (print ...) form."
  (interactive "sForm: ")
  (let* ((form-with-print (format "(print %s)" form))
         (sly-command (sly-interactive-eval form-with-print)))
    (sly-eval-last-expression)
    (message "Evaluated: %s" form-with-print)))

(defun salih/sly-compile-defun-with-print ()
  "Compile the current toplevel form in SLY, wrapping it with a (print ...) form."
  (interactive)
  (let* ((form (sly-sexp-at-point))
         (form-with-print (format "(print %s)" form))
         (sly-command (sly-interactive-eval form-with-print)))
    (sly-compile-defun)
    (message "Compiled: %s" form-with-print)))

(defvar salih/consult--source-books
  `(:name     "File"
    :narrow   ?f
    :category file
    :face     consult-file
    :history  file-name-history
    :state    ,#'consult--file-state
    :new      ,#'consult--file-action
    :items
    ,(lambda ()
       (let ((ht (consult--buffer-file-hash))
             items)
         (dolist (file (bound-and-true-p salih/books) (nreverse items))
           (unless (eq (aref file 0) ?/)
             (let (file-name-handler-alist)
               (setq file (expand-file-name file))))
           (unless (gethash file ht)
             (push (consult--fast-abbreviate-file-name file) items)))))))

(defun salih/org-roam-get-node-titles (node-list)
  "Applies `org-roam-node-title' function to the cdr of each element in NODE-LIST."
  (mapcar (lambda (node) (org-roam-node-title (cdr node)))
          node-list))


(defun salih/org-roam-get-node-files (node-list)
  "Applies `org-roam-node-file' function to the cdr of each element in NODE-LIST."
  (mapcar (lambda (node) (org-roam-node-file (cdr node)))
          node-list))


(setq roam-titles (salih/org-roam-get-node-titles
                   (org-roam-node-read--completions)))
(defun salih/get-org-roam-titles () roam-titles)

(setq org-roam-buffer-source
      `(:name     "Org-roam"
        :hidden   nil
        :narrow   ,consult-org-roam-buffer-narrow-key
        :annotate ,(lambda (cand)
                     (let* ((name (org-roam-node-from-title-or-alias cand)))
                       (if name (file-name-nondirectory (org-roam-node-file name))
                         "")))

        :action ,(lambda (name)
                   (if salih/temp-roam-insert
                       (progn
                         (setq salih/temp-roam-insert nil)
                         (let* ((node (org-roam-node-from-title-or-alias name))
                                (description (org-roam-node-title node))
                                (id (org-roam-node-id node)))
                           (insert (org-link-make-string
                                    (concat "id:" id)
                                    description))
                           (run-hook-with-args 'org-roam-post-node-insert-hook
                                               id
                                               description)))
                     (find-file (org-roam-node-file
                                 (org-roam-node-from-title-or-alias name)))))

        :new ,(lambda (name)
                (let* ((n (org-roam-node-create :title name)))
                  (org-roam-capture- :node n)
                  (when salih/temp-roam-insert
                    (progn
                      (setq salih/temp-roam-insert nil)
                      (let* ((node (org-roam-node-from-title-or-alias name))
                             (description (org-roam-node-title node))
                             (id (org-roam-node-id node)))
                        (insert (org-link-make-string
                                 (concat "id:" id)
                                 description))
                        (run-hook-with-args 'org-roam-post-node-insert-hook
                                            id
                                            description)))))


                (setq roam-titles (salih/org-roam-get-node-titles
                                   (org-roam-node-read--completions))))

        :items    ,#'salih/get-org-roam-titles))

(defun salih/org-noter-pdf--pdf-view-get-precise-info (mode window)
  (when (eq mode 'pdf-view-mode)
    (let (v-position h-position)
      (if (pdf-view-active-region-p)
          (let ((edges (car (pdf-view-active-region))))
            (setq v-position (min (nth 1 edges) (nth 3 edges))
                  h-position (min (nth 0 edges) (nth 2 edges))))

        (let ((event nil))
          (while (not (and (eq 'mouse-1 (car event))
                           (eq window (posn-window (event-start event)))))
            (setq event (read-event "Click where you want the start of the note to be!")))
          (let* ((col-row (posn-col-row (event-start event)))
                 (click-position (org-noter--conv-page-scroll-percentage
                                  (+ (window-vscroll) (cdr col-row))
                                  (+ (window-hscroll) (car col-row)))))
            (setq v-position (car click-position)
                  h-position (cdr click-position)))))
      v-position)))

(defun salih/eshell ()
  "Run eshell and set its directory to the current buffer's directory if eshell
is already running."
  (interactive)
  (let ((cwd (file-name-directory (or (buffer-file-name) default-directory))))
    (if (get-buffer "*eshell*")
        (progn
          (eshell)
          (eshell/cd cwd)
          (eshell-send-input))
      (eshell))))

(use-package proced
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t))

(defun salih/zathura-open ()
  (interactive)
  (let ((process-connection-type nil))
    (start-process "" nil "zathura" "-P"
                   (number-to-string
                    (pdf-view-current-page
                     (get-buffer-window (current-buffer)))) buffer-file-name)))

(defun salih/dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://ergoemacs.org/emacs/dired_sort.html'
Version 2015-07-30"
  (interactive)
  (let (-sort-by -arg)
    (setq -sort-by (ido-completing-read "Sort by:" '( "date" "size" "name" "dir")))
    (cond
     ((equal -sort-by "name") (setq -arg "-Al --si --time-style long-iso "))
     ((equal -sort-by "date") (setq -arg "-Al --si --time-style long-iso -t"))
     ((equal -sort-by "size") (setq -arg "-Al --si --time-style long-iso -S"))
     ((equal -sort-by "dir") (setq -arg "-Al --si --time-style long-iso --group-directories-first"))
     (t (error "logic error 09535" )))
    (dired-sort-other -arg )))

(defun salih/advise-once (symbol where function &optional props)
  (advice-add symbol :after (lambda (&rest _) (advice-remove symbol function)))
  (advice-add symbol where function props))

(defun salih/format-all-ensure-formatter ()
  (interactive)
  (if (or (derived-mode-p  'prog-mode) (eq major-mode 'bibtex-mode))
      (let ((inhibit-message t)
            (message-log-max nil))
        (call-interactively #'format-all-ensure-formatter))))

(after! git-gutter
  (unless (featurep 'tadwin)
    (modus-themes-with-colors
    (custom-set-faces
     ;; Replace green with blue if you use `modus-themes-deuteranopia'.
     `(git-gutter-fr:added ((,c :foreground ,bg-added-fringe)))
     ;; `(git-gutter-fr:deleted ((,class :foreground ,red-fringe-bg)))
     `(git-gutter-fr:modified ((,c :foreground ,bg-changed-fringe)))))))

(defmacro salih/disable-minor-mode-in-hook (hook mode-symbol)
  `(add-hook ,hook
             (lambda ()
               (when (bound-and-true-p ,mode-symbol)
                 (,mode-symbol -1)))))

(defun salih/dired-git-info-auto-enable ()
  "Enable dired-git-info only if there are less than 60 files."
  (when (< (count-lines (point-min) (point-max)) 60)
    (dired-git-info-auto-enable)))

(defun salih/insert-current-date ()
  (interactive)
    (let ((current-prefix-arg '(16)))
      (if (eq major-mode 'org-mode)
          (insert "- "))
      (call-interactively 'org-time-stamp-inactive)
      (insert ": ")))
current-prefix-arg
(defun salih/open-kitty-in-current-directory ()
  "Open the Kitty terminal in the current working directory."
  (interactive)
  (call-process "kitty" nil 0 nil "--directory" default-directory))

(defun salih/open-inbox ()
  (interactive)
  (setq mu4e-search-threads t)
  (if (featurep 'mu4e)
      (mu4e~headers-jump-to-maildir "/Inbox")
    (mu4e)))

(defun salih/open-rss ()
  (interactive)
  (if (featurep 'mu4e)
      (progn
        (setq mu4e-search-threads nil)
        (mu4e-search "maildir:\"/rss\" flag:unread")
        (mu4e-search-change-sorting :from))
    (progn
      (setq mu4e-search-threads t)
      (mu4e))))


(defun salih/mu4e-go-to-url ()
  (interactive)
  (setq-local browse-url-browser-function 'salih/open-url-in-chrome)
  (call-interactively #'mu4e-view-go-to-url))


(defun salih/org-ql-view--format-element (orig-fun &rest args)
  "This function will intercept the original function and
add the category to the result.

ARGS is `element' in `org-ql-view--format-element'"
  (if (not args)
      ""
    (let* ((element args)
           (properties (cadar element))
           (result (apply orig-fun element))
           (smt "")
           (category (org-entry-get (plist-get properties :org-marker) "CATEGORY")))
      (if (> (length category) 11)
          (setq category (substring category 0 10)))
      (if (< (length category) 11)
          (setq smt (make-string (- 11 (length category)) ?\s)))
      (org-add-props
          (format "   %-8s %s" (concat category ":" smt) result)
          (text-properties-at 0 result)))))

(defun cm/deft-parse-title (file contents)
  "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
  (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
    (if begin
	(string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
      (deft-base-filename file))))

(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))


(defvar org-roam-list-most-linked-count 5)
(cl-defmethod org-roam-node-backlinkscount-number ((node org-roam-node))
  "Access slot \"backlinks\" of org-roam-node struct CL-X. This is identical
toorg-roam-node-backlinkscount' with the difference that it returns a number
instead of a fromatted string. This is to be used in
`org-roam-node-sort-by-backlinks'"
  (let* ((count (caar (org-roam-db-query [:select (funcall count source)
                                          :from links :where (= dest $s1)
                                          :and (= type "id")]
                                         (org-roam-node-id node)))))
    count))

(defun org-roam-node-sort-by-backlinks (completion-a completion-b)
  "Sorting function for org-roam that sorts the list of nodes by the number of
backlinks. This is the sorting function in `org-roam-node-find-by-backlinks'"
  (let ((node-a (cdr completion-a))
        (node-b (cdr completion-b)))
    (>= (org-roam-node-backlinkscount-number node-a)
        (org-roam-node-backlinkscount-number node-b))))

(defun org-roam-node-find-by-backlinks ()
  "Essentially works like
org-roam-node-find' (although it uses a combination offind-file' and
org-roam-node-read' to accomplish that and notorg-roam-node-find' as only
org-roam-node-read' can take a sorting function as an argument) but the list of
nodes is sorted by the number of backlinks instead of most recent nodes. Sorting
is done with org-roam-node-sort-by-backlinks'"
  (interactive)
  (find-file (org-roam-node-file (org-roam-node-read nil nil #'org-roam-node-sort-by-backlinks))))

(defun salih/consult-org-roam-search-org-only ()
  (interactive)
  (let ((consult-ripgrep-args
         (concat
          consult-ripgrep-args
          " -g *.org")))
    (consult-org-roam-search)))

(defun salih/open-neotree-and-lsp ()
  (interactive)
  (neotree-project-dir)
  (lsp-treemacs-symbols)
  (call-interactively #'other-window))


(provide '+helper)


(defun salih/mu4e-compose-include-message (msg &optional)
  "Like mu4e-compose-attach-message, but include MSG as an inline attachment
instead."
  (let ((path (plist-get msg :path)))
    (unless (file-exists-p path)
      (mu4e-warn "Message file not found"))
    (mml-attach-file
     path
     "message/rfc822"
     (or (plist-get msg :subject) "No subject")
     "inline")))

(defun salih/mu4e-forward-html ()
  "Like `mu4e-compose-attach-captured-message’, but place the last captured
message as an inline attachment."
  (interactive)
  (message-goto-body)
  (let* ((plain (buffer-substring-no-properties (point) (point-max)))
         (msg mu4e-captured-message))
    (delete-region (point) (point-max))
    (insert "\n<#multipart type=alternative>\n<#part type=text/plain>\n")
    (insert plain)
    (salih/mu4e-compose-include-message msg)
    (insert "<#/multipart>\n")
    (message-goto-to)))
