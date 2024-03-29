;;; configs/~s/.doom.d/+helper.el -*- lexical-binding: t; -*-



;; basic definiton for keys.el

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
      (while (re-search-forward "\\(K\\|Q\\|R\\|B\\|N\\|P\\)[a-h][1-8]" (point-max) t)
        (let ((piece (string-to-char (match-string 1)))
              (destination (match-string 0)))
          (replace-match (concat (cdr (assoc piece piece-symbols)) (substring destination 1))
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

(defun insert-now-timestamp()
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
           (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'" (shell-quote-argument (expand-file-name $fpath )) "'")))
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
                   (file-name-sans-extension   (file-name-nondirectory (buffer-file-name))) " && ./"
                   (file-name-sans-extension  (file-name-nondirectory (buffer-file-name))) " && rm "
                   (file-name-sans-extension  (file-name-nondirectory (buffer-file-name)))) t  ) (other-window t)
  (end-of-add-hook 'c++-mode))



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



(defun highltier ()
  (interactive)
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (highlight-indent-guides-mode))

;; school

(defun salih/open-book ()
  "Search for a file in ~/me and open it."
  (interactive)
  (let ((default-directory salih/source-directory))
    (call-interactively 'find-file)))

;; let's hope for the best

(defun salih/epa-encrypt-file (recipients)
  "Encrypt the currently opened file for RECIPIENTS and delete the original."
  (interactive
   (list (epa-select-keys (epg-make-context epa-protocol)
                          "Select recipients for encryption. If no one is selected, symmetric encryption
  will be performed.")))
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
					(format "Encrypting %s..." (file-name-nondirectory file))))
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
  (add-hook 'after-save-hook 'xwidget-webkit-reload)
  (let ((file-path (buffer-file-name)))
    (when file-path
      (let ((xwidget (xwidget-webkit-browse-url (concat "file://" file-path))))
        (message "Opened file %s in an xwidget window." file-path)))))






(use-package! awqat
  :commands (awqat-display-prayer-time-mode
             awqat-times-for-day))


(defun salih/banner ()
  (let* ((banner '(
                   "       d8888                                     8888888888       888    d8b      "
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


;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((ksh . t)))



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
                                 (propertize (symbol-name action) 'face 'doom-dashboard-menu-desc)))
                        (format "%-37s" (buffer-string)))
                      ;; Lookup command keys dynamically
                      (propertize
                       (or key
                           (when-let*
                               ((keymaps
                                 (delq
                                  nil (list (when (bound-and-true-p evil-local-mode)
                                              (evil-get-auxiliary-keymap +doom-dashboard-mode-map 'normal))
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
                      (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
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
  "Return t if the current buffer is the dashboard or scratch, or is a real (file-visiting) buffer."
  (cond ((string-prefix-p "*sly-mrepl for sbcl*" (buffer-name (buffer-base-buffer)) ) t)
        ((string-prefix-p "*eshell*" (buffer-name (buffer-base-buffer)) ) t)
        ((string-prefix-p "*julia" (buffer-name (buffer-base-buffer)) ) t)
        ((string-prefix-p "*doom*" (buffer-name (buffer-base-buffer)) ) t)
        ((buffer-file-name (buffer-base-buffer)) t)
        (t nil)))




(defun centaur-tabs-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
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
                            magit-blame-mode
                            )))
     "Emacs")
    ((derived-mode-p 'prog-mode)
     "Editing")
    ((derived-mode-p 'dired-mode)
     "Dired")
    ((memq major-mode '(helpful-mode
                        help-mode))
     "Help")

    ((memq major-mode '(erc-mode))
     "Erc")


    ((memq major-mode '(elfeed-show-mode
                        elfeed-search-mode))
     "elfeed")


    ((memq major-mode '(pdf-view-mode
                        nov-mode
                        doc-view-mode))
     "PDF")

    ((memq major-mode '(org-mode
                        org-agenda-clockreport-mode
                        org-src-mode
                        org-agenda-mode
                        org-beamer-mode
                        org-indent-mode
                        org-bullets-mode
                        org-cdlatex-mode
                        org-agenda-log-mode
                        diary-mode))
     "OrgMode")
    (t
     (centaur-tabs-get-group-name (current-buffer))))))





(defun vulpea-project-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (seq-find                                 ; (3)
   (lambda (type)
     (or (eq type 'todo)
         (eq type 'done)))
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
        (if (vulpea-project-p)
            (setq tags (cons "project" tags))
          (setq tags (remove "project" tags)))

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
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (like tag (quote "%\"project\"%"))]))))

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







(defun salih/keyboard-config () (when (display-graphic-p)  (keyboard-translate ?\C-i ?\H-i)))
(salih/keyboard-config)





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

(defun salih/open-url-in-chrome (url)
  "Open the current URL in Chrome"
  (start-process "chromium" nil "chromium" url))


(defun salih/unescape-string (str)
  "Remove escape characters from a string."
  (replace-regexp-in-string "\\\\(.)" "\\1" str))


(defun salih/gomacro--sanitize-string (str)
  (salih/unescape-string str))


(advice-add 'gomacro--sanitize-string :override 'salih/gomacro--sanitize-string)

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
                                                                (thing-at-point 'line)))
                              index (point))
                        results)))))
          (setq index (1+ index)))))
    ;; (print results)))
    (seq-let (index point) (alist-get (completing-read "Jump to: " (reverse results)) results
                                      nil nil #'string=)
      (nov-goto-document index)
      (goto-char point))))


(defun salih/elfeed-copy-url ()
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (kill-new link)
      (message "URL: %s" link)
      link)))

(defun salih/elfeed-open-url ()
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (browse-url link))))


(defun salih/elfeed-open-url-in-chrome ()
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (salih/open-url-in-chrome link))))

(defun salih/insert-relative-file-path ()
  "Insert a relative file path selected by the user."
  (interactive)
  (let* ((current-buffer-file (buffer-file-name))
         (file-path (read-file-name "Insert file path: ")))
    (if (file-exists-p file-path)
        (if current-buffer-file
            (let ((relative-path (file-relative-name file-path (file-name-directory current-buffer-file))))
              (insert relative-path))
          (insert file-path))
      (message "File does not exist: %s" file-path))))


(defun salih/elfeed-tag-sort (a b)
  (let* ((a-tags (format "%s" (elfeed-entry-tags a)))
         (b-tags (format "%s" (elfeed-entry-tags b))))
    (if (string= a-tags b-tags)
        (< (elfeed-entry-date b) (elfeed-entry-date a)))
    (string< a-tags b-tags)))

(setf elfeed-search-sort-function #'salih/elfeed-tag-sort)



(defadvice org-agenda-get-some-entry-text (after modify-agenda-entry-text activate)
  "Modify the text returned by org-agenda-get-some-entry-text."
  (setq ad-return-value (salih/modify-agenda-entry-text ad-return-value)))


(defun salih/modify-agenda-entry-text (text)
  "Customize the agenda entry text."
  ;; Modify the 'text' variable as needed here
  (replace-regexp-in-string "^[[:space:]]*$\\|^[[:space:]]*>[[:space:]]*$" ""
                            (replace-regexp-in-string "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" "\\2" text)))



(defun salih/get-file-todos ()
  (interactive)
  (org-agenda-set-restriction-lock)
  (org-agenda nil "t"))



(defun salih/org--align-tags-here (to-col)
  "Align tags on the current headline to TO-COL.
Since TO-COL is derived from `org-tags-column', a negative value is
interpreted as alignment flush-right, a positive value as flush-left,
and 0 means insert a single space in between the headline and the tags."
  ;; source: https://list.orgmode.org/20200916225553.hrtxitzt46dzln7i@ionian.linksys.moosehall/
  (save-excursion
    (when (org-match-line org-tag-line-re)
      (let* ((tags-start (match-beginning 1))
             (tags-end (match-end 1))
             (tags-pixel-width
              (car (window-text-pixel-size (selected-window)
                                           tags-start tags-end)))
             (blank-start (progn
                            (goto-char tags-start)
                            (skip-chars-backward " \t")
                            (point)))
             ;; use this to avoid a 0-width space before tags on long lines:
             (blank-start-col (progn
                                (goto-char blank-start)
                                (current-column)))
             ;; this is to makes it work with org-indent-mode:
             (lpref (if (org-fold-folded-p) 0
                      (length (get-text-property (point) 'line-prefix)))))
        ;; If there is more than one space between the headline and
        ;; tags, delete the extra spaces.  Might be better to make the
        ;; delete region one space smaller rather than inserting a new
        ;; space?
        (when (> tags-start (1+  blank-start))
          (delete-region blank-start tags-start)
          (goto-char blank-start)
          (insert " "))
        (if (or (= to-col 0) (< (abs to-col) (1- blank-start-col)))
            ;; Just leave one normal space width
            (remove-text-properties blank-start (1+  blank-start)
                                    '(salih/display nil))
          (message "In here: %s" lpref)
          (let ((align-expr
                 (if (> to-col 0)
                     ;; Left-align positive values
                     (+ to-col lpref)
                   ;; Right-align negative values by subtracting the
                   ;; width of the tags.  Conveniently, the pixel
                   ;; specification allows us to mix units,
                   ;; subtracting a pixel width from a column number.
                   `(-  ,(- lpref to-col) (,tags-pixel-width)))))
            (put-text-property blank-start (1+  blank-start)
                               'salih/display
                               `(space . (:align-to ,align-expr)))))))))

(defun salih/fix-tag-alignment ()
  (setq org-tags-column 70) ;; adjust this
  (advice-add 'org--align-tags-here :override #'salih/org--align-tags-here)
  ;; this is needed to make it work with https://github.com/minad/org-modern:
  (add-to-list 'char-property-alias-alist '(display salih/display))
  ;; this is needed to align tags upon opening an org file:
  (org-align-tags t))


(defun salih/org-calendar-goto-agenda ()
  (interactive)
  (let ((org-agenda-span 1))
    (org-calendar-goto-agenda)))



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

(provide '+helper)
