;;; configs/~s/.doom.d/+helper.el -*- lexical-binding: t; -*-

;; basic definiton for `+bindings`
(defun salih/global (key-sequence)
  (kbd (concat salih/prefix-global key-sequence)))

(defun salih/mode (key-sequence)
  (kbd (concat salih/prefix-mode   key-sequence)))

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

(defun salih/org-archive-done-tasks () ;; org archive
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(defun salih/org-archive-killed-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/KILL" 'file))

(defun salih/chess-notation-to-symbols () ;; chess
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

;; [2025-05-03 Sat 05:35] fun fact, I took this function from an Israeli around
;; 4 years ago, and never stopped to read it but now. I'm adding Arabic support.
(defun salih/bidi-direction-toggle ()
  "Toggle bidirectional paragraph direction and Arabic input method."
  (interactive)
  (setq bidi-display-reordering t)
  (if (equal bidi-paragraph-direction 'right-to-left)
      (progn
        (setq bidi-paragraph-direction 'left-to-right)
        (deactivate-input-method))
    (progn
      (setq bidi-paragraph-direction 'right-to-left)
      (jinx-mode -1)
      (set-input-method "arabic")))
  (message "Direction: %s, Input method: %s"
           bidi-paragraph-direction
           (if current-input-method current-input-method "none")))

(defun salih/toggle-maximize-buffer () ;; window management
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(defun salih/neotree-project-dir ()
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

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
  "If current buffer is in eglot-mode, call eglot-rename. Otherwise, call
iedit-mode."
  (interactive)
  (if (and (featurep 'eglot) (eglot-managed-p))
      (call-interactively #'eglot-rename)
    (call-interactively #'iedit-mode)))


(defun salih/find-definition-or-lookup ()
  (interactive)
  "If current buffer is in eglot-mode, call eglot-find-definition. Otherwise, call
lookup."
  (if (eglot-managed-p)
      (call-interactively #'xref-find-definitions)
    (call-interactively #'+lookup/file)))

(defun salih/insert-now-timestamp()
  (interactive)
  (org-insert-time-stamp (current-time) t))

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
                           (start-process
                            "" nil "xdg-open" $fpath))) $file-list))))))

(defun salih/compile-and-run-cpp ()
  "Compile and run cpp files. One of the first functions I've ever written, when
I was learning competitive programming :)"
  (interactive)
  (save-buffer)
  (compile (concat "g++ "  (file-name-nondirectory (buffer-file-name)) " -o "
                   (file-name-sans-extension   (file-name-nondirectory
                                                (buffer-file-name))) " && ./"
                   (file-name-sans-extension  (file-name-nondirectory
                                               (buffer-file-name))) " && rm "
                   (file-name-sans-extension  (file-name-nondirectory
                                               (buffer-file-name)))) t)
  (other-window t)
  (end-of-add-hook 'c++-mode))

(defun salih/compile-and-run-c ()
  "Compile and run cpp files. One of the first functions I've ever written, when
I was learning competitive programming :)"
  (interactive)
  (save-buffer)
  (compile (concat "gcc "  (file-name-nondirectory (buffer-file-name)) " -o "
                   (file-name-sans-extension   (file-name-nondirectory
                                                (buffer-file-name))) " && ./"
                   (file-name-sans-extension  (file-name-nondirectory
                                               (buffer-file-name))) " && rm "
                   (file-name-sans-extension  (file-name-nondirectory
                                               (buffer-file-name)))) t)
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

(defun salih/open-book ()
  "Search for a file in ~/me and open it."
  (interactive)
  (let ((default-directory (concat salih/source-directory "/")))
    (call-interactively 'find-file)))

(defun salih/open-film ()
  "Search for a file in ~/me and open it."
  (interactive)
  (let ((default-directory "~/me/cinema/"))
    (call-interactively 'find-file)))

(defun salih/open-book-zathura ()
  "Search for a file in ~/me and open it. If the file is a PDF, open it in
Zathura."
  (interactive)
  (let ((default-directory (concat salih/source-directory "/")))
    (let ((file (read-file-name "Select file: " default-directory)))
      (if (string-equal (file-name-extension file) "pdf")
          (start-process "open" nil "open"
                         (expand-file-name file default-directory))
        (find-file file)))))

(defun salih/epa-encrypt-file (recipients)
  "Encrypt the currently opened file for RECIPIENTS and delete the original."
  (interactive
   (list (epa-select-keys (epg-make-context epa-protocol)
                          (concat
                           "Select recipients for encryption. "
                           "If no one is selected, symmetric encryption"
                           " will be performed."))))
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
  (let ((recipients (epa-select-keys (epg-make-context)
                                     (concat
                                      "Select recipients for encryption. "
                                      "If no one is selected, "
                                      "symmetric encryption"
                                      " will be performed."))))
    (dolist (file (dired-get-marked-files))
      (with-current-buffer (find-file-noselect file)
        (salih/epa-encrypt-file recipients)))
    (revert-buffer)))

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
          (unless (featurep 'org-roam) (require 'org-roam))
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


        (cond ((vulpea-project-p)
               (and (or (setq tags (cons "project" tags)) t)
                    (setq tags (remove "project_archived" tags))))

              ((vulpea-project-done-p)
               (and (or (setq tags (cons "project_archived" tags)) t)
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
  (browse-url-default-browser url))

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

(defadvice org-agenda-get-some-entry-text
    (after modify-agenda-entry-text activate)
  "Modify the text returned by org-agenda-get-some-entry-text."
  (setq ad-return-value (salih/modify-agenda-entry-text ad-return-value)))

(defun salih/modify-agenda-entry-text (text)
  "Customize the agenda entry text."
  ;; Modify the 'text' variable as needed here
  (replace-regexp-in-string
   "^[[:space:]]*$\\|^[[:space:]]*>[[:space:]]*$" ""
   (replace-regexp-in-string "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" "\\2"
                             text)))

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

(defun salih/sly-eval-with-print (form)
  "Evaluate FORM in the SLY REPL, wrapping it with a (print ...) form."
  (interactive "sForm: ")
  (let* ((form-with-print (format "(print %s)" form))
         (sly-command (sly-interactive-eval form-with-print)))
    (sly-eval-last-expression)
    (message "Evaluated: %s" form-with-print)))

(defun salih/sly-compile-defun-with-print ()
  "Compile the current toplevel form in SLY, wrapping it with a (print ...)
form."
  (interactive)
  (let* ((form (sly-sexp-at-point))
         (form-with-print (format "(print %s)" form))
         (sly-command (sly-interactive-eval form-with-print)))
    (sly-compile-defun)
    (message "Compiled: %s" form-with-print)))

(defun salih/org-roam-get-node-titles (node-list)
  "Applies `org-roam-node-title' function to the cdr of each element in
NODE-LIST."
  (mapcar (lambda (node) (org-roam-node-title (cdr node)))
          node-list))

(defun salih/org-roam-get-node-files (node-list)
  "Applies `org-roam-node-file' function to the cdr of each element in
NODE-LIST."
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
                       (if name (file-name-nondirectory
                                 (org-roam-node-file name)) "")))

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
                     (org-roam-node-visit
                      (org-roam-node-from-title-or-alias name))))

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
            (setq event
                  (read-event
                   "Click where you want the start of the note to be!")))
          (let* ((col-row (posn-col-row (event-start event)))
                 (click-position (org-noter--conv-page-scroll-percentage
                                  (+ (window-vscroll) (cdr col-row))
                                  (+ (window-hscroll) (car col-row)))))
            (setq v-position (car click-position)
                  h-position (cdr click-position)))))
      v-position)))

(defun salih/vterm ()
  "Run vterm and set its directory to the current buffer's directory if vterm
is already running."
  (interactive)
  (let ((cwd (file-name-directory (or (buffer-file-name) default-directory)))
        (vterm-buffer (get-buffer "*vterm*")))
    (if vterm-buffer
        (progn
          (switch-to-buffer vterm-buffer)
          (vterm-send-string (concat "cd " cwd))
          (vterm-send-return))
      (+vterm/here t))))

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

(defun salih/zathura-open ()
  (interactive)
  (let ((process-connection-type nil))
    (start-process "" nil "open"  buffer-file-name)))

(defun salih/dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://ergoemacs.org/emacs/dired_sort.html'
Version 2015-07-30"
  (interactive)
  (let (-sort-by -arg)
    (setq -sort-by
          (ido-completing-read "Sort by:" '( "date" "size" "name" "dir")))
    (cond
     ((equal -sort-by "name") (setq -arg "-Al --si --time-style long-iso "))
     ((equal -sort-by "date") (setq -arg "-Al --si --time-style long-iso -t"))
     ((equal -sort-by "size") (setq -arg "-Al --si --time-style long-iso -S"))
     ((equal -sort-by "dir")
      (setq -arg "-Al --si --time-style long-iso --group-directories-first"))
     (t (error "logic error 09535")))
    (dired-sort-other -arg)))

(defun salih/advise-once (symbol where function &optional props)
  (advice-add symbol :after (lambda (&rest _) (advice-remove symbol function)))
  (advice-add symbol where function props))

(defun salih/format-all-ensure-formatter ()
  (interactive)
  (if (or (derived-mode-p  'prog-mode) (eq major-mode 'bibtex-mode))
      (let ((inhibit-message t)
            (message-log-max nil))
        (call-interactively #'format-all-ensure-formatter))))

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
        (call-interactively 'org-add-note)
      (call-interactively 'org-time-stamp-inactive))
    (insert " ")))

(defun salih/open-kitty-in-current-directory ()
  "Open the Kitty terminal in the current working directory."
  (interactive)
  (call-process "kitty" nil 0 nil "--directory" default-directory))

(defun salih/open-inbox ()
  (interactive)
  (setq mu4e-search-threads t)
  (if (featurep 'mu4e)
      (progn
        (mu4e~headers-jump-to-maildir "/lr0@gmx.com/Inbox")
        (mu4e-search-change-sorting :date 'descending))
    (mu4e)))

(defun salih/load-last-open-rss-time ()
  "Load the last execution time from the cache file."
  (when (f-exists? salih/open-rss-lock-file)
    (with-temp-buffer
      (insert-file-contents salih/open-rss-lock-file)
      (read (current-buffer)))))

(defun salih/save-last-open-rss-time (time)
  "Save the last execution TIME to the cache file."
  (with-temp-file salih/open-rss-lock-file
    (insert (format "%S" time))))

(defun salih/different-day-p (last-time current-time)
  (let* ((next-day-p (or
                      (not (= (nth 4 (decode-time last-time))
                              (nth 4 (decode-time current-time))))
                      (not (= (nth 3 (decode-time last-time))
                              (nth 3 (decode-time current-time))))
                      (not (= (nth 5 (decode-time last-time))
                              (nth 5 (decode-time current-time)))))))
    next-day-p))

(defun salih/within-hour-window-p (last-time current-time)
  "Check if CURRENT-TIME is within one hour of LAST-TIME."
  (let* ((hour (nth 2 (decode-time last-time)))
         (next-day-p (or
                      (not (= (nth 4 (decode-time last-time))
                              (nth 4 (decode-time current-time))))
                      (not (= (nth 3 (decode-time last-time))
                              (nth 3 (decode-time current-time))))
                      (not (= (nth 5 (decode-time last-time))
                              (nth 5 (decode-time current-time))))))
         (next-hour (time-add last-time (seconds-to-time 3600))))
    (or next-day-p (< (float-time current-time) (float-time next-hour)))))

(defun salih/read-feeds-anyway () (interactive) (salih/open-rss t))

(defun salih/read-feeds () (interactive) (salih/open-rss nil))

(defun salih/open-rss (readanywayg)
  "Open RSS using mu4e, only callable once per hour within the same day."
  ;; [2024-10-30 Wed 22:41] Currently, Just run it
  (if readanywayg (salih/feeds--)
    (let* ((now (current-time))
           (last-open-time (salih/load-last-open-rss-time)))
      (if (or (not last-open-time)
              (salih/within-hour-window-p last-open-time now))
          (progn
            ;; Save only the first time within the hour window, not on
            ;; subsequent calls
            (when (salih/different-day-p last-open-time now)
              (salih/save-last-open-rss-time now))
            ;; Execute the main command
            (salih/feeds--))
        (message
         "This command can only be called once within the same hour of a day.")))))

(defun salih/feeds-- ()
  (if (featurep 'mu4e)
      (progn
        (setq mu4e-search-threads nil)
        (mu4e-search "maildir:\"/lr0@gmx.com/rss\" flag:unread")
        (mu4e-search-change-sorting :from 'descending))
    (progn
      (setq mu4e-search-threads t)
      (mu4e))))

(defun salih/mu4e-go-to-url ()
  (interactive)
  (let ((browse-url-browser-function 'salih/open-url-in-chrome))
    (call-interactively #'mu4e-view-go-to-url)))


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
           (category (org-entry-get (plist-get properties :org-marker)
                                    "CATEGORY")))
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
        (string-trim (substring contents begin (match-end 0))
                     "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
      (deft-base-filename file))))

(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))

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
  (find-file (org-roam-node-file
              (org-roam-node-read nil nil #'org-roam-node-sort-by-backlinks))))

(defun salih/consult-org-roam-search-org-only ()
  (interactive)
  (let ((consult-ripgrep-args
         (concat
          consult-ripgrep-args
          " -g *.org")))
    (consult-org-roam-search)))

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

(defun salih/org-add-update-rating ()
  "Add or update a rating for the entry"
  (interactive)
  (let* ((node (org-roam-node-from-title-or-alias "2024 ratings"))
         (nodebuf (find-file-noselect (org-roam-node-file node))))
    (with-current-buffer nodebuf
      (let* ((today (format-time-string "%Y-%m-%d"))
             (node-point (org-roam-node-point node))
             (existing-rating (org-entry-get node-point today)))
        (if existing-rating
            (message "Rating already exists for today.")
          (progn
            (let* ((rating (completing-read "Choose a rating: "
                                            '("amazing" "good"
                                              "meh" "bad" "awful")))
                   (nice-message (cond
                                  ((string= rating "amazing") "Great!")
                                  ((string= rating "good") "Keep going!")
                                  ((string= rating "meh")
                                   "Better days are comming.")
                                  ((string= rating "bad")
                                   "Don't forget the sunshie on a rainy day.")
                                  ((string= rating "awful")
                                   "This too shall pass."))))
              (org-entry-put node-point today
                             (format "(%s . \"%s\")"
                                     (current-time)
                                     rating))
              (message nice-message))))))))

(defun salih/pdf-occure ()
  (interactive)
  (save-window-excursion
    (pdf-occur-goto-occurrence)))

(defun salih/mu4e-compose-forward-html ()
  (interactive)
  (mu4e-action-capture-message (mu4e-message-at-point))
  (mu4e-compose-forward)
  (salih/mu4e-forward-html))

(defun salih/mu4e-org-store-and-capture ()
  "Store a link to the current message or query.
\(depending on `mu4e-org-link-query-in-headers-mode', and capture
it with org)."
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "f"))

(defun salih/eww-org-store-and-capture ()
  "Store a link to the current webpage in `eww-mode` and capture it with org."
  (interactive)
  (if (eq major-mode 'eww-mode)
      (let ((url (eww-current-url))
            (title (plist-get eww-data :title)))
        (unless url
          (error "No URL found for this eww buffer"))
        ;; Manually set the Org link properties
        (org-store-link-props
         :type "eww"
         :link url
         :description title)
        ;; Store the link and invoke the capture template
        (call-interactively 'org-store-link)
        (org-capture nil "f"))
    (error "Not in `eww-mode`")))


(defun salih/org-roam-capture-create-id ()
  "Create id for captured note and add it to org-roam-capture-template."
  (when (and (not org-note-abort)
             (org-roam-capture-p))
    (if salih/org-roam-dailies-capture-p
        (setq salih/org-roam-dailies-capture-p nil)
      (org-roam-capture--put :id (org-id-get-create)))))

(defun salih/capture-- (fn key &optional fleet?)
  (with-current-buffer
      (find-file-noselect (if fleet?
                              salih/org-roam-fleet-file
                            +org-capture-todo-file))
    (funcall fn nil key)))

(defun salih/org-capture-general ()
  (interactive)
  (salih/capture-- 'org-capture "f"))

(defun salih/org-capture-log ()
  (interactive)
  (salih/capture-- 'org-capture "n"))

(defun salih/org-roam-capture-fleet ()
  (interactive)
  (salih/capture-- 'org-roam-capture "f"))

(defun salih/org-roam-extract-subtree ()
  "Same as `org-roam-extract-subtree', but with main/ as top-level directory and
without history in the file name."
  "Convert current subtree at point to a node, and extract it into a new file."
  (interactive)
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (when (bobp) (user-error "Already a top-level node"))
    (org-id-get-create)
    (save-buffer)
    (org-roam-db-update-file)
    (let* ((template-info nil)
           (node (org-roam-node-at-point))
           (template (org-roam-format-template
                      (string-trim (org-capture-fill-template "${slug}.org"))
                      (lambda (key default-val)
                        (let ((fn (intern key))
                              (node-fn (intern (concat "org-roam-node-" key)))
                              (ksym (intern (concat ":" key))))
                          (cond
                           ((fboundp fn)
                            (funcall fn node))
                           ((fboundp node-fn)
                            (funcall node-fn node))
                           (t (let ((r (read-from-minibuffer
                                        (format "%s: " key) default-val)))
                                (plist-put template-info ksym r)
                                r)))))))
           (file-path
            (expand-file-name
             (read-file-name "Extract node to: "
                             (file-name-as-directory
                              (f-join
                               org-roam-directory "main"))
                             template nil template)
             (f-join org-roam-directory "main"))))
      (when (file-exists-p file-path)
        (user-error "%s exists. Aborting" file-path))
      (org-cut-subtree)
      (save-buffer)
      (with-current-buffer (find-file-noselect file-path)
        (org-paste-subtree)
        (while (> (org-current-level) 1) (org-promote-subtree))
        (save-buffer)
        (org-roam-promote-entire-buffer)
        (save-buffer)))))

(defun salih/org-roam-dailies-capture-today ()
  (interactive)
  (setq salih/org-roam-dailies-capture-p t)
  (call-interactively #'org-roam-dailies-capture-today))

(defun salih/fetch-first-post-url ()
  "Fetch the first post's URL from the API."
  (let
      ((url-request-method "GET")
       (api-url
        "https://api.al-akhbar.com/posts/todays-newsletter?&platform=headless"))
    (with-temp-buffer
      (url-insert-file-contents api-url) ;; Fetch the API response
      (let* ((json-object-type 'alist)   ;; Parse JSON as an alist
             (parsed-data (json-read-from-string (buffer-string)))
             (widgets (alist-get 'widgets parsed-data)) ;; Get 'widgets'
             (first-widget (aref widgets 0))
             (posts (alist-get 'posts first-widget))
             (first-post (aref posts 0)))
        (alist-get 'url first-post)))))

(defun salih/today ()
  "Return today's date in the format (M D Y)."
  (let ((time-lst (decode-time)))
    (list (decoded-time-month time-lst)
          (decoded-time-day time-lst)
          (decoded-time-year time-lst))))

(defun salih/read-al-akhbar ()
  ;; Currently, this does not really work.
  (when (time-equal-p (salih/today))
    (format "Read 00:30am [[%s][Today's Akhbar]]"
                       (salih/fetch-first-post-url))))



(defun salih/toggle-stats-on (&rest _)
  (setq org-log-into-drawer "STATS"))

(defun salih/toggle-logbook-on (&rest _)
  (setq org-log-into-drawer t))

(defun salih/toggle-log-int-drawer-off (&rest _)
  (when salih/adding-note?
    (setq org-log-into-drawer nil
          salih/adding-note? nil)))

(defun salih/load-random-theme ()
  (interactive)
  (load-theme (car (salih/get-random-theme 0))))

(defun salih/load-tomorrow-theme ()
  (interactive)
  (load-theme (car (salih/get-random-theme 1))))

(defun salih/load-real-random-theme ()
  (interactive)
  (load-theme (car (salih/get-random-theme (random 10)))))

(defun salih/org-add-week-to-timestamp ()
  "Add a week to the org timestamp at point."
  (interactive)
  (unless (org-at-timestamp-p 'lax)
    (error "Not at an org timestamp"))
  (dotimes (_ 7) (org-timestamp-up-day)))

(defun salih/org-add-to-anthology ()
  (interactive)
  (org-id-get-create)
  (org-set-tags ":drill")
  (org-entry-put (point) "CUSTOM_ID" (org-id-get)))

(defun salih/get-org-roam-nodes-with-tag (tag)
  "Get all Org Roam nodes that have the specified TAG."
  (org-roam-db-query
   [:select :distinct [nodes:file nodes:title]
    :from tags
    :left :join nodes
    :on (= tags:node-id nodes:id)
    :where (like tags:tag $s1)]
   tag))

(defun salih/get-unique-file-paths-for-tag (tag)
  "Get unique file paths for Org Roam nodes with the specified TAG."
  (let ((nodes (salih/get-org-roam-nodes-with-tag "drill")))
    (delete-dups (mapcar 'car nodes))))

(defun salih/org-noter-open-in-zathura ()
  "Get the value of a PROPERTY from the current Org heading."
  (interactive)
  (let ((path (org-entry-get nil "NOTER_DOCUMENT"))
        (page (org-entry-get nil "NOTER_PAGE")))
    (if page
        (start-process "" nil "zathura" "-P" page path)
      (start-process "" nil "zathura" path))))

(defun salih/randomize-lines (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

(defun salih/org-save-all-org-buffers ()
  "Save all Org buffers without user confirmation asynchronously."
  (interactive)
  (message "Saving all Org buffers...")
  (async-start
   (lambda ()
     (require 'org)
     (save-some-buffers t (lambda () (and (derived-mode-p 'org-mode) t)))
     (when (featurep 'org-id) (org-id-locations-save)))
   (lambda (_)
     (message "Saving all Org buffers... done"))))

(defun salih/org-agenda-no-full-f ()
  (interactive)
  (setq salih/vulpea-show-full nil)
  (org-agenda nil "f"))

(defun salih/org-agenda-no-full-l ()
  (interactive)
  (setq salih/vulpea-show-full nil)
  (org-agenda nil "l"))

(defun salih/org-agenda-full-f ()
  (interactive)
  (setq salih/vulpea-show-full t)
  (org-agenda nil "f"))

(defun salih/modeline--buffer-name ()
  "Return `buffer-name' with spaces around it."
  (format " %s " (buffer-name)))

(defun salih/modeline--major-mode-name ()
  "Return capitalized `major-mode' as a string."
  (capitalize (symbol-name major-mode)))

(defun mode-line-window-selected-p ()
   "Return non-nil if we're updating the mode line for the selected window.
This function is meant to be called in `:eval' mode line
constructs to allow altering the look of the mode line depending
on whether the mode line belongs to the currently selected window
or not."
   (let ((window (selected-window)))
     (or (eq window (old-selected-window))))
   (and (minibuffer-window-active-p (minibuffer-window))
        (with-selected-window (minibuffer-window)
          (eq window (minibuffer-selected-window)))))

(defun salih/doom-modeline-update-pdf-pages ()
  "Update PDF pages."
  (setq doom-modeline--pdf-pages
        (format "  %d/%d [%s٪]"
                (or (eval `(pdf-view-current-page)) 0)
                (pdf-cache-number-of-pages)
                (truncate
                 (* 100
                    (/ (float (or (eval `(pdf-view-current-page)) 0))
                       (pdf-cache-number-of-pages)))))))

(defun salih/doom-modeline-update-pdf-pages-no-percent ()
  "Update PDF pages."
  (setq doom-modeline--pdf-pages
        (format "  %d/%d "
                (or (eval `(pdf-view-current-page)) 0)
                (pdf-cache-number-of-pages))))

(defun salih/doom-modeline-update-pdf-pages-only-percent ()
  "Update PDF pages."
  (setq doom-modeline--pdf-pages
        (format "[%s％󠀥] "
                (truncate
                 (* 100
                    (/ (float (or (eval `(pdf-view-current-page)) 0))
                       (pdf-cache-number-of-pages)))))))

(defun salih/fetch-password (&rest params)
  (let ((match (car (apply #'auth-source-search params))))
    (if match (let ((secret (plist-get match :secret)))
                (if (functionp secret)
                    (funcall secret)
                  secret))
      (error "Password not found for %S" params))))

(defun salih/tracking-next-buffer--always-switch (&rest _args)
  "Advice to always switch to the next unread buffer, bypassing the `circe-mode`
check."
  (tracking-next-buffer))

(defmacro email-dir (m)
  `(concat "/" user-mail-address ,m))

(defun salih/set-custom-id-to-id (&rest _)
  "Set the CUSTOM_ID property to match the ID property in the current entry."
  (when-let ((id (org-entry-get nil "ID")))
    (org-entry-put nil "CUSTOM_ID" id)))

(defun salih/org-align-tags-in-directory (dir)
  "Re-align tags in all Org files under the given directory DIR."
  (interactive "DDirectory: ")
  (let ((org-files (directory-files-recursively dir "\\.org$")))
    (dolist (file org-files)
      (with-current-buffer (find-file-noselect file)
        (org-show-all)
        (org-align-tags t)
        (save-buffer)
        (kill-buffer)))))

(defun salih/eww-org-store-and-capture ()
  "Store a link to the current webpage in `eww-mode` and capture it with org."
  (interactive)
  (let ((url (eww-current-url))
        (title (plist-get eww-data :title)))
    (unless url
      (error "No URL found in the current eww buffer"))
    ;; Manually set link properties for Org
    (org-store-link-props
     :type "eww"
     :link url
     :description (or title url))
    ;; Call org-store-link and capture
    (call-interactively 'org-store-link)
    (org-capture nil "f")))

(defun salih/eww-browse-url-externally ()
  "Open the current EWW URL in external browser."
  (interactive)
  (let ((browse-url-browser-function 'salih/open-url-in-chrome))
    (browse-url (eww-current-url))))

(defun salih/ensure-eww-in-search (fn &rest args)
             (let ((browse-url-browser-function #'browse-url-default-browser))
               (apply fn args)))

(defun salih/org-roam-buffer ()
  "Display the Org Roam buffer for the node at point."
  (interactive)
  (let ((node (org-roam-node-at-point)))
    (when node
      (org-roam-buffer-display-dedicated node))))

(defun salih/mu4e-view-and-copy-html ()
  "View message as HTML in temp browser and copy to clipboard."
  (interactive)
  (let* ((msg (mu4e-message-at-point))
         (html-temp-file (make-temp-file "mu4e-html-" nil ".html")))
    (mu4e-action-view-in-browser msg)))

(defun salih/html2org-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (kill-new (shell-command-to-string "osascript -e 'the clipboard as \"HTML\"' | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' | pandoc -f html -t json | pandoc -f json -t org | sed 's/ / /g'"))
  (yank))

(defun salih/org-search-entries-with-today-date ()
  "Search all Org files for timestamps matching today's day and month."
  (interactive)
  (let* ((today (decode-time (current-time)))
         (day (format "%02d" (nth 3 today)))
         (month (format "%02d" (nth 4 today)))
         ;; Match both active <YYYY-MM-DD ...> and inactive [YYYY-MM-DD ...]
         (pattern (format "\\([<[]\\)[0-9]\\{4\\}-%s-%s[^]>]*[]>]?" month day))
         (consult-ripgrep-args
          (concat consult-ripgrep-args " -g *.org")))
    (consult-ripgrep org-roam-directory pattern)))

(defun salih/open-journal-file-for-today ()
  "List journal files in /Users/l/roam/journal that match today's MM-DD and let me open one."
  (interactive)
  (let* ((journal-dir "~/roam/journal/")
         (today (decode-time (current-time)))
         (month (format "%02d" (nth 4 today)))
         (day (format "%02d" (nth 3 today)))
         ;; Pattern to match filenames like 2023-06-08.org.gpg for today's MM-DD
         (match-pattern (format "-%s-%s\\.org\\.gpg$" month day))
         (files (directory-files journal-dir nil match-pattern)))
    (if files
        (let ((file-to-open (completing-read "Open journal file: " files nil t)))
          (find-file (expand-file-name file-to-open journal-dir)))
      (message "No journal files found for today (%s-%s)" month day))))

(defun salih/org-vocal-note ()
  "Record a vocal note and insert a link into the current org buffer."
  (interactive)
  (let* ((media-dir (expand-file-name salih/org-vocal-store))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (filename (format "vocal-note-%s.wav" timestamp))
         (filepath (expand-file-name filename media-dir))
         (recording-process nil))
    (unless (file-directory-p media-dir)
      (make-directory media-dir t))
    (message "Recording... Press any key to stop.")
    (setq recording-process
          (start-process "vocal-recording" nil "rec"
                        "-r" "44100"
                        "-c" "1"
                        "-b" "16"
                        filepath))
    (read-char)
    (when (and recording-process (process-live-p recording-process))
      (signal-process recording-process 'SIGINT)
      (while (process-live-p recording-process)
        (sleep-for 0.1))
      (sleep-for 0.5)) ; Additional time for file to be written
    (message "Recording stopped.")
    ;; Verify file exists before inserting link
    (if (file-exists-p filepath)
        (progn
          (insert (format "[[file:%s][%s]] " filepath timestamp))
          (message "Vocal note saved to %s" filepath))
      (message "Error: Recording file was not created at %s" filepath))))

(defun salih/org-vocal-note-with-transcription ()
  "Record a vocal note with optional transcription (requires whisper)."
  (interactive)
  (let* ((media-dir (expand-file-name salih/org-vocal-store))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (filename (format "vocal-note-%s.m4a" timestamp))
         (filepath (expand-file-name filename media-dir))
         (recording-process nil)
         (transcribe-p (y-or-n-p "Transcribe audio? (requires whisper) ")))
    (unless (file-directory-p media-dir)
      (make-directory media-dir t))
    (message "Recording... Press any key to stop.")
    (setq recording-process
          (start-process "vocal-recording" nil "ffmpeg"
                        "-f" "avfoundation"
                        "-i" ":0"
                        "-acodec" "aac"
                        "-ar" "44100"
                        "-ac" "1"
                        "-y"
                        filepath))
    (read-char)
    (when (and recording-process (process-live-p recording-process))
      (signal-process recording-process 'SIGINT)
      (while (process-live-p recording-process)
        (sleep-for 0.1))
      (sleep-for 1)) ; More time for ffmpeg to finalize the file
    (message "Recording stopped.")
    ;; Verify file exists before proceeding
    (if (file-exists-p filepath)
        (progn
          (insert (format "[[file:%s][🎤 %s]] " filepath timestamp))
          (when transcribe-p
            (message "Transcribing audio...")
            (let ((transcription
                   (shell-command-to-string
                    (format
                     "whisper --model small --output-format txt --output-dir /tmp %s && cat /tmp/%s.txt"
                     (shell-quote-argument filepath)
                     (file-name-sans-extension filename)))))
              (when (and transcription (not (string-empty-p transcription)))
                (insert (format "- %s" (string-trim transcription))))))
          (message "Vocal note saved to %s" filepath))
      (message "Error: Recording file was not created at %s" filepath))))


(provide '+helper)
