;;; ../configs/.doom.d/handy.el -*- lexical-binding: t; -*-
;;; contains some handy functions to use at once; not loaded by default

;; TODO clean this
(require 'evil)
(provide '+handy)

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


;; handy stuff


(defun salih/org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))


(defun salih/org-archive-killed-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/KILL" 'file))




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




(defun salih/bidi-direction-toggle ()
  (interactive "")
  (setq bidi-display-reordering t)
  (if (equal bidi-paragraph-direction 'right-to-left)
      (setq bidi-paragraph-direction 'left-to-right)
    (setq bidi-paragraph-direction 'right-to-left))
  (message "%s" bidi-paragraph-direction))


(defun salih/toggle-maximize-buffer ()
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))


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


(defun salih/rename-or-iedit ()
  "If current buffer is in lsp-mode, call lsp-rename. Otherwise, call
iedit-mode."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (call-interactively #'lsp-rename)
    (call-interactively #'iedit-mode)))


(defun insert-now-timestamp()
  (org-insert-time-stamp (current-time) t))


(defun salih/find-definition-or-lookup ()
  (interactive)
  "If current buffer is in lsp-mode, call lsp-find-definition. Otherwise, call
lookup."
  (if (bound-and-true-p lsp-mode)
      (call-interactively #'lsp-find-definition)
    (call-interactively #'+lookup/file)))


(defun salih/randomize-date-time ()
  "Randomize the time for the date on the current line."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "#\\+DATE: *<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)>" nil t)
      (let* ((date (match-string 1))
             (time (format "%02d:%02d:%02d" (random 24) (random 60) (random 60)))
             (new-date (concat date " " time)))
        (replace-match (concat "#+DATE: <" new-date ">"))))))


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


(defun salih/make-buffer-white ()
  (interactive)
  (setq buffer-face-mode-face `(:background "white"
                                :foreground "black"))
  (face-remap-add-relative 'hl-line :background "#e6e6e6")
  (face-remap-add-relative 'link :foreground "blue")
  (buffer-face-mode))


(defun salih/open-book ()
  "Search for a file in ~/me and open it."
  (interactive)
  (let ((default-directory "~/source/"))
    (call-interactively 'find-file)))


;;;; My embark / consult-preview-at-point-mode
;; It is meant to be used for the *Embark Collect* buffer from the list of
;; Org-roam nodes candidate It provides preview of the note at point as you move
;; throught the candidate list in *Embark Collect* buffer.
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



(defun highltier ()
  (interactive)
  (require 'highlight-indent-guides)
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (highlight-indent-guides-mode))



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



(defun salih/solaire-mode-real-buffer-custom-p ()
  "Return t if the current buffer is the dashboard or scratch, or is a real
(file-visiting) buffer."
  (cond ((string= (buffer-name (buffer-base-buffer)) "*sly-mrepl for sbcl*") t)
        ((buffer-file-name (buffer-base-buffer)) t)
        (t nil)))








(defun salih/xwidget-open-html ()
  "Open the current buffer's file path in an xwidget window."
  (interactive)
  (add-hook 'after-save-hook 'xwidget-webkit-reload)
  (let ((file-path (buffer-file-name)))
    (when file-path
      (let ((xwidget (xwidget-webkit-browse-url (concat "file://" file-path))))
        (message "Opened file %s in an xwidget window." file-path)))))