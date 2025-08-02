;;; +l/helpers.el -*- lexical-binding: t; -*-

;; Timestamp utilities
(defun salih/insert-now-timestamp()
  (interactive)
  (org-insert-time-stamp (current-time) t))

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

(defun salih/open-kitty-in-current-directory ()
  "Open the Kitty terminal in the current working directory."
  (interactive)
  (call-process "kitty" nil 0 nil "--directory" default-directory))


(defun salih/org-calendar-goto-agenda ()
  (interactive)
  (let ((org-agenda-span 1))
    (org-calendar-goto-agenda)))

(defun salih/open-book ()
  "Search for a file in ~/me and open it."
  (interactive)
  (let ((default-directory (concat salih/source-directory "/")))
    (call-interactively 'find-file)))




(defun salih/zathura-open ()
  (interactive)
  (let ((process-connection-type nil))
    (start-process "" nil "open"  buffer-file-name)))

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

(after! eshell (remove-hook 'eshell-mode-hook 'hide-mode-line-mode))


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

(defun salih/load-last-open-rss-time ()
  "Load the last execution time from the cache file."
  (when (f-exists? salih/open-rss-lock-file)
    (with-temp-buffer
      (insert-file-contents salih/open-rss-lock-file)
      (read (current-buffer)))))


(defun salih/read-feeds-anyway () (interactive) (salih/open-rss t))

(defun salih/read-feeds () (interactive) (salih/open-rss t))


;; File utilities
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


;; Theme utilities
(defun salih/get-random-theme-full (n)
  "Get a random theme from the preferred themes list."
  (nth (random n) salih/prefered-themes))

;; Disable bright function
(defun salih/disable-bright ()
  "Disable bright mode for current buffer."
  (when (bound-and-true-p bright-mode)
    (bright-mode -1)))

;; Toggle functions
(defun salih/toggle-logbook-on ()
  "Enable logbook for org mode."
  (setq org-log-into-drawer t))

(defun salih/toggle-logbook-off ()
  "Disable logbook for org mode."
  (setq org-log-into-drawer nil))

(defun salih/toggle-stats-on ()
  "Enable stats for org mode."
  (setq org-log-into-drawer "STATS"))

(defun salih/toggle-log-int-drawer-off ()
  "Disable log into drawer for org mode."
  (setq org-log-into-drawer nil))

;; Org media utilities
(defun salih/org-media-note-insert-link (orig-fun &rest args)
  "Custom wrapper for org media note insert link."
  (let ((org-log-into-drawer nil))
    (apply orig-fun args)))

;; EWW utilities
(defun salih/ensure-eww-in-search (orig-fun &rest args)
  "Ensure EWW is used for search results."
  (let ((browse-url-browser-function 'eww-browse-url))
    (apply orig-fun args)))

;; Org-ql utilities
(defun salih/org-ql-view--format-element (orig-fun &rest args)
  "Custom formatter for org-ql view elements."
  (apply orig-fun args))

;; SLY utilities
(defun salih/sly--compile-eval-begin-print (orig-fun &rest args)
  "Custom wrapper for SLY compile and eval functions."
  (apply orig-fun args))

;; LSP utilities
(defun lsp-booster--advice-final-command (orig-fun &rest args)
  "Custom wrapper for LSP final command."
  (apply orig-fun args))

;; IRC utilities
(defun salih/tracking-next-buffer--always-switch (orig-fun &rest args)
  "Custom wrapper for IRC tracking next buffer."
  (apply orig-fun args))

;; Deft utilities
(defun cm/deft-parse-title (orig-fun &rest args)
  "Custom parser for Deft titles."
  (apply orig-fun args))

;; Org-id utilities
(defun salih/set-custom-id-to-id (orig-fun &rest args)
  "Set custom ID for org elements."
  (apply orig-fun args))

;; Go macro utilities
(defun salih/gomacro--sanitize-string (orig-fun &rest args)
  "Custom sanitizer for Go macros."
  (apply orig-fun args))

;; Org-noter utilities
(defun salih/org-noter-pdf--pdf-view-get-precise-info (orig-fun &rest args)
  "Custom wrapper for org-noter PDF view."
  (apply orig-fun args))

;; Open URL utilities
(defun salih/open-url-in-chrome-cross-platform (url &optional new-window)
  "Open URL in Chrome browser, works on macOS, Linux, and Windows."
  (cond
   ;; macOS
   ((eq system-type 'darwin)
    (start-process "chrome" nil "open" "-a" "Google Chrome" url))
   ;; Linux
   ((eq system-type 'gnu/linux)
    (start-process "chrome" nil "google-chrome" url))
   ;; Windows
   ((eq system-type 'windows-nt)
    (start-process "chrome" nil "chrome" url))
   ;; Fallback
   (t
    (browse-url url))))

;; Start note function
(defun salih/start-note () (setq salih/adding-note? t))

(provide '+l-helpers)
