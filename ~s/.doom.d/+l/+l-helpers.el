;;; +l/helpers.el -*- lexical-binding: t; -*-

;; Timestamp utilities


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

(after! eshell
  (remove-hook 'eshell-mode-hook 'hide-mode-line-mode))

(after! vterm
  (remove-hook 'vterm-mode-hook 'hide-mode-line-mode))



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


(provide '+l-helpers)
