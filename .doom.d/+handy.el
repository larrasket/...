;;; ../configs/.doom.d/handy.el -*- lexical-binding: t; -*-
;;; contains some handy functions to use at once; not loaded by default

;; TODO clean this
(provide '+handy)

;; basic definiton for keys.el

(defun salih/global (key-sequence)
  (kbd (concat salih/prefix-global key-sequence)))
(defun salih/mode (key-sequence)
  (kbd (concat salih/prefix-mode   key-sequence)))



;; fix evil C-g methods

(require 'evil)

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

(defun gk-next-theme ()
  "Switch to the next theme in ‘custom-known-themes’.
If exhausted, disable themes.  If run again thereafter, wrap to
the beginning of the list."
  (interactive)
  (let* ((ct (or (car custom-enabled-themes)
                 (car custom-known-themes)))
         (next (cadr (memq ct custom-known-themes))))
    (when (memq next '(user changed))
      (setq next nil))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (if next
        (progn
          (load-theme next t)
          (message "Loaded theme ‘%S’" next))
      (message "All themes disabled"))))



(defun org-archive-subtree-if-match (match)
  "Archive all subtrees matching the given MATCH pattern."
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from
           (org-element-property :begin (org-element-at-point))))
   match 'tree))

(defun salih/org-archive-done-and-killed-tasks ()
  "Archive all DONE and KILL tasks in the current buffer."
  (interactive)
  (org-archive-subtree-if-match "/DONE")
  (org-archive-subtree-if-match "/KILL"))



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

(defun highltier ()
  (require 'highlight-indent-guides)
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (highlight-indent-guides-mode))

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
