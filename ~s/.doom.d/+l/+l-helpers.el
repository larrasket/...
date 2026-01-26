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

(after! embark
  (add-to-list 'embark-keymap-alist '(org-timestamp embark-org-timestamp-map))
  (defvar-keymap embark-org-timestamp-map
    :doc "Keymap for actions on an org timestamp."
    :parent embark-general-map
    "t" #'salih/org-add-week-to-timestamp)
  (define-key embark-url-map (kbd "c") 'salih/open-url-in-chrome-cross-platform)
  (define-key embark-org-link-map (kbd "RET") 'org-web-tools-read-url-as-org))


(after! vertico-multiform ;; if using vertico
  (add-to-list 'vertico-multiform-categories
               '(jinx (vertico-grid-annotate . 25)))

  (vertico-multiform-mode 1))

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

(defun salih/calculate-artist-favor-scores (org-file)
  "Calculate favor scores for artists in ORG-FILE using two approaches.
Returns a list of plists with artist info and scores."
  (require 'org)
  (require 'org-roam)
  
  ;; Inline function for simple approach
  (cl-flet ((simple-score (loved total)
              "Simple approach: (L/T) * log(L+1)"
              (if (and (> total 0) (> loved 0))
                  (* (/ (float loved) total)
                     (log (1+ loved)))
                0.0))
            
            ;; Inline function for information theory approach
            (entropy-score (loved total)
              "Information theory approach with entropy"
              (if (or (<= total 0) (< loved 0))
                  0.0
                (let* ((ratio (/ (float loved) total))
                       (confidence (- 1 (/ 1 (sqrt total))))
                       ;; Calculate entropy H(L,T)
                       (p1 ratio)
                       (p2 (- 1 ratio))
                       (entropy (if (and (> p1 0) (> p2 0))
                                    (- (+ (* p1 (log p1))
                                          (* p2 (log p2))))
                                  0.0))
                       ;; Normalize entropy bonus
                       (entropy-bonus (if (> total 1)
                                          (/ entropy (log (1+ total)))
                                        0.0)))
                  (* ratio
                     confidence
                     (1+ entropy-bonus))))))
    
    (with-temp-buffer
      (insert-file-contents org-file)
      (org-mode)
      (goto-char (point-min))
      
      (let ((results '()))
        ;; Find all artist headings (level 2 under "People")
        (while (re-search-forward "^\\*\\* " nil t)
          (let* ((heading (org-get-heading t t t t))
                 (id (org-entry-get (point) "ID"))
                 (nworks (org-entry-get (point) "NWORKS")))
            
            (when (and id nworks)
              (let* ((total (string-to-number nworks))
                     (node (org-roam-node-from-id id))
                     (backlinks (when node (org-roam-backlinks-get node)))
                     (loved (length backlinks))
                     (simple (simple-score loved total))
                     (entropy (entropy-score loved total)))
                
                (push (list :name heading
                            :id id
                            :loved loved
                            :total total
                            :simple-score simple
                            :entropy-score entropy)
                      results)))))
        
        ;; Return sorted by entropy score (descending)
        (sort results (lambda (a b)
                        (> (plist-get a :entropy-score)
                           (plist-get b :entropy-score))))))))


(defun salih/display-artist-favor-scores (org-file)
  "Calculate and display artist favor scores in a formatted buffer."
  (interactive "fOrg file: ")
  (let ((results (salih/calculate-artist-favor-scores org-file)))
    (with-current-buffer (get-buffer-create "*Artist Favor Scores*")
      (erase-buffer)
      (insert (format "%-30s %6s %6s %12s %12s\n"
                      "Artist" "Loved" "Total" "Simple" "Entropy"))
      (insert (make-string 80 ?-) "\n")
      
      (dolist (artist results)
        (insert (format "%-30s %6d %6d %12.4f %12.4f\n"
                        (plist-get artist :name)
                        (plist-get artist :loved)
                        (plist-get artist :total)
                        (plist-get artist :simple-score)
                        (plist-get artist :entropy-score))))
      
      (goto-char (point-min))
      (display-buffer (current-buffer)))))


(defun salih/add-diary-entry-to-hugo ()
  "Create a new Hugo diary entry for today and insert a diary template.
If the diary already exists, add a new time-stamped heading at the bottom."
  (interactive)
  (let* ((diary-dir
          (expand-file-name "content/diary/"
                            (file-name-as-directory salih/hugo-directory)))
         (date-iso (format-time-string "<%Y-%m-%d %a>"))
         (date-title (format-time-string "%B %-d, %Y"))
         (current-time (format-time-string "%H:%M"))
         (file-path (expand-file-name
                     (concat (format-time-string "%Y-%m-%d") ".org")
                     diary-dir)))
    ;; Ensure directory exists
    (unless (file-directory-p diary-dir)
      (make-directory diary-dir t))
    ;; Create & open file
    (find-file file-path)
    (if (= (buffer-size) 0)
        ;; New file - insert full template
        (progn
          (insert
           (format
            "#+title: \"Diary Entry - %s\"\n#+DATE: %s\n\n"
            date-title
            date-iso))
          (org-id-get-create)
          (goto-char (point-max))
          (insert (format "* %s\n" current-time)))
      ;; Existing file - add new heading with time
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert (format "* %s\n" current-time)))
    ;; Move cursor to end
    (goto-char (point-max))))



(defun salih/add-microblog-to-hugo ()
  "Create a new Hugo microblog file for today and insert a template."
  (interactive)
  (let* ((microblog-dir
          (expand-file-name "content/microblog/"
                            (file-name-as-directory salih/hugo-directory)))
         (date-iso (format-time-string "%Y-%m-%d"))
         (date-day (format-time-string "%a"))
         (time-str (format-time-string "%H:%M"))
         (id (org-id-new))
         ;; Find next available number for today
         (counter 1)
         file-path)
    
    (unless (file-directory-p microblog-dir)
      (make-directory microblog-dir t))
    
    ;; Find next available number
    (while (file-exists-p
            (expand-file-name
             (format "%s-%d.org" date-iso counter)
             microblog-dir))
      (setq counter (1+ counter)))
    
    (setq file-path (expand-file-name
                     (format "%s-%d.org" date-iso counter)
                     microblog-dir))
    
    (find-file file-path)
    
    (when (= (buffer-size) 0)
      (insert
       (format
        ":PROPERTIES:\n:ID:       %s\n:END:\n#+title: Microblog Post %d\n#+date: <%s %s %s>\n\n"
        id
        date-iso
        date-day
        time-str
        counter)))
    (goto-char (point-max))))



(provide '+l-helpers)
