;;; lr-tools.el --- Translate, browse, hugo, pic-log, prayer -*- lexical-binding: t; -*-

;;; --- Translation ---
(after! gt
  (setq gt-langs '("en" "ar")
        gt-default-translator
        (gt-translator
         :taker   (gt-taker :text 'buffer :pick 'paragraph)
         :engines (list (gt-google-engine))
         :render  (gt-buffer-render))))

;;; --- Browse / EWW ---
(defun salih/open-url-in-chrome-cross-platform (url &optional _new-window)
  "Open URL in system browser."
  (pcase system-type
    ('darwin    (start-process "chrome" nil "open" url))
    ('gnu/linux (start-process "chrome" nil "google-chrome" url))
    (_          (browse-url url))))

(advice-remove '+lookup/documentation #'salih/ensure-eww-in-search)

;;; --- Prayer times (lazy) ---
(after! awqat
  (setq calendar-latitude  29.392691
        calendar-longitude 30.828360
        awqat-mode-line-format " ${prayer} (${hours}h${minutes}m) "
        awqat-update-interval  (* 60 5)))

;; Start prayer modes lazily
(add-hook 'doom-first-buffer-hook
          (lambda ()
            (when (featurep 'awqat)
              (awqat-notification-mode 1)
              (awqat-display-prayer-time-mode))))

;;; --- Magit ---
(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

;;; --- YAML env to dotenv ---
(defun yaml-env-to-dotenv ()
  "Convert Helm-style YAML env entries into a .env file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^env:" nil t)
      (user-error "No env: section found"))
    (let ((env-vars '()))
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
             ((re-search-forward "^[[:space:]]+value: \\(\"?\\)\\(.+?\\)\\1$" end t)
              (setq value (match-string 2)))
             ((re-search-forward "^[[:space:]]+valueFrom:" end t)
              (let (secret key)
                (when (re-search-forward "^[[:space:]]+name: \\(.+\\)$" end t)
                  (setq secret (match-string 1)))
                (when (re-search-forward "^[[:space:]]+key: \\(.+\\)$" end t)
                  (setq key (match-string 1)))
                (when (and secret key)
                  (setq value (format "<secret:%s/%s>" secret key)))))))
          (when value (push (format "%s=%s" name value) env-vars))))
      (let ((file (read-file-name "Save .env file as: " nil nil nil ".env")))
        (with-temp-file file
          (insert (string-join (nreverse env-vars) "\n") "\n"))
        (message "Wrote %d env vars to %s" (length env-vars) file)))))

;;; --- Hugo diary / microblog ---
(defvar salih/hugo-root (expand-file-name salih/hugo-directory))

(defun salih/add-diary-entry-to-hugo ()
  "Create or append to a Hugo diary entry for today."
  (interactive)
  (let* ((diary-dir (expand-file-name "content/diary/" salih/hugo-root))
         (date-title (format-time-string "%Y/%m/%d"))
         (current-time-str (format-time-string "%H:%M"))
         (file-path (expand-file-name
                     (concat (format-time-string "%Y-%m-%d") ".org") diary-dir)))
    (unless (file-directory-p diary-dir) (make-directory diary-dir t))
    (find-file file-path)
    (if (= (buffer-size) 0)
        (progn
          (insert (format "#+title: %s\n#+DATE: <%s>\n\n"
                          date-title (format-time-string "%Y-%m-%d %a")))
          (org-id-get-create)
          (goto-char (point-max))
          (insert (format "* %s\n" current-time-str)))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* %s\n" current-time-str)))
    (goto-char (point-max))))

(defun salih/add-microblog-to-hugo ()
  "Create a Hugo microblog file for today."
  (interactive)
  (let* ((microblog-dir (expand-file-name "content/microblog/" salih/hugo-root))
         (date-iso (format-time-string "%Y-%m-%d"))
         (counter 1)
         file-path)
    (unless (file-directory-p microblog-dir) (make-directory microblog-dir t))
    (while (file-exists-p
            (expand-file-name (format "%s-%d.org" date-iso counter) microblog-dir))
      (setq counter (1+ counter)))
    (setq file-path (expand-file-name (format "%s-%d.org" date-iso counter) microblog-dir))
    (find-file file-path)
    (when (= (buffer-size) 0)
      (insert (format ":PROPERTIES:\n:ID:       %s\n:END:\n#+title: Microblog Post %s\n#+date: <%s %s %s>\n\n"
                      (org-id-new) date-iso date-iso
                      (format-time-string "%a") (format-time-string "%H:%M"))))
    (goto-char (point-max))))

;;; --- Transfer between microblog and diary ---
;; Mirror of scripts/check_post_lengths.py, but entry-at-point in Emacs.
;; Golden rule: preserve the relevant :ID: so internal [[id:...]] links survive.

(defun salih/--org-file-id ()
  "Return the first (file-level) :ID: in the buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" nil t)
      (match-string 1))))

(defun salih/--org-title ()
  "Return the #+title value, trimmed, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+title:[ \t]*\\(.*\\)$" nil t)
      (string-trim (match-string 1)))))

(defun salih/--org-date-stamp ()
  "Return the #+date/#+DATE angle-bracket stamp, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+date:[ \t]*\\(<[^>]*>\\)" nil t)
      (match-string 1))))

(defun salih/--org-body-after-header ()
  "Buffer body after the top property drawer and leading #+ / blank lines."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "[ \t]*:PROPERTIES:")
      (re-search-forward "^[ \t]*:END:[ \t]*\n" nil t))
    (while (looking-at "[ \t]*\\(#\\+\\|$\\)")
      (forward-line 1))
    (string-trim (buffer-substring-no-properties (point) (point-max)))))

(defun salih/--org-subtree-body ()
  "Body of the current subtree: contents after the heading and its drawer."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t) (point))))
      (forward-line 1)
      (when (looking-at "[ \t]*:PROPERTIES:")
        (re-search-forward "^[ \t]*:END:[ \t]*\n" end t))
      (while (and (< (point) end) (looking-at "[ \t]*$"))
        (forward-line 1))
      (string-trim-right (buffer-substring-no-properties (point) end)))))

(defun salih/--microblog-file-to-diary ()
  "Convert the whole current microblog file into a diary day."
  (let* ((file (buffer-file-name))
         (date-ymd (substring (file-name-base file) 0 10))
         (day-title (replace-regexp-in-string "-" "/" date-ymd))
         (org-id (salih/--org-file-id))
         (title (salih/--org-title))
         (stamp (or (salih/--org-date-stamp) (format "<%s>" date-ymd)))
         (time (if (string-match "[0-9]\\{1,2\\}:[0-9]\\{2\\}" stamp)
                   (match-string 0 stamp) "00:00"))
         (body (salih/--org-body-after-header))
         (heading (if (and title (not (string-prefix-p "Microblog Post" title)))
                      title
                    (read-string "Section heading: "
                                 (string-join (seq-take (split-string body) 6) " "))))
         (target (expand-file-name (concat "content/diary/" date-ymd ".org")
                                   salih/hugo-root))
         (this-buf (current-buffer)))
    (if (file-exists-p target)
        (progn
          (with-current-buffer (find-file-noselect target)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (format "\n* %s %s\n" time heading))
            (when org-id
              (insert (format ":PROPERTIES:\n:ID:       %s\n:CUSTOM_ID: %s\n:END:\n"
                              org-id org-id)))
            (insert body "\n")
            (save-buffer))
          (message "Appended to existing diary %s — file URL changed; check [[id:%s]] links"
                   (file-name-nondirectory target) org-id))
      (with-temp-file target
        (when org-id
          (insert (format ":PROPERTIES:\n:ID:       %s\n:CUSTOM_ID: %s\n:END:\n"
                          org-id org-id)))
        (insert (format "#+title: %s\n#+DATE: %s\n\n* %s %s\n%s\n"
                        day-title stamp time heading body)))
      (message "Created diary %s" (file-name-nondirectory target)))
    (delete-file file)
    (set-buffer-modified-p nil)
    (kill-buffer this-buf)
    (find-file target)
    (goto-char (point-max))))

(defun salih/--diary-heading-to-microblog ()
  "Extract the diary subtree at point into a new microblog file."
  (let* ((file (buffer-file-name))
         (date-ymd (substring (file-name-base file) 0 10))
         time heading title section-id body stamp)
    (save-excursion
      (org-back-to-heading t)
      (unless (looking-at "^\\*+[ \t]+\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)[ \t]*\\(.*\\)$")
        (user-error "Point is not inside a '* HH:MM' diary heading"))
      (setq time (match-string 1)
            heading (string-trim (match-string 2))
            title (if (string= heading "") (format "Microblog Post %s" date-ymd) heading))
      (let ((end (save-excursion (org-end-of-subtree t t) (point))))
        (setq section-id (save-excursion
                           (when (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" end t)
                             (match-string 1)))))
      (setq body (salih/--org-subtree-body)))
    (let ((dstamp (salih/--org-date-stamp)))
      ;; date stamp carries this section's time (day + weekday preserved)
      (setq stamp (if (and dstamp (string-match "<\\([0-9-]\\{10\\}\\(?: [A-Za-z]\\{3\\}\\)?\\)" dstamp))
                      (format "<%s %s>" (match-string 1 dstamp) time)
                    (format "<%s %s>" date-ymd time))))
    (let* ((micro-dir (expand-file-name "content/microblog/" salih/hugo-root))
           (counter 1) target)
      (while (file-exists-p (expand-file-name (format "%s-%d.org" date-ymd counter) micro-dir))
        (setq counter (1+ counter)))
      (setq target (expand-file-name (format "%s-%d.org" date-ymd counter) micro-dir))
      (with-temp-file target
        (when section-id
          (insert (format ":PROPERTIES:\n:ID:       %s\n:END:\n" section-id)))
        (insert (format "#+title: %s\n#+date: %s\n\n%s\n" title stamp body)))
      (save-excursion (org-back-to-heading t) (org-cut-subtree))
      (save-buffer)
      (when (save-excursion (goto-char (point-min)) (not (re-search-forward "^\\* " nil t)))
        (message "Diary %s now has no sections — clean up if needed"
                 (file-name-nondirectory file)))
      (find-file target)
      (message "Created microblog %s" (file-name-nondirectory target)))))

;;;###autoload
(defun salih/transfer-hugo-entry ()
  "Transfer the current entry between microblog and diary.
In a microblog file: convert the whole file into a diary day.
In a diary heading: extract that subtree into a new microblog file.
The relevant :ID: is preserved so internal [[id:...]] links keep resolving."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file (user-error "Buffer is not visiting a file"))
    (cond
     ((string-match-p "/content/microblog/" file) (salih/--microblog-file-to-diary))
     ((string-match-p "/content/diary/" file)     (salih/--diary-heading-to-microblog))
     (t (user-error "Not in a microblog file or a diary heading")))))

;;; --- Pic-log / film / music / exhibit ---
(defun salih/--generate-org-id ()
  (format "%s0" (downcase (substring (md5 (format "%s%s" (current-time) (random))) 0 8))))

(defun salih/--save-clipboard-image (dest-path)
  (= (call-process "pngpaste" nil nil nil dest-path) 0))

(defun salih/--ensure-dir (dir)
  (unless (file-directory-p dir) (make-directory dir t)))

(defun salih/--slugify (text)
  (let ((slug (downcase text)))
    (setq slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
    (replace-regexp-in-string "^-\\|-$" "" slug)))

(defun salih/pic-log-from-clipboard (description)
  "Create a pic-log entry from clipboard image."
  (interactive "sDescription: ")
  (let* ((date-str (format-time-string "%Y-%m-%d"))
         (img-name (format "pic_%s_%s.png" date-str (format-time-string "%H%M%S")))
         (img-dir (expand-file-name "static/media/images/featured-images/" salih/hugo-root))
         (img-path (expand-file-name img-name img-dir))
         (content-dir (expand-file-name "content/pic-log/" salih/hugo-root))
         (md-path (expand-file-name (format "%s.md" date-str) content-dir))
         (img-url (format "/media/images/featured-images/%s" img-name)))
    (salih/--ensure-dir img-dir)
    (salih/--ensure-dir content-dir)
    (if (salih/--save-clipboard-image img-path)
        (progn
          (when (file-exists-p md-path)
            (setq md-path (expand-file-name
                           (format "%s-%s.md" date-str (format-time-string "%H%M%S"))
                           content-dir)))
          (with-temp-file md-path
            (insert (format "---\ntitle: \"Pic - %s\"\ndate: %s\nimage: \"%s\"\ndescription: \"%s\"\n---\n"
                            (format-time-string "%B %d, %Y") date-str img-url description)))
          (message "Created pic-log: %s" (file-name-nondirectory md-path)))
      (error "Failed to paste image. Ensure pngpaste is installed"))))

(defun salih/film-from-clipboard (title director genre)
  "Create a film entry from clipboard image."
  (interactive "sFilm title (with year): \nsDirector: \nsGenre: ")
  (let* ((slug (salih/--slugify title))
         (img-name (format "%s.jpg" slug))
         (img-dir (expand-file-name "static/media/images/films-images/" salih/hugo-root))
         (img-path (expand-file-name img-name img-dir))
         (content-dir (expand-file-name "content/films/" salih/hugo-root))
         (org-path (expand-file-name (format "%s.org" slug) content-dir))
         (org-id (salih/--generate-org-id)))
    (salih/--ensure-dir img-dir)
    (salih/--ensure-dir content-dir)
    (if (salih/--save-clipboard-image img-path)
        (progn
          (with-temp-file org-path
            (insert (format ":PROPERTIES:\n:ID:       %s\n:CUSTOM_ID: %s\n:END:\n#+TITLE: %s\n#+DATE: %s\n#+IMAGE: /media/images/films-images/%s\n#+GENRE: %s\n#+DIRECTOR: %s\n\n"
                            org-id org-id title
                            (format-time-string "<%Y-%m-%d %a>")
                            img-name genre director)))
          (find-file org-path)
          (goto-char (point-max))
          (message "Created film: %s" (file-name-nondirectory org-path)))
      (error "Failed to paste image"))))

(defun salih/music-discovery-from-clipboard (artist title)
  "Create a music discovery entry from clipboard image."
  (interactive "sArtist: \nsTitle/Album: ")
  (let* ((slug (salih/--slugify (format "%s-%s" artist title)))
         (img-name (format "discovery_%s.jpg" slug))
         (img-dir (expand-file-name "static/media/images/music/" salih/hugo-root))
         (img-path (expand-file-name img-name img-dir))
         (content-dir (expand-file-name "content/music-pages/discoveries/" salih/hugo-root))
         (org-path (expand-file-name (format "%s.org" slug) content-dir))
         (org-id (salih/--generate-org-id)))
    (salih/--ensure-dir img-dir)
    (salih/--ensure-dir content-dir)
    (if (salih/--save-clipboard-image img-path)
        (progn
          (with-temp-file org-path
            (insert (format ":PROPERTIES:\n:ID:       %s\n:CUSTOM_ID: %s\n:END:\n#+TITLE: %s\n#+DATE: %s\n#+ARTIST: %s\n#+IMAGE: /media/images/music/%s\n\n"
                            org-id org-id title
                            (format-time-string "<%Y-%m-%d %a>")
                            artist img-name)))
          (find-file org-path)
          (goto-char (point-max))
          (message "Created music discovery: %s" (file-name-nondirectory org-path)))
      (error "Failed to paste image"))))

(defun salih/exhibit-from-clipboard (title _description)
  "Save exhibit image from clipboard."
  (interactive "sExhibit title: \nsDescription: ")
  (let* ((slug (salih/--slugify title))
         (img-name (format "exhibit_%s.jpg" slug))
         (img-dir (expand-file-name "static/media/images/exhibit/" salih/hugo-root))
         (img-path (expand-file-name img-name img-dir)))
    (salih/--ensure-dir img-dir)
    (if (salih/--save-clipboard-image img-path)
        (message "Saved exhibit image: %s" img-name)
      (error "Failed to paste image"))))

;;; --- Artist favor scores ---
(defun salih/calculate-artist-favor-scores (org-file)
  "Calculate favor scores for artists in ORG-FILE."
  (require 'org)
  (require 'org-roam)
  (cl-flet ((simple-score (loved total)
              (if (and (> total 0) (> loved 0))
                  (* (/ (float loved) total) (log (1+ loved))) 0.0))
            (entropy-score (loved total)
              (if (or (<= total 0) (< loved 0)) 0.0
                (let* ((ratio (/ (float loved) total))
                       (confidence (- 1 (/ 1 (sqrt total))))
                       (p1 ratio) (p2 (- 1 ratio))
                       (entropy (if (and (> p1 0) (> p2 0))
                                    (- (+ (* p1 (log p1)) (* p2 (log p2)))) 0.0))
                       (entropy-bonus (if (> total 1) (/ entropy (log (1+ total))) 0.0)))
                  (* ratio confidence (1+ entropy-bonus))))))
    (with-temp-buffer
      (insert-file-contents org-file)
      (org-mode)
      (goto-char (point-min))
      (let ((results '()))
        (while (re-search-forward "^\\*\\* " nil t)
          (let* ((heading (org-get-heading t t t t))
                 (id (org-entry-get (point) "ID"))
                 (nworks (org-entry-get (point) "NWORKS")))
            (when (and id nworks)
              (let* ((total (string-to-number nworks))
                     (node (org-roam-node-from-id id))
                     (backlinks (when node (org-roam-backlinks-get node)))
                     (loved (length backlinks)))
                (push (list :name heading :id id :loved loved :total total
                            :simple-score (simple-score loved total)
                            :entropy-score (entropy-score loved total))
                      results)))))
        (sort results (lambda (a b)
                        (> (plist-get a :entropy-score) (plist-get b :entropy-score))))))))

(defun salih/display-artist-favor-scores (org-file)
  "Display artist favor scores."
  (interactive "fOrg file: ")
  (let ((results (salih/calculate-artist-favor-scores org-file)))
    (with-current-buffer (get-buffer-create "*Artist Favor Scores*")
      (erase-buffer)
      (insert (format "%-30s %6s %6s %12s %12s\n" "Artist" "Loved" "Total" "Simple" "Entropy"))
      (insert (make-string 80 ?-) "\n")
      (dolist (artist results)
        (insert (format "%-30s %6d %6d %12.4f %12.4f\n"
                        (plist-get artist :name) (plist-get artist :loved)
                        (plist-get artist :total) (plist-get artist :simple-score)
                        (plist-get artist :entropy-score))))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'lr-tools)
