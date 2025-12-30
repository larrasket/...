;;; ../configs/~s/.doom.d/+l/+l-pic-log.el -*- lexical-binding: t; -*-
;;; pic-log.el --- Create pic-log entries from clipboard images -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions to create Hugo pic-log entries from clipboard images.
;; Usage: M-x salih/pic-log-from-clipboard
;; It will prompt for a description, save the clipboard image to
;; static/media/images/featured-images/, and create a markdown file
;; in content/pic-log/.

;;; Code:

(defvar salih/hugo-root "/Users/l/roam/hugo/"
  "Root directory of the Hugo site.")



(defun salih/pic-log-from-file (file-path description)
  "Create a pic-log entry from a FILE-PATH.
Prompts for DESCRIPTION, copies image to featured-images,
and creates markdown file in pic-log."
  (interactive "fImage file: \nsDescription: ")
  (let* ((date-str (format-time-string "%Y-%m-%d"))
         (date-pretty (format-time-string "%B %d, %Y"))
         (ext (file-name-extension file-path))
         (img-name (format "pic_%s_%s.%s" 
                           date-str 
                           (format-time-string "%H%M%S")
                           ext))
         (img-dir (expand-file-name "static/media/images/featured-images/" 
                                    salih/hugo-root))
         (img-dest (expand-file-name img-name img-dir))
         (content-dir (expand-file-name "content/pic-log/" salih/hugo-root))
         (md-path (expand-file-name (format "%s.md" date-str) content-dir))
         (img-url (format "/media/images/featured-images/%s" img-name)))
    
    ;; Ensure directories exist
    (unless (file-directory-p img-dir)
      (make-directory img-dir t))
    (unless (file-directory-p content-dir)
      (make-directory content-dir t))
    
    ;; Copy image file
    (copy-file file-path img-dest t)
    
    ;; Check if file for this date already exists
    (when (file-exists-p md-path)
      (setq md-path (expand-file-name 
                     (format "%s-%s.md" date-str (format-time-string "%H%M%S"))
                     content-dir)))
    
    ;; Create markdown file
    (with-temp-file md-path
      (insert (format "---
title: \"Pic - %s\"
date: %s
image: \"%s\"
description: \"%s\"
---
" 
                      date-pretty
                      date-str
                      img-url
                      description)))
    
    (message "Created pic-log: %s with image %s" 
             (file-name-nondirectory md-path)
             img-name)))



;;;; Utility Functions

(defun salih/--generate-org-id ()
  "Generate a simple org-roam style ID."
  (format "%s%s"
          (downcase (substring (md5 (format "%s%s" (current-time) (random))) 0 8))
          "0"))

(defun salih/--save-clipboard-image (dest-path)
  "Save clipboard image to DEST-PATH using pngpaste.
Returns t on success, nil on failure."
  (let ((exit-code (call-process "pngpaste" nil nil nil dest-path)))
    (= exit-code 0)))

(defun salih/--ensure-dir (dir)
  "Ensure DIR exists, creating it if necessary."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun salih/--slugify (text)
  "Convert TEXT to a URL-friendly slug."
  (let ((slug (downcase text)))
    (setq slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
    (setq slug (replace-regexp-in-string "^-\\|-$" "" slug))
    slug))

;;;; Film Entry

(defun salih/film-from-clipboard (title director genre)
  "Create a film entry from clipboard image.
Prompts for TITLE, DIRECTOR, and GENRE.
Saves clipboard image to films-images and creates org file in content/films/."
  (interactive "sFilm title (with year, e.g. 'Whiplash (2014)'): \nsDirector: \nsGenre (e.g. 'Drama, Music'): ")
  (let* ((date-str (format-time-string "<%Y-%m-%d %a>"))
         (org-id (salih/--generate-org-id))
         (slug (salih/--slugify title))
         (img-name (format "%s.jpg" slug))
         (img-dir (expand-file-name "static/media/images/films-images/"
                                    salih/hugo-root))
         (img-path (expand-file-name img-name img-dir))
         (content-dir (expand-file-name "content/films/" salih/hugo-root))
         (org-path (expand-file-name (format "%s.org" slug) content-dir))
         (img-url (format "/media/images/films-images/%s" img-name)))

    (salih/--ensure-dir img-dir)
    (salih/--ensure-dir content-dir)

    ;; Save clipboard image
    (if (salih/--save-clipboard-image img-path)
        (progn
          ;; Create org file
          (with-temp-file org-path
            (insert (format ":PROPERTIES:
:ID:       %s
:CUSTOM_ID: %s
:END:
#+TITLE: %s
#+DATE: %s
#+IMAGE: %s
#+GENRE: %s
#+DIRECTOR: %s

"
                            org-id
                            org-id
                            title
                            date-str
                            img-url
                            genre
                            director)))

          ;; Open the file for editing
          (find-file org-path)
          (goto-char (point-max))
          (message "Created film: %s with image %s"
                   (file-name-nondirectory org-path)
                   img-name))
      (error "Failed to paste image. Make sure you have an image copied and pngpaste installed"))))

;;;; Music Discovery Entry

(defun salih/music-discovery-from-clipboard (artist title)
  "Create a music discovery entry from clipboard image.
Prompts for ARTIST and TITLE.
Saves clipboard image and creates org file in content/music/discoveries/."
  (interactive "sArtist: \nsTitle/Album: ")
  (let* ((date-str (format-time-string "<%Y-%m-%d %a>"))
         (org-id (salih/--generate-org-id))
         (slug (salih/--slugify (format "%s-%s" artist title)))
         (img-name (format "discovery_%s.jpg" slug))
         (img-dir (expand-file-name "static/media/images/music/"
                                    salih/hugo-root))
         (img-path (expand-file-name img-name img-dir))
         (content-dir (expand-file-name "content/music-pages/discoveries/" salih/hugo-root))
         (org-path (expand-file-name (format "%s.org" slug) content-dir))
         (img-url (format "/media/images/music/%s" img-name)))

    (salih/--ensure-dir img-dir)
    (salih/--ensure-dir content-dir)

    ;; Save clipboard image
    (if (salih/--save-clipboard-image img-path)
        (progn
          ;; Create org file
          (with-temp-file org-path
            (insert (format ":PROPERTIES:
:ID:       %s
:CUSTOM_ID: %s
:END:
#+TITLE: %s
#+DATE: %s
#+ARTIST: %s
#+IMAGE: %s

"
                            org-id
                            org-id
                            title
                            date-str
                            artist
                            img-url)))

          ;; Open the file for editing
          (find-file org-path)
          (goto-char (point-max))
          (message "Created music discovery: %s with image %s"
                   (file-name-nondirectory org-path)
                   img-name))
      (error "Failed to paste image. Make sure you have an image copied and pngpaste installed"))))

;;;; Exhibit Entry

(defun salih/exhibit-from-clipboard (title description)
  "Save exhibit image from clipboard to exhibit directory.
Prompts for TITLE and DESCRIPTION (for filename generation).
Only saves the image, does not create any org file."
  (interactive "sExhibit title: \nsDescription: ")
  (let* ((slug (salih/--slugify title))
         (img-name (format "exhibit_%s.jpg" slug))
         (img-dir (expand-file-name "static/media/images/exhibit/"
                                    salih/hugo-root))
         (img-path (expand-file-name img-name img-dir)))

    (salih/--ensure-dir img-dir)

    ;; Save clipboard image only
    (if (salih/--save-clipboard-image img-path)
        (message "Saved exhibit image: %s" img-name)
      (error "Failed to paste image. Make sure you have an image copied and pngpaste installed"))))

;;;; Pic-log (moved from pic-log.el for consolidation)

(defun salih/pic-log-from-clipboard (description)
  "Create a pic-log entry from clipboard image.
Prompts for DESCRIPTION, saves clipboard image to featured-images,
and creates markdown file in pic-log with front matter."
  (interactive "sDescription: ")
  (let* ((date-str (format-time-string "%Y-%m-%d"))
         (date-pretty (format-time-string "%B %d, %Y"))
         (img-name (format "pic_%s_%s.png"
                           date-str
                           (format-time-string "%H%M%S")))
         (img-dir (expand-file-name "static/media/images/featured-images/"
                                    salih/hugo-root))
         (img-path (expand-file-name img-name img-dir))
         (content-dir (expand-file-name "content/pic-log/" salih/hugo-root))
         (md-path (expand-file-name (format "%s.md" date-str) content-dir))
         (img-url (format "/media/images/featured-images/%s" img-name)))

    (salih/--ensure-dir img-dir)
    (salih/--ensure-dir content-dir)

    ;; Save clipboard image
    (if (salih/--save-clipboard-image img-path)
        (progn
          ;; Check if file for this date already exists
          (when (file-exists-p md-path)
            (setq md-path (expand-file-name
                           (format "%s-%s.md" date-str (format-time-string "%H%M%S"))
                           content-dir)))

          ;; Create markdown file
          (with-temp-file md-path
            (insert (format "---
title: \"Pic - %s\"
date: %s
image: \"%s\"
description: \"%s\"
---
"
                            date-pretty
                            date-str
                            img-url
                            description)))

          (message "Created pic-log: %s with image %s"
                   (file-name-nondirectory md-path)
                   img-name))
      (error "Failed to paste image. Make sure you have an image copied and pngpaste installed"))))












(provide '+l-pic-log)
;;; pic-log.el ends here
