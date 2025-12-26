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
    
    ;; Ensure directories exist
    (unless (file-directory-p img-dir)
      (make-directory img-dir t))
    (unless (file-directory-p content-dir)
      (make-directory content-dir t))
    
    ;; Save clipboard image using pngpaste (macOS)
    (let ((exit-code (call-process "pngpaste" nil nil nil img-path)))
      (if (= exit-code 0)
          (progn
            ;; Check if file for this date already exists
            (when (file-exists-p md-path)
              ;; Append timestamp to filename to make unique
              (setq md-path (expand-file-name 
                             (format "%s-%s.md" date-str (format-time-string "%H%M%S"))
                             content-dir)))
            
            ;; Create markdown file with front matter
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
        (error "Failed to paste image from clipboard. Make sure you have an image copied and pngpaste installed (brew install pngpaste)")))))

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

(provide 'pic-log)
;;; pic-log.el ends here

