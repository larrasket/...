;;; config.el -*- lexical-binding: t; -*-
(put 'org-download-image-dir 'safe-local-variable
     (lambda (val) (stringp val)))
(require '+early)
(setq salih/temp-roam-insert nil)
(setq user-full-name                                    "Salih Muhammed"
      user-mail-address                                 "salih.moahabdelhafez@halan.com"
      user-first-name                                   "Salih"
      ;; TODO Commenting these temporarily until I get back to mu4e with iCloud.
      ;; user-stmp-server                                  "mail.gmx.com"
      ;; user-stmp-port                                    587
      user-short-username                               "lr0"
      user-config-repo-path                             "~/configs/~s"
      salih/blog-content-path                           "~/blog/content"
      salih/hugo-directory                              "~/roam/hugo/"
      org-roam-directory                                (file-truename "~/roam")
      doom-font                                         (font-spec :family "Pragmasevka" :size 16)
      ;; kaolin-dark
      ;; doom-badger
      ;; kaolin-temple
      doom-theme                                        'ef-elea-dark
      doom-modeline-icon                                t
      doom-modeline-height                              32
      display-line-numbers-type                         'relative
      ;; org
      org-directory                                     org-roam-directory
      org-id-locations-file                             "~/roam/.orgids"
      +org-capture-changelog-file                       "~/blog/content/nice.org"
      +org-capture-journal-file                         "~/blog/content/stack.org"
      salih/org-roam-fleet-file                         "~/roam/main/lr.org"
      salih/org-vocal-store                             "~/roam/media/vocal"
      +org-capture-todo-file                            "~/roam/main/life.org"
      salih/vulpea-show-full                            nil
      salih/adding-note?                                nil
      salih/org-agenda-full-f                           nil
      ;; this option is useful when you are up after 00:00. set 0 to the value
      ;; you sleep at. if you sleep at 02:00 it should be 2, if you sleep at
      ;; 02:30 it should be 3 and so on. Org agenda for the day will not overlap
      ;; until your day is done.
      ;; [2024-08-07 Wed 19:43] currently I sleep at 07:00.
      ;; [2024-08-08 Wed 23:41] Not anymore.
      ;; [2025-09-20 Sat 00:02] Quite outdated information huh :)
      ;; [2026-01-01 Thu 03:01] Quite outdating outdated
      org-extend-today-until                            2

      ;; other
      auto-save-no-message                              t
      dired-preview-delay                               0.1
      safe-local-variable-values
      '((org-download-image-dir
         . "../i")
        (salih/rebuild . t)))

(setq epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-select-keys                               'silent
      epa-file-encrypt-to                                user-mail-address)

(setq doom-modeline-icon nil)
(setq doom-modeline-height 30)
(setq doom-modeline-unicode-fallback t)
(setq doom-modeline-height 30)
(setq doom-modeline-bar-width 1)
(setq doom-modeline-major-mode-icon t
      doom-modeline-icon t
      doom-modeline-buffer-state-icon nil)


(setq gac-debounce-interval                             200
      gac-silent-message-p                              t)



;; currently org causes some annoying warnings because of org-element
;; breaking API updates.
;; [2024-04-26 Fri 02:01] I wrote "currently" above a long time ago
;; (perhaps can be detected from the git history, too lazy tho). Not sure
;; if it is still the case
;; [2024-11-20 Wed 11:45] Let's try without it!
;; [2024-11-22 Fri 12:07] Works fine so far.
;; [2025-06-08 Sun 12:20] It's back!
;; [2025-06-27 Fri 20:41] https://github.com/org-noter/org-noter/issues/111
;; [2025-06-27 Fri 20:42] https://list.orgmode.org/87qzzfd7bf.fsf@localhost/T/#t
;; [2025-09-20 Sat 00:02] I cleaned my org config. Let's give that a try again.
;; warning-minimum-level                             :error)


(require '+l-init)

(s/require
 '+bindings
 '+early)

(when (eq system-type 'darwin)
  (menu-bar-mode -1)
  (require 'ls-lisp)
  (setq mac-function-modifier 'control)
  (setq mac-option-key-is-meta               nil
        mac-command-key-is-meta              t
        mac-command-modifier                 'meta
        mac-option-modifier                  'none
        frame-title-format                   nil
        org-download-screenshot-method       "/usr/local/bin/pngpaste %s"
        ls-lisp-dirs-first                   t
        ls-lisp-use-insert-directory-program nil
        epg-pinentry-mode                    'loopback)
;;; Transparent titlebar
  ;; https://github.com/d12frosted/homebrew-emacs-plus/blob/master/Formula/emacs-plus.rb#L98
  ;; https://github.com/d12frosted/homebrew-emacs-plus/issues/55
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Properties-in-Mode.html#Properties-in-Mode
  ;; (modify-all-frames-parameters '((inhibit-double-buffering . t)))
  (add-to-list 'default-frame-alist    '(ns-appearance . 'dark))
  (add-to-list 'default-frame-alist    '(ns-transparent-titlebar . t)))








(breadcrumb-mode)
(global-jinx-mode)
(yas-global-mode 1)
(display-battery-mode)
(salih/keyboard-config)
(consult-org-roam-mode 1)
(global-visual-line-mode 1)
(awqat-notification-mode 1)
(awqat-display-prayer-time-mode)
;; (global-display-line-numbers-mode)

(custom-set-faces
 '(mode-line ((t (:family "Pragmasevka"))))
 '(mode-line-active ((t (:family "Pragmasevka"))))
 '(mode-line-inactive ((t (:family "Pragmasevka")))))

(set-popup-rules! '(("^\\*Project errors\\*" :size 0.25)))
(set-face-attribute 'shr-text nil :family "Arial" :height 180)


(setq modus-themes-common-palette-overrides
      '((fg-line-number-inactive bg-alt)
        (fg-line-number-active bg-alt)
        (bg-line-number-inactive unspecified)
        (bg-line-number-active unspecified)))

(custom-set-faces
 '(line-number ((t (:slant normal))))
 '(line-number-current-line ((t (:slant normal)))))

(setq modus-themes-italic-constructs t)
(setq modus-themes-bold-constructs t)

(set-fringe-style '(2 . 0))

(require 'spacious-padding)


(defun calculate-artist-favor-scores (org-file)
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

(defun display-artist-favor-scores (org-file)
  "Calculate and display artist favor scores in a formatted buffer."
  (interactive "fOrg file: ")
  (let ((results (calculate-artist-favor-scores org-file)))
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



(setq
 ;; Edit settings
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-agenda-tags-column 'auto
 org-ellipsis "…")

(setq dired-preview-max-size (* 1024 1024 30))


(setq evil-respect-visual-line-mode t)
(global-org-modern-mode -1)

(set-fringe-style '(1 . 1))
(setq evil-respect-visual-line-mode t)
