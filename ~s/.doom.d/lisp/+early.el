(require 'f)
(defalias 'l 'list)

(setq-default frame-title-format                        '("%b")
              shr-inhibit-images                        t
              bidi-paragraph-direction                  'left-to-right
              salih/me-location                         "~/me"
              org-download-image-dir                    "~/roam/media"
              indent-tabs-mode                          nil
              pdf-view-display-size                     'fit-width
              display-line-numbers-width                8)

(defmacro s/require (&rest packages)
  `(progn ,@(mapcar (lambda (pkg) `(if ,pkg (require ,pkg))) packages)))

(defun doom-theme-p? ()
  (or
   (string-prefix-p "kaolin-" (symbol-name doom-theme))
   (string-prefix-p "doom-" (symbol-name doom-theme))))

(defun kaolin-theme-p? ()
  (or
   (string-prefix-p "kaolin-" (symbol-name doom-theme))))

(defun salih/get-random-theme (inc)
  "Get a different theme every week based on the week number of the year."
  (let* ((current-week (+ inc (string-to-number (format-time-string "%U"))))
         (salih/prefered-themes (seq-filter (lambda (theme)
                                              (not (eq (cdr theme)
                                                       'nour)))
                                            salih/prefered-themes))
         (list-length (length salih/prefered-themes))
         (selected (nth (mod current-week list-length)
                        salih/prefered-themes)))
    selected))

(defun salih/get-random-theme-full (inc)
  "Get a different theme every week based on the week number of the year.
Excludes themes in the predefined skip list."
  (let* ((skip-list '(doom-bluloco-dark doom-bluloco-light doom-challenger-deep
                      doom-city-lights
                      doom-dark+
                      doom-ephemeral
                      doom-fairy-floss
                      doom-feather-dark
                      doom-flatwhite))
         (current-week (+ inc (string-to-number (format-time-string "%U"))))
         (available-themes (seq-filter (lambda (theme)
                                         (not (member theme skip-list)))
                                       (custom-available-themes)))
         (list-length (length available-themes))
         (selected (when (> list-length 0)
                     (nth (mod current-week list-length) available-themes))))
    selected))



(defun salih/get-random-nour-theme (inc)
  (let* ((salih/prefered-themes '((ef-frost . nour)
                                  (ef-light . nour))))
    (salih/get-random-theme inc)))

(defvar-local salih/modeline-buffer-name
     '(:eval
       (when (mode-line-window-selected-p)
         (propertize (salih/modeline--buffer-name)
                     'face 'salih/modeline-background)))
   "Mode line construct to display the buffer name.")


(defvar-local salih/modeline-major-mode
     '
     "Mode line construct to display the major mode.")

(define-minor-mode salih/consult-preview-at-point-mode
  "Preview minor mode for an *Embark Collect* buffer.
When moving around in the *Embark Collect* buffer, the candidate at point is
automatically previewed."
  :init-value nil :group 'consult
  (if salih/consult-preview-at-point-mode
      (add-hook 'post-command-hook #'salih/consult-preview-at-point nil 'local)
    (remove-hook 'post-command-hook #'salih/consult-preview-at-point 'local)))

(defvar salih/consult--source-books
  `(:name     "File"
    :narrow   ?f
    :category file
    :face     consult-file
    :history  file-name-history
    :state    ,#'consult--file-state
    :new      ,#'consult--file-action
    :items
    ,(lambda ()
       (let ((ht (consult--buffer-file-hash))
             items)
         (dolist (file (bound-and-true-p salih/books) (nreverse items))
           (unless (eq (aref file 0) ?/)
             (let (file-name-handler-alist)
               (setq file (expand-file-name file))))
           (unless (gethash file ht)
             (push (consult--fast-abbreviate-file-name file) items)))))))

(defvar salih/open-rss-lock-file (f-join doom-cache-dir "rss-locker")
  "File used to store the last execution time of `salih/open-rss`.")

;; lisp
(defvar salih/sly--compile-eval-begin-print-counter 0
  "a counter to distinguish compile/eval cycles")
(defun salih/sly--compile-eval-begin-print (&rest _)
  "print the counter value into REPL to distinguish compile/eval cycles."
  (sly-eval-async
   `(cl:format t "" ,(cl-incf salih/sly--compile-eval-begin-print-counter))))

(defvar org-roam-list-most-linked-count 5)

(defvar salih/org-roam-dailies-capture-p nil)



(defvar salih/prefered-themes '((doom-peacock             . dark)
                                ;; (doom-rouge               . dark)
                                ;; (doom-henna               . dark)
                                (kaolin-dark              . dark)
                                (ef-maris-dark            . dark)
                                (ef-deuteranopia-light    . nour)
                                (ef-elea-dark             . dark)
                                ;; (ef-cherie                . dark)
                                ;; (ef-bio                   . dark)
                                (ef-elea-dark             . dark)
                                (ef-elea-dark             . dark)
                                ;; (doom-rouge               . dark)
                                (ef-summer                . nour)
                                (ef-melissa-dark          . dark)
                                ;; (ef-duo-dark              . dark)
                                (ef-spring                . nour)
                                ;; (kaolin-valley-dark       . dark)
                                (ef-dark                  . dark)
                                (ef-trio-dark             . dark)
                                ;; (doom-rouge               . dark)
                                (kaolin-galaxy            . dark)
                                ;; (ef-day                   . nour)
                                (ef-duo-light             . nour)
                                (ef-elea-dark             . dark)
                                (ef-deuteranopia-dark     . dark)
                                (ef-trio-light            . nour)
                                (doom-badger              . dark)
                                (kaolin-dark              . dark)
                                (ef-symbiosis             . dark)
                                (ef-autumn                . dark)
                                (ef-frost                 . nour)
                                ;; (doom-rouge               . dark)
                                (ef-light                 . nour)
                                (ef-winter                . dark)
                                (kaolin-temple            . dark)
                                (ef-elea-dark             . dark)
                                (ef-cyprus                . nour)
                                (kaolin-ocean             . dark)
                                (ef-maris-light           . nour)
                                ;; (doom-rouge               . dark)
                                (ef-trio-light            . nour)
                                (kaolin-dark              . dark)
                                (ef-elea-dark             . dark)
                                (kaolin-bubblegum         . dark)
                                (ef-night                 . dark)))

;; [2024-07-25 Thu 06:45] currently, my glasses do not help me well with light
;; themes.
;; (setq salih/prefered-themes
;;       (seq-filter (lambda (theme)
;;                     (not (eq (cdr theme) 'nour)))
;;                   salih/prefered-themes))

(defun salih/really-random-theme ()
  (let* ((themes '(doom-peacock
                   kaolin-galaxy
                   ef-maris-dark
                   ef-deuteranopia-light
                   ef-elea-dark
                   ef-cherie
                   ef-bio
                   ;; doom-rouge
                   doom-feather-dark
                   ef-summer
                   ef-melissa-dark
                   ef-duo-dark
                   ef-spring
                   kaolin-valley-dark
                   ef-dark
                   ef-trio-dark
                   ;; doom-rouge
                   kaolin-dark
                   ef-day
                   ef-duo-light
                   ef-deuteranopia-dark
                   ef-trio-light
                   doom-badger
                   ef-symbiosis
                   ef-autumn
                   ef-frost
                   ;; doom-rouge
                   ef-light
                   ef-winter
                   kaolin-temple
                   ef-cyprus
                   kaolin-ocean
                   ef-maris-light
                   ;; doom-rouge
                   ef-trio-light
                   kaolin-bubblegum
                   ef-night
                   modus-vivendi-tritanopia kaolin-dark doom-nord
                   doom-opera
                   kaolin-aurora
                   doom-gruvbox doom-material-dark
                   ef-trio-light
                   doom-material doom-molokai
                   doom-monokai-machine doom-monokai-octagon
                   doom-monokai-pro doom-moonlight
                   doom-palenight
                   doom-solarized-dark
                   doom-spacegrey doom-vibrant
                   misterioso wombat))
         (random-theme (nth (random (length themes)) themes)))
    random-theme))

(defun salih/really-really-random-theme-load ()
  (interactive)
  (load-theme (salih/really-random-theme)))

(defface salih/modeline-background
   '((t :background "#3355bb" :foreground "white" :inherit bold))
   "Face with a red background for use on the mode line.")



























(defun salih/banner ()
  (let* ((banner '("       d8888                                     8888888888       888    d8b      "
                   "      d88888                                     888              888    Y8P      "
                   "     d88P888                                     888              888             "
                   "    d88P 888 88888b.d88b.   .d88b.  888d888      8888888  8888b.  888888 888      "
                   "   d88P  888 888 \"888 \"88b d88\"\"88b 888P\"        888         \"88b 888    888      "
                   "  d88P   888 888  888  888 888  888 888          888     .d888888 888    888      "
                   " d8888888888 888  888  888 Y88..88P 888          888     888  888 Y88b.  888      "
                   "d88P     888 888  888  888  \"Y88P\"  888          888     \"Y888888  \"Y888 888      "
                   ""
                   ""
                   ""
                   ""))

         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(provide '+early)
