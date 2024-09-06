(setq-default frame-title-format                        '("%b")
              shr-inhibit-images                        t
              bidi-paragraph-direction                  'left-to-right
              org-download-image-dir                    "~/roam/media"
              indent-tabs-mode                          nil
              pdf-view-display-size                     'fit-width)

(defalias 'l 'list)
(defmacro s/pb (filename)
  `(f-join salih/blog-content-path ,filename))

(defmacro s/pr (&rest args)
  `(f-join org-roam-directory ,@args))

(defmacro s/pc (&rest args)
  `(f-join user-config-repo-path ,@args))


(defmacro s/ufn ()
  `(car (split-string user-full-name " ")))

(defmacro s/pl (source-directory)
  `(mapcar 'file-truename (directory-files-recursively ,source-directory "" nil t)))

(defmacro s/cm (m)
  `(concat "/" user-mail-address ,m))

(defmacro s/require (&rest packages)
  `(progn
     ,@(mapcar (lambda (pkg) `(if ,pkg (require ,pkg))) packages)))

(defun doom-theme? ()
  (string-prefix-p "doom-" (symbol-name doom-theme)))


(defun salih/get-random-theme (inc)
  (let* ((current-day (+ inc (string-to-number (format-time-string "%d"))))
         (list-length (length salih/prefered-themes))
         (selected (nth (mod current-day list-length) salih/prefered-themes)))
    selected))


(setq salih/prefered-themes '((doom-peacock             . dark)
                              (doom-rouge               . dark)
                              (doom-henna               . dark)
                              (kaolin-galaxy            . dark)
                              (ef-maris-dark            . dark)
                              (ef-deuteranopia-light    . nour)
                              (ef-elea-dark             . dark)
                              (ef-cherie                . dark)
                              (doom-horizon             . dark)
                              (ef-bio                   . dark)
                              (doom-rouge               . dark)
                              (doom-feather-dark        . dark)
                              ;; (ef-summer                . nour) that's too happy for me
                              (ef-melissa-dark          . dark)
                              (ef-duo-dark              . dark)
                              (ef-spring                . nour)
                              (kaolin-valley-dark       . dark)
                              (ef-dark                  . dark)
                              (ef-trio-dark             . dark)
                              (doom-rouge               . dark)
                              (kaolin-dark              . dark)
                              (ef-day                   . nour)
                              (ef-duo-light             . nour)
                              (ef-deuteranopia-dark     . dark)
                              (ef-trio-light            . nour)
                              (doom-badger              . dark)
                              (ef-symbiosis             . dark)
                              (ef-autumn                . dark)
                              (ef-frost                 . nour)
                              (doom-rouge               . dark)
                              (ef-light                 . nour)
                              (ef-winter                . dark)
                              (kaolin-temple            . dark)
                              (ef-cyprus                . nour)
                              (kaolin-ocean             . dark)
                              (ef-maris-light           . nour)
                              (doom-rouge               . dark)
                              (ef-trio-light            . nour)
                              (kaolin-bubblegum         . dark)
                              (ef-night                 . dark)))

;; [2024-07-25 Thu 06:45] currently, my glasses do not help me well with light
;; themes.
;; (setq salih/prefered-themes
;;       (seq-filter (lambda (theme)
;;                     (not (eq (cdr theme) 'nour)))
;;                   salih/prefered-themes))





(provide '+early)
