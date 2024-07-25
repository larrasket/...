
(setq-default frame-title-format                        '("%b")
              shr-inhibit-images                        t
              bidi-paragraph-direction                  'left-to-right
              org-download-image-dir                    "~/roam/media"
              indent-tabs-mode                          nil
              pdf-view-display-size                     'fit-width)

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
     ,@(mapcar (lambda (pkg) `(require ,pkg)) packages)))




(defun salih/get-random-theme ()
  (let* ((current-day (string-to-number (format-time-string "%d")))
         (list-length (length salih/prefered-themes))
         (selected (nth (mod current-day list-length) salih/prefered-themes)))
    selected))
    

(setq salih/prefered-themes '((doom-peacock             . dark)
                              (doom-rouge               . dark)
                              (doom-henna               . dark)
                              (doom-monokai-classic     . dark)
                              (kaolin-galaxy            . dark)
                              (ef-maris-dark            . dark)
                              (ef-deuteranopia-light    . day)
                              (ef-elea-dark             . dark)
                              (ef-cherie                . dark)
                              (doom-horizon             . dark)
                              (ef-bio                   . dark)
                              (doom-feather-dark        . dark)
                              (ef-summer                . day)
                              (ef-melissa-dark          . dark)
                              (ef-duo-dark              . dark)
                              (ef-spring                . day)
                              (kaolin-valley-dark       . dark)
                              (ef-dark                  . dark)
                              (ef-trio-dark             . dark)
                              (doom-monokai-spectrum    . dark)
                              (kaolin-dark              . dark)
                              (ef-day                   . day)
                              (ef-duo-light             . day)
                              (ef-deuteranopia-dark     . dark)
                              (doom-monokai-spectrum    . dark)
                              (ef-trio-light            . day)
                              (doom-badger              . dark)
                              (ef-symbiosis             . dark)
                              (doom-monokai-spectrum    . dark)
                              (ef-autumn                . dark)
                              (ef-frost                 . day)
                              (ef-light                 . day)
                              (ef-winter                . dark)
                              (kaolin-temple            . dark)
                              (ef-cyprus                . nour)
                              (kaolin-ocean             . dark)
                              (ef-maris-light           . day)
                              (ef-trio-light            . day)
                              (kaolin-bubblegum         . dark)
                              (ef-night                 . dark)))



                              

(provide '+early)
