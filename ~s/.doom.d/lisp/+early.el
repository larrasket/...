
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
    




(setq salih/prefered-themes '(doom-peacock
                              ef-symbiosis
                              doom-monokai-spectrum
                              ef-autumn
                              ef-bio
                              ef-cherie
                              ef-winter
                              kaolin-valley-dark
                              kaolin-temple
                              kaolin-galaxy
                              doom-badger
                              kaolin-ocean
                              doom-monokai-spectrum
                              kaolin-temple
                              doom-monokai-spectrum
                              doom-monokai-classic
                              kaolin-bubblegum
                              doom-lantern
                              ef-duo-dark
                              doom-rouge
                              kaolin-temple
                              doom-horizon
                              ef-maris-dark
                              doom-challenger-deep
                              ef-elea-dark
                              ef-melissa-dark
                              ef-dark
                              kaolin-dark
                              ef-trio-dark
                              doom-old-hope
                              ef-deuteranopia-dark
                              kaolin-temple
                              doom-henna
                              doom-feather-dark
                              doom-monokai-spectrum
                              kaolin-temple
                              ef-night))



                              

(provide '+early)
