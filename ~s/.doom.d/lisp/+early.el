
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


(defun salih/get-random-theme ()
  (let* ((current-day (string-to-number (format-time-string "%d")))
         (list-length (length salih/prefered-themes))
         (selected (nth (mod current-day list-length) salih/prefered-themes)))
    selected))
    




(setq salih/prefered-themes '(ef-autumn
                              doom-badger
                              doom-challenger-deep
                              doom-ephemeral
                              doom-feather-dark
                              doom-henna
                              doom-horizon
                              doom-lantern
                              doom-material
                              doom-material-dark
                              doom-monokai-classic
                              doom-monokai-pro
                              doom-monokai-spectrum
                              doom-old-hope
                              doom-peacock
                              doom-rouge
                              kaolin-bubblegum
                              kaolin-dark
                              doom-monokai-spectrum
                              kaolin-eclipse
                              kaolin-galaxy
                              kaolin-ocean
                              kaolin-temple
                              kaolin-valley-dark
                              doom-monokai-spectrum
                              ef-bio
                              ef-cherie
                              ef-dark
                              doom-monokai-spectrum
                              ef-deuteranopia-dark
                              ef-duo-dark
                              ef-elea-dark
                              ef-maris-dark
                              ef-melissa-dark
                              ef-night
                              ef-symbiosis
                              ef-trio-dark
                              ef-winter))

                              

(provide '+early)
