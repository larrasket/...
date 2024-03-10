
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
  (nth
   (random (length salih/prefered-themes))
   salih/prefered-themes))

(setq salih/prefered-themes '(ef-autumn ef-bio ef-cherie ef-elea-dark
                              ef-frost ef-maris-dark ef-maris-light
                              ef-melissa-dark ef-night ef-symbiosis
                              ef-winter doom-1337 doom-Iosvkem
                              doom-acario-light doom-badger
                              doom-challenger-deep doom-dracula
                              doom-feather-dark doom-henna
                              doom-horizon doom-molokai doom-monokai-spectrum
                              doom-nord-aurora
                              doom-old-hope doom-opera doom-palenight
                              doom-plain-dark doom-rouge
                              doom-shades-of-purple doom-snazzy
                              doom-sourcerer doom-solarized-dark
                              doom-tomorrow-night doom-zenburn))

(provide '+early)
