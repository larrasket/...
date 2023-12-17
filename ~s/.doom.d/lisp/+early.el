
(setq-default frame-title-format                        '("%b")
              shr-inhibit-images                        t
              bidi-paragraph-direction                  'left-to-right
              org-download-image-dir                    "~/roam/media"
              indent-tabs-mode                          nil
              pdf-view-display-size                     'fit-width)

(defmacro salih/path-blog (filename)
  `(f-join salih/blog-content-path ,filename))

(defmacro salih/path-roam (&rest args)
  `(f-join org-roam-directory ,@args))

(defmacro salih/path-configs (&rest args)
  `(f-join user-config-repo-path ,@args))


(defmacro salih/user-first-name ()
  `(car (split-string user-full-name " ")))

(defmacro salih/path-list (source-directory)
  `(mapcar 'file-truename (directory-files-recursively ,source-directory "" nil t)))


(provide '+early)
