
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
