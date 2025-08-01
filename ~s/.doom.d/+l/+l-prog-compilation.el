;;; +l-prog-compilation.el -*- lexical-binding: t; -*-

;; Compilation utilities
(defun salih/compile-and-run-cpp ()
  "Compile and run cpp files. One of the first functions I've ever written, when
I was learning competitive programming :)"
  (interactive)
  (save-buffer)
  (compile (concat "g++ "  (file-name-nondirectory (buffer-file-name)) " -o "
                   (file-name-sans-extension   (file-name-nondirectory
                                                (buffer-file-name))) " && ./"
                   (file-name-sans-extension  (file-name-nondirectory
                                               (buffer-file-name))) " && rm "
                   (file-name-sans-extension  (file-name-nondirectory
                                               (buffer-file-name)))) t)
  (other-window t)
  (end-of-add-hook 'c++-mode))

(defun salih/compile-and-run-c ()
  "Compile and run cpp files. One of the first functions I've ever written, when
I was learning competitive programming :)"
  (interactive)
  (save-buffer)
  (compile (concat "gcc "  (file-name-nondirectory (buffer-file-name)) " -o "
                   (file-name-sans-extension   (file-name-nondirectory
                                                (buffer-file-name))) " && ./"
                   (file-name-sans-extension  (file-name-nondirectory
                                               (buffer-file-name))) " && rm "
                   (file-name-sans-extension  (file-name-nondirectory
                                               (buffer-file-name)))) t)
  (other-window t)
  (end-of-add-hook 'c-mode))

(defun salih/make-c ()
  (interactive)
  (save-buffer)
  (compile "make")
  (other-window t)
  (end-of-add-hook 'c-mode))

(provide '+l-prog-compilation) 