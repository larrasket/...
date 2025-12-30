;;; ../configs/~s/.doom.d/+l/+l-browse.el -*- lexical-binding: t; -*-


(use-package eww
  :config
  (defun salih/ensure-eww-in-search (fn &rest args)
    (let ((browse-url-browser-function #'browse-url-default-browser))
      (apply fn args)))


  (defun salih/open-url-in-chrome-cross-platform (url &optional new-window)
    "Open URL in Chrome browser, works on macOS, Linux, and Windows."
    (cond
     ;; macOS
     ((eq system-type 'darwin)
      (start-process "chrome" nil "open" url))
     ;; Linux
     ((eq system-type 'gnu/linux)
      (start-process "chrome" nil "google-chrome" url))
     ;; Windows
     ((eq system-type 'windows-nt)
      (start-process "chrome" nil "chrome" url))
     ;; Fallback
     (t
      (browse-url url)))))

(advice-add '+lookup/documentation             :around #'salih/ensure-eww-in-search)
(advice-add 'salih/mu4e-action-view-in-browser :around #'salih/ensure-eww-in-search)

(provide '+l-browse)
