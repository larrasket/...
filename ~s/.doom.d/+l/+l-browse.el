;;; ../configs/~s/.doom.d/+l/+l-browse.el -*- lexical-binding: t; -*-


(use-package eww
  :config
  (defun salih/ensure-eww-in-search (orig-fun &rest args)
    "Ensure EWW is used for search results."
    (let ((browse-url-browser-function 'eww-browse-url))
      (apply orig-fun args)))


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
