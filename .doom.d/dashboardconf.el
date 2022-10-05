;;; ../configs/.doom.d/dashboard.el -*- lexical-binding: t; -*-


(setq fancy-splash-image "/home/ghd/me/wDfR_KdH_400x400.jpg")
;; (setq dashboard-banner-logo-title "وَمَا الْحَيَاةُ الدُّنْيَا إِلَّا مَتَاعُ الْغُرُورِ")
(setq dashboard-startup-banner "~/.doom.d/pan.png")
(setq dashboard-center-content t)



(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-startup-banner "/home/ghd/me/wDfR_KdH_400x400.jpg")  ;; use custom image as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
			      (bookmarks . "book"))))
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq doom-fallback-buffer-name "*dashboard*")
(provide 'dashboardconf)
