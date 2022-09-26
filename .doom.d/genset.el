;;; genset.el -*- lexical-binding: t; -*-
(provide 'genset)

(setq-default fill-column 92)
(setq c-default-style "linux"
      c-basic-offset 4)
(setq org-roam-directory "~/roam")
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 12))
(custom-theme-set-faces! 'doom-tomorrow-night
  '(default :background "#121212"))
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "brave")
(org-babel-do-load-languages
    'org-babel-load-languages
    '((mermaid . t)
      (scheme . t)
      (csharp . t)))
(setq treemacs-position 'right)
(setq dired-sidebar-refresh-on-special-commands 't)
(setq user-full-name "Salh Jabr"
      user-mail-address "jabr@emailasso.net")
(setq fill-column 92)



(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))


;; bi

(setq bidi-paragraph-direction 'left-to-right)
   (setq-default bidi-paragraph-direction 'left-to-right)
    (defun bidi-direction-toggle ()
      (interactive "")
      (setq bidi-display-reordering t)
      (if (equal bidi-paragraph-direction 'right-to-left)
          (setq bidi-paragraph-direction 'left-to-right)
        (setq bidi-paragraph-direction 'right-to-left))
      (message "%s" bidi-paragraph-direction))

;; (add-hook 'prog-mode-hook 'real-auto-save-mode)
;; (setq real-auto-save-interval 5) ;; in seconds

(yas-global-mode 1)
(add-hook 'yas-minor-mode(lambda()
                           (yas-activate-extra-mode 'fundamental-mode)))


;; modline
;; (setq doom-modeline-height 8)
;; (setq doom-modeline-bar-width 2)
;; (setq doom-modeline-icon (display-graphic-p))
;; (setq doom-modeline-major-mode-icon t)
;; (setq doom-modeline-major-mode-color-icon t)
;; (setq doom-modeline-buffer-state-icon nil)
;; (setq doom-modeline-buffer-modification-icon nil)
;; (setq doom-modeline-unicode-fallback nil)
;; (setq doom-modeline-minor-modes nil)
;; (setq doom-modeline-enable-word-count nil)
;; (defun enable-doom-modeline-icons (_frame)
;;   (setq doom-modeline-icon t))

;; (add-hook 'after-make-frame-functions
;;           #'enable-doom-modeline-icons)

;; themes

(custom-theme-set-faces! 'doom-homage-black
  '(default :background "#121212"))
(custom-theme-set-faces! 'doom-one
  '(default :background "#121212"))
(custom-theme-set-faces! 'doom-one-light
  '(default :background "#ffffff"))



;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(line-number ((t (:inherit default :background "#232629" :foreground "#5d6658")))))


;; (custom-theme-set-faces! 'kaolin-aurora
;;
;;
;;
;;   '(default :background "#121221"))
;;
;;
;;
;; (setq doom-theme 'kaolin-dark)
;; (setq doom-theme 'leuven)
;; (setq doom-theme 'doom-acario-light)
;; (setq doom-theme 'doom-badger)
;; (setq doom-theme 'doom-gruvbox)
;; (setq doom-theme 'default)
;; (setq doom-theme 'kaolin-galaxy)
;; (setq doom-theme 'habamax)
;; (setq doom-theme 'humanoid-dark)
(setq doom-theme 'doom-acario-light)
;; (setq doom-theme 'modus-operandi)


;; IVY

(require 'ivy-posframe)
(setq ivy-posframe-height-alist '((swiper . 20)
                                  (t      . 40)))
(setq ivy-posframe-display-functions-alist
      '((swiper          . ivy-display-function-fallback)
        (complete-symbol . ivy-posframe-display-at-point)
        (counsel-M-x     . ivy-posframe-display-at-frame-top-center)
        (t               . ivy-posframe-display)))
(ivy-posframe-mode 1)
(setq ivy-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))



(setq display-buffer-alist
      `(("*compilation*" . ((name . "*compilation*")
                            ,@default-frame-alist))))

(defun open-popup-on-side-or-below (buffer &optional alist)
  (+popup-display-buffer-stacked-side-window-fn
   buffer (append `((side . ,(if (one-window-p)
                                 'right
                               'bottom)))
                  alist)))

(set-popup-rule! "\\*compilation\\*" :side 'bottom :width 2.5)




(setq display-line-numbers-type 'relative)
(menu-bar--display-line-numbers-mode-relative)
;; (menu-bar-mode)





(setq large-file-warning-threshold nil)


(add-hook! 'artist-mode-hook
  (defun seanfarley//turnoff-line-mode ()
    (setq display-line-number-mode -1)))


(setq fancy-splash-image "/home/ghd/me/wDfR_KdH_400x400.jpg")
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







(setq all-the-icons-color-icons nil)




(setq auto-save-visited-mode t)






;; ;; tabs

;; (add-hook 'dired-mode-hook 'centaur-tabs-local-mode)
;; (add-hook 'dired-sidebar-mode 'centaur-tabs-local-mode)
;; (add-hook 'org-agenda-mode-hook 'centaur-tabs-local-mode)
;; (centaur-tabs-mode t)
;; (setq centaur-tabs-set-modified-marker t)

;;     (defun centaur-tabs-buffer-groups ()
;;       "`centaur-tabs-buffer-groups' control buffers' group rules.

;;     Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
;;     All buffer name start with * will group to \"Emacs\".
;;     Other buffer group by `centaur-tabs-get-group-name' with project name."
;;       (list
;; 	(cond
;; 	 ((or (string-equal "*" (substring (buffer-name) 0 1))
;; 	      (memq major-mode '(magit-process-mode
;; 				 magit-status-mode
;; 				 magit-diff-mode
;; 				 magit-log-mode
;; 				 magit-file-mode
;; 				 magit-blob-mode
;; 				 magit-blame-mode
;; 				 )))
;; 	  "Emacs")
;; 	 ((derived-mode-p 'prog-mode)
;; 	  "Editing")
;; 	 ((derived-mode-p 'dired-mode)
;; 	  "Dired")
;; 	 ((memq major-mode '(helpful-mode
;; 			     help-mode))
;; 	  "Help")
;; 	 ((memq major-mode '(org-mode
;; 			     org-agenda-clockreport-mode
;; 			     org-src-mode
;; 			     org-agenda-mode
;; 			     org-beamer-mode
;; 			     org-indent-mode
;; 			     org-bullets-mode
;; 			     org-cdlatex-mode
;; 			     org-agenda-log-mode
;; 			     diary-mode))
;; 	  "OrgMode")
;; 	 (t
;; 	  (centaur-tabs-get-group-name (current-buffer))))))

;; ;; (setq centaur-tabs-gray-out-icons 'buffer)


;; (setq centaur-tabs-set-icons t)
;; (setq centaur-tabs-plain-icons t)
;; (setq centaur-tabs-set-close-button nil)




;; (defun centaur-tabs-hide-tab (x)
;;   "Do no to show buffer X in tabs."
;;   (let ((name (format "%s" x)))
;;     (or
;;      ;; Current window is not dedicated window.
;;      (window-dedicated-p (selected-window))

;;      ;; Buffer name not match below blacklist.
;;      ;; (string-prefix-p "*epc" name)
;;      ;; (string-prefix-p "*helm" name)
;;      ;; (string-prefix-p "*Helm" name)
;;      ;; (string-prefix-p "*Compile-Log*" name)
;;      ;; (string-prefix-p "*lsp" name)
;;      ;; (string-prefix-p "*company" name)
;;      ;; (string-prefix-p "*Flycheck" name)
;;      ;; (string-prefix-p "*tramp" name)
;;      ;; (string-prefix-p " *Mini" name)
;;      ;; (string-prefix-p "*help" name)
;;      ;; (string-prefix-p "*straight" name)
;;      ;; (string-prefix-p " *temp" name)
;;      ;; (string-prefix-p "*Help" name)
;;      ;; (string-prefix-p "*mybuf" name)
;;      ;; (string-prefix-p "*leetcode*" name)
;;      (string-prefix-p "*doom*" name)
;;      (string-prefix-p "*scratch*" name)
;;      (string-prefix-p "*Messages*" name)
;;      (string-prefix-p "*LSP Symbols List*" name)
;;      (string-prefix-p "*omnisharp*" name)

;;      ;; Is not magit buffer.
;;      (and (string-prefix-p "magit" name)
;; 	  (not (file-name-extension name)))
;;      )))





;;  (use-package centaur-tabs
;;    :load-path "~/.emacs.d/other/centaur-tabs"
;;    :config
;;    (setq centaur-tabs-style "bar"
;; 	  centaur-tabs-height 19
;; 	  centaur-tabs-set-icons nil
;; 	  centaur-tabs-set-modified-marker t
;; 	  ;; centaur-tabs-show-navigation-buttons t
;; 	  centaur-tabs-set-bar 'under
;; 	  x-underline-at-descent-line t)
;;    ;; (centaur-tabs-headline-match)
;;    ;; (setq centaur-tabs-gray-out-icons 'buffer)
;;    ;; (centaur-tabs-enable-buffer-reordering)
;;    ;; (setq centaur-tabs-adjust-buffer-order t)
;;    (centaur-tabs-mode t)
;;    (setq uniquify-separator "/")
;;    (setq uniquify-buffer-name-style 'forward)
;;    (defun centaur-tabs-buffer-groups ()
;;      (list
;;       (cond
;; 	((or (string-equal "*" (substring (buffer-name) 0 1))
;; 	     (memq major-mode '(magit-process-mode
;; 				magit-status-mode
;; 				magit-diff-mode
;; 				magit-log-mode
;; 				magit-file-mode
;; 				magit-blob-mode
;; 				magit-blame-mode
;; 				)))
;; 	 "Emacs")
;; 	((derived-mode-p 'prog-mode)
;; 	 "Editing")
;; 	((derived-mode-p 'dired-mode)
;; 	 "Dired")
;; 	((memq major-mode '(helpful-mode
;; 			    help-mode))
;; 	 "Help")
;; 	((memq major-mode '(org-mode
;; 			    org-agenda-clockreport-mode
;; 			    org-src-mode
;; 			    org-agenda-mode
;; 			    org-beamer-mode
;; 			    org-indent-mode
;; 			    org-bullets-mode
;; 			    org-cdlatex-mode
;; 			    org-agenda-log-mode
;; 			    diary-mode))
;; 	 "OrgMode")
;; 	(t
;; 	 (centaur-tabs-get-group-name (current-buffer))))))
;;    :hook
;;    (dashboard-mode . centaur-tabs-local-mode)
;;    (term-mode . centaur-tabs-local-mode)
;;    (calendar-mode . centaur-tabs-local-mode)
;;    (org-agenda-mode . centaur-tabs-local-mode)
;;    (helpful-mode . centaur-tabs-local-mode)
;;    :bind
;;    ("C-<prior>" . centaur-tabs-backward)
;;    ("C-<next>" . centaur-tabs-forward)
;;    ("C-c t s" . centaur-tabs-counsel-switch-group)
;;    ("C-c t p" . centaur-tabs-group-by-projectile-project)
;;    ("C-c t g" . centaur-tabs-group-buffer-groups)
;;    (:map evil-normal-state-map
;; 	  ("g t" . centaur-tabs-forward)
;; 	  ("g T" . centaur-tabs-backward)))




;; (add-hook! 'doom-init-ui-hook
;;   (centaur-tabs-change-fonts "Inconsolata" 76)
;;   )

;; git integeration


;; (add-hook 'prog-mode-hook 'git-gutter-mode)

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))



;; (setq-default org-download-image-dir 'nil)
