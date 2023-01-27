;;; mine/gen/config.el -*- lexical-binding: t; -*-

(setq completion-ignore-case t)
(setq-default frame-title-format '("%b"))
(add-to-list 'doom-emoji-fallback-font-families "Symbola")
(setq fancy-splash-image "~/.doom.d/pan.png")
(setq-default fill-column 92)
(setq fill-column 92)
(setq c-default-style "linux"
      c-basic-offset 4)
(setq org-roam-directory "~/roam")
;; (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 12))
(setq doom-font (font-spec :family "PragmataPro " :size 12 :dpi 99))
(custom-theme-set-faces! 'doom-tomorrow-night
  '(default :background "#121212"))
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((mermaid . t)
   (scheme . t)
   (csharp . t)))
(setq treemacs-position 'right)
(setq dired-sidebar-refresh-on-special-commands 't)
(setq user-full-name "Salih Muhammed"
      user-mail-address "ghd@keemail.me")



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


(yas-global-mode 1)
(add-hook 'yas-minor-mode(lambda() (yas-activate-extra-mode 'fundamental-mode)))





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
(setq large-file-warning-threshold nil)
(add-hook! 'artist-mode-hook
  (defun seanfarley//turnoff-line-mode ()
    (setq display-line-number-mode -1)))
(setq all-the-icons-color-icons nil)
(setq auto-save-visited-mode t)



(setq neo-theme  'icons)
(add-hook 'neotree-mode-hook(lambda () (solaire-mode -1)))
(setq neo-window-width 35)
(add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)






(defun my/center (width)
  (interactive "nBuffer width: ")
  (let* ((adj          (- (window-text-width)
                          width))
         (total-margin (+ adj
                          left-margin-width
                          right-margin-width)))
    (setq left-margin-width  (/ total-margin 2))
    (setq right-margin-width (- total-margin left-margin-width)))
  (set-window-buffer (selected-window) (current-buffer)))



(defun startpresent ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (setq header-line-format " ")
  (setq header-line-format nil)
  (display-line-numbers-mode 0)





  (require 'org-faces)

  ;; Hide emphasis markers on formatted text
  (setq org-hide-emphasis-markers t)

  ;; Resize Org headings
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))

  ;; Make the document title a bit bigger
  (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.3)

  ;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)





  (set-face-attribute 'default nil :font "JetBrains Mono" :weight 'light :height 110)
  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'light :height 100)
  (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'light :height 1.3)


  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (my/center 110)
  (vi-tilde-fringe-mode 0))
;; (centered-window-mode)
;; (visual-fill-column-mode 1)
;; (visual-line-mode 1)

(add-hook 'org-present-mode-hook 'startpresent)



(defun endpresent ()
  (interactive)
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (org-present-end)
  (visual-fill-column-mode 0)
  (visual-line-mode 0))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(add-hook 'dired-mode-hook 'org-download-enable)
(add-hook 'dired-mode-hook(lambda () (solaire-mode -1)))







(add-hook 'csv-mode-hook 'csv-align-mode)
(require 'saveplace-pdf-view)
(save-place-mode 1)


(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(use-package nov-xwidget
  :demand t
  :after nov
  :config
  (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
  (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files))
