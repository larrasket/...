;;; ../configs/.doom.d/present.el -*- lexical-binding: t; -*-


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

;; (setq fill-column 110)
;; (setq-default fill-column 110)

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
  (vi-tilde-fringe-mode 0)
  ;; (centered-window-mode)
  ;; (visual-fill-column-mode 1)
  ) ;; (visual-line-mode 1)




(add-hook 'org-present-mode-hook 'startpresent)



(defun endpresent ()
  (interactive)
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (org-present-end)
  (visual-fill-column-mode 0)
  (visual-line-mode 0))

(provide 'present)
