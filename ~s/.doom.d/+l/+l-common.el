;;; +l/common.el -*- lexical-binding: t; -*-

;; Theme-specific customizations
(use-package modus-themes
  :config
  (if (eq (cdr (assoc doom-theme salih/prefered-themes)) 'nour)
      (custom-set-faces!
       '(org-todo :weight normal)
       '(org-tag :weight normal)
       '(org-done :weight normal)
       '(org-agenda-done :strike-through nil)
       '(org-document-title :height 2.0 :weight normal)
       '(org-level-1 :weight normal :height 1.25)
       '(org-level-2 :weight normal)
       '(org-level-3 :weight normal)
       '(org-level-4 :weight normal)
       '(org-level-5 :weight normal)
       '(org-level-6 :weight normal)
       '(org-level-7 :weight bold))))

(provide '+l-common) 