;;; +l-org-fc.el -*- lexical-binding: t; -*-

;; Org-fc configuration
(use-package org-fc
  :custom
  (org-fc-flashcard-tag "drill")
  (org-fc-directories '("~/roam/main" "~/roam/other" "~/roam/references")))

(provide '+l-org-fc) 