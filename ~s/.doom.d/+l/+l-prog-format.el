;;; +l-prog-format.el -*- lexical-binding: t; -*-

;; Format-all configuration
(use-package format-all
  :custom
  (format-all-show-errors 'never))

;; Format utilities
(defun salih/format-all-ensure-formatter ()
  "Ensure format-all is available and configured."
  (when (and (featurep 'format-all)
             (not (member major-mode '(emacs-lisp-mode
                                      sql-mode
                                      TeX-mode
                                      clojure-mode
                                      LaTeX-mode))))
    (format-all-mode 1)))

;; Comment utilities
(defun salih/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active
region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (forward-line)))

;; Programming mode hooks
(add-hook! 'prog-mode-hook
  (setq prettify-symbols-alist '(("lambda" . 923)))
  (jinx-mode -1))

;; Python mode hooks
(add-hook! 'python-mode-hook
  (flycheck-mode -1))

;; Format-all and formatter hooks
(add-hook! '(bibtex-mode-hook
             prog-mode-hook)
           #'format-all-mode
           #'salih/format-all-ensure-formatter)

;; Disable format-all for specific modes
(add-hook! '(emacs-lisp-mode-hook
             sql-mode-hook
             TeX-mode-hook
             clojure-mode-hook
             LaTeX-mode-hook)
           (format-all-mode -1))

;; Smartparens hooks
(add-hook! '(prog-mode-hook)
  (smartparens-mode 1))

;; Auto-fill mode hooks for programming
(add-hook! 'prog-mode-hook #'auto-fill-mode)

(provide '+l-prog-format) 