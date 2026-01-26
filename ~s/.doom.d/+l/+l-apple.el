;;; ../configs/~s/.doom.d/+l/+l-apple.el -*- lexical-binding: t; -*-


(when (eq system-type 'darwin)
  (menu-bar-mode -1)
  (require 'ls-lisp)
  (setq mac-function-modifier 'control)
  (setq mac-option-key-is-meta               nil
        mac-command-key-is-meta              t
        mac-command-modifier                 'meta
        mac-option-modifier                  'none
        frame-title-format                   nil
        org-download-screenshot-method       "/usr/local/bin/pngpaste %s"
        ls-lisp-dirs-first                   t
        ls-lisp-use-insert-directory-program nil
        epg-pinentry-mode                    'loopback)
;;; Transparent titlebar
  ;; https://github.com/d12frosted/homebrew-emacs-plus/blob/master/Formula/emacs-plus.rb#L98
  ;; https://github.com/d12frosted/homebrew-emacs-plus/issues/55
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Properties-in-Mode.html#Properties-in-Mode
  ;; (modify-all-frames-parameters '((inhibit-double-buffering . t)))
  (add-to-list 'default-frame-alist    '(ns-appearance . 'dark))
  (add-to-list 'default-frame-alist    '(ns-transparent-titlebar . t)))

(provide '+l-apple)

