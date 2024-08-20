;;; configs/~s/.doom.d/+line.el -*- lexical-binding: t; -*-

(require 'doom-modeline)

(setq helm-ag-show-status-function #'doom-modeline-set-helm-modeline)

(defface my-modeline-background
  '((t :background "#3355bb" :foreground "white" :inherit bold))
  "Face with a red background for use on the mode line.")


(setq breadcrumb-project-max-length 0.5)
(setq breadcrumb-project-crumb-separator "/")
(setq breadcrumb-imenu-max-length 1.0)
(setq breadcrumb-imenu-crumb-separator " > ")

;;;
;;;

(setq mode-line-format nil)
(doom-modeline-def-modeline 'salih-line
  '(eldoc
    workspace-name
    window-number
    follow remote-host  word-count
    parrot)
  '(selection-info matches
    buffer-position compilation
    objed-state misc-info persp-name
    battery grip irc
    mu4e gnus
    github debug repl lsp minor-modes
    input-method indent-info
    buffer-encoding
    process vcs check time))

(defface my-modeline-background
  '((t :background "#3355bb" :foreground "white" :inherit bold))
  "Face with a red background for use on the mode line.")

(defun my-modeline--buffer-name ()
  "Return `buffer-name' with spaces around it."
  (format " %s " (buffer-name)))

(defvar-local my-modeline-buffer-name
    '(:eval
      (when (mode-line-window-selected-p)
        (propertize (my-modeline--buffer-name) 'face 'my-modeline-background)))
  "Mode line construct to display the buffer name.")

(defun my-modeline--major-mode-name ()
  "Return capitalized `major-mode' as a string."
  (capitalize (symbol-name major-mode)))


(defvar-local my-modeline-major-mode
    '
  "Mode line construct to display the major mode.")

(put 'my-modeline-major-mode 'risky-local-variable t)

(mode-line-window-selected-p)
(defun mode-line-window-selected-p ()
  "Return non-nil if we're updating the mode line for the selected window.
This function is meant to be called in `:eval' mode line
constructs to allow altering the look of the mode line depending
on whether the mode line belongs to the currently selected window
or not."
  (let ((window (selected-window)))
    (or (eq window (old-selected-window))))
  	(and (minibuffer-window-active-p (minibuffer-window))
  	     (with-selected-window (minibuffer-window)
  	       (eq window (minibuffer-selected-window)))))
(setq doom-modeline-mode-alist nil)

;; show with percent
(defun salih/doom-modeline-update-pdf-pages ()
  "Update PDF pages."
  (setq doom-modeline--pdf-pages
        (format "  %d/%d (%s٪)"
                (or (eval `(pdf-view-current-page)) 0)
                (pdf-cache-number-of-pages)
                (truncate
                 (* 100
                    (/ (float (or (eval `(pdf-view-current-page)) 0))
                       (pdf-cache-number-of-pages)) )))))


(setq-default mode-line-format
              '("%e"
                (:eval (doom-modeline-segment--bar))
                " " (:eval (list
                            (when spacious-padding-mode
                              (all-the-icons--icon-info-for-buffer))
                            " " (propertize (my-modeline--major-mode-name) 'face 'bold)))
                "  " (:eval (list
                             (propertize (buffer-name)
                                         'face (cond
                                                ((buffer-modified-p) 'doom-modeline-buffer-modified)
                                                ((doom-modeline--active) 'doom-modeline-buffer-file)
                                                (t 'mode-line-inactive))
                                         'help-echo "Buffer name mouse-1: Previous buffer\nmouse-3: Next buffer"
                                         'local-map mode-line-buffer-identification-keymap)))
                "  " (:eval (list (if (derived-mode-p 'pdf-view-mode)
                                      (propertize salih/doom-modeline-update-pdf-pages) "")))
                "  " (:eval (doom-modeline-format--salih-line))))

(provide '+line)
