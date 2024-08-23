;;; configs/~s/.doom.d/+line.el -*- lexical-binding: t; -*-

(require 'doom-modeline)

(defface salih/modeline-background
  '((t :background "#3355bb" :foreground "white" :inherit bold))
  "Face with a red background for use on the mode line.")

(setq breadcrumb-project-max-length             0.5
      breadcrumb-project-crumb-separator        "/"
      breadcrumb-imenu-max-length               1.0
      breadcrumb-imenu-crumb-separator          " > "
      helm-ag-show-status-function              #'doom-modeline-set-helm-modeline
      mode-line-format                          nil)

(doom-modeline-def-segment salih/selection-info
  "Information about the current selection.
Such as how many characters and lines are selected, or the NxM dimensions of a
block selection."
  (when (and (or mark-active (and (bound-and-true-p evil-local-mode)
                                  (eq evil-state 'visual)))
             (doom-modeline--active))
    (cl-destructuring-bind (beg . end)
      (if (and (bound-and-true-p evil-local-mode) (eq evil-state 'visual))
          (cons evil-visual-beginning evil-visual-end)
        (cons (region-beginning) (region-end)))
      (propertize
       (let ((lines (count-lines beg (min end (point-max)))))
         (concat (doom-modeline-spc)
                 (cond ((or (bound-and-true-p rectangle-mark-mode)
                            (and (bound-and-true-p evil-visual-selection)
                                 (eq 'block evil-visual-selection)))
                        (let ((cols (abs (- (doom-modeline-column end)
                                            (doom-modeline-column beg)))))
                          (format "%dx%dB" lines cols)))
                       ((and (bound-and-true-p evil-visual-selection)
                             (eq evil-visual-selection 'line))
                        (format "%dL" lines))
                       ((> lines 1)
                        (format "%dC %dL" (- end beg) lines))
                       (t
                        (format "%dC" (- end beg))))
                 (doom-modeline-spc)))
       'face 'doom-modeline-emphasis))))


(doom-modeline-def-segment salih/word-count
  "The buffer word count.
Displayed when in a major mode in `doom-modeline-continuous-word-count-modes'.
Respects `doom-modeline-enable-word-count'."
  (when (and doom-modeline-enable-word-count
             (member major-mode doom-modeline-continuous-word-count-modes)
             (derived-mode-p 'org-mode))
    (propertize (format " %dW" (count-words (point-min) (point-max)))
                'face (doom-modeline-face))))

(doom-modeline-def-modeline 'salih-line
  '(eldoc
    workspace-name
    window-number
    follow remote-host
    salih/word-count
    parrot)
  '(salih/selection-info matches
    buffer-position compilation
    objed-state misc-info persp-name
    battery grip irc
    mu4e gnus
    github debug repl lsp minor-modes
    input-method indent-info
    buffer-encoding
    process vcs check time))

(defface salih/modeline-background
  '((t :background "#3355bb" :foreground "white" :inherit bold))
  "Face with a red background for use on the mode line.")

(defun my-modeline--buffer-name ()
  "Return `buffer-name' with spaces around it."
  (format " %s " (buffer-name)))

(defvar-local my-modeline-buffer-name
    '(:eval
      (when (mode-line-window-selected-p)
        (propertize (my-modeline--buffer-name) 'face 'salih/modeline-background)))
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
