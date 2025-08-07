;;; +l-ui-modeline.el -*- lexical-binding: t; -*-

;; Modeline configuration
(use-package doom-modeline
  :custom
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-state-icon nil)
  (doom-modeline-icon t)
  (doom-modeline-mode-alist nil)
  (doom-modeline-modal-icon nil)
  (doom-modeline-lsp-icon nil)
  (doom-modeline-irc-stylize #'ignore)
  (all-the-icons-completion-mode nil)
  (global-hl-line-modes nil)
  (scroll-margin 4)
  :config
  ;; Custom modeline segments
  (defun salih/doom-modeline-update-pdf-pages-no-percent ()
  "Update PDF pages."
  (setq doom-modeline--pdf-pages
        (format "  %d/%d "
                (or (eval `(pdf-view-current-page)) 0)
                (pdf-cache-number-of-pages))))

  (defun salih/doom-modeline-update-pdf-pages-only-percent ()
  "Update PDF pages."
  (setq doom-modeline--pdf-pages
        (format "[%s％󠀥] "
                (truncate
                 (* 100
                    (/ (float (or (eval `(pdf-view-current-page)) 0))
                       (pdf-cache-number-of-pages)))))))

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
  
  (doom-modeline-def-segment salih/irc
    "A lightweight notification icon for unread IRC buffers."
    (when (and doom-modeline-irc
               (doom-modeline--segment-visible 'irc))
      (let* ((buffers (doom-modeline--get-buffers))
             (number (length buffers)))
        (when (> number 0)
          (let ((notification-icon
                 (propertize (doom-modeline-icon 'mdicon
                                                 "nf-md-message_processing"
                                                 "🗊"
                                                 "#"
                                                 :face 'doom-modeline-notification)
                             'help-echo (format "IRC Notifications: %d unread buffer(s)"
                                                number)
                             'mouse-face 'doom-modeline-highlight
                             'local-map (let ((map (make-sparse-keymap)))
                                          (cond
                                           ((doom-modeline--circe-p)
                                            (define-key map [mode-line mouse-1]
                                                        #'tracking-previous-buffer)
                                            (define-key map [mode-line mouse-3]
                                                        #'tracking-next-buffer))
                                           ((doom-modeline--erc-p)
                                            (define-key map [mode-line mouse-1]
                                                        #'erc-switch-to-buffer)
                                            (define-key map [mode-line mouse-3]
                                                        #'erc-track-switch-buffer))
                                           ((doom-modeline--rcirc-p)
                                            (define-key map [mode-line mouse-1]
                                                        #'rcirc-switch-to-server-buffer)
                                            (define-key map [mode-line mouse-3]
                                                        #'rcirc-next-active-buffer)))
                                          map))))
            notification-icon))))))

;; Cocaine configuration
(use-package cocaine-line
  :custom
  (cocaine-show-buffer-position nil)
  (cocaine-show-column-info nil)
  (cocaine-show-misc-info t)
  (cocaine-show-processes-info t))

;; Mode-line format
(setq mode-line-format nil)

(provide '+l-ui-modeline)
