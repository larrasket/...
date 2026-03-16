;;; lr-ui.el --- UI, theme, modeline, dashboard -*- lexical-binding: t; -*-

;;; --- Fringe ---
(set-fringe-style '(1 . 1))

;;; --- Faces ---
(custom-set-faces!
  '(line-number :slant normal)
  '(line-number-current-line :slant normal)
  '(mode-line :family "Pragmasevka")
  '(mode-line-active :family "Pragmasevka")
  '(mode-line-inactive :family "Pragmasevka")
  '(org-document-title :inherit outline-1 :height 1.5 :weight normal))

;;; --- Cursor ---
(setq evil-default-cursor '("#00ff00" box))

;;; --- Modeline ---
(setq doom-modeline-icon t
      doom-modeline-height 28
      doom-modeline-hud t
      doom-modeline-bar-width 1
      doom-modeline-major-mode-icon t
      doom-modeline-major-mode-color-icon t
      doom-modeline-buffer-state-icon t
      doom-modeline-buffer-modification-icon t
      doom-modeline-lsp-icon t
      doom-modeline-time-icon t
      doom-modeline-time-live-icon t
      doom-modeline-time-analogue-clock t
      doom-modeline-modal t
      doom-modeline-modal-icon t
      doom-modeline-modal-modern-icon t
      doom-modeline-buffer-name t
      doom-modeline-highlight-modified-buffer-name t
      doom-modeline-selection-info t
      doom-modeline-vcs-icon t
      doom-modeline-vcs-max-length 15
      doom-modeline-vcs-display-function #'doom-modeline-vcs-name
      doom-modeline-lsp t
      doom-modeline-irc t
      doom-modeline-battery t
      doom-modeline-time t
      doom-modeline-persp-name t
      doom-modeline-persp-icon t
      doom-modeline-workspace-name t
      doom-modeline-project-name t
      doom-modeline-check 'auto
      doom-modeline-unicode-fallback t
      doom-modeline-env-version t)



(after! doom-modeline
  ;; PDF page display
  (defun salih/doom-modeline-update-pdf-pages-no-percent ()
    "Update PDF pages."
    (setq doom-modeline--pdf-pages
          (format "  %d/%d "
                  (or (eval `(pdf-view-current-page)) 0)
                  (pdf-cache-number-of-pages))))

  ;; Selection info segment
  (doom-modeline-def-segment salih/selection-info
    "Information about the current selection."
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
         'face 'doom-modeline-emphasis)))))

;;; --- Breadcrumb (lazy) ---
(use-package! breadcrumb
  :defer t
  :init
  (add-hook 'doom-first-buffer-hook #'breadcrumb-mode)
  :config
  (setq breadcrumb-project-max-length 0.5
        breadcrumb-project-crumb-separator "/"
        breadcrumb-imenu-max-length 1.0
        breadcrumb-imenu-crumb-separator " > "
        breadcrumb-idle-time 20)
  (add-hook! 'doom-docs-org-mode-hook (breadcrumb-local-mode -1)))

;;; --- Dashboard ---
(defun salih/banner ()
  (let* ((banner '("       d8888                                     8888888888       888    d8b      "
                   "      d88888                                     888              888    Y8P      "
                   "     d88P888                                     888              888             "
                   "    d88P 888 88888b.d88b.   .d88b.  888d888      8888888  8888b.  888888 888      "
                   "   d88P  888 888 \"888 \"88b d88\"\"88b 888P\"        888         \"88b 888    888      "
                   "  d88P   888 888  888  888 888  888 888          888     .d888888 888    888      "
                   " d8888888888 888  888  888 Y88..88P 888          888     888  888 Y88b.  888      "
                   "d88P     888 888  888  888  \"Y88P\"  888          888     \"Y888888  \"Y888 888      "
                   "" "" "" ""))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'salih/banner
      +doom-dashboard-menu-sections
      '(("Recently opened files" :action recentf-open-files)))

(add-hook! '+doom-dashboard-functions :append
  (insert "\n"
          (+doom-dashboard--center
           +doom-dashboard--width
           (concat
            "The fear of the Lord is the beginning"
            " of wisdom; all those who practice it have\na good understanding."
            " His praise endures forever."))))

;;; --- Emoji fallback ---
(add-to-list 'doom-emoji-fallback-font-families "Symbola")

;;; --- Battery ---
(add-hook 'doom-first-buffer-hook #'display-battery-mode)

;;; --- SHR face ---
(after! shr
  (set-face-attribute 'shr-text nil :family "Optima" :height 180))

;;; --- Visual line mode (text/org only, not global) ---
(add-hook 'text-mode-hook #'visual-line-mode)

;;; --- Dired preview ---
(after! dired-preview
  (setq dired-preview-delay 0.1
        dired-preview-max-size (* 1024 1024 30)))

(provide 'lr-ui)
