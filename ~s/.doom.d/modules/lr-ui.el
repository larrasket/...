;;; lr-ui.el --- UI, theme, modeline, dashboard -*- lexical-binding: t; -*-

(require 'cl-lib)

;;; --- Fringe ---
(set-fringe-style '(1 . 1))

;;; --- Faces ---
(defconst salih/ef-maris-dark-line-number-palette-overrides
  '((fg-line-number-active fg-line-number-inactive)
    (bg-line-number-active unspecified)
    (bg-line-number-inactive unspecified))
  "Ef Maris Dark palette entries that keep line numbers visually neutral.")

(defvar ef-maris-dark-palette-overrides nil)
(dolist (override salih/ef-maris-dark-line-number-palette-overrides)
  (setq ef-maris-dark-palette-overrides
        (assq-delete-all (car override) ef-maris-dark-palette-overrides)))
(setq ef-maris-dark-palette-overrides
      (append salih/ef-maris-dark-line-number-palette-overrides
              ef-maris-dark-palette-overrides))

(defun salih/--neutralize-ef-maris-dark-line-numbers (&rest _)
  "Remove Ef Maris Dark's active line-number treatment."
  (if (memq 'ef-maris-dark custom-enabled-themes)
      (progn
        (face-spec-set
         'line-number
         '((t :background unspecified
              :box nil
              :inverse-video nil
              :overline nil
              :underline nil
              :slant normal
              :weight normal
              :extend nil))
         'face-override-spec)
        (face-spec-set
         'line-number-current-line
         '((t :inherit line-number
              :foreground unspecified
              :background unspecified
              :box nil
              :inverse-video nil
              :overline nil
              :underline nil
              :slant normal
              :weight normal
              :extend nil))
         'face-override-spec))
    (dolist (face '(line-number line-number-current-line))
      (face-spec-set face nil 'face-override-spec))))

(add-hook 'doom-load-theme-hook #'salih/--neutralize-ef-maris-dark-line-numbers)
(salih/--neutralize-ef-maris-dark-line-numbers)

(custom-set-faces!
  ;; Theme-neutral emphasis. Colors stay owned by the active theme.
  '(font-lock-keyword-face     :weight bold)
  '(font-lock-builtin-face     :weight bold)
  '(font-lock-comment-face           :slant italic)
  '(font-lock-comment-delimiter-face :slant italic)
  '(font-lock-doc-face               :slant italic)
  '(font-lock-doc-markup-face        :slant italic)
  '(font-lock-type-face              :slant italic)
  '(font-lock-variable-name-face     :slant italic)
  '(org-meta-line                    :slant italic)
  '(org-document-info                :slant italic)
  '(org-document-info-keyword        :slant italic)
  '(org-special-keyword              :slant italic)
  '(org-block-begin-line             :slant italic)
  '(org-block-end-line               :slant italic)
  '(treemacs-root-face               :weight bold :height 1.2)
  '(doom-themes-treemacs-root-face   :weight ultra-bold :height 1.2)
  '(rjsx-attr                        :slant italic :weight medium)
  '(org-level-1 :weight bold)
  '(org-level-2 :weight bold)
  '(org-level-3 :weight bold)
  '(org-level-4 :weight bold)
  '(org-level-5 :weight bold)
  '(org-level-6 :weight bold)
  '(org-level-7 :weight bold)
  '(org-level-8 :weight bold)
  '(orderless-match-face-0 :weight bold)
  '(orderless-match-face-1 :weight bold)
  '(orderless-match-face-2 :weight bold)
  '(orderless-match-face-3 :weight bold)
  '(org-document-title :inherit outline-1 :height 1.5 :weight normal))

;;; --- Cursor ---
(setq evil-default-cursor 'box)

;;; --- Modeline ---
(setq doom-modeline-icon t
      doom-modeline-height 28
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

;; Doom's evil-respect-visual-line-mode only remaps arrow keys; also remap j/k.
(after! evil
  (evil-define-minor-mode-key 'motion 'visual-line-mode
    "j" #'evil-next-visual-line
    "k" #'evil-previous-visual-line
    "0" #'evil-beginning-of-visual-line
    "$" #'evil-end-of-visual-line
    "^" #'evil-first-non-blank-of-visual-line))

;;; --- Dired preview ---
(after! dired-preview
  (setq dired-preview-delay 0.1
        dired-preview-max-size (* 1024 1024 30)))

(provide 'lr-ui)
