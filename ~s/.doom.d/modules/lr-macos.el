;;; lr-macos.el --- macOS-specific settings -*- lexical-binding: t; -*-

(when (eq system-type 'darwin)
  (menu-bar-mode -1)
  (require 'ls-lisp)
  (setq mac-function-modifier  'control
        mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier   'meta
        mac-option-modifier    'none
        frame-title-format     nil
        org-download-screenshot-method "/usr/local/bin/pngpaste %s"
        ls-lisp-dirs-first     t
        ls-lisp-use-insert-directory-program nil
        epg-pinentry-mode      'loopback)

  ;; ── Native chrome ────────────────────────────────────────────────────────
  ;;   ns-appearance dark        → dark traffic-light buttons / controls
  ;;   ns-transparent-titlebar t → title bar takes theme bg color, no gray strip
  ;;   undecorated-round-corners → preserve macOS native window rounding
  ;;   internal-border-width 12  → padding so text doesn't kiss the edge
  ;;   drag-internal-border t    → drag from anywhere in the padding
  ;;   tool-bar-lines 0          → no Emacs tool bar
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(undecorated-round-corners . t))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))

  ;; ── Liquid glass: alpha-background + macOS CGS native blur ───────────────
  ;; Provided by the `frame-transparency' community patch baked into
  ;; emacs-plus@31 (build.yml selects it).  ns-background-blur uses CGS
  ;; private APIs so it has to be in default-frame-alist (frame-creation
  ;; time) for the NSWindow's backing material to be configured before
  ;; display.
  (defvar salih/alpha-background 0.56
    "Frame background alpha (0.0-1.0). Lower = more glass.")
  (defvar salih/ns-background-blur 48
    "macOS background blur radius (px). 0 = no blur, 30+ = strong glass.")

  (setq salih/alpha-background 0.56
        salih/ns-background-blur 48)

  (defconst salih/--glass-face-palette
    '((default                         :background "#38466b" :foreground "#cbd6f4")
      (fringe                          :background "#38466b")
      (line-number                     :background "#38466b" :foreground "#8796c0" :slant normal)
      (line-number-current-line        :background "#38466b" :foreground "#dfe8ff" :slant normal)
      (hl-line                         :background "#435379")
      (mode-line                       :background "#303b5d" :foreground "#cbd6f4" :family "Pragmasevka")
      (mode-line-active                :background "#303b5d" :foreground "#cbd6f4" :family "Pragmasevka")
      (mode-line-inactive              :background "#242d49" :foreground "#8796c0" :family "Pragmasevka")
      (header-line                     :background "#38466b" :foreground "#cbd6f4")
      (vertical-border                 :background "#38466b" :foreground "#65749c")
      (window-divider                  :foreground "#65749c")
      (window-divider-first-pixel      :foreground "#65749c")
      (window-divider-last-pixel       :foreground "#65749c")
      (cursor                          :background "#a7fff4")
      (region                          :background "#53648f")
      (minibuffer-prompt               :foreground "#8faeff" :weight bold)
      (font-lock-comment-face          :foreground "#9faed6" :slant italic)
      (font-lock-comment-delimiter-face :foreground "#9faed6" :slant italic)
      (font-lock-doc-face              :foreground "#9faed6" :slant italic)
      (font-lock-doc-markup-face       :foreground "#bda9f4" :slant italic)
      (font-lock-keyword-face          :foreground "#8faeff" :weight bold)
      (font-lock-builtin-face          :foreground "#8bdfe8" :weight bold)
      (font-lock-string-face           :foreground "#a8d9b4")
      (font-lock-function-name-face    :foreground "#c4d0f2")
      (font-lock-variable-name-face    :foreground "#e7c98f" :slant italic)
      (font-lock-type-face             :foreground "#bda9f4" :slant italic)
      (font-lock-constant-face         :foreground "#f4a868")
      (font-lock-warning-face          :foreground "#f48395" :weight bold)
      (isearch                         :background "#e8c574" :foreground "#111a2b" :weight bold)
      (lazy-highlight                  :background "#5d6f9a" :foreground "#ecf1ff")
      (show-paren-match                :background "#8bdfe8" :foreground "#10192a" :weight bold)
      (doom-dashboard-banner           :foreground "#a3b1ec")
      (doom-dashboard-menu-title       :foreground "#8faeff" :weight bold)
      (doom-dashboard-menu-desc        :foreground "#cbd6f4")
      (corfu-default                   :background "#303b5d" :foreground "#cbd6f4")
      (vertico-current                 :background "#435379" :foreground "#ecf1ff"))
    "Default Ghostty-like face palette used while glass is enabled.")

  (defvar salih/glass-face-palette salih/--glass-face-palette
    "Face palette used while `salih/toggle-glass' is enabled.")

  (defconst salih/--opaque-face-palette
    '((default                         :background "#000000" :foreground "#a9b1d6")
      (fringe                          :background "#000000")
      (line-number                     :background "#000000" :foreground "#565f89" :slant normal)
      (line-number-current-line        :background "#000000" :foreground "#a9b1d6" :slant normal)
      (hl-line                         :background "#0d0d0d")
      (mode-line                       :background "#181818" :foreground "#a9b1d6" :family "Pragmasevka")
      (mode-line-active                :background "#181818" :foreground "#a9b1d6" :family "Pragmasevka")
      (mode-line-inactive              :background "#0a0a0a" :foreground "#565f89" :family "Pragmasevka")
      (header-line                     :background "#000000" :foreground "#a9b1d6")
      (vertical-border                 :background "#000000" :foreground "#1a1a1a")
      (window-divider                  :foreground "#1a1a1a")
      (window-divider-first-pixel      :foreground "#1a1a1a")
      (window-divider-last-pixel       :foreground "#1a1a1a")
      (cursor                          :background "#00ff00")
      (region                          :background "#283457")
      (minibuffer-prompt               :foreground "#7aa2f7" :weight bold)
      (font-lock-comment-face          :foreground "#565f89" :slant italic)
      (font-lock-comment-delimiter-face :foreground "#565f89" :slant italic)
      (font-lock-doc-face              :foreground "#565f89" :slant italic)
      (font-lock-doc-markup-face       :foreground "#a9b1d6" :slant italic)
      (font-lock-keyword-face          :foreground "#7aa2f7" :weight bold)
      (font-lock-builtin-face          :foreground "#7dcfff" :weight bold)
      (font-lock-string-face           :foreground "#9ece6a")
      (font-lock-function-name-face    :foreground "#7aa2f7")
      (font-lock-variable-name-face    :foreground "#c0caf5" :slant italic)
      (font-lock-type-face             :foreground "#bb9af7" :slant italic)
      (font-lock-constant-face         :foreground "#ff9e64")
      (font-lock-warning-face          :foreground "#f7768e" :weight bold)
      (isearch                         :background "#e0af68" :foreground "#000000" :weight bold)
      (lazy-highlight                  :background "#2f3b54" :foreground "#c0caf5")
      (show-paren-match                :background "#2e3c64" :foreground "#c0caf5" :weight bold)
      (doom-dashboard-banner           :foreground "#565f89")
      (doom-dashboard-menu-title       :foreground "#7aa2f7" :weight bold)
      (doom-dashboard-menu-desc        :foreground "#a9b1d6")
      (corfu-default                   :background "#1a1b26" :foreground "#c0caf5")
      (vertico-current                 :background "#1f2335" :foreground "#c0caf5"))
    "Default face palette restored when glass is disabled.")

  (defvar salih/opaque-face-palette salih/--opaque-face-palette
    "Face palette restored when glass is disabled.")

  (setq salih/glass-face-palette salih/--glass-face-palette
        salih/opaque-face-palette salih/--opaque-face-palette)

  (add-to-list 'default-frame-alist `(alpha-background . ,salih/alpha-background))
  (add-to-list 'default-frame-alist `(ns-background-blur . ,salih/ns-background-blur))
  (add-to-list 'default-frame-alist '(ns-alpha-elements ns-alpha-all))

  (defun salih/--apply-face-palette (palette)
    "Apply face attributes from PALETTE to existing faces."
    (dolist (spec palette)
      (when (facep (car spec))
        (apply #'set-face-attribute (car spec) nil (cdr spec)))))

  (defun salih/--apply-glass-palette ()
    "Apply the brighter blue-gray palette used by glass mode."
    (salih/--apply-face-palette salih/glass-face-palette))

  (defun salih/--apply-opaque-palette ()
    "Restore the solid dark palette used when glass mode is off."
    (salih/--apply-face-palette salih/opaque-face-palette))

  (defun salih/--apply-glass (&optional frame)
    "Re-apply transparency, blur, and glass palette to FRAME."
    (with-selected-frame (or frame (selected-frame))
      (set-frame-parameter nil 'alpha-background  salih/alpha-background)
      (set-frame-parameter nil 'ns-background-blur salih/ns-background-blur)
      (set-frame-parameter nil 'ns-alpha-elements '(ns-alpha-all))
      (salih/--apply-glass-palette)))

  ;; Break the gnus inheritance cycle that doom-themes-base introduces.
  ;; doom-themes-base sets `gnus-group-news-low-empty :inherit gnus-group-news-low'
  ;; while gnus's defface has the reverse direction.  Emacs 30 tolerated this;
  ;; Emacs 32 hard-errors with "Face inheritance results in inheritance cycle".
  ;; Corfu trips it on every child-frame creation: `face-spec-recalc' for the
  ;; new frame replays the cyclic theme spec from `theme-face' storage.
  ;;
  ;; `set-face-attribute' only patches the LIVE frame; the cyclic spec stays
  ;; in theme storage and re-applies on every new frame.  `face-spec-set' with
  ;; `face-override-spec' wins against `theme-face' for all current AND
  ;; future frames — that's what corfu needs.
  (defun salih/--break-gnus-face-cycle (&rest _)
    (dolist (face '(gnus-group-news-low-empty gnus-group-news-low))
      (when (facep face)
        (face-spec-set face '((t :inherit unspecified)) 'face-override-spec))))
  (add-hook 'doom-load-theme-hook #'salih/--break-gnus-face-cycle)
  (salih/--break-gnus-face-cycle)

  ;; Re-apply on each theme load (theme load can reset background-color
  ;; which alpha-background composites against) and on every new frame
  ;; (emacsclient -c, etc).
  (add-hook 'doom-load-theme-hook       #'salih/--apply-glass)
  (add-hook 'after-make-frame-functions #'salih/--apply-glass)
  (unless (daemonp)
    (add-hook 'window-setup-hook        #'salih/--apply-glass))
  (when (display-graphic-p)
    (salih/--apply-glass))

  (defun salih/toggle-glass ()
    "Toggle the glass effect on/off."
    (interactive)
    (let* ((cur (frame-parameter nil 'alpha-background))
           (off (or (null cur) (= cur 1.0)))
           (new-alpha (if off salih/alpha-background 1.0))
           (new-blur  (if off salih/ns-background-blur 0)))
      (modify-all-frames-parameters
       `((alpha-background . ,new-alpha)
         (ns-background-blur . ,new-blur)
         (ns-alpha-elements ns-alpha-all)))
      (if off
          (salih/--apply-glass-palette)
        (salih/--apply-opaque-palette))
      (message "glass: alpha=%s blur=%s" new-alpha new-blur)))

  (defun salih/set-glass (alpha blur)
    "Set ALPHA (0.0-1.0) and BLUR radius (0-50) on every frame."
    (interactive "nAlpha (0.0-1.0): \nnBlur (0-50): ")
    (setq salih/alpha-background  alpha
          salih/ns-background-blur blur)
    (modify-all-frames-parameters
     `((alpha-background . ,alpha)
       (ns-background-blur . ,blur)
       (ns-alpha-elements ns-alpha-all)))
    (salih/--apply-glass-palette)
    (message "glass: alpha=%s blur=%s" alpha blur)))


(defun salih/mail-current-file ()
  "Open macOS Mail app with a new message and attach the current buffer's file."
  (interactive)
  (unless buffer-file-name (user-error "Buffer is not visiting a file"))
  (when (buffer-modified-p) (save-buffer))
  (let ((script (format
                 "tell application \"Mail\"
  set newMessage to make new outgoing message with properties {subject:\"\", content:\"\", visible:true}
  tell newMessage
    make new attachment with properties {file name:\"%s\"} at after the last paragraph
  end tell
  activate
end tell" buffer-file-name)))
    (shell-command (concat "osascript -e " (shell-quote-argument script)))))

(provide 'lr-macos)
