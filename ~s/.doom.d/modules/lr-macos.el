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
  (defvar salih/alpha-background 0.55
    "Frame background alpha (0.0-1.0). Lower = more glass.")
  (defvar salih/ns-background-blur 44
    "macOS background blur radius (px). 0 = no blur, 30+ = strong glass.")

  (defvar salih/glass-face-palette
    '((default                    :background "#273454")
      (fringe                     :background "#273454")
      (line-number                :background "#273454")
      (line-number-current-line   :background "#273454")
      (hl-line                    :background "#314061")
      (mode-line                  :background "#243050")
      (mode-line-active           :background "#243050")
      (mode-line-inactive         :background "#1d2742")
      (header-line                :background "#273454")
      (vertical-border            :background "#273454" :foreground "#51617f")
      (window-divider             :foreground "#51617f")
      (window-divider-first-pixel :foreground "#51617f")
      (window-divider-last-pixel  :foreground "#51617f"))
    "Face palette used while `salih/toggle-glass' is enabled.")

  (defvar salih/opaque-face-palette
    '((default                    :background "#000000")
      (fringe                     :background "#000000")
      (line-number                :background "#000000")
      (line-number-current-line   :background "#000000")
      (hl-line                    :background "#0d0d0d")
      (mode-line                  :background "#181818")
      (mode-line-active           :background "#181818")
      (mode-line-inactive         :background "#0a0a0a")
      (header-line                :background "#000000")
      (vertical-border            :background "#000000" :foreground "#1a1a1a")
      (window-divider             :foreground "#1a1a1a")
      (window-divider-first-pixel :foreground "#1a1a1a")
      (window-divider-last-pixel  :foreground "#1a1a1a"))
    "Face palette restored when glass is disabled.")

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
