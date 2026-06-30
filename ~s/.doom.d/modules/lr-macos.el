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
  ;;   ns-transparent-titlebar t → titlebar follows the frame material
  ;;   undecorated-round-corners → preserve macOS native window rounding
  ;;   internal-border-width 12  → padding so text doesn't kiss the edge
  ;;   drag-internal-border t    → drag from anywhere in the padding
  ;;   tool-bar-lines 0          → no Emacs tool bar
  (defvar salih/ns-transparent-titlebar t
    "Non-nil makes the macOS titlebar transparent.")
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist
               `(ns-transparent-titlebar . ,salih/ns-transparent-titlebar))
  (add-to-list 'default-frame-alist '(undecorated-round-corners . t))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))

  ;; ── Liquid glass: NSGlassEffectView + theme-owned tint ───────────────────
  ;; Provided by a local emacs-plus@31 patch layered after the community
  ;; `frame-transparency' patch.  Native builds expose the same material shape
  ;; Ghostty uses: regular/clear NSGlassEffectView, tint opacity 0.01, and a
  ;; saturation multiplier for the inactive tint overlay.
  ;;
  ;; Keep color ownership with the active theme.  This module only adjusts
  ;; frame material parameters and uses the theme's current default background
  ;; as the frame tint, so the effect remains portable across Doom themes.
  (defvar salih/glass-style 'macos-glass-regular
    "Current macOS glass preset.")
  (defvar salih/alpha-background 0.01
    "Frame background alpha used with native glass.")
  (defvar salih/ns-background-blur 0
    "CGS background blur radius used with native glass.")
  (defvar salih/ns-alpha-elements
    '(ns-alpha-all)
    "Frame elements that receive `alpha-background'.")
  (defvar salih/ns-alpha-glyphs-min-alpha 0.24
    "Minimum alpha for non-default glyph backgrounds on native glass builds.")
  (defvar salih/ns-glass-material 'regular
    "Native glass material: `regular', `clear', or nil.")
  (defvar salih/ns-glass-tint-opacity 0.01
    "Native glass tint opacity, matching Ghostty's background-opacity.")
  (defvar salih/ns-glass-saturation 1.2
    "Native glass inactive overlay saturation multiplier.")
  (defvar salih/ns-glass-inactive-opacity nil
    "Native glass inactive overlay opacity.
Nil lets Emacs choose light/dark defaults.")
  (defvar salih/ns-glass-corner-radius 0
    "Native glass corner radius.")
  (defvar salih/glass-fallback-alpha-background 0.70
    "Fallback alpha for Emacs builds without native glass material support.")
  (defvar salih/glass-fallback-background-blur 30
    "Fallback CGS blur for Emacs builds without native glass material support.")

  (defconst salih/glass-presets
    '((macos-glass-regular
       :material regular
       :alpha 0.01
       :glyphs-min-alpha 0.24
       :blur 0
       :tint-opacity 0.05
       :saturation 1.9
       :inactive-opacity 0.05
       :corner-radius 2
       :fallback-alpha 0.70
       :fallback-blur 30)
      (macos-glass-clear
       :material clear
       :alpha 0.01
       :glyphs-min-alpha 0.22
       :blur 0
       :tint-opacity 0.01
       :saturation 1.2
       :inactive-opacity nil
       :corner-radius 0
       :fallback-alpha 0.58
       :fallback-blur 40))
    "Ghostty-named presets available to `salih/set-glass-style'.")

  (defvar salih/--glass-build-warning-shown nil
    "Non-nil once the missing patched Emacs build warning was shown.")
  (defvar salih/--native-glass-build-p-cache :unset
    "Cached result of checking whether this Emacs supports native glass.")

  (defun salih/--plist-get (plist prop)
    "Return PROP from PLIST."
    (cadr (memq prop plist)))

  (defun salih/--glass-preset (style)
    "Return the Ghostty glass preset for STYLE."
    (or (alist-get style salih/glass-presets)
        (user-error "Unknown glass style: %s" style)))

  (defun salih/--apply-glass-style-values (style)
    "Load STYLE values into the active glass variables."
    (let ((preset (salih/--glass-preset style)))
      (setq salih/glass-style style
            salih/ns-glass-material (salih/--plist-get preset :material)
            salih/alpha-background (salih/--plist-get preset :alpha)
            salih/ns-alpha-glyphs-min-alpha
            (salih/--plist-get preset :glyphs-min-alpha)
            salih/ns-background-blur (salih/--plist-get preset :blur)
            salih/ns-alpha-elements '(ns-alpha-all)
            salih/ns-glass-tint-opacity (salih/--plist-get preset :tint-opacity)
            salih/ns-glass-saturation (salih/--plist-get preset :saturation)
            salih/ns-glass-inactive-opacity
            (salih/--plist-get preset :inactive-opacity)
            salih/ns-glass-corner-radius (salih/--plist-get preset :corner-radius)
            salih/glass-fallback-alpha-background
            (salih/--plist-get preset :fallback-alpha)
            salih/glass-fallback-background-blur
            (salih/--plist-get preset :fallback-blur))))

  (salih/--apply-glass-style-values salih/glass-style)

  (defun salih/--emacs-binary-has-string-p (needle)
    "Return non-nil if the current Emacs executable contains NEEDLE."
    (let ((binary (or (car command-line-args)
                      (expand-file-name invocation-name invocation-directory))))
      (and binary
           (file-executable-p binary)
           (executable-find "strings")
           (with-temp-buffer
             (and (zerop (call-process "strings" nil t nil binary))
                  (progn
                    (goto-char (point-min))
                    (search-forward needle nil t)))))))

  (defun salih/--native-glass-build-p ()
    "Return non-nil if this Emacs binary exposes native glass parameters."
    (if (eq salih/--native-glass-build-p-cache :unset)
        (setq salih/--native-glass-build-p-cache
              (and (salih/--emacs-binary-has-string-p "ns-glass-material")
                   (salih/--emacs-binary-has-string-p "ns-alpha-glyphs-alpha")))
      salih/--native-glass-build-p-cache))

  (defun salih/--warn-unless-glass-build ()
    "Warn if this Emacs binary is missing the frame-transparency patch."
    (unless salih/--glass-build-warning-shown
      (setq salih/--glass-build-warning-shown t)
      (unless (and (salih/--emacs-binary-has-string-p "ns-background-blur")
                   (salih/--emacs-binary-has-string-p "ns-alpha-elements")
                   (salih/--emacs-binary-has-string-p "ns-alpha-glyphs-alpha")
                   (salih/--native-glass-build-p))
        (display-warning
         'lr-macos
         "Liquid glass needs emacs-plus@31 built with frame-transparency and the local ns-glass-effect patch. The emacs-plus-app cask ignores ~/.config/emacs-plus/build.yml, so native glass will not work in that binary."
         :warning))))

  (defun salih/--effective-alpha-background ()
    "Return the alpha value for this Emacs build."
    (if (salih/--native-glass-build-p)
        salih/alpha-background
      salih/glass-fallback-alpha-background))

  (defun salih/--effective-background-blur ()
    "Return the blur value for this Emacs build."
    (if (salih/--native-glass-build-p)
        salih/ns-background-blur
      salih/glass-fallback-background-blur))

  (defun salih/--effective-alpha-glyphs-alpha ()
    "Return the alpha for theme-owned row and selection backgrounds."
    (max (salih/--effective-alpha-background)
         salih/ns-alpha-glyphs-min-alpha))

  (defun salih/--theme-background ()
    "Return the active theme's default background, if it defines one."
    (let ((background (face-background 'default nil t)))
      (unless (or (memq background '(nil unspecified))
                  (member background '("unspecified-bg" "unspecified-fg")))
        background)))

  (defun salih/--theme-background-frame-parameters ()
    "Return a frame background parameter derived from the active theme."
    (when-let* ((background (salih/--theme-background)))
      `((background-color . ,background))))

  (defun salih/--native-glass-frame-parameters ()
    "Return native glass frame parameters, when this Emacs supports them."
    (when (salih/--native-glass-build-p)
      `((ns-alpha-glyphs-alpha . ,(salih/--effective-alpha-glyphs-alpha))
        (ns-glass-material . ,salih/ns-glass-material)
        (ns-glass-tint-opacity . ,salih/ns-glass-tint-opacity)
        (ns-glass-saturation . ,salih/ns-glass-saturation)
        (ns-glass-inactive-opacity . ,salih/ns-glass-inactive-opacity)
        (ns-glass-corner-radius . ,salih/ns-glass-corner-radius))))

  (defun salih/--glass-frame-parameters ()
    "Return frame parameters for the current glass preset."
    (append
     (salih/--theme-background-frame-parameters)
     `((ns-transparent-titlebar . ,salih/ns-transparent-titlebar)
       (alpha-background . ,(salih/--effective-alpha-background))
       (ns-background-blur . ,(salih/--effective-background-blur))
       (ns-alpha-elements . ,salih/ns-alpha-elements))
     (salih/--native-glass-frame-parameters)))

  (defun salih/--opaque-frame-parameters ()
    "Return frame parameters for a solid opaque frame."
    (append
     (salih/--theme-background-frame-parameters)
     `((ns-transparent-titlebar . ,salih/ns-transparent-titlebar)
       (alpha-background . 1.0)
       (ns-background-blur . 0)
       (ns-alpha-elements . ,salih/ns-alpha-elements))
     (when (salih/--native-glass-build-p)
       '((ns-alpha-glyphs-alpha . nil)
         (ns-glass-material . nil)))))

  (dolist (parameter (salih/--glass-frame-parameters))
    (unless (eq (car parameter) 'background-color)
      (add-to-list 'default-frame-alist parameter)))

  (defun salih/--apply-glass (&optional frame)
    "Re-apply transparency and blur to FRAME."
    (when (display-graphic-p)
      (salih/--warn-unless-glass-build))
    (with-selected-frame (or frame (selected-frame))
      (dolist (parameter (salih/--glass-frame-parameters))
        (set-frame-parameter nil (car parameter) (cdr parameter)))))

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
           (off (or (null cur) (= cur 1.0))))
      (modify-all-frames-parameters
       (if off
           (salih/--glass-frame-parameters)
         (salih/--opaque-frame-parameters)))
      (message "glass: %s alpha=%s blur=%s"
               (if off salih/glass-style 'off)
               (if off (salih/--effective-alpha-background) 1.0)
               (if off (salih/--effective-background-blur) 0))))

  (defun salih/set-glass (alpha blur)
    "Set ALPHA (0.0-1.0) and BLUR radius (0-50) on every frame."
    (interactive "nAlpha (0.0-1.0): \nnBlur (0-50): ")
    (setq salih/alpha-background  alpha
          salih/ns-background-blur blur)
    (modify-all-frames-parameters (salih/--glass-frame-parameters))
    (message "glass: alpha=%s glyph-alpha=%s blur=%s material=%s"
             (salih/--effective-alpha-background)
             (salih/--effective-alpha-glyphs-alpha)
             (salih/--effective-background-blur)
             (if (salih/--native-glass-build-p) salih/ns-glass-material 'fallback)))

  (defun salih/set-glass-style (style)
    "Apply a Ghostty glass STYLE preset to every frame."
    (interactive
     (list (intern
            (completing-read
             "Glass style: "
             (mapcar (lambda (preset) (symbol-name (car preset)))
                     salih/glass-presets)
             nil t nil nil (symbol-name salih/glass-style)))))
    (salih/--apply-glass-style-values style)
    (modify-all-frames-parameters (salih/--glass-frame-parameters))
    (message "glass: %s alpha=%s glyph-alpha=%s blur=%s material=%s"
             style
             (salih/--effective-alpha-background)
             (salih/--effective-alpha-glyphs-alpha)
             (salih/--effective-background-blur)
             (if (salih/--native-glass-build-p) salih/ns-glass-material 'fallback))))


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
