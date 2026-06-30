;;; lr-macos-glass-test.el --- tests for lr-macos glass helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(when (boundp 'native-comp-jit-compilation)
  (setq native-comp-jit-compilation nil))

(defmacro add-hook! (&rest _)
  nil)

(defmacro after! (&rest body)
  `(progn ,@body))

(defmacro defadvice! (&rest _)
  nil)

(defmacro custom-set-faces! (&rest _)
  nil)

(defvar doom-load-theme-hook nil)

(defconst lr-macos-test--file
  (or load-file-name buffer-file-name))

(defun lr-macos-test--module-path ()
  (expand-file-name "../modules/lr-macos.el" (file-name-directory lr-macos-test--file)))

(defun lr-macos-test--glass-patch-path ()
  (expand-file-name "../../.config/emacs-plus/ns-glass-effect.patch"
                    (file-name-directory lr-macos-test--file)))

(defun lr-macos-test--build-config-path ()
  (expand-file-name "../../.config/emacs-plus/build.yml"
                    (file-name-directory lr-macos-test--file)))

(defun lr-macos-test--load-module ()
  (cl-letf (((symbol-function 'menu-bar-mode) (lambda (&optional _arg) nil))
            ((symbol-function 'display-graphic-p) (lambda (&optional _display) nil))
            ((symbol-function 'daemonp) (lambda () nil))
            ((symbol-function 'facep) (lambda (_face) t))
            ((symbol-function 'face-spec-set) (lambda (&rest _) nil)))
    (let ((system-type 'darwin)
          (default-frame-alist nil))
      (load (lr-macos-test--module-path) nil t))))

(lr-macos-test--load-module)

(defmacro lr-macos-test--capture-effects (&rest body)
  "Run BODY while recording frame parameters and face attributes."
  (declare (indent 0))
  `(let (lr-macos-test--frame-parameters
         lr-macos-test--face-attributes)
     (cl-letf (((symbol-function 'modify-all-frames-parameters)
                (lambda (parameters)
                  (setq lr-macos-test--frame-parameters parameters)))
               ((symbol-function 'set-frame-parameter)
                (lambda (_frame parameter value)
                  (push (cons parameter value) lr-macos-test--frame-parameters)))
               ((symbol-function 'set-face-attribute)
                (lambda (face frame &rest attributes)
                  (push (list face frame attributes) lr-macos-test--face-attributes)))
               ((symbol-function 'salih/--native-glass-build-p) (lambda () t))
               ((symbol-function 'facep) (lambda (_face) t))
               ((symbol-function 'message) (lambda (&rest _) nil)))
       ,@body
       (list lr-macos-test--frame-parameters
             lr-macos-test--face-attributes))))

(defconst lr-macos-test--alpha-elements '(ns-alpha-all))

(defun lr-macos-test--reset-regular-glass ()
  (salih/--apply-glass-style-values 'macos-glass-regular))

(ert-deftest salih-glass-defaults-are-theme-neutral-material-values ()
  (lr-macos-test--reset-regular-glass)
  (should salih/ns-transparent-titlebar)
  (should (eq salih/glass-style 'macos-glass-regular))
  (should (= salih/alpha-background 0.01))
  (should (= salih/ns-alpha-glyphs-min-alpha 0.24))
  (should (= salih/ns-background-blur 0))
  (should (equal salih/ns-alpha-elements lr-macos-test--alpha-elements))
  (should (eq salih/ns-glass-material 'regular))
  (should (= salih/ns-glass-tint-opacity 0.05))
  (should (= salih/ns-glass-saturation 1.9))
  (should (= salih/ns-glass-inactive-opacity 0.05))
  (should (= salih/ns-glass-corner-radius 2))
  (should (= salih/glass-fallback-alpha-background 0.70))
  (should (= salih/glass-fallback-background-blur 30))
  (should-not (boundp 'salih/glass-background))
  (should-not (boundp 'salih/glass-foreground))
  (should-not (boundp 'salih/glass-face-palette))
  (should-not (boundp 'salih/opaque-face-palette))
  (should-not (fboundp 'salih/--apply-glass-palette))
  (should-not (fboundp 'salih/--apply-opaque-palette)))

(ert-deftest salih-glass-code-does-not-hard-code-color-values ()
  (with-temp-buffer
    (insert-file-contents (lr-macos-test--module-path))
    (should-not (re-search-forward "\"#[0-9A-Fa-f]\\{6\\}\"" nil t))))

(ert-deftest salih-glass-build-config-hash-matches-native-patch ()
  (let ((patch-hash
         (with-temp-buffer
           (insert-file-contents-literally (lr-macos-test--glass-patch-path))
           (secure-hash 'sha256 (current-buffer)))))
    (with-temp-buffer
      (insert-file-contents (lr-macos-test--build-config-path))
      (should (search-forward (format "sha256: %s" patch-hash) nil t)))))

(ert-deftest salih-glass-native-patch-keeps-alpha-elements-handler-slot ()
  (with-temp-buffer
    (insert-file-contents (lr-macos-test--glass-patch-path))
    (should
     (search-forward
      "   ns_set_background_blur,\n   NULL,\n+  ns_set_alpha_glyphs_alpha,\n+  ns_set_glass_material,"
      nil t))))

(ert-deftest salih-glass-native-patch-routes-glyph-background-alpha ()
  (with-temp-buffer
    (insert-file-contents (lr-macos-test--glass-patch-path))
    (should
     (search-forward
      "+            alpha = ns_alpha_glyphs_background_alpha (f, face);"
      nil t))
    (should
     (search-forward
      "+ns_alpha_glyphs_background_alpha (struct frame *f, struct face *face)"
      nil t))
    (should
     (search-forward
      "+  if (face && face->background == FRAME_BACKGROUND_PIXEL (f))"
      nil t))
    (should (search-forward "+    return f->alpha_background;" nil t))
    (goto-char (point-min))
    (let ((count 0))
      (while (search-forward "ns_alpha_glyphs_background_alpha (s->f" nil t)
        (setq count (1+ count)))
      (should (>= count 6)))))

(ert-deftest salih-theme-background-ignores-unspecified-backgrounds ()
  (cl-letf (((symbol-function 'face-background)
             (lambda (&rest _) "unspecified-bg")))
    (should-not (salih/--theme-background))
    (should-not (salih/--theme-background-frame-parameters))))

(ert-deftest salih-glass-preset-refreshes-style-values-on-module-reload ()
  (let ((salih/glass-style 'macos-glass-clear)
        (salih/alpha-background 0.55)
        (salih/ns-alpha-glyphs-min-alpha 0.55)
        (salih/ns-background-blur 44)
        (salih/ns-alpha-elements '(ns-alpha-all)))
    (lr-macos-test--load-module)
    (should (eq salih/glass-style 'macos-glass-clear))
    (should (= salih/alpha-background 0.01))
    (should (= salih/ns-alpha-glyphs-min-alpha 0.22))
    (should (= salih/ns-background-blur 0))
    (should (eq salih/ns-glass-material 'clear))
    (should (= salih/glass-fallback-alpha-background 0.58))
    (should (= salih/glass-fallback-background-blur 40))
    (should (equal salih/ns-alpha-elements lr-macos-test--alpha-elements))))

(ert-deftest salih-apply-glass-applies-frame-parameters-only ()
  (pcase-let ((`(,frame-parameters ,face-attributes)
               (lr-macos-test--capture-effects
                 (lr-macos-test--reset-regular-glass)
                 (salih/--apply-glass))))
    (should (member '(ns-transparent-titlebar . t) frame-parameters))
    (should (member '(alpha-background . 0.01) frame-parameters))
    (should (member '(ns-background-blur . 0) frame-parameters))
    (should (member `(ns-alpha-elements . ,lr-macos-test--alpha-elements)
                    frame-parameters))
    (should (member '(ns-alpha-glyphs-alpha . 0.24) frame-parameters))
    (should (member '(ns-glass-material . regular) frame-parameters))
    (should (member '(ns-glass-tint-opacity . 0.05) frame-parameters))
    (should (member '(ns-glass-saturation . 1.9) frame-parameters))
    (should (member '(ns-glass-inactive-opacity . 0.05) frame-parameters))
    (should (member '(ns-glass-corner-radius . 2) frame-parameters))
    (should-not face-attributes)))

(ert-deftest salih-set-glass-updates-material-values-only ()
  (pcase-let ((`(,frame-parameters ,face-attributes)
               (lr-macos-test--capture-effects
                 (lr-macos-test--reset-regular-glass)
                 (salih/set-glass 0.56 45))))
    (should (= salih/alpha-background 0.56))
    (should (= salih/ns-background-blur 45))
    (should (equal frame-parameters
                   `((ns-transparent-titlebar . t)
                     (alpha-background . 0.56)
                     (ns-background-blur . 45)
                     (ns-alpha-elements . ,lr-macos-test--alpha-elements)
                     (ns-alpha-glyphs-alpha . 0.56)
                     (ns-glass-material . regular)
                     (ns-glass-tint-opacity . 0.05)
                     (ns-glass-saturation . 1.9)
                     (ns-glass-inactive-opacity . 0.05)
                     (ns-glass-corner-radius . 2))))
    (should-not face-attributes)))

(ert-deftest salih-set-glass-style-applies-clear-preset ()
  (pcase-let ((`(,frame-parameters ,face-attributes)
               (lr-macos-test--capture-effects
                 (salih/set-glass-style 'macos-glass-clear))))
    (should (eq salih/glass-style 'macos-glass-clear))
    (should (= salih/alpha-background 0.01))
    (should (= salih/ns-background-blur 0))
    (should (eq salih/ns-glass-material 'clear))
    (should (equal frame-parameters
                   `((ns-transparent-titlebar . t)
                     (alpha-background . 0.01)
                     (ns-background-blur . 0)
                     (ns-alpha-elements . ,lr-macos-test--alpha-elements)
                     (ns-alpha-glyphs-alpha . 0.22)
                     (ns-glass-material . clear)
                     (ns-glass-tint-opacity . 0.01)
                     (ns-glass-saturation . 1.2)
                     (ns-glass-inactive-opacity . nil)
                     (ns-glass-corner-radius . 0))))
    (should-not face-attributes)))

(ert-deftest salih-toggle-glass-off-keeps-theme-faces ()
  (pcase-let ((`(,frame-parameters ,face-attributes)
               (lr-macos-test--capture-effects
                 (cl-letf (((symbol-function 'frame-parameter)
                            (lambda (&rest _) salih/alpha-background)))
                   (salih/toggle-glass)))))
    (should (equal frame-parameters
                   `((ns-transparent-titlebar . t)
                     (alpha-background . 1.0)
                     (ns-background-blur . 0)
                     (ns-alpha-elements . ,lr-macos-test--alpha-elements)
                     (ns-alpha-glyphs-alpha . nil)
                     (ns-glass-material . nil))))
    (should-not face-attributes)))

(ert-deftest salih-toggle-glass-on-restores-regular-frame-only ()
  (pcase-let ((`(,frame-parameters ,face-attributes)
               (lr-macos-test--capture-effects
                 (lr-macos-test--reset-regular-glass)
                 (cl-letf (((symbol-function 'frame-parameter)
                            (lambda (&rest _) 1.0)))
                   (salih/toggle-glass)))))
    (should (equal frame-parameters
                   `((ns-transparent-titlebar . t)
                     (alpha-background . 0.01)
                     (ns-background-blur . 0)
                     (ns-alpha-elements . ,lr-macos-test--alpha-elements)
                     (ns-alpha-glyphs-alpha . 0.24)
                     (ns-glass-material . regular)
                     (ns-glass-tint-opacity . 0.05)
                     (ns-glass-saturation . 1.9)
                     (ns-glass-inactive-opacity . 0.05)
                     (ns-glass-corner-radius . 2))))
    (should-not face-attributes)))

(ert-deftest salih-zero-frame-alpha-keeps-glyph-backgrounds-visible ()
  (pcase-let ((`(,frame-parameters ,face-attributes)
               (lr-macos-test--capture-effects
                 (lr-macos-test--reset-regular-glass)
                 (salih/set-glass 0.0 1))))
    (should (= salih/alpha-background 0.0))
    (should (equal frame-parameters
                   `((ns-transparent-titlebar . t)
                     (alpha-background . 0.0)
                     (ns-background-blur . 1)
                     (ns-alpha-elements . ,lr-macos-test--alpha-elements)
                     (ns-alpha-glyphs-alpha . 0.24)
                     (ns-glass-material . regular)
                     (ns-glass-tint-opacity . 0.05)
                     (ns-glass-saturation . 1.9)
                     (ns-glass-inactive-opacity . 0.05)
                     (ns-glass-corner-radius . 2))))
    (should-not face-attributes)))

;;; lr-macos-glass-test.el ends here
