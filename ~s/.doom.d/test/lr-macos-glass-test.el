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
               ((symbol-function 'facep) (lambda (_face) t))
               ((symbol-function 'message) (lambda (&rest _) nil)))
       ,@body
       (list lr-macos-test--frame-parameters
             lr-macos-test--face-attributes))))

(ert-deftest salih-glass-defaults-are-material-values-only ()
  (should (= salih/alpha-background 0.78))
  (should (= salih/ns-background-blur 1))
  (should-not (boundp 'salih/glass-face-palette))
  (should-not (boundp 'salih/opaque-face-palette))
  (should-not (fboundp 'salih/--apply-glass-palette))
  (should-not (fboundp 'salih/--apply-opaque-palette)))

(ert-deftest salih-glass-preset-refreshes-material-values-on-module-reload ()
  (let ((salih/alpha-background 0.55)
        (salih/ns-background-blur 44))
    (lr-macos-test--load-module)
    (should (= salih/alpha-background 0.78))
    (should (= salih/ns-background-blur 1))))

(ert-deftest salih-apply-glass-applies-frame-parameters-only ()
  (pcase-let ((`(,frame-parameters ,face-attributes)
               (lr-macos-test--capture-effects
                 (let ((salih/alpha-background 0.78)
                       (salih/ns-background-blur 1))
                   (salih/--apply-glass)))))
    (should (member '(alpha-background . 0.78) frame-parameters))
    (should (member '(ns-background-blur . 1) frame-parameters))
    (should (member '(ns-alpha-elements ns-alpha-all) frame-parameters))
    (should-not face-attributes)))

(ert-deftest salih-set-glass-updates-material-values-only ()
  (pcase-let ((`(,frame-parameters ,face-attributes)
               (lr-macos-test--capture-effects
                 (salih/set-glass 0.56 45))))
    (should (= salih/alpha-background 0.56))
    (should (= salih/ns-background-blur 45))
    (should (equal frame-parameters
                   '((alpha-background . 0.56)
                     (ns-background-blur . 45)
                     (ns-alpha-elements ns-alpha-all))))
    (should-not face-attributes)))

(ert-deftest salih-toggle-glass-off-keeps-theme-faces ()
  (pcase-let ((`(,frame-parameters ,face-attributes)
               (lr-macos-test--capture-effects
                 (cl-letf (((symbol-function 'frame-parameter)
                            (lambda (&rest _) salih/alpha-background)))
                   (salih/toggle-glass)))))
    (should (equal frame-parameters
                   '((alpha-background . 1.0)
                     (ns-background-blur . 0)
                     (ns-alpha-elements ns-alpha-all))))
    (should-not face-attributes)))

;;; lr-macos-glass-test.el ends here
