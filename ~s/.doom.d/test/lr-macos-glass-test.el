;;; lr-macos-glass-test.el --- tests for lr-macos glass helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

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

(defun lr-macos-test--face-attributes (face palette)
  (cdr (assq face palette)))

(ert-deftest salih-glass-palettes-include-core-faces ()
  (dolist (face '(default fringe line-number line-number-current-line hl-line
                  mode-line mode-line-active mode-line-inactive header-line
                  vertical-border window-divider window-divider-first-pixel
                  window-divider-last-pixel cursor region minibuffer-prompt
                  font-lock-comment-face font-lock-doc-face font-lock-keyword-face
                  font-lock-string-face font-lock-function-name-face
                  font-lock-variable-name-face font-lock-type-face isearch
                  lazy-highlight show-paren-match doom-dashboard-banner
                  doom-dashboard-menu-title doom-dashboard-menu-desc
                  corfu-default vertico-current))
    (should (assq face salih/glass-face-palette))
    (should (assq face salih/opaque-face-palette))))

(ert-deftest salih-glass-defaults-are-ghostty-material-values ()
  (should (= salih/alpha-background 0.62))
  (should (= salih/ns-background-blur 48))
  (should (equal (lr-macos-test--face-attributes 'default salih/glass-face-palette)
                 '(:background "#303b5d" :foreground "#c4ceec")))
  (should (equal (lr-macos-test--face-attributes 'mode-line salih/glass-face-palette)
                 '(:background "#283150" :foreground "#c4ceec" :family "Pragmasevka")))
  (should (equal (lr-macos-test--face-attributes 'font-lock-comment-face salih/glass-face-palette)
                 '(:foreground "#93a2ca" :slant italic))))

(ert-deftest salih-glass-preset-refreshes-on-module-reload ()
  (let ((salih/alpha-background 0.55)
        (salih/ns-background-blur 44)
        (salih/glass-face-palette '((default :background "#273454"))))
    (lr-macos-test--load-module)
    (should (= salih/alpha-background 0.62))
    (should (= salih/ns-background-blur 48))
    (should (equal (lr-macos-test--face-attributes 'default salih/glass-face-palette)
                   '(:background "#303b5d" :foreground "#c4ceec")))))

(ert-deftest salih-apply-glass-applies-frame-parameters-and-faces ()
  (pcase-let ((`(,frame-parameters ,face-attributes)
               (lr-macos-test--capture-effects
                 (let ((salih/alpha-background 0.62)
                       (salih/ns-background-blur 48))
                   (salih/--apply-glass)))))
    (should (member '(alpha-background . 0.62) frame-parameters))
    (should (member '(ns-background-blur . 48) frame-parameters))
    (should (member '(ns-alpha-elements ns-alpha-all) frame-parameters))
    (should (assoc 'default face-attributes))
    (should (assoc 'mode-line face-attributes))
    (should (assoc 'font-lock-keyword-face face-attributes))))

(ert-deftest salih-set-glass-updates-values-and-applies-glass-palette ()
  (pcase-let ((`(,frame-parameters ,face-attributes)
               (lr-macos-test--capture-effects
                 (salih/set-glass 0.56 45))))
    (should (= salih/alpha-background 0.56))
    (should (= salih/ns-background-blur 45))
    (should (equal frame-parameters
                   '((alpha-background . 0.56)
                     (ns-background-blur . 45)
                     (ns-alpha-elements ns-alpha-all))))
    (should (assoc 'default face-attributes))))

(ert-deftest salih-toggle-glass-off-restores-opaque-palette ()
  (pcase-let ((`(,frame-parameters ,face-attributes)
               (lr-macos-test--capture-effects
                 (cl-letf (((symbol-function 'frame-parameter)
                            (lambda (&rest _) salih/alpha-background)))
                   (salih/toggle-glass)))))
    (should (equal frame-parameters
                   '((alpha-background . 1.0)
                     (ns-background-blur . 0)
                     (ns-alpha-elements ns-alpha-all))))
    (should (member (list 'default nil '(:background "#000000" :foreground "#a9b1d6"))
                    face-attributes))))

;;; lr-macos-glass-test.el ends here
