# Ghostty-like Glass Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make Doom Emacs glass mode look closer to Ghostty by pairing frame alpha/blur with a brighter blue-gray glass face palette.

**Architecture:** Keep the feature contained in `modules/lr-macos.el`. Add a small palette abstraction and helper functions that apply either the glass palette or the existing opaque black palette, then call those helpers from the existing glass apply/toggle/set paths.

**Tech Stack:** Doom Emacs Lisp, macOS NS frame parameters from the `frame-transparency` patch, `ert`/batch Emacs syntax verification.

---

## File Structure

- Modify: `modules/lr-macos.el`
  - Owns macOS-only frame parameters and glass behavior.
  - Add glass and opaque face palette variables.
  - Add helper functions to apply face specs safely only when faces exist.
  - Update existing glass commands to apply both frame parameters and palettes.

- Create: `test/lr-macos-glass-test.el`
  - Loads `modules/lr-macos.el` with a temporary `system-type` of `darwin`.
  - Stubs Doom/macOS-specific dependencies enough to evaluate helper functions.
  - Verifies palette data shape and toggle/set behavior without requiring a graphical macOS frame.

## Task 1: Add Test Coverage for Glass Palette Behavior

**Files:**
- Create: `test/lr-macos-glass-test.el`
- Modify: none

- [ ] **Step 1: Create the failing ERT test file**

```elisp
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

(defvar lr-macos-test--frame-parameters nil)
(defvar lr-macos-test--face-attributes nil)

(defun lr-macos-test--reset-state ()
  (setq lr-macos-test--frame-parameters nil
        lr-macos-test--face-attributes nil))

(defun add-to-list (list-var element &optional append compare-fn)
  (set list-var (cons element (symbol-value list-var))))

(defun menu-bar-mode (&optional _arg)
  nil)

(defun display-graphic-p (&optional _display)
  nil)

(defun daemonp ()
  nil)

(defun modify-all-frames-parameters (parameters)
  (setq lr-macos-test--frame-parameters parameters))

(defun set-frame-parameter (_frame parameter value)
  (push (cons parameter value) lr-macos-test--frame-parameters))

(defun set-face-attribute (face frame &rest attributes)
  (push (list face frame attributes) lr-macos-test--face-attributes))

(defun facep (_face)
  t)

(defun face-spec-set (&rest _)
  nil)

(defun selected-frame ()
  'lr-macos-test-frame)

(defmacro with-selected-frame (_frame &rest body)
  `(progn ,@body))

(let ((system-type 'darwin)
      (default-frame-alist nil)
      (load-path (cons (expand-file-name "../modules" (file-name-directory load-file-name))
                       load-path)))
  (load (expand-file-name "../modules/lr-macos.el" (file-name-directory load-file-name)) nil t))

(ert-deftest salih-glass-palettes-include-core-faces ()
  (dolist (face '(default fringe line-number line-number-current-line hl-line
                  mode-line mode-line-active mode-line-inactive header-line
                  vertical-border window-divider window-divider-first-pixel
                  window-divider-last-pixel))
    (should (assq face salih/glass-face-palette))
    (should (assq face salih/opaque-face-palette))))

(ert-deftest salih-apply-glass-applies-frame-parameters-and-faces ()
  (lr-macos-test--reset-state)
  (let ((salih/alpha-background 0.55)
        (salih/ns-background-blur 44))
    (salih/--apply-glass))
  (should (member '(alpha-background . 0.55) lr-macos-test--frame-parameters))
  (should (member '(ns-background-blur . 44) lr-macos-test--frame-parameters))
  (should (member '(ns-alpha-elements ns-alpha-all) lr-macos-test--frame-parameters))
  (should (assoc 'default lr-macos-test--face-attributes))
  (should (assoc 'mode-line lr-macos-test--face-attributes)))

(ert-deftest salih-set-glass-updates-values-and-applies-glass-palette ()
  (lr-macos-test--reset-state)
  (salih/set-glass 0.56 45)
  (should (= salih/alpha-background 0.56))
  (should (= salih/ns-background-blur 45))
  (should (equal lr-macos-test--frame-parameters
                 '((alpha-background . 0.56)
                   (ns-background-blur . 45)
                   (ns-alpha-elements ns-alpha-all))))
  (should (assoc 'default lr-macos-test--face-attributes)))

(ert-deftest salih-toggle-glass-off-restores-opaque-palette ()
  (lr-macos-test--reset-state)
  (cl-letf (((symbol-function 'frame-parameter)
             (lambda (&rest _) salih/alpha-background)))
    (salih/toggle-glass))
  (should (equal lr-macos-test--frame-parameters
                 '((alpha-background . 1.0)
                   (ns-background-blur . 0)
                   (ns-alpha-elements ns-alpha-all))))
  (should (member (list 'default nil '(:background "#000000"))
                  lr-macos-test--face-attributes)))

;;; lr-macos-glass-test.el ends here
```

- [ ] **Step 2: Run the tests and verify they fail before implementation**

Run:

```bash
emacs -Q --batch -L modules -l test/lr-macos-glass-test.el -f ert-run-tests-batch-and-exit
```

Expected: FAIL with missing variables such as `salih/glass-face-palette` and `salih/opaque-face-palette`.

- [ ] **Step 3: Commit the failing tests**

```bash
git add test/lr-macos-glass-test.el
git commit -m "test: cover macos glass palette"
```

## Task 2: Implement the Glass Palette Helpers

**Files:**
- Modify: `modules/lr-macos.el:35-106`
- Test: `test/lr-macos-glass-test.el`

- [ ] **Step 1: Add palette variables after the glass alpha/blur variables**

```elisp
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
```

- [ ] **Step 2: Remove the ad hoc `setq` overrides**

Delete:

```elisp
  (setq salih/alpha-background 0.624)
  (setq salih/ns-background-blur 39)

  ;; (salih/toggle-glass)
  ;; (salih/toggle-glass)
```

- [ ] **Step 3: Add helper functions before `salih/--apply-glass`**

```elisp
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
```

- [ ] **Step 4: Run the tests and verify they still fail until commands are wired**

Run:

```bash
emacs -Q --batch -L modules -l test/lr-macos-glass-test.el -f ert-run-tests-batch-and-exit
```

Expected: FAIL in tests that expect `salih/--apply-glass`, `salih/toggle-glass`, and `salih/set-glass` to apply palettes.

## Task 3: Wire Palettes into Apply, Toggle, and Set Commands

**Files:**
- Modify: `modules/lr-macos.el:51-106`
- Test: `test/lr-macos-glass-test.el`

- [ ] **Step 1: Update default frame alpha/blur entries to use the new defaults**

Keep these entries, which will now use `0.55` and `44`:

```elisp
  (add-to-list 'default-frame-alist `(alpha-background . ,salih/alpha-background))
  (add-to-list 'default-frame-alist `(ns-background-blur . ,salih/ns-background-blur))
  (add-to-list 'default-frame-alist '(ns-alpha-elements ns-alpha-all))
```

- [ ] **Step 2: Update `salih/--apply-glass`**

```elisp
  (defun salih/--apply-glass (&optional frame)
    "Re-apply transparency, blur, and glass palette to FRAME."
    (with-selected-frame (or frame (selected-frame))
      (set-frame-parameter nil 'alpha-background  salih/alpha-background)
      (set-frame-parameter nil 'ns-background-blur salih/ns-background-blur)
      (set-frame-parameter nil 'ns-alpha-elements '(ns-alpha-all))
      (salih/--apply-glass-palette)))
```

- [ ] **Step 3: Update `salih/toggle-glass`**

```elisp
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
```

- [ ] **Step 4: Update `salih/set-glass`**

```elisp
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
    (message "glass: alpha=%s blur=%s" alpha blur))
```

- [ ] **Step 5: Run the tests and verify they pass**

Run:

```bash
emacs -Q --batch -L modules -l test/lr-macos-glass-test.el -f ert-run-tests-batch-and-exit
```

Expected: PASS for all ERT tests.

- [ ] **Step 6: Byte-compile the modified file**

Run:

```bash
emacs -Q --batch -L modules -f batch-byte-compile modules/lr-macos.el
```

Expected: exit code `0`; any generated `modules/lr-macos.elc` should be removed before committing unless this repo already tracks byte-compiled files.

- [ ] **Step 7: Commit the implementation**

```bash
git add modules/lr-macos.el test/lr-macos-glass-test.el
git commit -m "feat: brighten macos glass mode"
```

## Task 4: Manual Visual Verification

**Files:**
- Modify: none
- Test: graphical Emacs on macOS

- [ ] **Step 1: Reload the module in graphical Emacs**

Run inside Emacs:

```elisp
(load! "modules/lr-macos")
```

Expected: the frame shifts to a brighter blue-gray glass base.

- [ ] **Step 2: Test the toggle command**

Run inside Emacs:

```elisp
(salih/toggle-glass)
(salih/toggle-glass)
```

Expected: first call restores an opaque black background; second call restores brighter blue-gray glass.

- [ ] **Step 3: Tune the preset if needed**

Start with:

```elisp
(salih/set-glass 0.55 44)
```

If it is still too dark, try:

```elisp
(salih/set-glass 0.50 46)
```

If it is too washed out, try:

```elisp
(salih/set-glass 0.58 42)
```

Expected: choose the closest Ghostty-like balance and update the defaults in `modules/lr-macos.el` if the better values differ from `0.55` and `44`.
