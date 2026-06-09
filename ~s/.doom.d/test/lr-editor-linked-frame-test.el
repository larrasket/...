;;; lr-editor-linked-frame-test.el --- tests for linked frame helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(when (boundp 'native-comp-jit-compilation)
  (setq native-comp-jit-compilation nil))

(defmacro add-hook! (&rest _)
  nil)

(defmacro after! (&rest body)
  nil)

(defmacro map! (&rest _)
  nil)

(defmacro defadvice! (&rest _)
  nil)

(defmacro use-package! (&rest _)
  nil)

(defmacro set-popup-rules! (&rest _)
  nil)

(defmacro custom-set-faces! (&rest _)
  nil)

(defmacro evil-define-key (&rest _)
  nil)

(defmacro evil-define-minor-mode-key (&rest _)
  nil)

(defvar text-mode-map (make-sparse-keymap))
(defvar org-mode-map (make-sparse-keymap))
(defvar org-noter-notes-mode-map (make-sparse-keymap))
(defvar org-noter-doc-mode-map (make-sparse-keymap))
(defvar nov-mode-map (make-sparse-keymap))
(defvar nov-button-map (make-sparse-keymap))
(defvar pdf-view-mode-map (make-sparse-keymap))
(defvar pdf-occur-buffer-mode-map (make-sparse-keymap))
(defvar lsp-mode-map (make-sparse-keymap))
(defvar scala-ts-mode-map (make-sparse-keymap))
(defvar dired-mode-map (make-sparse-keymap))
(defvar wordnut-mode-map (make-sparse-keymap))
(defvar eww-mode-map (make-sparse-keymap))
(defvar shr-map (make-sparse-keymap))
(defvar mu4e-headers-mode-map (make-sparse-keymap))
(defvar mu4e-view-mode-map (make-sparse-keymap))
(defvar mu4e-view-active-urls-keymap (make-sparse-keymap))
(defvar calendar-mode-map (make-sparse-keymap))
(defvar org-fc-review-flip-mode nil)
(defvar org-fc-review-rate-mode nil)
(defvar org-present-mode-keymap (make-sparse-keymap))
(defvar doom-leader-alt-key nil)
(defvar persp-mode nil)

(defconst lr-editor-linked-frame-test--file
  (or load-file-name buffer-file-name))

(defun lr-editor-linked-frame-test--module-path ()
  (expand-file-name "../modules/lr-editor.el"
                    (file-name-directory lr-editor-linked-frame-test--file)))

(defun lr-editor-linked-frame-test--load-module ()
  (cl-letf (((symbol-function 'add-hook) (lambda (&rest _) nil)))
    (load (lr-editor-linked-frame-test--module-path) nil t)))

(lr-editor-linked-frame-test--load-module)

(ert-deftest salih-linked-frame-ensure-group-clears-doom-workspace-parameter ()
  (let ((parameters nil)
        (source-frame 'source-frame))
    (cl-letf (((symbol-function 'selected-frame) (lambda () source-frame))
              ((symbol-function 'frame-parameter)
               (lambda (frame parameter)
                 (alist-get (cons frame parameter) parameters nil nil #'equal)))
              ((symbol-function 'set-frame-parameter)
               (lambda (frame parameter value)
                 (setf (alist-get (cons frame parameter) parameters nil nil #'equal)
                       value))))
      (let ((group (salih/--linked-frame-ensure-group source-frame)))
        (should (stringp group))
        (should (equal group
                       (alist-get (cons source-frame 'salih-linked-frame-group)
                                  parameters nil nil #'equal)))
        (should-not (alist-get (cons source-frame 'workspace)
                               parameters nil nil #'equal))))))

(ert-deftest salih-linked-frame-apply-state-selects-buffer-and-restores-position ()
  (let ((target-workspace nil)
        (added-buffer nil)
        (restored-window-state nil)
        (buffer (get-buffer-create " *salih-linked-frame-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (erase-buffer)
          (insert "first line\nsecond line\nthird line\n")
          (let ((state (list :workspace "main"
                             :buffer buffer
                             :window-state 'linked-window-layout
                             :point 15
                             :start 1
                             :hscroll 0
                             :vscroll 0)))
            (let ((persp-mode t))
              (cl-letf (((symbol-function 'frame-live-p) (lambda (_frame) t))
                        ((symbol-function 'select-frame) (lambda (&rest _) nil))
                        ((symbol-function 'set-frame-parameter) (lambda (&rest _) nil))
                        ((symbol-function 'salih/--linked-frame-workspace-name)
                         (lambda (&optional _frame) target-workspace))
                        ((symbol-function '+workspace-switch)
                         (lambda (workspace &optional _auto-create-p)
                           (setq target-workspace workspace)
                           t))
                        ((symbol-function 'persp-add-buffer)
                         (lambda (buf &rest _)
                           (setq added-buffer buf)))
                        ((symbol-function 'window-state-put)
                         (lambda (state &rest _)
                           (setq restored-window-state state)))
                        ((symbol-function 'window-state-buffers)
                         (lambda (_state) (list buffer)))
                        ((symbol-function 'get-current-persp) (lambda () 'main-persp)))
                (salih/--linked-frame-apply-state state (selected-frame))
                (should (equal target-workspace "main"))
                (should (eq added-buffer buffer))
                (should (eq restored-window-state 'linked-window-layout))
                (should (eq (window-buffer) buffer))
                (should (= (window-point) 15))
                (should (= (window-start) 1))))))
      (kill-buffer buffer))))

(ert-deftest salih-linked-frame-apply-state-skips-layout-rebuild-when-unchanged ()
  ;; Rebuilding the window tree on every command is slow and flickers the
  ;; mirrored frame, so when the sibling already matches the source geometry
  ;; we take the cheap path and only update the selected window's contents.
  (let ((put-called nil)
        (buffer (get-buffer-create " *salih-linked-frame-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (erase-buffer)
          (insert "alpha\nbeta\ngamma\n")
          (let ((state (list :workspace "main"
                             :buffer buffer
                             :window-state 'linked-window-layout
                             :window-signature 'same-layout
                             :point 3
                             :start 1
                             :hscroll 0
                             :vscroll 0))
                (persp-mode t))
            (cl-letf (((symbol-function 'frame-live-p) (lambda (_frame) t))
                      ((symbol-function 'select-frame) (lambda (&rest _) nil))
                      ((symbol-function 'set-frame-parameter) (lambda (&rest _) nil))
                      ((symbol-function 'salih/--linked-frame-workspace-name)
                       (lambda (&optional _frame) "main"))
                      ((symbol-function 'salih/--linked-frame-window-signature)
                       (lambda (&optional _frame) 'same-layout))
                      ((symbol-function 'salih/--linked-frame-add-state-buffers-to-workspace)
                       (lambda (&rest _) nil))
                      ((symbol-function 'window-state-put)
                       (lambda (&rest _) (setq put-called t))))
              (salih/--linked-frame-apply-state state (selected-frame))
              (should-not put-called)
              (should (eq (window-buffer) buffer))
              (should (= (window-point) 3)))))
      (kill-buffer buffer))))

(ert-deftest salih-linked-frame-state-captures-window-layout ()
  (cl-letf (((symbol-function 'window-state-get)
             (lambda (_window &optional _writable)
               'linked-window-layout))
            ((symbol-function 'salih/--linked-frame-workspace-name)
             (lambda (&optional _frame) "main")))
    (let ((state (salih/--linked-frame-state (selected-frame))))
      (should (eq (plist-get state :window-state) 'linked-window-layout)))))

(ert-deftest salih-linked-frame-sync-mode-tracks-workspace-and-shared-buffer ()
  ;; Same workspace -> full sync (workspace + layout + cursor).
  ;; Different workspace but the sibling shows the same buffer -> light sync
  ;; that mirrors only that buffer's scroll/cursor, never the workspace or
  ;; layout (that full cross-workspace apply was the original bug).
  ;; Different workspace and the buffer is not shown -> no sync.
  (let ((buffer (get-buffer-create " *salih-linked-frame-test*")))
    (unwind-protect
        (let ((state (list :workspace "main" :buffer buffer)))
          (cl-letf (((symbol-function 'salih/--linked-frame-workspace-name)
                     (lambda (&optional _frame) "main")))
            (should (eq (salih/--linked-frame-sync-mode state (selected-frame))
                        'full)))
          (cl-letf (((symbol-function 'salih/--linked-frame-workspace-name)
                     (lambda (&optional _frame) "other"))
                    ((symbol-function 'salih/--linked-frame-windows-showing)
                     (lambda (_buffer _frame) (list (selected-window)))))
            (should (eq (salih/--linked-frame-sync-mode state (selected-frame))
                        'buffer)))
          (cl-letf (((symbol-function 'salih/--linked-frame-workspace-name)
                     (lambda (&optional _frame) "other"))
                    ((symbol-function 'salih/--linked-frame-windows-showing)
                     (lambda (_buffer _frame) nil)))
            (should-not (salih/--linked-frame-sync-mode state (selected-frame)))))
      (kill-buffer buffer))))

(ert-deftest salih-linked-frame-apply-buffer-scroll-mirrors-scroll-only ()
  ;; The cross-workspace light sync must move the shared buffer's window to the
  ;; source point/scroll without switching workspace or rebuilding the layout.
  (let ((switched nil)
        (put-called nil)
        (buffer (get-buffer-create " *salih-linked-frame-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (erase-buffer)
          (insert "one\ntwo\nthree\nfour\n")
          (let ((state (list :buffer buffer :point 9 :start 5 :hscroll 0 :vscroll 0)))
            (cl-letf (((symbol-function 'frame-live-p) (lambda (_frame) t))
                      ((symbol-function 'salih/--linked-frame-windows-showing)
                       (lambda (_buffer _frame) (list (selected-window))))
                      ((symbol-function '+workspace-switch)
                       (lambda (&rest _) (setq switched t)))
                      ((symbol-function 'window-state-put)
                       (lambda (&rest _) (setq put-called t))))
              (set-window-buffer (selected-window) buffer)
              (salih/--linked-frame-apply-buffer-scroll state (selected-frame))
              (should (= (window-point) 9))
              (should (= (window-start) 5))
              (should-not switched)
              (should-not put-called))))
      (kill-buffer buffer))))

(ert-deftest salih-linked-frame-schedule-sync-never-propagates-errors ()
  ;; A sync failure must never break the command loop or get the hook disabled
  ;; -- an error in `post-command-hook' makes Emacs feel "disconnected".
  (cl-letf (((symbol-function 'salih/--linked-frame-p) (lambda (&rest _) t))
            ((symbol-function 'minibufferp) (lambda (&rest _) nil))
            ((symbol-function 'salih/--linked-frame-sync)
             (lambda (&rest _) (error "boom"))))
    (should (progn (salih/--linked-frame-schedule-sync) t))))

(ert-deftest salih-linked-frame-sync-does-nothing-without-siblings ()
  ;; With no other linked frame there is nothing to mirror, so the sync must
  ;; not even capture state (no per-command work or side effects).
  (let ((captured nil))
    (cl-letf (((symbol-function 'salih/--linked-frame-p) (lambda (&rest _) t))
              ((symbol-function 'frame-live-p) (lambda (&rest _) t))
              ((symbol-function 'salih/--linked-frame-siblings)
               (lambda (&rest _) (list (selected-frame))))
              ((symbol-function 'salih/--linked-frame-state)
               (lambda (&rest _) (setq captured t) nil)))
      (salih/--linked-frame-sync (selected-frame))
      (should-not captured))))

(ert-deftest salih-linked-frame-enables-cursor-in-non-selected-windows ()
  (with-temp-buffer
    (insert-file-contents (lr-editor-linked-frame-test--module-path))
    (should (search-forward "(setq-default cursor-in-non-selected-windows t)" nil t))))

(ert-deftest salih-linked-frame-keybinding-replaces-plain-make-frame ()
  (with-temp-buffer
    (insert-file-contents (lr-editor-linked-frame-test--module-path))
    (should (search-forward ":nvi \"M-n\" #'salih/make-linked-frame" nil t))
    (goto-char (point-min))
    (should-not (search-forward ":nvi \"M-n\" #'make-frame" nil t))))

;;; lr-editor-linked-frame-test.el ends here
