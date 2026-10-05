;;; lr-track-live-clock-test.el --- tests for the live (write-closed) clock line -*- lexical-binding: t; -*-

;;; Commentary:
;; The clock line on disk must never lie.
;;
;; org writes `CLOCK: [start]' and leaves it open until something closes it.
;; If you forget, or Emacs dies, that line is a claim with no end -- which is
;; how 299 intervals in this corpus came to hold 48.9% of all clocked hours.
;;
;; The fix is NOT to close it automatically (the owner banned that: "please
;; never do that. shit should be explicit").  It is to keep it CONTINUOUSLY
;; CLOSED: the tick advances the trailing stamp while you are actually working,
;; so the line always reads `CLOCK: [start]--[last-active] => dur'.  Stop
;; working and it simply stops growing.  Nothing is ever rewritten backwards,
;; nothing is closed on your behalf, and there is no dangling line to forget.
;;
;; Verified against real org before writing any of this (see the probes in the
;; session log): rewriting a running clock's line in place keeps `org-clocking-p'
;; true, keeps `org-clock-marker' valid, is idempotent across ticks, and a later
;; explicit `org-clock-out' correctly EXTENDS the line to the real end time
;; rather than keeping a stale stamp.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-clock)

(when (boundp 'native-comp-jit-compilation)
  (setq native-comp-jit-compilation nil))

;; `emacs -Q' has no Doom.  Stub what lr-track touches at load time.
(defmacro add-hook! (&rest _) nil)
(defmacro after! (&rest body) `(progn ,@body))
(defmacro defadvice! (&rest _) nil)
(defvar lr-track-test--tmp
  (file-name-as-directory (make-temp-file "lr-track-test" t)))
(defvar doom-data-dir (expand-file-name "etc/" lr-track-test--tmp))
(defvar doom-cache-dir (expand-file-name "cache/" lr-track-test--tmp))
(make-directory doom-data-dir t)
(make-directory doom-cache-dir t)

(add-to-list 'load-path (expand-file-name "modules" (locate-dominating-file
                                                     (or load-file-name buffer-file-name)
                                                     "modules")))
(require 'lr-track)

(setq org-clock-persist nil
      make-backup-files nil
      org-clock-out-remove-zero-time-clocks nil)


;;;; fixture

(defmacro lr-track-test--clocked (start-float &rest body)
  "Clock into a fresh one-heading org file at START-FLOAT and run BODY.
Binds `file' and `buf'.  Always cancels the clock and kills the buffer."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "lr-track-clk" t)))
          (file (expand-file-name "t.org" dir))
          buf)
     (with-temp-file file (insert "* TODO Write report\n"))
     (setq buf (find-file-noselect file))
     (unwind-protect
         (with-current-buffer buf
           (goto-char (point-min))
           (org-clock-in nil (seconds-to-time ,start-float))
           ,@body)
       (when (org-clocking-p) (ignore-errors (org-clock-cancel)))
       (when (buffer-live-p buf)
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf))
       (ignore-errors (delete-directory dir t)))))

(defun lr-track-test--clock-lines (buffer)
  "Every CLOCK line in BUFFER, in order."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let (out)
        (while (re-search-forward "^[ \t]*CLOCK:.*$" nil t)
          (push (string-trim (match-string 0)) out))
        (nreverse out)))))


;;;; the core invariant

(ert-deftest lr-track-live-clock-line-is-closed-while-running ()
  "After a tick, the running clock's line carries an end stamp and a duration.
This is the whole point: there is never a dangling `CLOCK: [start]' to forget."
  (let ((t0 (- (float-time) 3600)))
    (lr-track-test--clocked t0
      (should (org-clocking-p))
      ;; before: open
      (should (string-match-p "\\`CLOCK: \\[[^]]*\\]\\'"
                              (car (lr-track-test--clock-lines buf))))
      (lr-track--advance-clock-line (+ t0 1800))
      (let ((line (car (lr-track-test--clock-lines buf))))
        (should (string-match-p "--\\[" line))
        (should (string-match-p "=>" line))
        (should (string-match-p "0:30" line)))
      ;; and org still considers the clock live
      (should (org-clocking-p)))))

(ert-deftest lr-track-live-clock-line-is-idempotent-across-ticks ()
  "Many ticks produce ONE line, not one per tick.  A duplicate here would
silently multiply the owner's logged hours."
  (let ((t0 (- (float-time) 3600)))
    (lr-track-test--clocked t0
      (dolist (m '(5 10 15 20 25 30))
        (lr-track--advance-clock-line (+ t0 (* 60 m))))
      (should (= 1 (length (lr-track-test--clock-lines buf))))
      (should (string-match-p "0:30" (car (lr-track-test--clock-lines buf)))))))

(ert-deftest lr-track-live-clock-line-never-goes-backwards ()
  "The stamp only ever moves forward.  A tick that arrives with an earlier
time (NTP step, a stale closure) must not shorten an interval already logged."
  (let ((t0 (- (float-time) 3600)))
    (lr-track-test--clocked t0
      (lr-track--advance-clock-line (+ t0 1800))     ; 0:30
      (lr-track--advance-clock-line (+ t0 600))      ; backwards: ignore
      (should (string-match-p "0:30" (car (lr-track-test--clock-lines buf))))
      (should (= 1 (length (lr-track-test--clock-lines buf)))))))

(ert-deftest lr-track-live-clock-line-refuses-an-end-before-the-start ()
  "Never write an inverted interval."
  (let ((t0 (- (float-time) 3600)))
    (lr-track-test--clocked t0
      (lr-track--advance-clock-line (- t0 600))
      ;; unchanged: still the original open line
      (should (string-match-p "\\`CLOCK: \\[[^]]*\\]\\'"
                              (car (lr-track-test--clock-lines buf)))))))


;;;; the explicit clock-out must still be exact

(ert-deftest lr-track-live-clock-explicit-clock-out-extends-to-the-real-end ()
  "An explicit `org-clock-out' AFTER a tick must record the real end time, not
the stale tick stamp.  If this regressed, every manual clock-out would silently
discard the minutes since the last tick."
  (let ((t0 (- (float-time) 3600)))
    (lr-track-test--clocked t0
      (lr-track--advance-clock-line (+ t0 1800))          ; tick says 0:30
      (lr-track--with-pristine-org-globals
        (org-clock-out nil t (seconds-to-time (+ t0 2700))))  ; real end 0:45
      (should-not (org-clocking-p))
      (let ((line (car (lr-track-test--clock-lines buf))))
        (should (string-match-p "0:45" line))
        (should-not (string-match-p "0:30" line)))
      (should (= 1 (length (lr-track-test--clock-lines buf)))))))


;;;; autosave

(ert-deftest lr-track-live-clock-does-not-save-when-autosave-is-off ()
  "With `lr-track-autosave-clock' nil the tick must NEVER save the buffer -- the
original I2 behaviour, kept as the opt-out.  The stub COUNTS calls rather than
signalling: a signalling stub would abort the save it is meant to detect, so the
test would pass BECAUSE the alarm fired."
  (let ((t0 (- (float-time) 3600))
        (saves nil)
        (lr-track-autosave-clock nil))
    (lr-track-test--clocked t0
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest _) (push 'save saves) nil))
                ((symbol-function 'basic-save-buffer)
                 (lambda (&rest _) (push 'basic saves) nil)))
        (lr-track--advance-clock-line (+ t0 1800)))
      (should-not saves)
      ;; left dirty, for the owner's own save to flush
      (should (buffer-modified-p buf)))))

(ert-deftest lr-track-live-clock-autosaves-the-advance-to-disk ()
  "With autosave on, a real advance flushes to disk: the buffer is no longer
modified and the file on disk carries the advanced line."
  (let ((t0 (- (float-time) 3600))
        (lr-track-autosave-clock t))
    (lr-track-test--clocked t0
      (should (lr-track--advance-clock-line (+ t0 1800)))
      (should-not (buffer-modified-p buf))
      ;; the on-disk bytes carry the closed line, not just the buffer
      (let ((on-disk (with-temp-buffer (insert-file-contents file) (buffer-string))))
        (should (string-match-p "CLOCK: \\[[^]]*\\]--\\[[^]]*\\] =>  0:30" on-disk))))))

(ert-deftest lr-track-live-clock-autosave-neutralises-the-three-rewriters ()
  "The autosave must NOT run this config's before-save rewriters -- it is a
clean flush, not a reformat, and must never trigger an org-roam reindex on a
timer.  Defines the three as recorders on `before-save-hook' and asserts none
fire during the autosave."
  (let ((t0 (- (float-time) 3600))
        (lr-track-autosave-clock t)
        (ran nil))
    (cl-letf (((symbol-function 'toc-org-insert-toc)
               (lambda (&rest _) (push 'toc ran)))
              ((symbol-function 'vulpea-project-update-tag)
               (lambda (&rest _) (push 'vulpea ran)))
              ((symbol-function 'org-roam-link-replace-all)
               (lambda (&rest _) (push 'roam ran))))
      (lr-track-test--clocked t0
        (with-current-buffer buf
          (add-hook 'before-save-hook #'toc-org-insert-toc nil t)
          (add-hook 'before-save-hook #'vulpea-project-update-tag nil t)
          (add-hook 'before-save-hook #'org-roam-link-replace-all nil t))
        (should (lr-track--advance-clock-line (+ t0 1800)))
        (should-not (buffer-modified-p buf))   ; it did save
        (should-not ran)))))                    ; but the rewriters did not run

(ert-deftest lr-track-live-clock-autosave-touches-only-the-clocked-buffer ()
  "It must save exactly the clocked buffer, never `save-some-buffers'.  A second
dirty org buffer must be left untouched."
  (let* ((t0 (- (float-time) 3600))
         (lr-track-autosave-clock t)
         (other-dir (file-name-as-directory (make-temp-file "lr-other" t)))
         (other-file (expand-file-name "other.org" other-dir))
         other)
    (with-temp-file other-file (insert "* other\n"))
    (setq other (find-file-noselect other-file))
    (with-current-buffer other (goto-char (point-max)) (insert "dirty\n"))
    (unwind-protect
        (lr-track-test--clocked t0
          (should (buffer-modified-p other))
          (lr-track--advance-clock-line (+ t0 1800))
          (should (buffer-modified-p other)))   ; the other buffer stays dirty
      (when (buffer-live-p other)
        (with-current-buffer other (set-buffer-modified-p nil))
        (kill-buffer other))
      (ignore-errors (delete-directory other-dir t)))))

(ert-deftest lr-track-live-clock-autosave-does-nothing-on-a-same-minute-noop ()
  "A tick that does not actually change the line must not save either."
  (let ((t0 (- (float-time) 3600))
        (lr-track-autosave-clock t)
        (saves nil))
    (lr-track-test--clocked t0
      (lr-track--advance-clock-line (+ t0 1800))          ; real advance -> saves
      (set-buffer-modified-p nil)
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest _) (push 'save saves) nil)))
        ;; same minute: no change, must not return t, must not save
        (should-not (lr-track--advance-clock-line (+ t0 1830)))
        (should-not saves)))))

(ert-deftest lr-track-live-clock-is-a-no-op-with-no-clock ()
  "No clock, no writes, no errors."
  (let* ((dir (file-name-as-directory (make-temp-file "lr-track-noclk" t)))
         (file (expand-file-name "t.org" dir))
         buf)
    (with-temp-file file (insert "* TODO Nothing\n"))
    (setq buf (find-file-noselect file))
    (unwind-protect
        (progn
          (should-not (org-clocking-p))
          (should-not (lr-track--advance-clock-line (float-time)))
          (should-not (lr-track-test--clock-lines buf)))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest lr-track-live-clock-respects-the-toggle ()
  "`lr-track-live-clock-line' nil restores plain org behaviour: the line stays
open and nothing is rewritten."
  (let ((t0 (- (float-time) 3600))
        (lr-track-live-clock-line nil))
    (lr-track-test--clocked t0
      (lr-track--tick-live-clock)
      (should (string-match-p "\\`CLOCK: \\[[^]]*\\]\\'"
                              (car (lr-track-test--clock-lines buf)))))))


;;;; the tick phase: only advance while actually working

(ert-deftest lr-track-live-clock-advances-only-while-working ()
  "The stamp advances for engaged/reading and STOPS for away/elsewhere/slept.
That is what makes walking away self-limiting without any clock surgery: the
line simply stops growing at the last moment the owner was really there."
  (let ((t0 (- (float-time) 3600)))
    (lr-track-test--clocked t0
      (let ((lr-track--stable-state 'engaged))
        (lr-track--tick-live-clock)
        (should (string-match-p "--\\[" (car (lr-track-test--clock-lines buf)))))
      ;; capture what engaged wrote, then go away: it must not move
      (let ((frozen (car (lr-track-test--clock-lines buf))))
        (dolist (s '(away slept elsewhere))
          (let ((lr-track--stable-state s))
            (lr-track--tick-live-clock)
            (should (equal frozen (car (lr-track-test--clock-lines buf))))))))))

(provide 'lr-track-live-clock-test)
;;; lr-track-live-clock-test.el ends here
