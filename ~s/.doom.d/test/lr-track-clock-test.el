;;; lr-track-clock-test.el --- tests for the honest machine clock -*- lexical-binding: t; -*-

;;; Commentary:
;; Stage 1 of lr-track v3: the running clock earns time from PRESENCE, never
;; from a guess about what he was doing.
;;
;; The probe reads three facts every tick (HID idle, the console lock, the
;; last wake) whether or not a clock runs.  The pure presence step turns them
;; into here / away.  The pure clock step then decides, per place, how far the
;; live CLOCK line may advance and where it pauses:
;;   machine (and either, in Stage 1): the end follows the last input; an away
;;     pauses it at the last input, and nothing but his key resumes it;
;;   away: the end follows now while he is away; 15 min of activity pauses it
;;     at the activity's start.
;; His own clock-out of a paused clock ends at the pause, with a note saying
;; so.  Saves are throttled to one every 5 min, forced at a pause and at kill.
;;
;; Presence states and pause states are built by hand here, so every test
;; pins the clock rules alone.  Anything that writes uses a scratch org file in
;; a temp directory.  Times are minute-aligned or injected, never the wall
;; clock's seconds.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Match the live Emacs: the straight builds of org and evil come first.
(let ((b "/Users/l/.emacs.d/.local/straight/build-31.0.91/"))
  (dolist (p '("org" "evil"))
    (when (file-directory-p (concat b p)) (push (concat b p) load-path))))

(require 'org)
(require 'org-clock)

(when (boundp 'native-comp-jit-compilation)
  (setq native-comp-jit-compilation nil))

;; `emacs -Q' has no Doom.  Stub what lr-track touches at load time.
(defmacro add-hook! (&rest _) nil)
(defmacro after! (&rest body) `(progn ,@body))
(defmacro defadvice! (&rest _) nil)
(defvar lr-track-clock-test--tmp
  (file-name-as-directory (make-temp-file "lr-track-clock-test" t)))
(defvar doom-data-dir (expand-file-name "etc/" lr-track-clock-test--tmp))
(defvar doom-cache-dir (expand-file-name "cache/" lr-track-clock-test--tmp))
(make-directory doom-data-dir t)
(make-directory doom-cache-dir t)

(defconst lr-track-clock-test--root
  (locate-dominating-file (or load-file-name buffer-file-name) "modules")
  "The repo root, the directory holding modules/ and test/.")

(add-to-list 'load-path (expand-file-name "modules" lr-track-clock-test--root))
(require 'lr-track)
;; lr-track requires it itself once Stage 1 lands; guarded so this file still
;; loads (and each test fails on its own) before it exists.
(require 'lr-track-presence nil t)

;; Stage 1 state, declared so the `let's below bind the module's variables
;; dynamically even before the module defines them.
(defvar lr-track--clock-pause)
(defvar lr-track--presence)
(defvar lr-track--last-sample)
(defvar lr-track--internal)
(defvar lr-track--probe-command)

(setq org-clock-persist nil
      make-backup-files nil
      org-clock-out-remove-zero-time-clocks nil)


;;;; helpers

(defconst lr-track-clock-test--s
  1791033600.0
  "A fixed minute-aligned instant for the pure tests (2026-10-03).")

(defun lr-track-clock-test--minute (offset)
  "Start of the current wall-clock minute plus OFFSET seconds, as a float.
Call it outside any `float-time' stub."
  (+ (* 60.0 (floor (float-time) 60)) offset))

(defun lr-track-clock-test--p (mode &rest kv)
  "A hand-built presence state in MODE with every contract field present.
KV is a plist overriding the defaults."
  (let ((p (list :mode mode :last-input nil :prev-time nil :wake nil
                 :first-time nil :away-from nil :away-kind nil :saw-lock nil
                 :return nil :last-away nil :breaks nil :segments nil)))
    (while kv (setq p (plist-put p (pop kv) (pop kv))))
    p))

(defun lr-track-clock-test--clock (start &rest kv)
  "A hand-built CLOCK plist for `lr-track--clock-step' starting at START.
Defaults: open line, machine place, 10 h max, not paused.  KV overrides."
  (let ((c (list :start start :end nil :place 'machine :max 36000.0
                 :paused-at nil :why nil)))
    (while kv (setq c (plist-put c (pop kv) (pop kv))))
    c))

(defun lr-track-clock-test--step (clock p now)
  "Run `lr-track--clock-step' on CLOCK, P and NOW, asserting it is pure.
Return the result normalised to (:advance-to F :paused-at F :why SYM) with
floats, so `equal' compares it and a failure prints the whole verdict."
  (let* ((c0 (copy-tree clock))
         (p0 (copy-tree p))
         (r (lr-track--clock-step clock p now))
         (adv (plist-get r :advance-to))
         (pa (plist-get r :paused-at)))
    (should (equal clock c0))
    (should (equal p p0))
    (list :advance-to (and adv (float adv))
          :paused-at (and pa (float pa))
          :why (plist-get r :why))))

(defun lr-track-clock-test--stamp (x)
  "The inactive org stamp for float time X, as org writes it in CLOCK lines."
  (format-time-string (org-time-stamp-format t t) (seconds-to-time x)))

(defun lr-track-clock-test--clock-lines (buffer)
  "Every CLOCK line in BUFFER, trimmed, in order."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let (out)
        (while (re-search-forward "^[ \t]*CLOCK:.*$" nil t)
          (push (string-trim (match-string 0)) out))
        (nreverse out)))))

(defun lr-track-clock-test--disk (file)
  "The bytes of FILE on disk, as a string."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun lr-track-clock-test--ends-at (x)
  "Regexp matching a closed CLOCK line whose end stamp is float time X."
  (regexp-quote (concat "--" (lr-track-clock-test--stamp x))))

(defun lr-track-clock-test--pause (paused-at why)
  "Record by hand that the running clock paused at PAUSED-AT because of WHY."
  (setq lr-track--clock-pause
        (list :start (float-time org-clock-start-time)
              :paused-at paused-at :why why)))

(defun lr-track-clock-test--marker-at (re)
  "A marker at the start of the first line matching RE in this buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward re)
    (copy-marker (line-beginning-position))))

(defmacro lr-track-clock-test--clocked (spec &rest body)
  "Clock into a fresh scratch org file and run BODY there.
SPEC is (START TEXT &optional HEADING-RE).  TEXT is written to t.org in a new
temp directory, point goes to the first line matching HEADING-RE (default the
first heading) and the clock starts at START.  BODY runs in that buffer with
`file' and `buf' bound and the pause state isolated.  The clock is always
cancelled, the buffer killed and the directory deleted."
  (declare (indent 1) (debug t))
  (let ((start (nth 0 spec))
        (text (nth 1 spec))
        (re (or (nth 2 spec) "^\\*+ ")))
    `(let* ((dir (file-name-as-directory (make-temp-file "lr-track-clk" t)))
            (file (expand-file-name "t.org" dir))
            (lr-track--clock-pause nil)
            (lr-track--internal nil)
            buf)
       (with-temp-file file (insert ,text))
       (setq buf (find-file-noselect file))
       (unwind-protect
           (with-current-buffer buf
             (goto-char (point-min))
             (re-search-forward ,re)
             (beginning-of-line)
             (org-clock-in nil (seconds-to-time ,start))
             ,@body)
         (when (org-clocking-p)
           (let ((lr-track--internal t)) (ignore-errors (org-clock-cancel))))
         (when (buffer-live-p buf)
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf))
         (ignore-errors (delete-directory dir t))))))

(defmacro lr-track-clock-test--with-now (init &rest body)
  "Run BODY with `now' bound to INIT and `(float-time)' answering `now'.
`(float-time TIME)' still converts TIME.  BODY may `setq' `now' to move time."
  (declare (indent 1) (debug t))
  (let ((orig (make-symbol "orig")))
    `(let ((now ,init)
           (,orig (symbol-function 'float-time)))
       (cl-letf (((symbol-function 'float-time)
                  (lambda (&optional tm) (if tm (funcall ,orig tm) now))))
         ,@body))))

(defmacro lr-track-clock-test--capturing (var &rest body)
  "Run BODY collecting every `message' string into VAR (newest first).
Nothing reaches the echo area or stderr while it runs."
  (declare (indent 1) (debug t))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (let ((s (and fmt (apply #'format-message fmt args))))
                    (push s ,var)
                    s))))
       ,@body)))

(defmacro lr-track-clock-test--probe-world (emacs-idle stub-spawn &rest body)
  "Run BODY as one clean tick world for the probe.
Emacs idle reads EMACS-IDLE seconds, `(float-time)' answers `now' (bound, 1000
s past the real now), and the module's sensing state starts empty.  Every
`make-process' call is pushed onto `spawned'; when STUB-SPAWN is non-nil it
then signals instead of spawning, so no real ioreg ever runs."
  (declare (indent 2) (debug t))
  (let ((orig (make-symbol "orig")))
    `(let ((spawned nil)
           (,orig (symbol-function 'make-process))
           (lr-track--sys-idle-value 'unknown)
           (lr-track--sys-idle-stamp nil)
           (lr-track--sys-idle-process nil)
           (lr-track--sys-idle-watchdog nil)
           (lr-track--sense-failures 0)
           (lr-track--last-sample nil)
           (lr-track--presence (lr-track-clock-test--p 'unknown))
           (lr-track--tick-attention '(:state unknown))
           (lr-track--last-state nil)
           (lr-track--stable-state 'unknown)
           (lr-track--stable-since nil)
           (lr-track--last-engaged nil)
           (lr-track--last-tick nil)
           (lr-track--phase-failures nil)
           (lr-track--state nil)
           (lr-track--focused-p t)
           (lr-track-checkin-on-return nil))
       (lr-track-clock-test--with-now (+ (float-time) 1000.0)
         (cl-letf (((symbol-function 'lr-track-emacs-idle-seconds)
                    (lambda () ,emacs-idle))
                   ((symbol-function 'current-idle-time)
                    (lambda () (seconds-to-time ,emacs-idle)))
                   ((symbol-function 'make-process)
                    (lambda (&rest args)
                      (push args spawned)
                      (if ,stub-spawn
                          (error "lr-track test: spawn stubbed")
                        (apply ,orig args)))))
           (unwind-protect (progn ,@body)
             (lr-track-sense-cleanup)))))))


;;;; probe and cadence (contract 2, 4.9)

(defconst lr-track-clock-test--real-probe-output
  (concat "      \"HIDIdleTime\" = 993495833\n"
          "      \"IOConsoleLocked\" = No\n"
          "{ sec = 1791033656, usec = 177813 } Sat Oct  3 16:20:56 2026\n")
  "What the probe printed on this Mac on 2026-10-04 (contract section 2).")

(ert-deftest lr-track-probe-command-is-the-contract-pipeline ()
  "One shell, three facts: HID idle, the console lock, the last wake.  Nothing
that names an app, a window or a process."
  (should (equal lr-track--probe-command
                 (list "/bin/sh" "-c"
                       (concat "ioreg -r -c IOHIDSystem -d 1 -w 0"
                               " | grep -m1 HIDIdleTime;"
                               " ioreg -n Root -d 1 | grep -m1 IOConsoleLocked;"
                               " sysctl -n kern.waketime")))))

(ert-deftest lr-track-probe-runs-without-a-clock ()
  "Presence is sensed all day, not only while clocked: with no clock and Emacs
idle 2 min, a tick spawns the probe, and only the contract's probe."
  (skip-unless (eq system-type 'darwin))
  (should-not (lr-track--clocking-p))
  (lr-track-clock-test--probe-world 120.0 t
    (lr-track--tick lr-track--generation)
    (should spawned)
    (dolist (args spawned)
      (should (equal (plist-get args :command) lr-track--probe-command)))))

(ert-deftest lr-track-probe-synthesized-when-emacs-saw-input-under-30s ()
  "Fresh Emacs input already proves presence: no subprocess, and the sample is
synthesized as (:time NOW :idle EMACS-IDLE :locked nil :wake LAST-WAKE)."
  (lr-track-clock-test--probe-world 5.0 t
    (setq lr-track--last-sample
          (list :time (- now 30.0) :idle 40.0 :locked nil :wake 1791000000.0)
          lr-track--presence
          (lr-track-clock-test--p 'here :last-input (- now 70.0)
                                  :prev-time (- now 30.0) :wake 1791000000.0))
    (lr-track--tick lr-track--generation)
    (should-not spawned)
    (should (= (plist-get lr-track--last-sample :time) now))
    (should (= (plist-get lr-track--last-sample :idle) 5.0))
    (should (null (plist-get lr-track--last-sample :locked)))
    (should (= (plist-get lr-track--last-sample :wake) 1791000000.0))))

(ert-deftest lr-track-probe-30s-boundary ()
  "Under 30 s of Emacs idle the probe is skipped; past it the probe runs."
  (skip-unless (eq system-type 'darwin))
  (lr-track-clock-test--probe-world 29.0 t
    (lr-track--tick lr-track--generation)
    (should-not spawned))
  (lr-track-clock-test--probe-world 31.0 t
    (lr-track--tick lr-track--generation)
    (should spawned)))

(ert-deftest lr-track-probe-sentinel-keeps-idle-cache-and-stores-sample ()
  "The existing sentinel stack parses the probe's output: the old idle cache
still holds the idle seconds (the Stage 1 classifier reads it), and the full
sample lands in `lr-track--last-sample' with exactly four fields, stamped
with the time of the parse."
  (skip-unless (and (eq system-type 'darwin) (file-executable-p "/bin/cat")))
  (let* ((dir (file-name-as-directory (make-temp-file "lr-track-probe" t)))
         (out (expand-file-name "probe.txt" dir)))
    (with-temp-file out (insert lr-track-clock-test--real-probe-output))
    (unwind-protect
        (lr-track-clock-test--probe-world 120.0 nil
          ;; The real pipeline, replaced by a printer of its real output.
          (let ((lr-track--probe-command (list "/bin/cat" out))
                (tries 0))
            (lr-track--tick lr-track--generation)
            (should spawned)
            (while (and (< tries 100)
                        (not (plist-get lr-track--last-sample :wake)))
              (accept-process-output nil 0.05)
              (cl-incf tries))
            (let ((s lr-track--last-sample))
              (should (equal (sort (cl-loop for k in s by #'cddr collect
                                            (symbol-name k))
                                   #'string<)
                             '(":idle" ":locked" ":time" ":wake")))
              (should (= (plist-get s :time) now))
              (should (< (abs (- (plist-get s :idle) 0.993495833)) 1e-6))
              (should (null (plist-get s :locked)))
              (should (= (plist-get s :wake) 1791033656.0)))
            (should (numberp lr-track--sys-idle-value))
            (should (< (abs (- lr-track--sys-idle-value 0.993495833)) 1e-6))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest lr-track-next-interval-follows-presence ()
  "Cadence: a tick every 30 s while he is here, every 60 s while away."
  (let ((lr-track--presence
         (lr-track-clock-test--p 'here :last-input lr-track-clock-test--s)))
    (should (= (lr-track--next-interval) 30)))
  (let ((lr-track--presence
         (lr-track-clock-test--p 'away :last-input lr-track-clock-test--s
                                 :away-from lr-track-clock-test--s
                                 :away-kind 'locked)))
    (should (= (lr-track--next-interval) 60))))


;;;; places (contract 4.1)

(defconst lr-track-clock-test--places-text
  (concat
   "* avey\n:PROPERTIES:\n:TRACK_KEY:   1\n:TRACK_PLACE: machine\n"
   ":TRACK_MAX:   10:00\n:END:\n"
   "** TODO Write report\n"
   "* practice\n:PROPERTIES:\n:TRACK_KEY:   6\n:TRACK_PLACE: away\n"
   ":TRACK_MAX:   4:00\n:END:\n"
   "** TODO Scales\n"
   "*** Drill\n"
   "** Override\n:PROPERTIES:\n:TRACK_PLACE: machine\n:TRACK_MAX:   2:00\n"
   ":END:\n"
   "* reading\n:PROPERTIES:\n:TRACK_KEY:   5\n:TRACK_PLACE: either\n"
   ":TRACK_MAX:   8:30\n:END:\n"
   "* sleep\n:PROPERTIES:\n:TRACK_KEY:   9\n:TRACK_PLACE: away\n"
   ":TRACK_MAX:   16:00\n:END:\n"
   "* NoMax\n:PROPERTIES:\n:TRACK_PLACE: away\n:END:\n"
   "* Unmapped\n** TODO Thing\n")
  "Streams with places and maxima, a nested task under each, and an unmapped
heading.")

(defmacro lr-track-clock-test--in-places (&rest body)
  "Run BODY in an org buffer holding `lr-track-clock-test--places-text'."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (org-mode)
     (insert lr-track-clock-test--places-text)
     ,@body))

(defun lr-track-clock-test--place-of (re)
  "The place of the heading on the first line matching RE, checked for shape."
  (let ((pl (lr-track--clock-place (lr-track-clock-test--marker-at re))))
    (should (consp pl))
    (should (symbolp (car pl)))
    (should (numberp (cdr pl)))
    pl))

(ert-deftest lr-track-clock-place-default-machine ()
  "No TRACK_PLACE anywhere above a heading means machine with the 10 h max.
A bad marker never signals: it answers the same default."
  (lr-track-clock-test--in-places
    (let ((pl (lr-track-clock-test--place-of "^\\*\\* TODO Thing")))
      (should (eq (car pl) 'machine))
      (should (= (cdr pl) 36000.0)))
    (let ((pl (lr-track-clock-test--place-of "^\\* Unmapped")))
      (should (eq (car pl) 'machine))
      (should (= (cdr pl) 36000.0))))
  (dolist (bad (list nil (make-marker)))
    (let ((pl (lr-track--clock-place bad)))
      (should (eq (car pl) 'machine))
      (should (= (cdr pl) 36000.0))))
  (with-temp-buffer
    (insert "not org at all\n")
    (let ((pl (lr-track--clock-place (copy-marker (point-min)))))
      (should (eq (car pl) 'machine))
      (should (= (cdr pl) 36000.0)))))

(ert-deftest lr-track-clock-place-inherited-from-stream ()
  "A task takes the place of the nearest heading above it carrying one: tasks
and grandchildren under `practice' are away; a child with its own TRACK_PLACE
keeps its own."
  (lr-track-clock-test--in-places
    (dolist (re '("^\\* practice" "^\\*\\* TODO Scales" "^\\*\\*\\* Drill"))
      (let ((pl (lr-track-clock-test--place-of re)))
        (should (eq (car pl) 'away))
        (should (= (cdr pl) 14400.0))))
    (let ((pl (lr-track-clock-test--place-of "^\\*\\* TODO Write report")))
      (should (eq (car pl) 'machine))
      (should (= (cdr pl) 36000.0)))
    (let ((pl (lr-track-clock-test--place-of "^\\*\\* Override")))
      (should (eq (car pl) 'machine))
      (should (= (cdr pl) 7200.0)))
    (should (eq (car (lr-track-clock-test--place-of "^\\* reading")) 'either))))

(ert-deftest lr-track-clock-place-max-parsed ()
  "TRACK_MAX is H:MM, parsed to seconds; a place without one gets 10 h."
  (lr-track-clock-test--in-places
    (should (= (cdr (lr-track-clock-test--place-of "^\\* avey")) 36000.0))
    (should (= (cdr (lr-track-clock-test--place-of "^\\* practice")) 14400.0))
    (should (= (cdr (lr-track-clock-test--place-of "^\\* reading")) 30600.0))
    (should (= (cdr (lr-track-clock-test--place-of "^\\* sleep")) 57600.0))
    (let ((pl (lr-track-clock-test--place-of "^\\* NoMax")))
      (should (eq (car pl) 'away))
      (should (= (cdr pl) 36000.0)))))


;;;; the pure clock step (contract 4.2)

(ert-deftest lr-track-machine-advances-to-last-input ()
  "While he is here the end is his last input, and only ever forward."
  (let* ((s lr-track-clock-test--s)
         (here (lambda (l) (lr-track-clock-test--p 'here :last-input l
                                                   :prev-time (+ l 20.0)))))
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s) (funcall here (+ s 300))
                    (+ s 320))
                   (list :advance-to (+ s 300) :paused-at nil :why nil)))
    ;; same last input as the end: hold
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :end (+ s 300))
                    (funcall here (+ s 300)) (+ s 400))
                   (list :advance-to nil :paused-at nil :why nil)))
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :end (+ s 300))
                    (funcall here (+ s 600)) (+ s 620))
                   (list :advance-to (+ s 600) :paused-at nil :why nil)))
    ;; an input from before the clock started earns nothing
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s) (funcall here (- s 100))
                    (+ s 20))
                   (list :advance-to nil :paused-at nil :why nil)))))

(ert-deftest lr-track-machine-bridges-break-under-15m ()
  "A 10 min gap keeps him here: the line holds (no pause), and on return his
new input carries the end straight over the break."
  (let* ((s lr-track-clock-test--s)
         (clock (lr-track-clock-test--clock s :end (+ s 1200))))
    ;; 10 min into the break, still here: hold, never pause
    (should (equal (lr-track-clock-test--step
                    clock
                    (lr-track-clock-test--p 'here :last-input (+ s 1200)
                                            :prev-time (+ s 1770))
                    (+ s 1800))
                   (list :advance-to nil :paused-at nil :why nil)))
    ;; back: the break is now inside the block, and the end jumps past it
    (should (equal (lr-track-clock-test--step
                    clock
                    (lr-track-clock-test--p
                     'here :last-input (+ s 1860) :prev-time (+ s 1830)
                     :breaks (list (list :from (+ s 1200) :to (+ s 1830))))
                    (+ s 1870))
                   (list :advance-to (+ s 1860) :paused-at nil :why nil)))))

(ert-deftest lr-track-machine-pauses-at-last-input-after-15m ()
  "No input for 15 min is an away: the clock pauses at the last input, the
moment he actually left, not when the away was noticed."
  (let ((s lr-track-clock-test--s))
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :end (+ s 1140))
                    (lr-track-clock-test--p 'away :last-input (+ s 1200)
                                            :away-from (+ s 1200)
                                            :away-kind 'idle)
                    (+ s 2100))
                   (list :advance-to (+ s 1200) :paused-at (+ s 1200)
                         :why 'idle)))
    ;; the end already sits at the last input: pause, nothing to write
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :end (+ s 1200))
                    (lr-track-clock-test--p 'away :last-input (+ s 1200)
                                            :away-from (+ s 1200)
                                            :away-kind 'idle)
                    (+ s 2100))
                   (list :advance-to nil :paused-at (+ s 1200) :why 'idle)))))

(ert-deftest lr-track-machine-pauses-on-lock ()
  "A lock of 15 min or more pauses the clock at the last input, kind locked."
  (let ((s lr-track-clock-test--s))
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :end (+ s 1440))
                    (lr-track-clock-test--p 'away :last-input (+ s 1500)
                                            :away-from (+ s 1500)
                                            :away-kind 'locked :saw-lock t)
                    (+ s 2400))
                   (list :advance-to (+ s 1500) :paused-at (+ s 1500)
                         :why 'locked)))))

(ert-deftest lr-track-machine-pauses-when-mac-asleep ()
  "A sleep is an away too: pause at the last input, kind asleep."
  (let ((s lr-track-clock-test--s))
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :end (+ s 600))
                    (lr-track-clock-test--p 'away :last-input (+ s 660)
                                            :away-from (+ s 660)
                                            :away-kind 'asleep)
                    (+ s 9000))
                   (list :advance-to (+ s 660) :paused-at (+ s 660)
                         :why 'asleep)))))

(ert-deftest lr-track-machine-away-before-start-pauses-at-start ()
  "A clock runs while presence, sampled a minute or more after its start,
still says away since before it: the pause is the start itself and nothing
is written (never an end at or before the start).  A clock-in with presence
not yet sampled past it holds instead (see
`lr-track-regression-clock-in-while-presence-lags')."
  (let ((s lr-track-clock-test--s))
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s)
                    (lr-track-clock-test--p 'away :last-input (- s 1200)
                                            :away-from (- s 1200)
                                            :away-kind 'idle
                                            :prev-time (+ s 60))
                    (+ s 60))
                   (list :advance-to nil :paused-at s :why 'idle)))))

(ert-deftest lr-track-machine-pauses-at-an-away-no-step-saw ()
  "An away can begin and end between two clock steps: the Mac slept and his
first sample after the wake already carries input, or the sample that starts
an away and the one that ends it are stepped in the same tick.  Presence is
here again, but its latest away began on this clock's watch and ended past
the line's end: the clock pauses at that away's start, never bridges it."
  (let ((s lr-track-clock-test--s))
    ;; the night: line at 10 min, asleep 11 min to 2 h, typing again since
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :end (+ s 600))
                    (lr-track-clock-test--p
                     'here :last-input (+ s 7230) :prev-time (+ s 7230)
                     :return (+ s 7200)
                     :last-away (list :from (+ s 660) :to (+ s 7200)
                                      :kind 'asleep))
                    (+ s 7240))
                   (list :advance-to (+ s 660) :paused-at (+ s 660)
                         :why 'asleep)))
    ;; an idle away, either place: the same
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :place 'either
                                                :end (+ s 1200))
                    (lr-track-clock-test--p
                     'here :last-input (+ s 2400) :return (+ s 2390)
                     :last-away (list :from (+ s 1230) :to (+ s 2390)
                                      :kind 'idle))
                    (+ s 2410))
                   (list :advance-to (+ s 1230) :paused-at (+ s 1230)
                         :why 'idle)))
    ;; a line started at the return (its minute) owes nothing to that away
    (let ((r (+ s 30)))
      (should (equal (lr-track-clock-test--step
                      (lr-track-clock-test--clock s)
                      (lr-track-clock-test--p
                       'here :last-input (+ s 600) :return r
                       :last-away (list :from (- s 1500) :to r :kind 'locked))
                      (+ s 620))
                     (list :advance-to (+ s 600) :paused-at nil :why nil))))
    ;; an away before the clock started is no part of it
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :end (+ s 300))
                    (lr-track-clock-test--p
                     'here :last-input (+ s 900) :return (- s 600)
                     :last-away (list :from (- s 3000) :to (- s 600)
                                      :kind 'idle))
                    (+ s 920))
                   (list :advance-to (+ s 900) :paused-at nil :why nil)))
    ;; a line already past that away's end does not pause over it
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :end (+ s 3000))
                    (lr-track-clock-test--p
                     'here :last-input (+ s 3300) :return (+ s 2000)
                     :last-away (list :from (+ s 600) :to (+ s 2000)
                                      :kind 'idle))
                    (+ s 3320))
                   (list :advance-to (+ s 3300) :paused-at nil :why nil)))))

(ert-deftest lr-track-machine-never-resumes-itself ()
  "A pause is never undone by the clock: later input, a later away or more
time change nothing.  Only his key resumes."
  (let ((s lr-track-clock-test--s))
    (dolist (why '(idle locked asleep max))
      (let ((clock (lr-track-clock-test--clock s :end (+ s 1200)
                                               :paused-at (+ s 1200) :why why)))
        (should (equal (lr-track-clock-test--step
                        clock
                        (lr-track-clock-test--p 'here :last-input (+ s 5000)
                                                :return (+ s 4000))
                        (+ s 5010))
                       (list :advance-to nil :paused-at (+ s 1200) :why why)))
        (should (equal (lr-track-clock-test--step
                        clock
                        (lr-track-clock-test--p 'away :last-input (+ s 6000)
                                                :away-from (+ s 6000)
                                                :away-kind 'idle)
                        (+ s 7000))
                       (list :advance-to nil :paused-at (+ s 1200)
                             :why why)))))
    ;; the away place too
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :place 'away :max 14400.0
                                                :end (+ s 3000)
                                                :paused-at (+ s 3000)
                                                :why 'activity)
                    (lr-track-clock-test--p 'away :last-input (+ s 3500)
                                            :away-from (+ s 3500)
                                            :away-kind 'locked)
                    (+ s 6000))
                   (list :advance-to nil :paused-at (+ s 3000)
                         :why 'activity)))))

(ert-deftest lr-track-machine-unknown-presence-holds ()
  "Before the first usable sample nothing is known: hold, and never pause."
  (let ((s lr-track-clock-test--s))
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :end (+ s 600))
                    (lr-track-clock-test--p 'unknown)
                    (+ s 3000))
                   (list :advance-to nil :paused-at nil :why nil)))
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :place 'either)
                    (lr-track-clock-test--p 'unknown :last-input (+ s 900))
                    (+ s 3000))
                   (list :advance-to nil :paused-at nil :why nil)))))

(ert-deftest lr-track-either-behaves-as-machine ()
  "In Stage 1 an either stream earns exactly what a machine stream earns."
  (let ((s lr-track-clock-test--s))
    (dolist (sc (list (list (lr-track-clock-test--p 'here :last-input (+ s 300))
                            (+ s 320))
                      (list (lr-track-clock-test--p 'away :last-input (+ s 700)
                                                    :away-from (+ s 700)
                                                    :away-kind 'locked)
                            (+ s 1700))
                      (list (lr-track-clock-test--p 'unknown) (+ s 60))))
      (should (equal (lr-track-clock-test--step
                      (lr-track-clock-test--clock s :place 'either
                                                  :end (+ s 240))
                      (nth 0 sc) (nth 1 sc))
                     (lr-track-clock-test--step
                      (lr-track-clock-test--clock s :place 'machine
                                                  :end (+ s 240))
                      (nth 0 sc) (nth 1 sc)))))))

(ert-deftest lr-track-away-advances-while-away ()
  "An away stream (practice, life, sleep) earns time while he is away from the
machine: the end follows now."
  (let* ((s lr-track-clock-test--s)
         (gone (lr-track-clock-test--p 'away :last-input (- s 600)
                                       :away-from (- s 600)
                                       :away-kind 'locked)))
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :place 'away :max 14400.0)
                    gone (+ s 600))
                   (list :advance-to (+ s 600) :paused-at nil :why nil)))
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :place 'away :max 14400.0
                                                :end (+ s 600))
                    gone (+ s 1200))
                   (list :advance-to (+ s 1200) :paused-at nil :why nil)))
    ;; no time has passed since the last write: nothing to do
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :place 'away :max 14400.0
                                                :end (+ s 1200))
                    gone (+ s 1200))
                   (list :advance-to nil :paused-at nil :why nil)))))

(ert-deftest lr-track-away-holds-short-activity ()
  "A glance at the machine during practice holds the line (no pause), and when
he leaves again the end jumps to now, bridging the glance."
  (let* ((s lr-track-clock-test--s)
         (clock (lr-track-clock-test--clock s :place 'away :max 14400.0
                                            :end (+ s 3000))))
    ;; back 5 min ago, active: hold
    (should (equal (lr-track-clock-test--step
                    clock
                    (lr-track-clock-test--p 'here :last-input (+ s 3350)
                                            :return (+ s 3060)
                                            :prev-time (+ s 3330))
                    (+ s 3360))
                   (list :advance-to nil :paused-at nil :why nil)))
    ;; gone again: the end follows now, over the glance
    (should (equal (lr-track-clock-test--step
                    clock
                    (lr-track-clock-test--p 'away :last-input (+ s 3350)
                                            :away-from (+ s 3350)
                                            :away-kind 'idle
                                            :return (+ s 3060))
                    (+ s 4260))
                   (list :advance-to (+ s 4260) :paused-at nil :why nil)))))

(ert-deftest lr-track-away-pauses-at-activity-start-after-15m ()
  "15 min of activity at the machine ends the away stream's claim: it pauses
at the start of that activity (his return), not at now."
  (let ((s lr-track-clock-test--s))
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :place 'away :max 14400.0
                                                :end (+ s 3000))
                    (lr-track-clock-test--p 'here :last-input (+ s 4055)
                                            :return (+ s 3060)
                                            :prev-time (+ s 4030))
                    (+ s 4060))
                   (list :advance-to (+ s 3060) :paused-at (+ s 3060)
                         :why 'activity)))))

(ert-deftest lr-track-away-activity-counts-from-clock-start ()
  "Declared while already at the machine: the activity counts from the clock's
start, not from when he sat down, so it gets its first 15 min."
  (let* ((s lr-track-clock-test--s)
         (clock (lr-track-clock-test--clock s :place 'away :max 14400.0)))
    (should (equal (lr-track-clock-test--step
                    clock
                    (lr-track-clock-test--p 'here :last-input (+ s 590)
                                            :return (- s 3600))
                    (+ s 600))
                   (list :advance-to nil :paused-at nil :why nil)))
    (let ((r (lr-track-clock-test--step
              clock
              (lr-track-clock-test--p 'here :last-input (+ s 990)
                                      :return (- s 3600))
              (+ s 1000))))
      (should (= (plist-get r :paused-at) s))
      (should (eq (plist-get r :why) 'activity))
      ;; nothing beyond the start to write; never an end before it
      (should (or (null (plist-get r :advance-to))
                  (= (plist-get r :advance-to) s))))))

(ert-deftest lr-track-max-pauses ()
  "TRACK_MAX caps any place: the clock pauses at start plus max."
  (let ((s lr-track-clock-test--s))
    ;; machine, last input past the 1 h max
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :max 3600.0 :end (+ s 3000))
                    (lr-track-clock-test--p 'here :last-input (+ s 3700))
                    (+ s 3720))
                   (list :advance-to (+ s 3600) :paused-at (+ s 3600)
                         :why 'max)))
    ;; under the max: an ordinary advance
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :max 3600.0 :end (+ s 3000))
                    (lr-track-clock-test--p 'here :last-input (+ s 3500))
                    (+ s 3520))
                   (list :advance-to (+ s 3500) :paused-at nil :why nil)))
    ;; away place, still away, now past its 4 h max
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :place 'away :max 14400.0
                                                :end (+ s 14000))
                    (lr-track-clock-test--p 'away :last-input (- s 100)
                                            :away-from (- s 100)
                                            :away-kind 'asleep)
                    (+ s 14460))
                   (list :advance-to (+ s 14400) :paused-at (+ s 14400)
                         :why 'max)))))

(ert-deftest lr-track-never-backwards-property ()
  "1,000 random sequences of presence and time, fed through the step the way
the tick feeds it.  Whatever presence says, the end never moves backwards,
never lands before the start or after now, never passes its own pause, and a
pause, once made, is never undone or moved."
  (random "lr-track-never-backwards")
  (let ((modes '(here away unknown))
        (kinds '(idle locked asleep))
        (places '(machine either away))
        (maxes '(1800.0 3600.0 14400.0 36000.0)))
    (dotimes (_ 1000)
      (let* ((start (+ lr-track-clock-test--s (random 100000)))
             (clock (lr-track-clock-test--clock
                     start :place (nth (random 3) places)
                     :max (nth (random 4) maxes)))
             (now (+ start (random 120))))
        (dotimes (_ 25)
          (setq now (+ now (random 900)))
          (let* ((mode (nth (random 3) modes))
                 (p (lr-track-clock-test--p
                     mode
                     :last-input (- now (random 2400))
                     :prev-time (- now (random 60))
                     :first-time (- start (random 7200))
                     :away-from (and (eq mode 'away) (- now (random 5400)))
                     :away-kind (and (eq mode 'away) (nth (random 3) kinds))
                     :return (and (zerop (random 2)) (- now (random 7200)))))
                 (end (plist-get clock :end))
                 (was (plist-get clock :paused-at))
                 (r (lr-track-clock-test--step clock p now))
                 (adv (plist-get r :advance-to))
                 (pa (plist-get r :paused-at)))
            (when adv
              (should (>= adv start))
              (when end (should (> adv end)))
              (should (<= adv now))
              (when pa (should (<= adv pa))))
            (cond
             (was
              (should-not adv)
              (should (and pa (= pa was)))
              (should (eq (plist-get r :why) (plist-get clock :why))))
             (pa
              (should (>= pa start))
              (should (memq (plist-get r :why)
                            '(idle locked asleep max activity))))
             (t (should-not (plist-get r :why))))
            (when adv
              (setq clock (plist-put (copy-sequence clock) :end adv)))
            (when (and pa (not was))
              (setq clock (plist-put (plist-put (copy-sequence clock)
                                                :paused-at pa)
                                     :why (plist-get r :why))))))))))


;;;; the tick and the pause state (contract 4.3, 4.4)

(ert-deftest lr-track-running-line-end-reads-the-tail ()
  "The running line's end is the stamp after its start stamp; nil while open."
  (let ((t0 (lr-track-clock-test--minute -3600.0)))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (should-not (lr-track--running-line-end))
      (lr-track--advance-clock-line (+ t0 1800))
      (should (= (lr-track--running-line-end) (+ t0 1800))))))

(ert-deftest lr-track-tick-advances-real-line-to-last-input ()
  "The tick writes the step's verdict into the real CLOCK line: one line,
ending at the minute of the last input, the clock still running.  Presence
decides; the old classifier's state no longer gates the advance."
  (let ((t0 (lr-track-clock-test--minute 0.0)))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (lr-track-clock-test--with-now (+ t0 1560)
        (let ((lr-track--stable-state 'away)
              (lr-track--presence
               (lr-track-clock-test--p 'here :last-input (+ t0 1500))))
          (lr-track--tick-live-clock))
        (should (org-clocking-p))
        (let ((lines (lr-track-clock-test--clock-lines buf)))
          (should (= 1 (length lines)))
          (should (string-match-p
                   (concat (lr-track-clock-test--ends-at (+ t0 1500))
                           " =>  0:25\\'")
                   (car lines))))
        (should (= (lr-track--running-line-end) (+ t0 1500)))
        (should-not (lr-track--paused-at))
        ;; unknown presence: the line holds
        (setq now (+ t0 1800))
        (let ((lr-track--presence
               (lr-track-clock-test--p 'unknown :last-input (+ t0 1790))))
          (lr-track--tick-live-clock))
        (should (= (lr-track--running-line-end) (+ t0 1500)))))))

(ert-deftest lr-track-tick-away-place-advances-to-now ()
  "The tick reads the place from the clocked heading's stream: a practice task
advances to now while he is away from the machine."
  (let ((t0 (lr-track-clock-test--minute 0.0)))
    (lr-track-clock-test--clocked
        (t0 (concat "* practice\n:PROPERTIES:\n:TRACK_PLACE: away\n"
                    ":TRACK_MAX:   4:00\n:END:\n** TODO Scales\n")
            "^\\*\\* TODO Scales")
      (lr-track-clock-test--with-now (+ t0 1260)
        (let ((lr-track--presence
               (lr-track-clock-test--p 'away :last-input (- t0 600)
                                       :away-from (- t0 600)
                                       :away-kind 'locked)))
          (lr-track--tick-live-clock))
        (should (= (lr-track--running-line-end) (+ t0 1260)))
        (should-not (lr-track--paused-at))))))

(ert-deftest lr-track-tick-pause-records-state-and-forces-save ()
  "When the step pauses, the tick writes the line up to the pause, records
(:start :paused-at :why) for this clock, mirrors it to state.eld, and saves
at once even though the last save was a minute ago.  The next tick, with him
back, changes nothing."
  (let ((t0 (lr-track-clock-test--minute 0.0))
        (lr-track-autosave-clock t)
        (lr-track--state nil))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (let ((start (float-time org-clock-start-time)))
        (lr-track-clock-test--with-now (+ t0 700)
          (let ((lr-track--presence
                 (lr-track-clock-test--p 'here :last-input (+ t0 690))))
            (lr-track--tick-live-clock))
          (lr-track--maybe-autosave buf t)  ; a save happened just now
          (should-not (buffer-modified-p buf))
          (setq now (+ t0 760))
          (let ((lr-track--presence
                 (lr-track-clock-test--p 'away :last-input (+ t0 750)
                                         :away-from (+ t0 750)
                                         :away-kind 'locked)))
            (lr-track--tick-live-clock))
          ;; written up to the pause
          (should (= (lr-track--running-line-end) (+ t0 720)))
          (should (org-clocking-p))
          ;; recorded for this clock
          (should (= (plist-get lr-track--clock-pause :start) start))
          (should (= (plist-get lr-track--clock-pause :paused-at) (+ t0 750)))
          (should (eq (plist-get lr-track--clock-pause :why) 'locked))
          (should (= (lr-track--paused-at) (+ t0 750)))
          ;; saved at once, 60 s after the last save
          (should-not (buffer-modified-p buf))
          (should (string-match-p (lr-track-clock-test--ends-at (+ t0 720))
                                  (lr-track-clock-test--disk file)))
          ;; mirrored to state.eld (the heartbeat runs right after in a tick)
          (lr-track--run-phase 'heartbeat)
          (let ((cp (plist-get (lr-track--read-state) :clock-pause)))
            (should (= (plist-get cp :paused-at) (+ t0 750)))
            (should (eq (plist-get cp :why) 'locked)))
          ;; back at the machine: the paused line stays where it is
          (setq now (+ t0 820))
          (let ((lr-track--presence
                 (lr-track-clock-test--p 'here :last-input (+ t0 810)
                                         :return (+ t0 790))))
            (lr-track--tick-live-clock))
          (should (= (lr-track--running-line-end) (+ t0 720)))
          (should (= (lr-track--paused-at) (+ t0 750))))))))

(ert-deftest lr-track-tick-sleep-with-input-at-wake-pauses-at-departure ()
  "The real phases, real presence steps: he types at 10 min, the Mac sleeps,
and the first sample after the wake already shows his input.  Presence goes
away and back in that one step, so no tick ever sees it away; the machine
clock must still pause where he left, not run across the night.  Likewise
for an idle away whose start and end are stepped in one tick."
  (let ((t0 (lr-track-clock-test--minute -7200.0))
        (lr-track-autosave-clock nil)
        (lr-track--state nil))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (let ((lr-track--presence (lr-track--presence-init))
            (lr-track--presence-stepped nil)
            (lr-track--last-sample nil))
        (cl-flet ((tick (at sample)
                    (lr-track-clock-test--with-now at
                      (setq lr-track--last-sample sample)
                      (lr-track--tick-presence)
                      (lr-track--tick-live-clock))))
          ;; presence sees him from the clock-in on (a first sample after
          ;; the line's start would leave that span unseen, and pause it)
          (tick (+ t0 1)
                (list :time (+ t0 1) :idle 1.0 :locked nil
                      :wake (- t0 86400.0)))
          (tick (+ t0 600)
                (list :time (+ t0 600) :idle 10.0 :locked nil
                      :wake (- t0 86400.0)))
          (should (eq (plist-get lr-track--presence :mode) 'here))
          (should (= (lr-track--running-line-end) (+ t0 540)))
          ;; asleep from 10:00 to 1:00:00; he typed 5 s after the wake
          (tick (+ t0 3610)
                (list :time (+ t0 3610) :idle 5.0 :locked nil
                      :wake (+ t0 3600.0)))
          (should (eq (plist-get lr-track--presence :mode) 'here))
          (should (eq (plist-get (plist-get lr-track--presence :last-away)
                                 :kind)
                      'asleep))
          (should (= (lr-track--paused-at) (+ t0 590)))
          (should (eq (plist-get lr-track--clock-pause :why) 'asleep))
          (should (= (lr-track--running-line-end) (+ t0 540)))
          ;; and it stays paused while he works on
          (tick (+ t0 4200)
                (list :time (+ t0 4200) :idle 2.0 :locked nil
                      :wake (+ t0 3600.0)))
          (should (= (lr-track--running-line-end) (+ t0 540)))
          (should (= (lr-track--paused-at) (+ t0 590))))))
    ;; an idle away: the sample that starts it and the one that ends it are
    ;; both stepped before the clock step (the synthesized-sample path)
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (let ((lr-track--presence (lr-track--presence-init))
            (lr-track--presence-stepped nil)
            (lr-track--last-sample nil))
        (lr-track-clock-test--with-now (+ t0 600)
          ;; presence sees him from the clock-in on
          (lr-track--step-sample (list :time t0 :idle 0.0 :locked nil
                                       :wake nil))
          (lr-track--step-sample (list :time (+ t0 600) :idle 0.0 :locked nil
                                       :wake nil))
          (lr-track--tick-live-clock)
          (should (= (lr-track--running-line-end) (+ t0 600))))
        (lr-track-clock-test--with-now (+ t0 1600)
          (lr-track--step-sample (list :time (+ t0 1560) :idle 960.0
                                       :locked nil :wake nil))
          (lr-track--step-sample (list :time (+ t0 1600) :idle 3.0
                                       :locked nil :wake nil))
          (should (eq (plist-get lr-track--presence :mode) 'here))
          (lr-track--tick-live-clock)
          (should (= (lr-track--paused-at) (+ t0 600)))
          (should (eq (plist-get lr-track--clock-pause :why) 'idle))
          (should (= (lr-track--running-line-end) (+ t0 600))))))))

(ert-deftest lr-track-pause-state-resets-on-clock-change ()
  "The pause belongs to one clock.  A pause recorded for another start does
not apply (and the tick advances the live clock), and clocking out or in
clears it."
  (let ((t0 (lr-track-clock-test--minute -3600.0)))
    (lr-track-clock-test--clocked
        (t0 "* TODO Write report\n* TODO Read paper\n")
      (let ((s (float-time org-clock-start-time)))
        (setq lr-track--clock-pause (list :start s :paused-at (+ s 600)
                                          :why 'idle))
        (should (= (lr-track--paused-at) (+ s 600)))
        ;; recorded for a different start: not this clock's pause
        (setq lr-track--clock-pause (list :start (- s 60) :paused-at (+ s 600)
                                          :why 'idle))
        (should-not (lr-track--paused-at))
        (let ((lr-track--presence
               (lr-track-clock-test--p 'here :last-input (+ t0 1200))))
          (lr-track--tick-live-clock))
        (should (= (lr-track--running-line-end) (+ t0 1200)))
        ;; clocking out clears it
        (setq lr-track--clock-pause (list :start s :paused-at (+ s 1200)
                                          :why 'idle))
        (let ((lr-track--internal t))
          (org-clock-out nil t (seconds-to-time (+ t0 1200))))
        (should-not lr-track--clock-pause)
        ;; clocking in clears it
        (setq lr-track--clock-pause (list :start s :paused-at (+ s 600)
                                          :why 'idle))
        (goto-char (point-min))
        (re-search-forward "^\\* TODO Read paper")
        (beginning-of-line)
        (org-clock-in)
        (should-not lr-track--clock-pause)
        (should-not (lr-track--paused-at))))))


;;;; autosave (contract 4.5)

(ert-deftest lr-track-autosave-throttle-5m-and-forced-at-pause ()
  "Advances save at most once per 5 min; a pause saves at once."
  (let ((t0 (lr-track-clock-test--minute 0.0))
        (lr-track-autosave-clock t))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (cl-flet ((here (l) (lr-track-clock-test--p 'here :last-input l)))
        (lr-track-clock-test--with-now (+ t0 700)
          ;; A: the window opens with a save now
          (let ((lr-track--presence (here (+ t0 690))))
            (lr-track--tick-live-clock))
          (lr-track--maybe-autosave buf t)
          (should-not (buffer-modified-p buf))
          ;; B: 2 min later a real advance is written but not saved
          (setq now (+ t0 820))
          (let ((lr-track--presence (here (+ t0 810))))
            (lr-track--tick-live-clock))
          (should (= (lr-track--running-line-end) (+ t0 780)))
          (should (buffer-modified-p buf))
          (should-not (string-match-p (lr-track-clock-test--ends-at (+ t0 780))
                                      (lr-track-clock-test--disk file)))
          ;; asked directly inside the window, unforced: still nothing
          (setq now (+ t0 999))
          (lr-track--maybe-autosave buf nil)
          (should (buffer-modified-p buf))
          ;; C: 310 s after A the next advance saves
          (setq now (+ t0 1010))
          (let ((lr-track--presence (here (+ t0 1000))))
            (lr-track--tick-live-clock))
          (should-not (buffer-modified-p buf))
          (should (string-match-p (lr-track-clock-test--ends-at (+ t0 960))
                                  (lr-track-clock-test--disk file)))
          ;; D: 60 s after C a pause saves at once
          (setq now (+ t0 1070))
          (let ((lr-track--presence
                 (lr-track-clock-test--p 'away :last-input (+ t0 1065)
                                         :away-from (+ t0 1065)
                                         :away-kind 'idle)))
            (lr-track--tick-live-clock))
          (should (= (lr-track--paused-at) (+ t0 1065)))
          (should-not (buffer-modified-p buf))
          (should (string-match-p (lr-track-clock-test--ends-at (+ t0 1020))
                                  (lr-track-clock-test--disk file)))
          ;; E: forced always saves; unforced waits out the window
          (setq now (+ t0 1080))
          (save-excursion (goto-char (point-max)) (insert "\n"))
          (lr-track--maybe-autosave buf nil)
          (should (buffer-modified-p buf))
          (lr-track--maybe-autosave buf t)
          (should-not (buffer-modified-p buf)))))))

(ert-deftest lr-track-kill-emacs-forces-a-save ()
  "Quitting Emacs flushes the clock line's own advance even inside the 5 min
window, where the throttle is holding it.  (Only that: text he left unsaved
stays his call, `lr-track-regression-kill-save-keeps-his-no'.)"
  (let ((t0 (lr-track-clock-test--minute 0.0))
        (lr-track-autosave-clock t)
        (lr-track--state nil))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (lr-track-clock-test--with-now (+ t0 700)
        (lr-track--maybe-autosave buf t)
        (should-not (buffer-modified-p buf))
        (setq now (+ t0 760))
        (should (lr-track--advance-clock-line (+ t0 720)))
        (should (buffer-modified-p buf))     ; held by the throttle
        (lr-track--on-kill-emacs)
        (should-not (buffer-modified-p buf))
        (should (string-match-p (lr-track-clock-test--ends-at (+ t0 720))
                                (lr-track-clock-test--disk file)))))))

(ert-deftest lr-track-save-buffer-is-silent ()
  "The autosave never prints a Wrote line: `save-silently' and
`inhibit-message' are t around the save, and the save still happens."
  (let* ((dir (file-name-as-directory (make-temp-file "lr-track-save" t)))
         (file (expand-file-name "s.org" dir))
         (seen nil)
         buf)
    (with-temp-file file (insert "* x\n"))
    (setq buf (find-file-noselect file))
    (unwind-protect
        (progn
          (with-current-buffer buf (goto-char (point-max)) (insert "y\n"))
          (cl-letf (((symbol-function 'save-buffer)
                     (lambda (&rest _)
                       (push (list save-silently inhibit-message) seen)
                       (set-buffer-modified-p nil)))
                    ((symbol-function 'basic-save-buffer)
                     (lambda (&rest _)
                       (push (list save-silently inhibit-message) seen)
                       (set-buffer-modified-p nil))))
            (lr-track--save-buffer buf))
          (should seen)
          (should (cl-every (lambda (s) (equal s '(t t))) seen))
          ;; and for real
          (with-current-buffer buf (goto-char (point-max)) (insert "z\n"))
          (lr-track--save-buffer buf)
          (should-not (buffer-modified-p buf))
          (should (string-match-p "^z$" (lr-track-clock-test--disk file))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (ignore-errors (delete-directory dir t)))))


;;;; clock-out at the pause (contract 4.6)

(ert-deftest lr-track-clock-out-advice-installed-at-load ()
  "Deploy is by `load' (I13), so the filter must be in place after loading the
module, without toggling the mode."
  (should (advice-member-p #'lr-track--clock-out-args 'org-clock-out))
  (should (boundp 'lr-track--internal))
  (should (null (default-toplevel-value 'lr-track--internal))))

(ert-deftest lr-track-clock-out-filter-at-pause ()
  "His clock-out of a paused clock (no AT-TIME) ends it at the pause; the
other arguments pass through untouched."
  (let ((t0 (lr-track-clock-test--minute -3600.0)))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (let ((p (+ t0 1500)))
        (lr-track-clock-test--pause p 'idle)
        (let ((args (lr-track--clock-out-args (list 'done t nil))))
          (should (eq (nth 0 args) 'done))
          (should (eq (nth 1 args) t))
          (should (= (float-time (nth 2 args)) p)))
        ;; O and SPC c o call it with no arguments at all
        (lr-track-clock-test--capturing _msgs
          (org-clock-out))
        (should-not (org-clocking-p))
        (let ((line (car (lr-track-clock-test--clock-lines buf))))
          (should (string-match-p (lr-track-clock-test--ends-at p) line))
          (should (string-match-p " =>  0:25\\'" line)))))))

(ert-deftest lr-track-clock-out-filter-nil-when-live ()
  "A live clock (or a pause recorded for another clock) leaves AT-TIME nil,
so org ends it now, and no ended note or message appears."
  (let ((t0 (lr-track-clock-test--minute -3600.0)))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (should-not (lr-track--paused-at))
      (should (equal (lr-track--clock-out-args (list nil nil nil))
                     (list nil nil nil)))
      (setq lr-track--clock-pause (list :start (- t0 600) :paused-at (+ t0 600)
                                        :why 'idle))
      (should (equal (lr-track--clock-out-args (list nil nil nil))
                     (list nil nil nil)))
      (lr-track-clock-test--capturing msgs
        (org-clock-out)
        (should-not (org-clocking-p))
        (should (string-match-p " =>  1:0[01]\\'"
                                (car (lr-track-clock-test--clock-lines buf))))
        (should-not (string-match-p "- lr ended" (buffer-string)))
        (should-not (cl-some (lambda (m) (and m (string-match-p "paused" m)))
                             msgs))))))

(ert-deftest lr-track-clock-out-filter-respects-explicit-at-time ()
  "An explicit AT-TIME always wins, paused or not, and earns no ended note."
  (let ((t0 (lr-track-clock-test--minute -3600.0)))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (lr-track-clock-test--pause (+ t0 600) 'idle)
      (let ((x (seconds-to-time (+ t0 1200))))
        (should (equal (lr-track--clock-out-args (list nil nil x))
                       (list nil nil x)))
        (lr-track-clock-test--capturing _msgs
          (org-clock-out nil nil x)))
      (should (string-match-p " =>  0:20\\'"
                              (car (lr-track-clock-test--clock-lines buf))))
      (should-not (string-match-p "- lr ended" (buffer-string))))))

(ert-deftest lr-track-clock-out-filter-bypassed-when-internal ()
  "Internal clock-outs bind `lr-track--internal' and the filter stays out of
the way: org ends the clock now, with no ended note."
  (let ((t0 (lr-track-clock-test--minute -3600.0)))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (lr-track-clock-test--pause (+ t0 600) 'idle)
      (let ((lr-track--internal t))
        (should (equal (lr-track--clock-out-args (list nil nil nil))
                       (list nil nil nil)))
        (lr-track-clock-test--capturing _msgs
          (org-clock-out)))
      (should (string-match-p " =>  1:0[01]\\'"
                              (car (lr-track-clock-test--clock-lines buf))))
      (should-not (string-match-p "- lr ended" (buffer-string))))))

(ert-deftest lr-track-switch-path-uses-pause ()
  "Clocking into another task while one is paused (org's own switch calls
`org-clock-out' with no AT-TIME) ends the paused one at its pause, notes it,
and clears the pause."
  (let ((t0 (lr-track-clock-test--minute -3600.0)))
    (lr-track-clock-test--clocked
        (t0 "* TODO Write report\n* TODO Read paper\n")
      (let ((p (+ t0 600)))
        (lr-track-clock-test--pause p 'locked)
        (lr-track-clock-test--capturing msgs
          (goto-char (point-min))
          (re-search-forward "^\\* TODO Read paper")
          (beginning-of-line)
          (org-clock-in)
          (should (member
                   (format "Clocked out of Write report at %s, where it paused."
                           (lr-track--ts-hm p))
                   msgs)))
        (should (org-clocking-p))
        (should (equal org-clock-current-task "Read paper"))
        (let ((closed (car (lr-track-clock-test--clock-lines buf))))
          (should (string-match-p (lr-track-clock-test--ends-at p) closed))
          (should (string-match-p " =>  0:10\\'" closed)))
        (should (string-match-p
                 (concat "CLOCK: .* =>  0:10\n"
                         "[ \t]*- lr ended: where it paused "
                         (regexp-quote (lr-track--ts-hm p)) "\n")
                 (buffer-string)))
        (should-not lr-track--clock-pause)
        (should-not (lr-track--paused-at))))))

(ert-deftest lr-track-ended-note-below-line ()
  "The ended note sits on the line right after the closed CLOCK line, with
that line's indentation, and the echo says where it ended."
  (let ((t0 (lr-track-clock-test--minute -3600.0))
        (org-adapt-indentation t))
    (lr-track-clock-test--clocked
        (t0 "* Project\n** TODO Write report\n" "^\\*\\* TODO Write report")
      (let* ((p (+ t0 1500))
             (hm (lr-track--ts-hm p)))
        (lr-track-clock-test--pause p 'idle)
        (lr-track-clock-test--capturing msgs
          (org-clock-out)
          (should (member
                   (format "Clocked out of Write report at %s, where it paused."
                           hm)
                   msgs)))
        (should-not (org-clocking-p))
        (let* ((lines (split-string (buffer-substring-no-properties
                                     (point-min) (point-max))
                                    "\n"))
               (i (cl-position-if
                   (lambda (l) (string-match-p "^[ \t]*CLOCK: " l)) lines))
               (clock-line (nth i lines))
               (indent (progn (string-match "^[ \t]*" clock-line)
                              (match-string 0 clock-line))))
          (should (> (length indent) 0))   ; the case where indentation matters
          (should (string-match-p " =>  0:25\\'" clock-line))
          (should (equal (nth (1+ i) lines)
                         (concat indent "- lr ended: where it paused " hm))))
        (should (= 1 (how-many "- lr ended" (point-min) (point-max))))))))

(ert-deftest lr-track-zero-length-message ()
  "Paused under a minute after the start: org removes the zero-time line, so
there is nothing to note, and the echo says no time was recorded."
  (let ((t0 (lr-track-clock-test--minute -3600.0))
        (org-clock-out-remove-zero-time-clocks t))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (lr-track-clock-test--pause (+ t0 30) 'idle)
      (lr-track-clock-test--capturing msgs
        (org-clock-out)
        (should (member (concat "Write report: no time recorded"
                                " (you left right after clocking in).")
                        msgs))
        (should-not (cl-some (lambda (m)
                               (and m (string-prefix-p "Clocked out of" m)))
                             msgs)))
      (should-not (org-clocking-p))
      (should-not (lr-track-clock-test--clock-lines buf))
      (should-not (string-match-p "- lr ended" (buffer-string))))))


;;;; paused clock string (contract 4.7)

(ert-deftest lr-track-paused-clock-string ()
  "A paused clock says so in the modeline: TASK (at most 20 chars) paused
HH:MM, in the warning face.  A live clock keeps org's own string."
  (let ((t0 (lr-track-clock-test--minute -3600.0)))
    (lr-track-clock-test--clocked
        (t0 "* TODO Prepare the quarterly review slides\n")
      (let ((live (org-clock-get-clock-string)))
        (should-not (string-match-p "paused" live))
        (should (string-match-p "Prepare the quarterly review slides" live)))
      (lr-track-clock-test--pause (+ t0 1500) 'idle)
      (let ((s (org-clock-get-clock-string)))
        (should (string= s (format "Prepare the quarterl paused %s"
                                   (lr-track--ts-hm (+ t0 1500)))))
        (should (eq (get-text-property 0 'face s) 'warning))))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (lr-track-clock-test--pause (+ t0 600) 'locked)
      (should (string= (org-clock-get-clock-string)
                       (format "Write report paused %s"
                               (lr-track--ts-hm (+ t0 600))))))))


;;;; S0 (contract 4.8)

(ert-deftest lr-track-s0-defaults-silence-old-prompts ()
  "The old coach goes quiet by default: no return or startup check-in, no
banners, no elsewhere nag.  Its code stays until Stage 2."
  (dolist (pair '((lr-track-checkin-on-return . nil)
                  (lr-track-checkin-on-startup . nil)
                  (lr-track-daily-banner-budget . 0)
                  (lr-track-elsewhere-checkin-seconds . nil)))
    (should (equal (cons (car pair)
                         (eval (car (get (car pair) 'standard-value)) t))
                   pair))
    (should (equal (cons (car pair) (default-toplevel-value (car pair)))
                   pair)))
  (should (fboundp 'lr-track-checkin))
  (should (fboundp 'lr-track--trigger-checkin)))


;;;; phases (contract 4.9)

(ert-deftest lr-track-phases-include-presence-and-header ()
  "Presence is stepped right after sensing, before the clock reads it, and the
header refresh runs last."
  (should (equal lr-track--phases
                 '(sense presence live-clock heartbeat activity nudge modeline
                         header))))

(ert-deftest lr-track-presence-phase-steps-each-new-sample-once ()
  "The presence phase steps the newest sample exactly once, keeps the result
in `lr-track--presence', and mirrors a compact copy to state.eld."
  (let* ((base 3.0e9)              ; later than any sample another test made
         (s1 (list :time base :idle 5.0 :locked nil :wake nil))
         (s2 (list :time (+ base 30.0) :idle 35.0 :locked nil :wake nil))
         (init (lr-track--presence-init))
         (want1 (lr-track--presence-step init s1))
         (orig (symbol-function 'lr-track--presence-step))
         (calls 0)
         (lr-track--presence init)
         (lr-track--last-sample s1)
         (lr-track--state nil)
         (lr-track--phase-failures nil))
    (cl-letf (((symbol-function 'lr-track--presence-step)
               (lambda (p s) (cl-incf calls) (funcall orig p s))))
      (should-not (lr-track--run-phase 'presence))
      (should (= calls 1))
      (should (equal lr-track--presence want1))
      ;; the same sample again: not stepped twice
      (should-not (lr-track--run-phase 'presence))
      (should (= calls 1))
      (setq lr-track--last-sample s2)
      (should-not (lr-track--run-phase 'presence))
      (should (= calls 2))
      (should (equal lr-track--presence (funcall orig want1 s2))))
    (let ((m (plist-get (lr-track--read-state) :presence)))
      (should m)
      (should (eq (plist-get m :mode) (plist-get lr-track--presence :mode)))
      (should (= (plist-get m :last-input)
                 (plist-get lr-track--presence :last-input)))
      (should (cl-subsetp (cl-loop for k in m by #'cddr collect k)
                          '(:mode :last-input :away-from :away-kind :return
                                  :last-away))))))

(ert-deftest lr-track-header-phase-refreshes-only-when-ask-is-loaded ()
  "The header phase calls `lr-track-ask-refresh-header' when it exists, and is
a quiet success when it does not (lr-track never requires lr-track-ask)."
  (let ((calls 0)
        (lr-track--phase-failures nil))
    (cl-letf (((symbol-function 'lr-track-ask-refresh-header)
               (lambda (&rest _) (cl-incf calls))))
      (should-not (lr-track--run-phase 'header))
      (should (= calls 1)))
    (cl-letf (((symbol-function 'lr-track-ask-refresh-header) nil))
      (should-not (lr-track--run-phase 'header))
      (should (eql 0 (alist-get 'header lr-track--phase-failures))))))

(ert-deftest lr-track-header-phase-never-loads-org ()
  "org loads lazily, never from a tick: until something else has loaded org
the header phase does not refresh (reading time.org would load it), and the
header only shows in an agenda, which loads org itself."
  (let ((calls 0)
        (lr-track--phase-failures nil)
        (orig (symbol-function 'featurep)))
    (cl-letf (((symbol-function 'lr-track-ask-refresh-header)
               (lambda (&rest _) (cl-incf calls))))
      ;; Emacs 31's `featurep' ignores a let-bound `features': stub it
      (cl-letf (((symbol-function 'featurep)
                 (lambda (f &optional sub)
                   (and (not (eq f 'org)) (funcall orig f sub)))))
        (should-not (lr-track--run-phase 'header)))
      (should (= calls 0))
      (should-not (lr-track--run-phase 'header))
      (should (= calls 1)))))

(defun lr-track-clock-test--source-forms (file)
  "Every top-level form of FILE, read without evaluating."
  (with-temp-buffer
    (insert-file-contents file)
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun lr-track-clock-test--tree-has (needle tree)
  "Non-nil when NEEDLE is `equal' to TREE or to any subtree of it."
  (or (equal needle tree)
      (and (consp tree)
           (or (lr-track-clock-test--tree-has needle (car tree))
               (lr-track-clock-test--tree-has needle (cdr tree))))))

(ert-deftest lr-track-presence-initialised-by-enable-not-by-load ()
  "Deploy is by `load' mid-day (I13): `lr-track--presence' is a `defvar', so a
reload keeps the live value, and only `lr-track--enable' starts it fresh."
  (let* ((forms (lr-track-clock-test--source-forms
                 (expand-file-name "modules/lr-track.el"
                                   lr-track-clock-test--root)))
         (decl (cl-find-if (lambda (f) (and (consp f)
                                            (eq (nth 1 f) 'lr-track--presence)
                                            (memq (car f)
                                                  '(defvar defconst defcustom
                                                     setq defvar-local))))
                           forms))
         (enable (cl-find-if (lambda (f) (and (consp f) (eq (car f) 'defun)
                                              (eq (nth 1 f) 'lr-track--enable)))
                             forms)))
    (should decl)
    (should (eq (car decl) 'defvar))
    (should-not (cl-some (lambda (f) (and (consp f) (eq (car f) 'setq)
                                          (memq 'lr-track--presence f)))
                         forms))
    (should enable)
    (should (lr-track-clock-test--tree-has '(lr-track--presence-init) enable))))


;;;; regressions from the round 1 review

(defmacro lr-track-clock-test--fresh-presence (&rest body)
  "Run BODY with presence, its stepping and the blind mark starting empty."
  (declare (indent 0) (debug t))
  `(let ((lr-track--presence (lr-track--presence-init))
         (lr-track--presence-stepped nil)
         (lr-track--last-sample nil)
         (lr-track--blind-since nil)
         (lr-track--sys-idle-value 'unknown)
         (lr-track--sys-idle-stamp nil)
         (lr-track--sense-failures 0))
     ,@body))

(defun lr-track-clock-test--feed (at idle &optional locked wake)
  "Step the sample (AT IDLE LOCKED WAKE) into presence, as the phase does."
  (lr-track--step-sample (list :time (float at) :idle (float idle)
                               :locked locked :wake wake)))

(ert-deftest lr-track-regression-clock-in-while-presence-lags ()
  "He clocks in within a tick of his return, while presence still says away
from a sample taken before he sat down.  The clock must not pause at its own
start (his clock-out would then delete the hour he worked): it holds until a
sample a minute past the start, then earns time from his input."
  (let ((s lr-track-clock-test--s))
    ;; presence's newest sample is older than a minute past the start: hold
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s)
                    (lr-track-clock-test--p 'away :last-input (- s 3000)
                                            :away-from (- s 3000)
                                            :away-kind 'idle
                                            :prev-time (+ s 5))
                    (+ s 60))
                   (list :advance-to nil :paused-at nil :why nil)))
    ;; sampled past that and still away: the pause at the start stands
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s)
                    (lr-track-clock-test--p 'away :last-input (- s 3000)
                                            :away-from (- s 3000)
                                            :away-kind 'idle
                                            :prev-time (+ s 61))
                    (+ s 90))
                   (list :advance-to nil :paused-at s :why 'idle))))
  ;; the real phases: here until B-3600, away, back at B+10, clocked in at
  ;; B+20 (org floors the start to B), working in another app after that
  (let ((b (lr-track-clock-test--minute -7200.0))
        (lr-track-autosave-clock nil)
        (lr-track--state nil))
    (lr-track-clock-test--clocked (b "* TODO Write report\n")
      (lr-track-clock-test--fresh-presence
        (lr-track-clock-test--feed (- b 3600) 0.0)
        (lr-track-clock-test--feed (- b 2600) 1000.0)
        (lr-track-clock-test--feed (+ b 0.05) 3600.05)
        (should (lr-track--presence-away-p lr-track--presence))
        ;; the first tick after the clock-in still steps the old sample
        (lr-track-clock-test--with-now (+ b 60)
          (lr-track--tick-live-clock))
        (should-not (lr-track--paused-at))
        ;; the next sample sees his input: he is here, the clock earns it
        (lr-track-clock-test--feed (+ b 60.05) 5.0)
        (should (eq (plist-get lr-track--presence :mode) 'here))
        (lr-track-clock-test--with-now (+ b 61)
          (lr-track--tick-live-clock))
        ;; he works on: a sample every tick
        (cl-loop for x from 90 to 3600 by 30
                 do (lr-track-clock-test--feed (+ b x) 5.0))
        (lr-track-clock-test--with-now (+ b 3600)
          (lr-track--tick-live-clock))
        (should-not (lr-track--paused-at))
        (should (= (lr-track--running-line-end) (+ b 3540)))))))

(ert-deftest lr-track-regression-sleep-right-after-clock-in ()
  "He clocks in and closes the lid before the next sample: presence dates the
sleep from the last input it saw, before the clock's start.  The machine
clock must pause at its start, not run through the night."
  (let ((s lr-track-clock-test--s))
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s)
                    (lr-track-clock-test--p
                     'here :last-input (+ s 3605) :prev-time (+ s 3610)
                     :return (+ s 3600)
                     :last-away (list :from (- s 600) :to (+ s 3600)
                                      :kind 'asleep))
                    (+ s 3620))
                   (list :advance-to nil :paused-at s :why 'asleep))))
  (let ((t0 (lr-track-clock-test--minute -7200.0))
        (lr-track-autosave-clock nil)
        (lr-track--state nil))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (lr-track-clock-test--fresh-presence
        ;; reading, idle 10 min, at the last sample before the clock-in
        (lr-track-clock-test--feed (- t0 20) 580.0 nil (- t0 86400.0))
        (should (eq (plist-get lr-track--presence :mode) 'here))
        ;; the Mac wakes an hour later and he types at once
        (lr-track-clock-test--feed (+ t0 3610) 5.0 nil (+ t0 3600.0))
        (lr-track-clock-test--with-now (+ t0 3610)
          (lr-track--tick-live-clock))
        (should (= (lr-track--paused-at) t0))
        (should (eq (plist-get lr-track--clock-pause :why) 'asleep))
        (lr-track-clock-test--feed (+ t0 4200) 2.0 nil (+ t0 3600.0))
        (lr-track-clock-test--with-now (+ t0 4200)
          (lr-track--tick-live-clock))
        (should-not (lr-track--running-line-end))))))

(ert-deftest lr-track-regression-same-start-stamp-uses-marker-line ()
  "An answer can write a closed line with the running line's own start stamp
into the same entry, above it.  The writer and the end reader take the line
at `org-clock-marker', never the first one with that stamp."
  (let ((t0 (lr-track-clock-test--minute -7200.0)))
    (lr-track-clock-test--clocked (t0 "* practice\n")
      (let ((closed (format "CLOCK: %s--%s =>  0:39"
                            (lr-track-clock-test--stamp t0)
                            (lr-track-clock-test--stamp (+ t0 2340)))))
        ;; a closed line at the top of the LOGBOOK, as a label writes it
        (save-excursion
          (goto-char org-clock-marker)
          (beginning-of-line)
          (insert closed "\n"))
        (should-not (lr-track--running-line-end))
        (should (lr-track--advance-clock-line (+ t0 3900)))
        (should (= (lr-track--running-line-end) (+ t0 3900)))
        (let ((lines (lr-track-clock-test--clock-lines buf)))
          (should (equal (car lines) closed))
          (should (string-match-p (lr-track-clock-test--ends-at (+ t0 3900))
                                  (cadr lines))))
        (save-excursion
          (goto-char org-clock-marker)
          (should (string-match-p (lr-track-clock-test--ends-at (+ t0 3900))
                                  (buffer-substring (line-beginning-position)
                                                    (line-end-position)))))))))

(ert-deftest lr-track-regression-clock-in-on-paused-task ()
  "Agenda I or SPC c i on the paused task itself: org would say Clock
continues and leave it paused.  It ends at its pause, with its note, and a
new line runs from now, as a switch to another task would."
  (let ((t0 (lr-track-clock-test--minute -3600.0)))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (let ((p (+ t0 600)))
        (lr-track-clock-test--pause p 'idle)
        (org-clock-in)
        (should (org-clocking-p))
        (should-not (lr-track--paused-at))
        (should (> (float-time org-clock-start-time) p))
        (let ((lines (lr-track-clock-test--clock-lines buf)))
          (should (= 2 (length lines)))
          (should (string-match-p "\\`CLOCK: \\[[^]]*\\]\\'" (car lines)))
          (should (string-match-p (lr-track-clock-test--ends-at p) (cadr lines))))
        (should (string-match-p (concat "- lr ended: where it paused "
                                        (regexp-quote (lr-track--ts-hm p)))
                                (buffer-string)))))))

(ert-deftest lr-track-regression-zero-time-clock-out-drops-lr-note ()
  "His clock-out of a clock paused within a minute of its start: org removes
the zero-time line, and the `- lr' note under it goes too, with the drawer
it leaves empty."
  (let ((t0 (lr-track-clock-test--minute -3600.0))
        (org-clock-out-remove-zero-time-clocks t))
    (lr-track-clock-test--clocked (t0 "* avey\n")
      (save-excursion
        (goto-char org-clock-marker)
        (end-of-line)
        (insert "\n- lr declared: from 09:00, said with SPC d 1"))
      (lr-track-clock-test--pause (+ t0 30) 'idle)
      (lr-track-clock-test--capturing msgs
        (org-clock-out))
      (should-not (org-clocking-p))
      (should-not (string-match-p "- lr " (buffer-string)))
      (should-not (string-match-p ":LOGBOOK:" (buffer-string))))))

(ert-deftest lr-track-regression-stale-buffer-never-prompts ()
  "The clocked file changes on disk under its unmodified buffer.  The tick
must not edit or save it (each would ask, from a timer): no advance, the
pause is recorded but not saved, and no question is reached."
  (let ((t0 (lr-track-clock-test--minute 0.0))
        (lr-track-autosave-clock t)
        (lr-track--state nil)
        (asked nil))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (lr-track-clock-test--with-now (+ t0 700)
        (lr-track--maybe-autosave buf t)
        (should-not (buffer-modified-p buf))
        ;; another writer changes the file
        (let ((other (concat (lr-track-clock-test--disk file) "* Another\n")))
          (with-temp-buffer
            (insert other)
            (let ((write-region-inhibit-fsync t))
              (write-region (point-min) (point-max) file nil 'quiet)))
          (set-file-times file (time-add (current-time) 60))
          (should-not (verify-visited-file-modtime buf))
          (cl-letf (((symbol-function 'ask-user-about-supersession-threat)
                     (lambda (&rest a) (push (cons 'supersession a) asked)))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (&rest a) (push (cons 'yes-or-no-p a) asked) t))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest a) (push (cons 'y-or-n-p a) asked) t)))
            (setq now (+ t0 1300))
            (let ((lr-track--presence
                   (lr-track-clock-test--p 'here :last-input (+ t0 1290))))
              (lr-track--tick-live-clock))
            (setq now (+ t0 2400))
            (let ((lr-track--presence
                   (lr-track-clock-test--p 'away :last-input (+ t0 1300)
                                           :away-from (+ t0 1300)
                                           :away-kind 'locked
                                           :prev-time (+ t0 2390))))
              (lr-track--tick-live-clock))
            (lr-track--on-kill-emacs))
          (should-not asked)
          (should-not (buffer-modified-p buf))
          (should-not (lr-track--running-line-end))
          (should (= (lr-track--paused-at) (+ t0 1300)))
          (should (equal other (lr-track-clock-test--disk file)))
          ;; accept the disk's time, so the cleanup's cancel asks nothing
          (with-current-buffer buf (set-visited-file-modtime)))))))

(ert-deftest lr-track-regression-kill-save-keeps-his-no ()
  "He answered no to saving the clocked file when quitting: kill-emacs must
not save his unsaved text behind his back.  Only the clock line's own
advance is ever flushed there."
  (let ((t0 (lr-track-clock-test--minute 0.0))
        (lr-track-autosave-clock t)
        (lr-track--state nil))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (lr-track-clock-test--with-now (+ t0 700)
        (lr-track--maybe-autosave buf t)
        (setq now (+ t0 760))
        (should (lr-track--advance-clock-line (+ t0 720)))
        (save-excursion (goto-char (point-max)) (insert "A paragraph he dropped.\n"))
        (lr-track--on-kill-emacs)
        (should (buffer-modified-p buf))
        (should-not (string-match-p "he dropped"
                                    (lr-track-clock-test--disk file)))))))

(ert-deftest lr-track-regression-recover-never-asks-unfocused ()
  "Crash recovery runs from an idle timer: with no Emacs frame focused it
leaves the clock alone instead of reading a key nobody sees."
  (let ((reads nil)
        (noninteractive nil))
    (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) nil))
              ((symbol-function 'read-char-choice)
               (lambda (&rest a) (push a reads) ?y)))
      (should (eq ?n (lr-track--recover-prompt "Task" (current-time))))
      (should-not reads))))

(ert-deftest lr-track-regression-sense-never-reads-buffer-mode ()
  "Spec 3.1: the buffer he looks at is never read.  The classifier gets no
major mode, so a PDF in front of him tells it nothing."
  (let ((modes nil)
        (orig (symbol-function 'lr-track--classify)))
    (with-temp-buffer
      (setq major-mode 'pdf-view-mode)
      (save-window-excursion
        (set-window-buffer (selected-window) (current-buffer))
        (cl-letf (((symbol-function 'lr-track--classify)
                   (lambda (e s gap focused mode interval)
                     (push mode modes)
                     (funcall orig e s gap focused mode interval)))
                  ((symbol-function 'lr-track-emacs-idle-seconds) (lambda () 1000.0)))
          (lr-track-attention-state))))
    (should (equal modes '(nil)))))

(ert-deftest lr-track-regression-blind-stretch-pauses-machine-clock ()
  "The probe fails for hours (no ioreg, garbage, a hung probe); then he types
in Emacs.  Nothing vouches for the stretch: presence starts an away at his
last input and dates the return at the new input, so the machine clock
pauses there instead of bridging hours.  A late tick that spans the stretch
with no failure (App Nap, a blocked Emacs) vouches for it no better, and
pauses the same way (round 3); samples at the tick rate bridge as before."
  (let ((t0 (lr-track-clock-test--minute -14400.0))
        (lr-track-autosave-clock nil)
        (lr-track--state nil))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (lr-track-clock-test--fresh-presence
        ;; presence sees him from the clock-in on
        (lr-track-clock-test--feed t0 0.0)
        (lr-track-clock-test--feed (+ t0 600) 10.0)
        (lr-track-clock-test--with-now (+ t0 600) (lr-track--tick-live-clock))
        (should (= (lr-track--running-line-end) (+ t0 540)))
        ;; the probe fails from here on
        (lr-track-clock-test--with-now (+ t0 660)
          (lr-track--sense-note-failure 'parse))
        (should lr-track--blind-since)
        (lr-track--step-sample (list :time (+ t0 660.0) :idle 'unknown
                                     :locked 'unknown :wake nil))
        ;; three hours on, he types in Emacs (the synthesized sample)
        (lr-track-clock-test--feed (+ t0 11400) 3.0)
        (should-not lr-track--blind-since)
        (should (eq (plist-get lr-track--presence :mode) 'here))
        (let ((gone (plist-get lr-track--presence :last-away)))
          (should (= (plist-get gone :from) (+ t0 590)))
          (should (= (plist-get gone :to) (+ t0 11396))))
        (lr-track-clock-test--with-now (+ t0 11400) (lr-track--tick-live-clock))
        (should (= (lr-track--paused-at) (+ t0 590)))
        (should (= (lr-track--running-line-end) (+ t0 540))))))
  (let ((t0 (lr-track-clock-test--minute -14400.0))
        (lr-track-autosave-clock nil)
        (lr-track--state nil))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (lr-track-clock-test--fresh-presence
        (lr-track-clock-test--feed t0 0.0)
        (lr-track-clock-test--feed (+ t0 600) 10.0)
        (lr-track-clock-test--feed (+ t0 2400) 3.0)
        (lr-track-clock-test--with-now (+ t0 2400) (lr-track--tick-live-clock))
        (should (= (lr-track--paused-at) (+ t0 590)))
        (should (= (lr-track--running-line-end) (+ t0 540))))))
  (let ((t0 (lr-track-clock-test--minute -14400.0))
        (lr-track-autosave-clock nil)
        (lr-track--state nil))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (lr-track-clock-test--fresh-presence
        (lr-track-clock-test--feed t0 0.0)
        (cl-loop for x from 30 to 2400 by 30
                 do (lr-track-clock-test--feed (+ t0 x) 3.0))
        (lr-track-clock-test--with-now (+ t0 2400) (lr-track--tick-live-clock))
        (should-not (lr-track--paused-at))
        (should (= (lr-track--running-line-end) (+ t0 2340)))))))

(ert-deftest lr-track-regression-header-error-never-backs-off ()
  "The header is display only: a time.org that cannot be read is logged,
and the tick keeps its pace (presence and the clock line need it)."
  (let ((lr-track--phase-failures nil))
    (cl-letf (((symbol-function 'lr-track-ask-refresh-header)
               (lambda (&rest _) (error "File is not readable"))))
      (dotimes (_ 4)
        (should-not (lr-track--run-phase 'header))))
    (should (eql 0 (alist-get 'header lr-track--phase-failures)))))

(ert-deftest lr-track-regression-one-state-write-per-tick-while-typing ()
  "While he types only his last input moves: the presence phase keeps the
compact copy in memory for the heartbeat to flush, and writes state.eld at
once only when more than that changed."
  (let* ((base 3.5e9)
         (writes 0)
         (lr-track--presence (lr-track--presence-init))
         (lr-track--presence-stepped nil)
         (lr-track--state nil))
    (cl-letf (((symbol-function 'lr-track--write-state)
               (lambda (&rest _) (cl-incf writes))))
      (setq lr-track--last-sample (list :time base :idle 2.0 :locked nil :wake nil))
      (lr-track--tick-presence)
      (should (= writes 1))                ; unknown to here
      (dotimes (i 5)
        (setq lr-track--last-sample (list :time (+ base (* 30 (1+ i))) :idle 2.0
                                          :locked nil :wake nil))
        (lr-track--tick-presence))
      (should (= writes 1))
      (should (= (plist-get (plist-get lr-track--state :presence) :last-input)
                 (+ base 148.0)))
      (setq lr-track--last-sample (list :time (+ base 1200) :idle 1000.0
                                        :locked nil :wake nil))
      (lr-track--tick-presence)
      (should (= writes 2)))))                ; here to away

(ert-deftest lr-track-regression-config-s0-line ()
  "Deploy is by `load' over a live session, where a defcustom keeps its old
value: config.el sets the S0 values itself, after requiring lr-track."
  (let* ((forms (lr-track-clock-test--source-forms
                 (expand-file-name "config.el" lr-track-clock-test--root)))
         (req (cl-position '(require 'lr-track) forms :test #'equal))
         (s0 (cl-position-if
              (lambda (f)
                (and (eq (car-safe f) 'setq)
                     (equal (cl-loop for (k v) on (cdr f) by #'cddr
                                     when (memq k '(lr-track-checkin-on-return
                                                    lr-track-checkin-on-startup
                                                    lr-track-daily-banner-budget
                                                    lr-track-elsewhere-checkin-seconds))
                                     collect (cons k v))
                            '((lr-track-checkin-on-return . nil)
                              (lr-track-checkin-on-startup . nil)
                              (lr-track-daily-banner-budget . 0)
                              (lr-track-elsewhere-checkin-seconds . nil)))))
              forms)))
    (should req)
    (should s0)
    (should (> s0 req))))


;;;; regressions from the round 2 review

(ert-deftest lr-track-regression-clock-out-after-wake-ends-at-pause ()
  "The Mac wakes after a night.  The tick that wakes spawns the probe, and
its sample lands after that tick.  He clocks out, or switches, before the
next tick: the clock is settled first, so the line ends where he left, not
after the night."
  (dolist (how '(clock-out switch))
    (let ((t0 (lr-track-clock-test--minute -43200.0))
          (lr-track-autosave-clock nil)
          (lr-track--state nil))
      (lr-track-clock-test--clocked
          (t0 "* TODO Write report\n* TODO Read paper\n")
        (lr-track-clock-test--fresh-presence
          (lr-track-clock-test--feed t0 0.0 nil (- t0 86400.0))
          ;; he works an hour: a sample every tick
          (cl-loop for x from 30 to 3570 by 30
                   do (lr-track-clock-test--feed (+ t0 x) 0.0 nil (- t0 86400.0)))
          (lr-track-clock-test--feed (+ t0 3599) 0.0 nil (- t0 86400.0))
          (lr-track-clock-test--with-now (+ t0 3600)
            (lr-track--tick-live-clock))
          (should (= (lr-track--running-line-end) (+ t0 3540)))
          ;; the lid closes; 8 h later the wake sample arrives, unstepped
          (let ((w (+ t0 3600 28800.0)))
            (setq lr-track--last-sample
                  (list :time (+ w 1.0) :idle 1.0 :locked nil :wake w))
            (lr-track-clock-test--with-now (+ w 15)
              (lr-track-clock-test--capturing msgs
                (if (eq how 'clock-out)
                    (org-clock-out)
                  (goto-char (point-min))
                  (re-search-forward "^\\* TODO Read paper")
                  (beginning-of-line)
                  (org-clock-in))
                (should (member (format "Clocked out of Write report at %s, where it paused."
                                        (lr-track--ts-hm (+ t0 3599)))
                                msgs)))))
          (let ((closed (seq-find (lambda (l) (string-match-p "--" l))
                                  (lr-track-clock-test--clock-lines buf))))
            (should (equal (list how t)
                           (list how (and (string-match-p
                                           (lr-track-clock-test--ends-at (+ t0 3540))
                                           closed)
                                          t))))
            (should (string-match-p " =>  0:59\\'" closed)))
          (should (string-match-p "- lr ended: where it paused" (buffer-string))))))))

(ert-deftest lr-track-regression-pause-never-before-line-end ()
  "A pause whose target is before the line's current end (a glance merged
into an earlier away, an away found late, a lowered max, activity on an
away stream) pauses where the line reads.  His clock-out then keeps the
minutes he worked; it never moves the end back or deletes the line."
  (let ((s lr-track-clock-test--s))
    ;; the glance: the away starts 41 min before the line's start
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :end (+ s 120))
                    (lr-track-clock-test--p 'away :last-input (+ s 150)
                                            :away-from (- s 2460) :away-kind 'idle
                                            :prev-time (+ s 1200))
                    (+ s 1210))
                   (list :advance-to nil :paused-at (+ s 120) :why 'idle)))
    ;; an away no step saw, begun before the line's end
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :end (+ s 600))
                    (lr-track-clock-test--p 'here :last-input (+ s 2010)
                                            :prev-time (+ s 2010)
                                            :return (+ s 2000)
                                            :last-away (list :from (+ s 300)
                                                             :to (+ s 2000)
                                                             :kind 'locked))
                    (+ s 2020))
                   (list :advance-to nil :paused-at (+ s 600) :why 'locked)))
    ;; TRACK_MAX lowered below what the line already holds
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :end (+ s 1200) :max 600.0)
                    (lr-track-clock-test--p 'here :last-input (+ s 1300))
                    (+ s 1310))
                   (list :advance-to nil :paused-at (+ s 1200) :why 'max)))
    ;; an away stream: activity since a return before the line's end
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :end (+ s 700) :place 'away)
                    (lr-track-clock-test--p 'here :last-input (+ s 1100)
                                            :return (+ s 100))
                    (+ s 1110))
                   (list :advance-to nil :paused-at (+ s 700) :why 'activity)))
    ;; and over random states, a pause is never before the end
    (random "lr-track-pause-never-before-end")
    (dotimes (_ 500)
      (let* ((end (+ s 60 (random 7200)))
             (mode (nth (random 2) '(here away)))
             (r (lr-track-clock-test--step
                 (lr-track-clock-test--clock
                  s :end end :place (nth (random 2) '(machine away))
                  :max (nth (random 3) '(600.0 3600.0 36000.0)))
                 (lr-track-clock-test--p
                  mode :last-input (+ s (random 9000))
                  :prev-time (+ end 600 (random 60))
                  :away-from (and (eq mode 'away) (- end (random 9000)))
                  :away-kind 'idle
                  :return (and (zerop (random 2)) (+ s (random 7200)))
                  :last-away (and (zerop (random 2))
                                  (list :from (- end (random 3600))
                                        :to (+ end 1 (random 3600))
                                        :kind 'idle)))
                 (+ end 9600))))
        (when (plist-get r :paused-at)
          (should (>= (plist-get r :paused-at) end))))))
  ;; the real line: 2 min typed, then the glance merge pauses it
  (let ((t0 (lr-track-clock-test--minute -3600.0))
        (lr-track-autosave-clock nil)
        (lr-track--state nil)
        (org-clock-out-remove-zero-time-clocks t))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (lr-track-clock-test--with-now (+ t0 150)
        (let ((lr-track--presence
               (lr-track-clock-test--p 'here :last-input (+ t0 125)
                                       :first-time (- t0 7200))))
          (lr-track--tick-live-clock))
        (should (= (lr-track--running-line-end) (+ t0 120)))
        (setq now (+ t0 1200))
        (let ((lr-track--presence
               (lr-track-clock-test--p 'away :last-input (+ t0 125)
                                       :first-time (- t0 7200)
                                       :away-from (- t0 2460) :away-kind 'idle
                                       :prev-time (+ t0 1190))))
          (lr-track--tick-live-clock))
        (should (= (lr-track--paused-at) (+ t0 120)))
        (lr-track-clock-test--capturing _msgs
          (org-clock-out)))
      (let ((lines (lr-track-clock-test--clock-lines buf)))
        (should (= 1 (length lines)))
        (should (string-match-p " =>  0:02\\'" (car lines))))
      (should (string-match-p "- lr ended: where it paused" (buffer-string))))))

(ert-deftest lr-track-regression-cancel-and-zero-clock-out-take-lr-note ()
  "His own cancel (SPC c c, agenda X) and his own clock-out that org removes
as zero time, on a live line an answer started: the `- lr' note under the
line goes with it, and so does a LOGBOOK left empty.  A clock-in later on
the same heading never finds a stale note under its new line."
  (let ((t0 (lr-track-clock-test--minute -3600.0))
        (org-clock-out-remove-zero-time-clocks t))
    (dolist (how '(cancel clock-out explicit-at))
      (lr-track-clock-test--clocked (t0 "* avey\n")
        (save-excursion
          (goto-char org-clock-marker)
          (end-of-line)
          (insert "\n- lr declared: from 09:00, said with SPC d 1"))
        (lr-track-clock-test--capturing _msgs
          (pcase how
            ('cancel (org-clock-cancel))
            ;; the same minute: org's clock-out time is the start's
            ('clock-out
             (cl-letf (((symbol-function 'org-current-time)
                        (lambda (&rest _) (seconds-to-time (+ t0 20)))))
               (org-clock-out)))
            ('explicit-at (org-clock-out nil nil (seconds-to-time (+ t0 30))))))
        (should-not (org-clocking-p))
        (should (equal (list how nil)
                       (list how (string-match-p "- lr " (buffer-string)))))
        (should (equal (list how nil)
                       (list how (string-match-p ":LOGBOOK:" (buffer-string)))))))))

(ert-deftest lr-track-regression-write-protected-file-never-prompts ()
  "The clocked file is write-protected on disk (a chmod, or the Finder lock,
which leave its modification time alone).  The tick's autosave, the forced
save at a pause and the kill-emacs save skip it and log it: none of them
opens a question from a timer."
  (skip-unless (not (zerop (user-uid))))
  (let ((t0 (lr-track-clock-test--minute -1200.0))
        (lr-track-autosave-clock t)
        (lr-track--state nil)
        (asked nil))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (let ((save-silently t) (inhibit-message t)) (save-buffer))
      (set-file-modes file #o444)
      (should-not (file-writable-p file))
      (unwind-protect
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest a) (push (cons 'yes-or-no-p a) asked) nil))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest a) (push (cons 'y-or-n-p a) asked) nil))
                    ((symbol-function 'ask-user-about-supersession-threat)
                     (lambda (&rest a) (push (cons 'supersession a) asked))))
            (with-current-buffer (get-buffer-create " *lr-track-log*")
              (erase-buffer))
            (lr-track-clock-test--with-now (+ t0 700)
              ;; a here advance, with the autosave due
              (let ((lr-track--presence
                     (lr-track-clock-test--p 'here :last-input (+ t0 690))))
                (lr-track--tick-live-clock))
              (should (= (lr-track--running-line-end) (+ t0 660)))
              ;; a pause, which forces a save
              (setq now (+ t0 1900))
              (let ((lr-track--presence
                     (lr-track-clock-test--p 'away :last-input (+ t0 800)
                                             :away-from (+ t0 800)
                                             :away-kind 'locked
                                             :prev-time (+ t0 1890))))
                (lr-track--tick-live-clock))
              (should (= (lr-track--paused-at) (+ t0 800)))
              (lr-track--on-kill-emacs)))
        (set-file-modes file #o644))
      (should-not asked)
      (should (buffer-modified-p buf))
      (should (string-match-p "save-write-protected"
                              (with-current-buffer " *lr-track-log*"
                                (buffer-string)))))))

(ert-deftest lr-track-regression-deploy-mid-clock-starts-presence-without-moving-the-line ()
  "Spec v3 I13.  A machine clock runs, its line at T0+60m.  Then 2 h pass that
no presence sample sees: the mode was off (SPC d t), or the new code was
deployed by `load' over the running clock.  Presence starts fresh, and its
first sample says he is here: the line must not cross the 2 h nobody saw.
It pauses where it reads, why `unknown'.  A first sample within 3 min of
the line's end vouches for nothing missing, and the clock earns as usual.
A line still open is exempt (round 3): a pause at its start would delete it."
  (let ((t0 (lr-track-clock-test--minute -14400.0))
        (lr-track-autosave-clock nil)
        (lr-track--state nil))
    (lr-track-clock-test--clocked (t0 "* avey\n")
      (lr-track-clock-test--with-now (+ t0 3630)
        (let ((lr-track--presence
               (lr-track-clock-test--p 'here :last-input (+ t0 3600)
                                       :prev-time (+ t0 3600) :first-time t0)))
          (lr-track--tick-live-clock))
        (should (= (lr-track--running-line-end) (+ t0 3600)))
        (setq now (+ t0 (* 3 3600) 30))
        (lr-track-clock-test--fresh-presence
          (setq lr-track--last-sample
                (list :time now :idle 5.0 :locked nil :wake nil))
          (lr-track--tick-presence)
          (lr-track--tick-live-clock)
          (should (= (lr-track--running-line-end) (+ t0 3600)))
          (should (= (lr-track--paused-at) (+ t0 3600)))
          (should (eq (plist-get lr-track--clock-pause :why) 'unknown))))))
  (let ((s lr-track-clock-test--s))
    ;; an open line, started 10 min before presence's first sample: his own
    ;; clock-in, or an open line org resumed at his word.  A pause at its
    ;; start would delete it at his clock-out (round 3), so it earns as usual
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s)
                    (lr-track-clock-test--p 'here :last-input (+ s 595)
                                            :prev-time (+ s 600)
                                            :first-time (+ s 600))
                    (+ s 600))
                   (list :advance-to (+ s 595) :paused-at nil :why nil)))
    ;; within 3 min of the line's end: no gap, the clock earns
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :end (+ s 600))
                    (lr-track-clock-test--p 'here :last-input (+ s 770)
                                            :prev-time (+ s 775)
                                            :first-time (+ s 775))
                    (+ s 780))
                   (list :advance-to (+ s 770) :paused-at nil :why nil)))))

(ert-deftest lr-track-regression-probe-buffers-never-leak ()
  "Stage 1 probes every tick.  A probe the watchdog kills, and a spawn that
fails, leave no ' *lr-track-probe*' buffer behind."
  (skip-unless (eq system-type 'darwin))
  (let ((lr-track--probe-command '("/bin/sh" "-c" "sleep 20"))
        (lr-track--sense-timeout-seconds 60.0)
        (lr-track--sys-idle-process nil)
        (lr-track--sys-idle-watchdog nil)
        (lr-track--sense-failures 0)
        (lr-track--sys-idle-value 'unknown)
        (lr-track--sys-idle-stamp nil)
        (lr-track--blind-since nil))
    (cl-flet ((probe-buffers ()
                (seq-count (lambda (b) (string-prefix-p " *lr-track-probe*"
                                                        (buffer-name b)))
                           (buffer-list))))
      (let ((before (probe-buffers)))
        (dotimes (_ 3)
          (lr-track--sys-idle-probe)
          (should (process-live-p lr-track--sys-idle-process))
          ;; the watchdog, as its timer would run it
          (cancel-timer lr-track--sys-idle-watchdog)
          (lr-track--sys-idle-watchdog-fire))
        (should (= 3 lr-track--sense-failures))
        (should (= before (probe-buffers)))
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest _) (signal 'file-error '("fork" "no more processes")))))
          (dotimes (_ 3) (lr-track--sys-idle-probe)))
        (should (= 6 lr-track--sense-failures))
        (should (= before (probe-buffers)))))))

(defmacro lr-track-clock-test--mode-state (&rest body)
  "Run BODY with the globals `lr-track--enable' and teardown touch bound."
  (declare (indent 0) (debug t))
  `(let ((lr-track--generation lr-track--generation)
         (lr-track--timer nil)
         (lr-track--presence (lr-track--presence-init))
         (lr-track--presence-stepped nil)
         (lr-track--last-sample nil)
         (lr-track--blind-since nil)
         (lr-track--state nil)
         (lr-track--state-prev nil)
         (lr-track--recovered nil)
         (lr-track--stable-state 'unknown)
         (lr-track--stable-since nil)
         (lr-track--last-state nil)
         (lr-track--episode nil)
         (lr-track--incidents nil)
         (lr-track--away-pending nil)
         (lr-track--snooze-until nil)
         (lr-track--last-close nil)
         (lr-track--modeline-cache "")
         (lr-track--focused-p t)
         (lr-track--unfocused-since nil)
         (lr-track--sys-idle-process nil)
         (lr-track--sys-idle-watchdog nil))
     (cl-letf (((symbol-function 'lr-track--schedule) #'ignore)
               ((symbol-function 'lr-track--install-seams) #'ignore)
               ((symbol-function 'lr-track--remove-seams) #'ignore))
       ,@body)))

(defun lr-track-clock-test--our-timers (fn)
  "Timers, idle or not, whose function is FN."
  (seq-filter (lambda (tm) (eq (timer--function tm) fn))
              (append timer-list timer-idle-list)))

(ert-deftest lr-track-regression-recover-timer-reaped ()
  "Each enable arms the crash-recovery idle timer through a named function,
so the next enable and the teardown cancel it: none is left to run
`lr-track--recover' (and its key read) after the mode is off."
  (lr-track-clock-test--mode-state
    (lr-track--enable)
    (lr-track--enable)
    (lr-track--enable)
    (should (= 1 (length (lr-track-clock-test--our-timers
                          #'lr-track--recover-quietly))))
    (lr-track--teardown)
    (should-not (lr-track-clock-test--our-timers #'lr-track--recover-quietly))
    (should-not (seq-some (lambda (tm)
                            (let ((f (timer--function tm)))
                              (and (not (symbolp f))
                                   (string-match-p "lr-track--recover"
                                                   (format "%S" f)))))
                          (append timer-list timer-idle-list)))))

(ert-deftest lr-track-regression-s0-old-prompt-paths-stay-silent ()
  "S0 is behaviour, not four values: with the defaults, every path of the old
coach stays silent.  A return after an hour away (focus and the confirm),
the tick's return check, the nudge phase under each old state with a clock
running, the activity phase, and enable after a previous session: no check-
in, no auto clock-out, no read and no banner, and enable arms only the
crash-recovery timer."
  (let ((calls nil)
        (focused t)
        (noninteractive nil))
    (cl-letf* (((symbol-function 'lr-track--frame-focused-now-p) (lambda () focused))
               ((symbol-function 'lr-track--clocking-p) (lambda () t))
               ((symbol-function 'lr-track--clock-task) (lambda () "avey"))
               ((symbol-function 'lr-track--clock-elapsed) (lambda () 99999.0))
               ((symbol-function 'run-at-time)
                (lambda (secs repeat fn &rest args)
                  ;; the focus path's confirm runs at once; a timer of ours
                  ;; (a banner, a prompt) is recorded; Emacs's own pass
                  (cond
                   ((eq fn #'lr-track--confirm-return)
                    (funcall fn) (timer-create))
                   ((and (symbolp fn)
                         (not (string-prefix-p "lr-track" (symbol-name fn))))
                    (timer-create))
                   (t (push (list 'run-at-time secs repeat fn) calls)
                      (timer-create))))))
      (dolist (f '(lr-track-checkin lr-track--trigger-checkin
                   lr-track--open-checkin-if-seen lr-track--autoout
                   lr-track--close-running-clock lr-track--activity-log
                   lr-track--charge read-char-choice read-string completing-read
                   y-or-n-p yes-or-no-p read-key read-from-minibuffer))
        (advice-add f :around (lambda (_orig &rest _) (push f calls) nil)
                    '((name . lr-track-clock-test--s0-record))))
      (unwind-protect
          (let ((lr-track--focused-p t)
                (lr-track--unfocused-since nil)
                (lr-track--stable-state 'unknown)
                (lr-track--tick-attention '(:state unknown))
                (lr-track--incidents nil)
                (lr-track--episode nil)
                (lr-track--snooze-until nil)
                (lr-track--away-pending nil)
                (lr-track--cooldowns nil))
            (setq focused nil) (lr-track--focus-change)
            (setq lr-track--unfocused-since (- (float-time) 3600))
            (setq focused t) (lr-track--focus-change)
            (lr-track--confirm-return)
            (lr-track--maybe-prompt-on-return)
            (dolist (st '(away slept elsewhere engaged))
              (setq lr-track--stable-state st
                    lr-track--tick-attention (list :state st :gap 7200.0)
                    lr-track--incidents nil)
              (lr-track--nudge-check)
              (lr-track--nudge-check))
            (setq lr-track--stable-state 'away) (lr-track--tick-activity)
            (setq lr-track--stable-state 'engaged) (lr-track--tick-activity))
        (dolist (f '(lr-track-checkin lr-track--trigger-checkin
                     lr-track--open-checkin-if-seen lr-track--autoout
                     lr-track--close-running-clock lr-track--activity-log
                     lr-track--charge read-char-choice read-string completing-read
                     y-or-n-p yes-or-no-p read-key read-from-minibuffer))
          (advice-remove f 'lr-track-clock-test--s0-record))))
    (should-not calls))
  ;; enable after a previous session arms no startup check-in
  (lr-track-clock-test--mode-state
    ;; a copy: `append' shares its last list, which a new timer mutates
    (let ((before (append timer-list timer-idle-list nil)))
      (cl-letf (((symbol-function 'lr-track--read-state)
                 (lambda () (list :heartbeat (- (float-time) 7200) :clean t))))
        (lr-track--enable))
      (let ((new (seq-remove (lambda (tm) (memq tm before))
                             (append timer-list timer-idle-list))))
        (unwind-protect
            (should (equal (mapcar #'timer--function new)
                           '(lr-track--recover-quietly)))
          (mapc #'cancel-timer new))))))

;;;; regressions from the round 3 review

(defmacro lr-track-clock-test--resolving-answer (key &rest body)
  "Run BODY with org's dangling-clock resolution answered by KEY.
The prompt reads KEY, the minutes default, and the files are the current
buffer's file.  Messages are captured in `msgs'."
  (declare (indent 1) (debug t))
  `(let ((org-clock-resolve-expert t)
         (org-clock-in-resume t)
         (org-clock-out-remove-zero-time-clocks t)
         (org-clock-auto-clock-resolution 'when-no-clock-is-running))
     (cl-letf (((symbol-function 'read-char-exclusive) (lambda (&rest _) ,key))
               ((symbol-function 'read-number)
                (lambda (_prompt &optional default &rest _) default))
               ((symbol-function 'org-files-list)
                (lambda () (list (buffer-file-name)))))
       (lr-track-clock-test--capturing msgs
         ,@body))))

(ert-deftest lr-track-regression-org-resolution-keeps-dangling-line ()
  "Org resolves a line a previous session left open by clocking it out with
that line stood in for the running clock (`org-with-clock').  His K says
keep all: lr-track must neither settle nor pause that line, or its 3 h are
deleted as zero time.  Nor may it clear the real clock's pause, or name it
in an echo about a clock that did not end."
  (let* ((now (lr-track-clock-test--minute 0.0))
         (dstart (- now 10800.0))
         (lr-track-autosave-clock nil)
         (lr-track--state nil))
    ;; his SPC c i on another task runs the resolution first
    (lr-track-clock-test--fresh-presence
      (lr-track-clock-test--feed (- now 10) 2.0)
      (lr-track-clock-test--resolving-answer ?K
        (lr-track-clock-test--clocked
            (now (format "* TODO Write the report\n:LOGBOOK:\nCLOCK: %s\n:END:\n* TODO Other task\n"
                         (lr-track-clock-test--stamp dstart))
                 "^\\* TODO Other")
          (should (seq-find
                   (lambda (l)
                     (string-match-p
                      (concat "\\`CLOCK: " (regexp-quote (lr-track-clock-test--stamp dstart))
                              "--.* =>  3:0[01]\\'")
                      l))
                   (lr-track-clock-test--clock-lines buf)))
          (should-not (seq-find (lambda (m) (string-match-p "no time recorded" m))
                                msgs))
          (should-not lr-track--clock-pause)))))
  ;; his real clock is paused when he runs M-x org-resolve-clocks
  (let* ((now (lr-track-clock-test--minute 0.0))
         (t0 (- now 7200.0))
         (dstart (- now 108000.0))
         (lr-track-autosave-clock nil)
         (lr-track--state nil)
         (org-clock-auto-clock-resolution nil))
    (lr-track-clock-test--fresh-presence
      (lr-track-clock-test--feed (- now 10) 2.0)
      (lr-track-clock-test--clocked
          (t0 (format "* TODO avey\n* TODO old task\n:LOGBOOK:\nCLOCK: %s\n:END:\n"
                      (lr-track-clock-test--stamp dstart)))
        (lr-track--advance-clock-line (+ t0 3600))
        (lr-track-clock-test--pause (+ t0 3600) 'asleep)
        (lr-track-clock-test--resolving-answer ?K
          (org-resolve-clocks t)
          (should (equal (lr-track--paused-at) (+ t0 3600)))
          (should (org-clocking-p))
          (should-not (seq-find (lambda (m) (string-match-p "no time recorded" m))
                                msgs))
          (should (seq-find (lambda (l) (string-match-p
                                         (concat "\\`CLOCK: "
                                                 (regexp-quote (lr-track-clock-test--stamp dstart))
                                                 "--.* => 30:0[01]\\'")
                                         l))
                            (lr-track-clock-test--clock-lines buf))))))))

(ert-deftest lr-track-regression-resumed-open-line-earns ()
  "Doom sets `org-clock-in-resume': his SPC c i on a heading with an open
line from a previous session resumes that line, 2 h old, while presence has
seen him only now.  No sample saw those 2 h, but the line is open: his own
clock-in, at his word.  It must not pause at its own start, or the 30 min he
then types are never earned and his SPC c o deletes the line."
  (let* ((t0 (lr-track-clock-test--minute 0.0))
         (dstart (- t0 7200.0))
         (lr-track-autosave-clock nil)
         (lr-track--state nil)
         (org-clock-in-resume t)
         (org-clock-auto-clock-resolution nil)
         (org-clock-out-remove-zero-time-clocks t)
         (dir (file-name-as-directory (make-temp-file "lr-track-clk" t)))
         (file (expand-file-name "t.org" dir))
         (lr-track--clock-pause nil)
         (buf nil))
    (with-temp-file file
      (insert (format "* TODO Write the report\n:LOGBOOK:\nCLOCK: %s\nCLOCK: [2026-10-02 Fri 18:00]--[2026-10-02 Fri 19:00] =>  1:00\n:END:\n"
                      (lr-track-clock-test--stamp dstart))))
    (setq buf (find-file-noselect file))
    (unwind-protect
        (lr-track-clock-test--fresh-presence
          (lr-track-clock-test--feed (- t0 10) 2.0)
          (with-current-buffer buf
            (goto-char (point-min))
            (org-clock-in))
          (should (= (float-time org-clock-start-time) dstart))
          (dotimes (i 30)
            (let ((tk (+ t0 (* 60 (1+ i)))))
              (lr-track-clock-test--feed tk 3.0)
              (lr-track-clock-test--with-now tk (lr-track--tick-live-clock))))
          (should-not (lr-track--paused-at))
          (should (>= (lr-track--running-line-end) (+ t0 1740)))
          (lr-track-clock-test--capturing msgs
            (with-current-buffer buf (org-clock-out))
            (should-not (seq-find (lambda (m) (string-match-p "no time recorded" m))
                                  msgs)))
          (should (seq-find (lambda (l)
                              (string-match-p
                               (concat "\\`CLOCK: " (regexp-quote (lr-track-clock-test--stamp dstart))
                                       "--.* =>  2:")
                               l))
                            (lr-track-clock-test--clock-lines buf))))
      (when (org-clocking-p)
        (let ((lr-track--internal t)) (ignore-errors (org-clock-cancel))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest lr-track-regression-reenable-keeps-current-presence ()
  "SPC d t twice while he works mid-clock (I13 asks not to, but the key is
his).  Presence whose newest sample is under 3 min old still vouches for
now, so it is kept: the line is not left unseen, and the clock earns the
minutes he types after."
  (let ((t0 (lr-track-clock-test--minute -7200.0))
        (lr-track-autosave-clock nil)
        (lr-track--state nil)
        (lr-track--state-prev nil)
        (lr-track--last-tick nil)
        (lr-track--interval 30.0))
    (lr-track-clock-test--clocked (t0 "* TODO avey\n")
      (lr-track-clock-test--fresh-presence
        ;; 57 min of typing, then 2.5 min of reading
        (cl-loop for k from 0 to 114
                 do (lr-track-clock-test--feed (+ t0 (* 30 k)) 2.0))
        (cl-loop for k from 1 to 5
                 do (lr-track-clock-test--feed (+ t0 3420 (* 30 k)) (+ 2.0 (* 30 k))))
        (lr-track-clock-test--with-now (+ t0 3570) (lr-track--tick-live-clock))
        (let ((before (lr-track--running-line-end)))
          (unwind-protect
              (lr-track-clock-test--with-now (+ t0 3575)
                (lr-track-mode -1)
                (lr-track-mode 1))
            (lr-track-mode -1))
          ;; he types 10 more minutes
          (cl-loop for k from 0 to 20
                   do (let ((tk (+ t0 3580 (* 30 k))))
                        (lr-track-clock-test--feed tk 2.0)
                        (lr-track-clock-test--with-now tk
                          (lr-track--tick-live-clock))))
          (should-not (lr-track--paused-at))
          (should (> (lr-track--running-line-end) (+ before 540))))))))

(ert-deftest lr-track-regression-lr-note-grammar-only ()
  "Only lr-track's own `- lr TAG: ' notes go with a cancelled or zero-time
line.  An entry with no clock drawer puts the running line right above his
text, which may start with `- lr': it stays.  And a cancel org refuses
(the marker sits at the line's start, so org keeps the line) keeps the note."
  (let ((text (concat "* TODO Fix the tracker\n:PROPERTIES:\n:CLOCK_INTO_DRAWER: nil\n"
                      ":END:\n- lr track idea: presence only, never activity\n"
                      "- second point\n"))
        (lr-track-autosave-clock nil)
        (org-clock-out-remove-zero-time-clocks t))
    ;; his cancel
    (lr-track-clock-test--clocked ((lr-track-clock-test--minute 0.0) text)
      (org-clock-cancel)
      (should (string-match-p "^- lr track idea: presence only, never activity$"
                              (buffer-string))))
    ;; his clock-in, then an immediate clock-out: org removes the 0:00 line
    (lr-track-clock-test--fresh-presence
      (lr-track-clock-test--clocked ((lr-track-clock-test--minute 0.0) text)
        (lr-track-clock-test--capturing msgs (org-clock-out))
        (should-not (lr-track-clock-test--clock-lines buf))
        (should (string-match-p "^- lr track idea: presence only, never activity$"
                                (buffer-string))))))
  ;; the marker at the line's start: org keeps the line, so its note stays
  (lr-track-clock-test--clocked
      ((lr-track-clock-test--minute 0.0)
       "* TODO avey\n:LOGBOOK:\nCLOCK: [2026-10-04 Sun 09:00]--[2026-10-04 Sun 10:00] =>  1:00\n:END:\n")
    (save-excursion
      (goto-char org-clock-marker) (end-of-line)
      (insert "\n- lr now: from 10:05, your return"))
    (save-excursion
      (goto-char org-clock-marker) (beginning-of-line)
      (let ((txt (buffer-substring (point) (line-end-position))))
        (delete-region (point) (line-end-position))
        (insert txt)))
    (should (= 0 (save-excursion (goto-char org-clock-marker) (current-column))))
    (cl-letf (((symbol-function 'sit-for) #'ignore))
      (lr-track-clock-test--capturing msgs (org-clock-cancel)))
    (should (string-match-p "^CLOCK: \\[[^]]*\\]\n- lr now: from 10:05, your return$"
                            (buffer-string)))))

(ert-deftest lr-track-regression-save-no-file-never-prompts ()
  "A clock in an org buffer that visits no file: lr-track never saves it,
since `save-buffer' would ask for a file name from the tick, from y or from
his clock-out.  The line stays in the buffer, and the skip is logged."
  (let* ((t0 (lr-track-clock-test--minute -600.0))
         (calls nil)
         (lr-track-autosave-clock t)
         (lr-track--state nil)
         (lr-track--clock-pause nil)
         (buf (get-buffer-create "*lr-track no-file org*")))
    (unwind-protect
        (cl-letf (((symbol-function 'read-file-name)
                   (lambda (&rest args) (push args calls) (error "Prompted"))))
          (lr-track-clock-test--fresh-presence
            (with-current-buffer buf
              (org-mode)
              (insert "* TODO jot\n")
              (goto-char (point-min))
              (let ((org-clock-auto-clock-resolution nil))
                (org-clock-in nil (seconds-to-time t0))))
            (lr-track-clock-test--feed (+ t0 590) 2.0)
            (with-current-buffer buf (set-buffer-modified-p t))
            ;; the tick
            (lr-track-clock-test--with-now (+ t0 600) (lr-track--tick-live-clock))
            (should (= (lr-track--running-line-end) (+ t0 540)))
            (lr-track--save-buffer buf)
            ;; his clock-out
            (lr-track-clock-test--with-now (+ t0 610)
              (with-current-buffer buf (org-clock-out)))
            (should-not calls)
            (should (buffer-modified-p buf))
            (should (string-match-p "save-no-file"
                                    (with-current-buffer (get-buffer-create
                                                          lr-track--log-buffer)
                                      (buffer-string))))))
      (when (org-clocking-p)
        (let ((lr-track--internal t)) (ignore-errors (org-clock-cancel))))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf))))

(ert-deftest lr-track-regression-late-tick-spanning-away-pauses ()
  "He leaves at 15:00 with the Mac unlocked and awake, and is back at 15:25.
App Nap holds the tick: the next sample comes at 15:25:30, with his input
20 s before it.  HID idle dates only that last input, so the 25 min would
read as continuous input and be credited.  A sample gap of 15 min or more
with no wake in it is as blind as a failing probe: the clock pauses at 15:00.
A wake in the gap is a sleep, which the step itself dates at the wake."
  (dolist (wake '(nil t))
    (let ((t0 (lr-track-clock-test--minute -7200.0))
          (lr-track-autosave-clock nil)
          (lr-track--state nil))
      (lr-track-clock-test--clocked (t0 "* TODO avey\n")
        (lr-track-clock-test--fresh-presence
          (cl-loop for x from 0 to 3600 by 30
                   do (lr-track-clock-test--feed (+ t0 x) 0.0 nil (- t0 86400.0)))
          (lr-track-clock-test--feed (+ t0 3640) 40.0 nil (- t0 86400.0))
          (lr-track-clock-test--with-now (+ t0 3640) (lr-track--tick-live-clock))
          (should (= (lr-track--running-line-end) (+ t0 3600)))
          (lr-track-clock-test--feed (+ t0 5130) 20.0 nil
                                     (if wake (+ t0 5000.0) (- t0 86400.0)))
          (lr-track-clock-test--with-now (+ t0 5130) (lr-track--tick-live-clock))
          (let ((gone (plist-get lr-track--presence :last-away)))
            (should (equal (list wake (+ t0 3600))
                           (list wake (plist-get gone :from))))
            (should (equal (list wake (if wake (+ t0 5000) (+ t0 5109)))
                           (list wake (plist-get gone :to))))
            ;; no sample saw the stretch: `unseen', never a claimed away
            ;; (round 3.2); a wake is seen, so the sleep is `asleep'
            (should (eq (plist-get gone :kind) (if wake 'asleep 'unseen))))
          (should (equal (list wake (+ t0 3600))
                         (list wake (lr-track--paused-at))))
          (should (= (lr-track--running-line-end) (+ t0 3600))))))))

(ert-deftest lr-track-regression-s0-load-over-old-session ()
  "Deploy is by `load' over a running session whose S0 options still hold
their old defaults (t, t, 14): `defcustom' keeps live values, so the load
itself puts the new ones (nil, nil, 0), and S0 is silent with no setq.  An
option customized through Custom is his and stays."
  (let ((lr-track-checkin-on-return t)
        (lr-track-checkin-on-startup t)
        (lr-track-daily-banner-budget 14)
        (lr-track-elsewhere-checkin-seconds nil))
    (load (expand-file-name "modules/lr-track.el" lr-track-clock-test--root) nil t)
    (should (equal '(nil nil 0 nil)
                   (list lr-track-checkin-on-return lr-track-checkin-on-startup
                         lr-track-daily-banner-budget
                         lr-track-elsewhere-checkin-seconds))))
  (let ((lr-track-daily-banner-budget 14))
    (put 'lr-track-daily-banner-budget 'saved-value '(14))
    (unwind-protect
        (progn
          (load (expand-file-name "modules/lr-track.el" lr-track-clock-test--root)
                nil t)
          (should (= 14 lr-track-daily-banner-budget)))
      (put 'lr-track-daily-banner-budget 'saved-value nil))))

(ert-deftest lr-track-regression-away-place-ends-at-newest-sample ()
  "An away-place line (sleep, life, practice) advances while presence says
away, but only to what presence observed: its newest sample.  A blind probe
from 07:50 must not book the hours after it to sleep, and a return must not
overshoot by a tick."
  (let ((s lr-track-clock-test--s))
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :end (+ s 3600) :place 'away
                                                :max 57600.0)
                    (lr-track-clock-test--p 'away :last-input (- s 60)
                                            :away-from (- s 60) :away-kind 'idle
                                            :prev-time (+ s 7200))
                    (+ s 14400))
                   (list :advance-to (+ s 7200) :paused-at nil :why nil)))
    ;; the newest sample is now: the end follows it, as before
    (should (equal (lr-track-clock-test--step
                    (lr-track-clock-test--clock s :end (+ s 3600) :place 'away
                                                :max 57600.0)
                    (lr-track-clock-test--p 'away :last-input (- s 60)
                                            :away-from (- s 60) :away-kind 'idle
                                            :prev-time (+ s 7200))
                    (+ s 7200))
                   (list :advance-to (+ s 7200) :paused-at nil :why nil)))))

(ert-deftest lr-track-regression-modeline-badge-follows-presence ()
  "The badge beside org's paused clock reads presence, as the clock does:
nothing while he is here (whatever the old classifier says), away for the
minutes since his last input while he is away, slept while the Mac slept."
  (let* ((now 1791000000.0)
         (lr-track--modeline-cache "")
         (lr-track--stable-state 'away)
         (lr-track--stable-since (- now 600)))
    (cl-letf (((symbol-function 'lr-track--clocking-p) (lambda () t)))
      (lr-track-clock-test--with-now now
        (let ((lr-track--presence (lr-track-clock-test--p 'here :last-input (- now 30))))
          (lr-track--modeline-refresh)
          (should (equal "" lr-track--modeline-cache)))
        (let ((lr-track--presence (lr-track-clock-test--p 'away :away-from (- now 1589)
                                                          :away-kind 'locked)))
          (lr-track--modeline-refresh)
          (should (equal " o away 26m " lr-track--modeline-cache)))
        (let ((lr-track--presence (lr-track-clock-test--p 'away :away-from (- now 9000)
                                                          :away-kind 'asleep))
              (lr-track--stable-state 'engaged))
          (lr-track--modeline-refresh)
          (should (equal " z slept " lr-track--modeline-cache)))))))

(ert-deftest lr-track-regression-zero-time-echo-says-why ()
  "A clock that paused at its own start earns nothing, and his clock-out
says why from the pause: he stayed at the Mac (an away stream's activity),
nothing was seen after it started, or he left right after clocking in."
  (dolist (c '((activity . "you stayed at the Mac, so it earned nothing")
               (unknown . "nothing was seen after it started")
               (idle . "you left right after clocking in")))
    (let ((t0 (lr-track-clock-test--minute -1800.0))
          (lr-track-autosave-clock nil)
          (lr-track--state nil)
          (org-clock-out-remove-zero-time-clocks t))
      (lr-track-clock-test--clocked (t0 "* TODO life\n")
        (lr-track-clock-test--fresh-presence
          (lr-track-clock-test--pause t0 (car c))
          (lr-track-clock-test--capturing msgs
            (org-clock-out)
            (should (member (format "life: no time recorded (%s)." (cdr c))
                            msgs))))))))

(ert-deftest lr-track-regression-clock-in-holds-the-here-block ()
  "A clock-in while he is here marks his here-block as held, so presence
never merges it into the night as a glance (`lr-track--presence-leave')."
  (let* ((t0 (lr-track-clock-test--minute -600.0))
         (lr-track--presence (lr-track-clock-test--p 'here :return (- t0 30)
                                                     :last-input t0
                                                     :prev-time t0)))
    (lr-track-clock-test--clocked (t0 "* TODO study\n")
      (should (equal (plist-get lr-track--presence :held) (- t0 30))))))


;;;; regressions from the round 3.2 review

(ert-deftest lr-track-regression-clock-in-paused-task-keeps-its-heading ()
  "avey's only line came from SPC d 1, and he left within its minute: the
clock paused at T0 + 28 s, in the start's minute.  Back, his SPC c i with
point on avey's :LOGBOOK: line, or on its CLOCK line, clocks avey again.
The advice's clock-out removes the 0:00 line, its note and the drawer they
emptied, and point must not fall onto the next heading, study."
  (dolist (where '("^:LOGBOOK:" "^CLOCK: "))
    (let ((t0 (lr-track-clock-test--minute -3600.0))
          (lr-track-autosave-clock nil)
          (lr-track--state nil)
          (org-clock-out-remove-zero-time-clocks t))
      (lr-track-clock-test--clocked (t0 "* avey\n* study\n")
        (lr-track-clock-test--fresh-presence
          (save-excursion
            (goto-char org-clock-marker)
            (end-of-line)
            (insert "\n- lr declared: from "
                    (lr-track--ts-hm t0) ", said with SPC d 1"))
          (lr-track-clock-test--pause (+ t0 28) 'idle)
          (goto-char (point-min))
          (re-search-forward where)
          (beginning-of-line)
          (lr-track-clock-test--capturing msgs
            (call-interactively #'org-clock-in))
          (should (equal (list where "avey")
                         (list where (and (org-clocking-p)
                                          (substring-no-properties
                                           org-clock-current-task)))))
          (goto-char (point-min))
          (re-search-forward "^\\* study")
          (should (equal (list where "")
                         (list where (string-trim
                                      (buffer-substring-no-properties
                                       (point) (point-max))))))
          (should-not (string-match-p "- lr declared" (buffer-string))))))))

(ert-deftest lr-track-regression-edited-start-moves-the-clock ()
  "He moves the running line's start with S-up or S-down.  The live line is
closed in form, so org leaves `org-clock-start-time' alone; lr-track follows
the line's own start.  A pause before the new start is dropped (his clock-out
never writes an inverted line), one after it is kept for the new start, and
a live line moved earlier goes on advancing."
  (let ((org-time-stamp-rounding-minutes '(0 1))
        (lr-track-autosave-clock nil)
        (lr-track--state nil))
    (cl-flet ((shift-start (n)
                (goto-char (point-min))
                (re-search-forward "CLOCK: \\[[0-9-]+ [A-Za-z]+ [0-9]+:")
                (dotimes (_ (abs n))
                  (if (> n 0) (org-shiftup) (org-shiftdown))))
              (closed (buf)
                (seq-find (lambda (l) (string-match-p "--" l))
                          (lr-track-clock-test--clock-lines buf))))
      ;; A: paused at t0 + 30 min; he moves the start to t0 + 45 min
      (let ((t0 (lr-track-clock-test--minute -7200.0)))
        (lr-track-clock-test--clocked (t0 "* TODO avey\n")
          (lr-track-clock-test--fresh-presence
            (lr-track--advance-clock-line (+ t0 1800))
            (lr-track-clock-test--pause (+ t0 1800) 'idle)
            (shift-start 45)
            (lr-track-clock-test--capturing msgs
              (org-clock-out))
            (let ((line (closed buf)))
              (should (string-prefix-p
                       (concat "CLOCK: " (lr-track-clock-test--stamp (+ t0 2700)) "--")
                       line))
              (should-not (string-match-p "=> *-" line))
              (should-not (string-match-p (lr-track-clock-test--ends-at (+ t0 1800))
                                          line))))))
      ;; A2: paused at t0 + 30 min; he moves the start 20 min earlier
      (let ((t0 (lr-track-clock-test--minute -7200.0)))
        (lr-track-clock-test--clocked (t0 "* TODO avey\n")
          (lr-track-clock-test--fresh-presence
            (lr-track--advance-clock-line (+ t0 1800))
            (lr-track-clock-test--pause (+ t0 1800) 'idle)
            (shift-start -20)
            (should (equal (lr-track--paused-at) (+ t0 1800)))
            (lr-track-clock-test--capturing msgs
              (org-clock-out))
            (should (equal (concat "CLOCK: " (lr-track-clock-test--stamp (- t0 1200))
                                   "--" (lr-track-clock-test--stamp (+ t0 1800))
                                   " =>  0:50")
                           (closed buf))))))
      ;; B: a live line moved 20 min earlier keeps advancing
      (let ((t0 (lr-track-clock-test--minute -7200.0)))
        (lr-track-clock-test--clocked (t0 "* TODO avey\n")
          (lr-track-clock-test--fresh-presence
            (lr-track-clock-test--feed t0 0.0)
            (lr-track-clock-test--feed (+ t0 600) 0.0)
            (lr-track-clock-test--with-now (+ t0 600) (lr-track--tick-live-clock))
            (should (= (lr-track--running-line-end) (+ t0 600)))
            (shift-start -20)
            (cl-loop for x from 630 to 3600 by 30
                     do (lr-track-clock-test--feed (+ t0 x) 0.0))
            (lr-track-clock-test--with-now (+ t0 3600) (lr-track--tick-live-clock))
            (should (= (float-time org-clock-start-time) (- t0 1200)))
            (should (equal (concat "CLOCK: " (lr-track-clock-test--stamp (- t0 1200))
                                   "--" (lr-track-clock-test--stamp (+ t0 3600))
                                   " =>  1:20")
                           (car (lr-track-clock-test--clock-lines buf))))))))))

(ert-deftest lr-track-regression-failing-save-keeps-the-pause ()
  "He works an hour, the last tick 10 min before the lid closes, sleeps 8 h,
and clocks out before the wake sample is stepped.  The settle advances the
line, and that advance's throttled save fails (an after-save-hook error, as
org-roam's DB update can raise).  The failure is logged; the step goes on
and records the pause, so the line still ends where he left."
  (let ((t0 (lr-track-clock-test--minute -43200.0))
        (lr-track-autosave-clock t)
        (lr-track--state nil))
    (lr-track-clock-test--clocked (t0 "* TODO Write report\n")
      (lr-track-clock-test--fresh-presence
        (lr-track-clock-test--feed t0 0.0 nil (- t0 86400.0))
        (cl-loop for x from 30 to 3000 by 30
                 do (lr-track-clock-test--feed (+ t0 x) 0.0 nil (- t0 86400.0)))
        (lr-track-clock-test--with-now (+ t0 3001) (lr-track--tick-live-clock))
        (let ((inhibit-message t)) (save-buffer))
        (cl-loop for x from 3030 to 3570 by 30
                 do (lr-track-clock-test--feed (+ t0 x) 0.0 nil (- t0 86400.0)))
        (lr-track-clock-test--feed (+ t0 3599) 0.0 nil (- t0 86400.0))
        (add-hook 'after-save-hook (lambda () (error "database is locked")) nil t)
        (unwind-protect
            (let ((w (+ t0 3600 28800.0)))
              (setq lr-track--last-sample
                    (list :time (+ w 1.0) :idle 1.0 :locked nil :wake w))
              (lr-track-clock-test--with-now (+ w 15)
                (lr-track-clock-test--capturing msgs
                  (org-clock-out)
                  (should (member (format "Clocked out of Write report at %s, where it paused."
                                          (lr-track--ts-hm (+ t0 3599)))
                                  msgs)))))
          (kill-local-variable 'after-save-hook))
        (let ((closed (seq-find (lambda (l) (string-match-p "--" l))
                                (lr-track-clock-test--clock-lines buf))))
          (should (string-match-p (lr-track-clock-test--ends-at (+ t0 3540)) closed))
          (should (string-match-p " =>  0:59\\'" closed)))))))

(ert-deftest lr-track-regression-early-clock-in-holds-the-block ()
  "Lunch, locked, 16:00 to 16:25.  He unlocks at 16:25:05 and clocks in at
16:25:30, before any sample saw him back (his SPC c i, or an answer's start
floored to 16:25).  Called away at 16:27, he locks again until 16:50.  The
block his line started in must be held when its return is stepped, so the
second away is its own and not the lunch with his line merged into it."
  (dolist (start-off '(30 0))
    (let* ((t1 (lr-track-clock-test--minute -7200.0))
           (start (+ t1 start-off))
           (lr-track-autosave-clock nil)
           (lr-track--state nil))
      (lr-track-clock-test--fresh-presence
        ;; typing until 25 min before t1, then locked
        (cl-loop for x from -3600 to -1500 by 30
                 do (lr-track-clock-test--feed (+ t1 x) 1.0))
        (cl-loop for x from -1440 to 0 by 60
                 do (lr-track-clock-test--feed (+ t1 x) (- (+ t1 x) (- t1 1500)) t))
        (should (eq (plist-get lr-track--presence :mode) 'away))
        (lr-track-clock-test--clocked (start "* TODO study\n")
          (should-not (plist-get lr-track--presence :held))
          ;; the sample that sees him back, then 1 min of typing
          (lr-track-clock-test--feed (+ t1 60) 2.0)
          (should (equal (list start-off t1)
                         (list start-off (plist-get lr-track--presence :held))))
          (lr-track-clock-test--feed (+ t1 90) 2.0)
          (lr-track-clock-test--feed (+ t1 120) 2.0)
          ;; locked again from t1 + 2 min to t1 + 25 min
          (cl-loop for x from 180 to 1500 by 60
                   do (lr-track-clock-test--feed (+ t1 x) (- x 118.0) t))
          (lr-track-clock-test--feed (+ t1 1530) 2.0)
          (let ((gone (plist-get lr-track--presence :last-away)))
            (should (equal (list start-off (+ t1 118.0) 'locked)
                           (list start-off (plist-get gone :from)
                                 (plist-get gone :kind))))))))))

;;;; regressions from the round 3.3 review

(ert-deftest lr-track-regression-resolve-restart-ends-at-pause ()
  "M-x org-resolve-clocks while his real clock is paused, and his s or k for
a dangling line in another task: org clocks that task in again, and that
clock-in interrupts his clock.  It is a switch of his, as SPC c i on another
task is: his clock ends at the pause with the ended note, never at now
across the away it paused for."
  (dolist (key '(?s ?k))
    (let* ((now (lr-track-clock-test--minute 0.0))
           (t0 (- now 7200.0))
           (paused (+ t0 3000.0))
           (dstart (- now 108000.0))
           (lr-track-autosave-clock nil)
           (lr-track--state nil)
           (org-clock-auto-clock-resolution nil))
      (lr-track-clock-test--fresh-presence
        (lr-track-clock-test--feed (- now 10) 2.0)
        (lr-track-clock-test--clocked
            (t0 (format "* TODO avey\n* TODO old task\n:LOGBOOK:\nCLOCK: %s\n:END:\n"
                        (lr-track-clock-test--stamp dstart)))
          (lr-track--advance-clock-line paused)
          (lr-track-clock-test--pause paused 'asleep)
          (lr-track-clock-test--resolving-answer key
            (org-resolve-clocks)
            (should (equal (list key "old task")
                           (list key org-clock-current-task)))
            (should (equal (list key t)
                           (list key
                                 (and (seq-find
                                       (lambda (l)
                                         (string-prefix-p
                                          (concat "CLOCK: "
                                                  (lr-track-clock-test--stamp t0) "--"
                                                  (lr-track-clock-test--stamp paused))
                                          l))
                                       (lr-track-clock-test--clock-lines buf))
                                      t))))
            (should (string-match-p
                     (concat "^- lr ended: where it paused "
                             (regexp-quote (lr-track--ts-hm paused)) "$")
                     (buffer-string)))
            (should (seq-find (lambda (m) (and m (string-match-p "where it paused\\." m)))
                              msgs))))))))

(ert-deftest lr-track-regression-resolve-cancel-keeps-real-pause ()
  "His clock paused at its end before the lid closed.  He was back 10 min,
away 20 min, back again; the clock stays paused.  M-x org-resolve-clocks,
and his C for a dangling line in another task: org cancels that line with
it stood in for the running clock.  That cancel ended the dangling line,
not his clock: the pause stays, and the next ticks leave his line where it
paused, never across the away after."
  (let* ((now (lr-track-clock-test--minute 0.0))
         (t0 (- now 14400.0))
         (wake (+ t0 6600.0))
         (left (+ wake 610.0))
         (dstart (- now 108000.0))
         (lr-track-autosave-clock nil)
         (lr-track--state nil)
         (org-clock-auto-clock-resolution nil))
    (lr-track-clock-test--fresh-presence
      (lr-track-clock-test--clocked
          (t0 (format "* TODO avey\n* TODO old task\n:LOGBOOK:\nCLOCK: %s\n:END:\n"
                      (lr-track-clock-test--stamp dstart)))
        (cl-flet ((tick (at idle &optional woke)
                    (lr-track-clock-test--feed at idle nil (or woke 1000.0))
                    (lr-track-clock-test--with-now at (lr-track--tick-live-clock))))
          ;; 50 min of typing, the lid shut, 10 min back
          (cl-loop for k from 0 to 100 do (tick (+ t0 (* 30 k)) 3.0))
          (tick (+ wake 10) 4.0 wake)
          (cl-loop for k from 1 to 20 do (tick (+ wake 10 (* 30 k)) 3.0 wake))
          (let ((paused (lr-track--paused-at))
                (end (lr-track--running-line-end)))
            (should (and paused (< paused wake)))
            ;; away 20 min (no input, unlocked), then back 5 min
            (cl-loop for k from 1 to 40 do (tick (+ left (* 30 k)) (* 30.0 k) wake))
            (cl-loop for k from 1 to 10 do (tick (+ left 1200 (* 30 k)) 3.0 wake))
            (should (equal paused (lr-track--paused-at)))
            (lr-track-clock-test--resolving-answer ?C
              (org-resolve-clocks))
            (should-not (seq-find (lambda (l) (string-prefix-p
                                               (concat "CLOCK: "
                                                       (lr-track-clock-test--stamp dstart))
                                               l))
                                  (lr-track-clock-test--clock-lines buf)))
            (should (equal paused (lr-track--paused-at)))
            ;; the ticks go on while he types
            (cl-loop for k from 11 to 14 do (tick (+ left 1200 (* 30 k)) 3.0 wake))
            (should (equal paused (lr-track--paused-at)))
            (should (equal end (lr-track--running-line-end)))))))))

(ert-deftest lr-track-regression-paused-end-he-moved-stays ()
  "avey pauses at t0+40m: 30 min at the whiteboard, no input.  Back, he
corrects the end the org way, S-up or S-down on the end stamp.  His SPC c o,
or SPC c i on another task, then ends the line where he put it: never back
at the old pause, which would move the line backwards (S-up) or book again
the minutes he took off (S-down)."
  (dolist (case '((20 . out) (20 . switch) (-5 . out)))
    (let ((t0 (lr-track-clock-test--minute -10800.0))
          (lr-track-autosave-clock nil)
          (lr-track--state nil)
          (org-clock-auto-clock-resolution nil)
          (org-clock-out-remove-zero-time-clocks t)
          (org-time-stamp-rounding-minutes '(0 1)))
      (lr-track-clock-test--fresh-presence
        (lr-track-clock-test--feed (- t0 20) 2.0)
        (lr-track-clock-test--clocked
            (t0 (concat "* TODO avey\n  :LOGBOOK:\n"
                        "  CLOCK: [2026-10-02 Fri 18:00]--[2026-10-02 Fri 19:00] =>  1:00\n"
                        "  :END:\n* TODO study\n"))
          ;; typing until t0+40m, then no input; back at t0+70m
          (cl-loop for x from 30 to 4320 by 30
                   do (let ((at (+ t0 x)))
                        (lr-track-clock-test--feed
                         at (if (or (<= x 2400) (> x 4200)) 0.0 (- x 2400.0)))
                        (lr-track-clock-test--with-now at
                          (lr-track--tick-live-clock))))
          (should (equal (+ t0 2400) (lr-track--paused-at)))
          (should (equal (+ t0 2400) (lr-track--running-line-end)))
          ;; his S-up (or S-down) on the end stamp's minutes
          (save-excursion
            (goto-char org-clock-marker)
            (re-search-forward "--\\[" (line-end-position))
            (re-search-forward "[0-9][0-9]:[0-9][0-9]" (line-end-position))
            (backward-char 1)
            (dotimes (_ (abs (car case)))
              (if (> (car case) 0) (org-shiftup) (org-shiftdown))))
          (let ((his (+ t0 2400 (* 60 (car case)))))
            (should (equal his (lr-track--running-line-end)))
            (lr-track-clock-test--capturing msgs
              (pcase (cdr case)
                ('out (org-clock-out))
                ('switch (goto-char (point-max))
                         (re-search-backward "^\\* TODO study")
                         (org-clock-in))))
            (should (equal (list case t)
                           (list case
                                 (and (seq-find
                                       (lambda (l)
                                         (string-prefix-p
                                          (concat "CLOCK: "
                                                  (lr-track-clock-test--stamp t0) "--"
                                                  (lr-track-clock-test--stamp his))
                                          l))
                                       (lr-track-clock-test--clock-lines buf))
                                      t))))))))))

(ert-deftest lr-track-regression-short-gap-across-away-mark-pauses ()
  "He types until 14:00, then is away (Mac awake, unlocked) until 14:16.
App Nap holds the tick from 14:09:30 to 14:20:30: the samples saw 9.5 min of
idle, then input 2 s old.  The gap is under 15 min, but the input after it
is 20 min after his last: that is no proof he typed, and the 16 min away
must not be credited.  The clock pauses at 14:00, why `unknown' (nothing saw
him leave or come back)."
  (let ((t0 (lr-track-clock-test--minute -7200.0))
        (lr-track-autosave-clock nil)
        (lr-track--state nil))
    (lr-track-clock-test--clocked (t0 "* TODO avey\n")
      (lr-track-clock-test--fresh-presence
        (let ((l (+ t0 3300.0))
              (old (- t0 86400.0)))
          (cl-loop for x from 0 to 3300 by 30
                   do (lr-track-clock-test--feed (+ t0 x) 0.0 nil old))
          (cl-loop for x from 30 to 570 by 30
                   do (lr-track-clock-test--feed (+ l x) (float x) nil old))
          (lr-track-clock-test--with-now (+ l 570) (lr-track--tick-live-clock))
          (should (= (lr-track--running-line-end) l))
          (should-not (lr-track--paused-at))
          ;; the first sample after the nap
          (lr-track-clock-test--feed (+ l 1230.5) 2.0 nil old)
          (lr-track-clock-test--with-now (+ l 1230.5) (lr-track--tick-live-clock))
          (should (equal (list l 'unknown)
                         (list (lr-track--paused-at)
                               (plist-get (lr-track--current-pause) :why))))
          (should (= (lr-track--running-line-end) l)))))))

(defconst lr-track-clock-test--stage1-defs
  '(lr-track--advanced-tick lr-track--ask-exit-fn lr-track--autosaved-at
    lr-track--blind-sample lr-track--blind-since lr-track--buffer-current-p
    lr-track--clock-cancel-note lr-track--clock-default-max
    lr-track--clock-in-paused-task lr-track--clock-out-args
    lr-track--clock-out-ended lr-track--clock-pause lr-track--clock-place
    lr-track--clock-step lr-track--clock-string lr-track--current-pause
    lr-track--delete-note-line lr-track--drop-ended lr-track--forget-pause
    lr-track--goto-running-line lr-track--internal lr-track--last-clock-end
    lr-track--last-sample lr-track--maybe-autosave
    lr-track--note-below-running-line lr-track--note-under-clock-line
    lr-track--on-clock-out lr-track--parse-max lr-track--paused-at
    lr-track--presence lr-track--presence-compact lr-track--presence-stepped
    lr-track--probe-command lr-track--probe-skip-seconds
    lr-track--recover-quietly lr-track--running-line-end
    lr-track--settle-clock lr-track--step-sample lr-track--tick-header
    lr-track--tick-presence lr-track--tick-probe lr-track-autosave-interval
    lr-track-interval-away lr-track-interval-here lr-track--save-buffer
    lr-track--tick-live-clock lr-track--advance-clock-line
    lr-track--sys-idle-sentinel lr-track--sys-idle-probe
    ;; round 3
    lr-track--clock-stood-in-p lr-track--org-resolving-p
    lr-track--zero-time-why lr-track--clock-ended lr-track--note-tags
    lr-track--note-re lr-track--cancel-takes-line-p lr-track--hold-here-block
    lr-track--presence-fresh-p lr-track--s0-old-defaults
    lr-track--s0-silence-old-defaults lr-track--modeline-refresh
    ;; round 3.2
    lr-track--sync-running-start lr-track--away-why lr-track--hold-early-line
    ;; round 3.3
    lr-track--sync-paused-end lr-track--org-ends-it-p lr-track--tick-stalled-p
    lr-track--emacs-sample lr-track--settle-at-key)
  "Definitions Stage 1 added to lr-track.el or rewrote.")

(defun lr-track-clock-test--wide-docstrings (file only)
  "Docstring source lines of FILE wider than 80 columns, as (LINE WIDTH SYM).
ONLY, when non-nil, limits the scan to definitions of those symbols."
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (goto-char (point-min))
    (let (out)
      (condition-case nil
          (while t
            (forward-comment (buffer-size))
            (let* ((beg (point))
                   (form (read (current-buffer)))
                   (idx (and (consp form)
                             (pcase (car form)
                               ((or 'defun 'defmacro 'cl-defun 'defvar 'defconst
                                    'defcustom 'defvar-local 'defface)
                                3)
                               ('define-minor-mode 2)
                               ('define-derived-mode 4)))))
              (when (and idx (stringp (nth idx form))
                         (or (null only) (memq (nth 1 form) only)))
                (save-excursion
                  (goto-char beg)
                  (down-list 1)
                  (forward-sexp idx)
                  (forward-comment (buffer-size))
                  (let ((end (save-excursion (forward-sexp 1) (point))))
                    (while (< (point) end)
                      (let ((w (- (line-end-position) (line-beginning-position))))
                        (when (> w 80)
                          (push (list (line-number-at-pos) w (nth 1 form)) out)))
                      (forward-line 1)))))))
        (end-of-file nil))
      (nreverse out))))

(ert-deftest lr-track-regression-stage1-docstrings-within-80 ()
  "Every docstring Stage 1 wrote is wrapped at 80 columns: the two new
modules whole, and what Stage 1 added to lr-track.el."
  (let ((dir (expand-file-name "modules" lr-track-clock-test--root)))
    (should-not (lr-track-clock-test--wide-docstrings
                 (expand-file-name "lr-track-presence.el" dir) nil))
    (should-not (lr-track-clock-test--wide-docstrings
                 (expand-file-name "lr-track-ask.el" dir) nil))
    (should-not (lr-track-clock-test--wide-docstrings
                 (expand-file-name "lr-track.el" dir)
                 lr-track-clock-test--stage1-defs))))

(provide 'lr-track-clock-test)
;;; lr-track-clock-test.el ends here
