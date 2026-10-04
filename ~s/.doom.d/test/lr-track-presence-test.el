;;; lr-track-presence-test.el --- presence and probe tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Presence is the only thing lr-track senses: the lock, HID idle, and the
;; Mac's wake time.  Never what he did.  These tests pin the two pure pieces of
;; `lr-track-presence':
;;
;;   - `lr-track--parse-probe' keeps idle seconds, locked yes or no, and the
;;     wake time, and nothing else (I16).  A failed parse is `unknown', never 0.
;;   - `lr-track--presence-step' turns samples into here, breaks and aways.
;;     The last input is sample time minus HID idle, while unlocked.  A gap of
;;     3 to 15 min is a break, 15 min or more is an away from the last input,
;;     a wake after the previous sample is an away of kind asleep, a return is
;;     the previous sample time (or the wake), and a glance under 3 min between
;;     two aways joins them, so the night stays one away.
;;
;; The scripted days below never read the wall clock: every time is seconds
;; after a fixed epoch, and samples fall on a 30 s grid.
;;
;; Run from the repo:
;;   emacs -Q --batch -L modules -l test/lr-track-presence-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(let ((b "/Users/l/.emacs.d/.local/straight/build-31.0.91/"))
  (dolist (p '("org" "evil")) (when (file-directory-p (concat b p)) (push (concat b p) load-path))))

(require 'ert)
(require 'cl-lib)

(when (boundp 'native-comp-jit-compilation)
  (setq native-comp-jit-compilation nil))

;; `emacs -Q' has no Doom.  Stub what lr-track touches at load time.
(defmacro add-hook! (&rest _) nil)
(defmacro after! (&rest body) `(progn ,@body))
(defmacro defadvice! (&rest _) nil)
(defvar lr-track-presence-test--tmp
  (file-name-as-directory (make-temp-file "lr-track-presence-test" t)))
(defvar doom-data-dir (expand-file-name "etc/" lr-track-presence-test--tmp))
(defvar doom-cache-dir (expand-file-name "cache/" lr-track-presence-test--tmp))
(make-directory doom-data-dir t)
(make-directory doom-cache-dir t)

(defconst lr-track-presence-test--modules
  (expand-file-name "modules" (locate-dominating-file
                               (or load-file-name buffer-file-name)
                               "modules"))
  "The modules directory of this config.")
(add-to-list 'load-path lr-track-presence-test--modules)

;; Guarded, so a missing module fails each test on its own (void-function)
;; instead of stopping the whole file.  `lr-track' is loaded too because it is
;; where the module lives in the running config; the standalone test below
;; proves `lr-track-presence' needs nothing from it.
(dolist (feature '(lr-track-presence lr-track))
  (condition-case err
      (require feature)
    (error (message "lr-track-presence-test: %s did not load: %S" feature err))))


;;;; fixtures

(defconst lr-track-presence-test--t0 1791000000.0
  "The fixed epoch every scripted day starts at.
Fixed so no test reads the wall clock.  Integral, so every derived time below
is exact in a float.")

(defconst lr-track-presence-test--real-output
  (concat "      \"HIDIdleTime\" = 993495833\n"
          "      \"IOConsoleLocked\" = No\n"
          "{ sec = 1791033656, usec = 177813 } Sat Oct  3 16:20:56 2026\n")
  "The probe's real output, verified on this Mac on 2026-10-04.")

(defun lr-track-presence-test--at (s)
  "The epoch time S seconds after `lr-track-presence-test--t0'."
  (+ lr-track-presence-test--t0 (float s)))

(defun lr-track-presence-test--sample (s idle &optional locked wake)
  "A probe sample taken S seconds after the epoch.
IDLE is HID idle seconds (or `unknown'), LOCKED the lock state, WAKE the
kern.waketime as seconds after the epoch, or nil."
  (list :time (lr-track-presence-test--at s)
        :idle (if (numberp idle) (float idle) idle)
        :locked locked
        :wake (and wake (lr-track-presence-test--at wake))))

(cl-defun lr-track-presence-test--world
    (&key (from 0) to (every 30) input locked asleep (wake -86400))
  "Probe samples of a scripted Mac, every EVERY seconds from FROM to TO.
Times are seconds after `lr-track-presence-test--t0'.  INPUT is a list of
\(A . B): the keyboard or pointer is in use from A to B.  LOCKED is a list of
\(A . B): the screen is locked from A until just before B.  ASLEEP is a list
of (A . B): the Mac sleeps from A to B, so no sample falls strictly inside,
and kern.waketime reads B from then on.  WAKE is the waketime before any of
them (a day before the epoch by default, so it never reads as a sleep)."
  (let (out)
    (cl-loop
     for s from from to to by every
     unless (cl-some (lambda (iv) (and (> s (car iv)) (< s (cdr iv)))) asleep)
     do (let ((last nil) (w wake))
          (dolist (iv input)
            (when (<= (car iv) s)
              (let ((seen (min s (cdr iv))))
                (setq last (if last (max last seen) seen)))))
          (dolist (iv asleep)
            (when (<= (cdr iv) s) (setq w (max w (cdr iv)))))
          (push (list :time (lr-track-presence-test--at s)
                      :idle (float (- s (or last (- from 3600))))
                      :locked (and (cl-some (lambda (iv) (and (<= (car iv) s)
                                                              (< s (cdr iv))))
                                            locked)
                                   t)
                      :wake (lr-track-presence-test--at w))
                out)))
    (nreverse out)))

(defun lr-track-presence-test--run (samples &optional p)
  "Step SAMPLES through `lr-track--presence-step', from P or the initial state."
  (let ((p (or p (lr-track--presence-init))))
    (dolist (x samples p)
      (setq p (lr-track--presence-step p x)))))

(defun lr-track-presence-test--upto (samples s)
  "The state after stepping those SAMPLES taken at or before S."
  (let ((limit (lr-track-presence-test--at s)))
    (lr-track-presence-test--run
     (cl-remove-if (lambda (x) (> (plist-get x :time) limit)) samples))))

(defun lr-track-presence-test--state (&rest kvs)
  "A fresh initial presence state with KVS put over it."
  (let ((p (copy-tree (lr-track--presence-init))))
    (while kvs (setq p (plist-put p (pop kvs) (pop kvs))))
    p))

(defun lr-track-presence-test--rel (x)
  "X as float seconds after the epoch when it is a number, else X itself."
  (if (numberp x) (float (- x lr-track-presence-test--t0)) x))

(defun lr-track-presence-test--get (p key)
  "KEY of plist P, relative to the epoch when it is a time."
  (lr-track-presence-test--rel (plist-get p key)))

(defun lr-track-presence-test--away (a)
  "Away plist A as (FROM TO KIND), times relative to the epoch, or nil."
  (and a (list (lr-track-presence-test--get a :from)
               (lr-track-presence-test--get a :to)
               (plist-get a :kind))))

(defun lr-track-presence-test--segs (p)
  "P's segments as (KIND FROM TO DETAIL), relative times, newest first."
  (mapcar (lambda (g) (list (plist-get g :kind)
                            (lr-track-presence-test--get g :from)
                            (lr-track-presence-test--get g :to)
                            (plist-get g :detail)))
          (plist-get p :segments)))

(defun lr-track-presence-test--breaks (p)
  "P's breaks as (FROM TO), relative times, newest first."
  (mapcar (lambda (b) (list (lr-track-presence-test--get b :from)
                            (lr-track-presence-test--get b :to)))
          (plist-get p :breaks)))

(defun lr-track-presence-test--keys (plist)
  "The keys of PLIST, sorted by name."
  (sort (cl-loop for (k _v) on plist by #'cddr collect k)
        (lambda (a b) (string< (symbol-name a) (symbol-name b)))))


;;;; constants

(ert-deftest lr-track-presence-constants-match-contract ()
  "The five presence thresholds, in seconds."
  (should (= 180.0 lr-track-presence-break-seconds))
  (should (= 900.0 lr-track-presence-away-seconds))
  (should (= 180.0 lr-track-presence-glance-seconds))
  (should (= 10800.0 lr-track-presence-keep-breaks-seconds))
  (should (= 129600.0 lr-track-presence-keep-segments-seconds)))


;;;; probe parsing

(ert-deftest lr-track-probe-parse-idle-locked-wake ()
  "The real probe output parses to idle seconds, unlocked, and the wake time.
HIDIdleTime is nanoseconds; the wake keeps whole seconds and drops usec."
  (let ((s (lr-track--parse-probe lr-track-presence-test--real-output)))
    (should (floatp (plist-get s :idle)))
    (should (< (abs (- (plist-get s :idle) 0.993495833)) 1e-9))
    (should (plist-member s :locked))
    (should (eq nil (plist-get s :locked)))
    (should (floatp (plist-get s :wake)))
    (should (= 1791033656.0 (plist-get s :wake))))
  ;; 15 minutes of idle reads as 900 s
  (let ((s (lr-track--parse-probe
            (concat "      \"HIDIdleTime\" = 900000000000\n"
                    "      \"IOConsoleLocked\" = No\n"
                    "{ sec = 1791033656, usec = 0 } Sat Oct  3 16:20:56 2026\n"))))
    (should (= 900.0 (plist-get s :idle)))))

(ert-deftest lr-track-probe-parse-locked-yes ()
  "`Yes' on the IOConsoleLocked line is t; idle and wake still parse."
  (let ((s (lr-track--parse-probe
            (replace-regexp-in-string "= No" "= Yes"
                                      lr-track-presence-test--real-output))))
    (should (eq t (plist-get s :locked)))
    (should (< (abs (- (plist-get s :idle) 0.993495833)) 1e-9))
    (should (= 1791033656.0 (plist-get s :wake)))))

(ert-deftest lr-track-probe-parse-garbage-is-unknown-never-zero ()
  "Every failure is `unknown' (idle and lock) or nil (wake), never 0.
A 0 idle would read as input right now and make every away unreachable."
  ;; non-strings: nothing known at all
  (dolist (out (list nil 42 'garbage '(1 2)))
    (let ((s (lr-track--parse-probe out)))
      (should (= 6 (length s)))
      (should (eq 'unknown (plist-get s :idle)))
      (should (eq 'unknown (plist-get s :locked)))
      (should (plist-member s :wake))
      (should (eq nil (plist-get s :wake)))))
  ;; strings with nothing usable
  (dolist (out (list ""
                     "garbage\nmore garbage\n"
                     "      \"HIDIdleTime\" = \n"
                     "      \"HIDIdleTime\" = -5\n"
                     "      \"HIDIdleTime\" = abc\n"
                     ;; 40 days: past the 30-day sanity bound
                     "      \"HIDIdleTime\" = 3456000000000000\n"
                     ;; exactly 30 days is already out
                     "      \"HIDIdleTime\" = 2592000000000000\n"))
    (let ((s (lr-track--parse-probe out)))
      (should (eq 'unknown (plist-get s :idle)))
      (should-not (and (numberp (plist-get s :idle)) (zerop (plist-get s :idle))))
      (should (eq 'unknown (plist-get s :locked)))
      (should (eq nil (plist-get s :wake)))))
  ;; one bad line spoils only its own field
  (let ((s (lr-track--parse-probe
            (concat "      \"HIDIdleTime\" = 5000000000\n"
                    "{ sec = 1791033656, usec = 177813 } Sat Oct  3 16:20:56 2026\n"))))
    (should (= 5.0 (plist-get s :idle)))
    (should (eq 'unknown (plist-get s :locked)))
    (should (= 1791033656.0 (plist-get s :wake))))
  (let ((s (lr-track--parse-probe
            (concat "      \"HIDIdleTime\" = 5000000000\n"
                    "      \"IOConsoleLocked\" = Maybe\n"
                    "{ sec = , usec = 0 }\n"))))
    (should (= 5.0 (plist-get s :idle)))
    (should (eq 'unknown (plist-get s :locked)))
    (should (eq nil (plist-get s :wake))))
  (let ((s (lr-track--parse-probe
            (concat "      \"HIDIdleTime\" = garbage\n"
                    "      \"IOConsoleLocked\" = No\n"))))
    (should (eq 'unknown (plist-get s :idle)))
    (should (eq nil (plist-get s :locked)))
    (should (eq nil (plist-get s :wake))))
  ;; 29 days is still a real idle
  (should (= 2505600.0 (plist-get (lr-track--parse-probe
                                   "      \"HIDIdleTime\" = 2505600000000000\n")
                                  :idle))))

(ert-deftest lr-track-probe-parse-keeps-no-other-field ()
  "Only idle, locked and wake survive the parse: no app, user or window text
\(I16).  The output here carries extra ioreg properties and a stray line."
  (let* ((out (concat
               "      \"HIDIdleTime\" = 5000000000\n"
               "      \"HIDIdleTimeDelta\" = 123456\n"
               "      \"IOConsoleLocked\" = Yes\n"
               "      \"IOConsoleUsers\" = ({\"kCGSSessionUserNameKey\"=\"someone\"})\n"
               "{ sec = 1791033656, usec = 177813 } Sat Oct  3 16:20:56 2026\n"
               "Safari \"Bank login\" frontmost\n"))
         (s (lr-track--parse-probe out)))
    (should (= 6 (length s)))
    (should (equal '(:idle :locked :wake) (lr-track-presence-test--keys s)))
    (should (= 5.0 (plist-get s :idle)))
    (should (eq t (plist-get s :locked)))
    (should (= 1791033656.0 (plist-get s :wake)))
    (should-not (cl-some #'stringp s))
    (should-not (string-match-p "someone\\|Safari\\|Bank\\|Delta"
                                (prin1-to-string s))))
  (dolist (out (list lr-track-presence-test--real-output "" nil))
    (should (equal '(:idle :locked :wake)
                   (lr-track-presence-test--keys (lr-track--parse-probe out))))))

(ert-deftest lr-track-probe-parse-idle-agrees-with-hid-idle-parser ()
  "The idle field follows the existing HID idle rules exactly (reuse, not a
second parser that could drift)."
  (dolist (out (list lr-track-presence-test--real-output
                     nil 42 "" "garbage\n"
                     "      \"HIDIdleTime\" = 5000000000\n"
                     "      \"HIDIdleTime\" = 2592000000000000\n"
                     "      \"HIDIdleTime\" = -5\n"))
    (should (equal (lr-track--parse-hid-idle out)
                   (plist-get (lr-track--parse-probe out) :idle)))))


;;;; the initial state and the first sample

(ert-deftest lr-track-presence-init-shape ()
  "The initial state: mode unknown, every other listed field nil."
  (let ((p (lr-track--presence-init)))
    (should (eq 'unknown (plist-get p :mode)))
    (dolist (k '(:last-input :prev-time :wake :first-time :away-from :away-kind
                 :saw-lock :return :last-away :breaks :segments))
      (should (plist-member p k))
      (should (eq nil (plist-get p k))))
    ;; a fresh state each call: stepping one never leaks into the next
    (should-not (eq p (lr-track--presence-init)))))

(ert-deftest lr-track-presence-first-sample-here ()
  "A first sample with under 15 min of idle starts here.
The last input is the sample time minus idle; here-since is the first time."
  (let ((p (lr-track--presence-step (lr-track--presence-init)
                                    (lr-track-presence-test--sample 1000 30 nil 500))))
    (should (eq 'here (plist-get p :mode)))
    (should (equal 970.0 (lr-track-presence-test--get p :last-input)))
    (should (equal 1000.0 (lr-track-presence-test--get p :first-time)))
    (should (equal 1000.0 (lr-track-presence-test--get p :prev-time)))
    (should (equal 500.0 (lr-track-presence-test--get p :wake)))
    (should-not (plist-get p :away-from))
    (should-not (plist-get p :away-kind))
    (should-not (plist-get p :return))
    (should-not (plist-get p :last-away))
    (should-not (plist-get p :breaks))
    (should-not (plist-get p :segments))
    (should-not (lr-track--presence-away-p p))
    (should (= (lr-track-presence-test--at 1000) (lr-track--presence-here-since p)))))

(ert-deftest lr-track-presence-first-sample-away ()
  "A first sample with 15 min of idle or more starts away from the last input.
The kind is `locked' when the screen is locked, else `idle'."
  (let ((p (lr-track--presence-step (lr-track--presence-init)
                                    (lr-track-presence-test--sample 5000 1200))))
    (should (eq 'away (plist-get p :mode)))
    (should (lr-track--presence-away-p p))
    (should (equal 3800.0 (lr-track-presence-test--get p :away-from)))
    (should (equal 3800.0 (lr-track-presence-test--get p :last-input)))
    (should (eq 'idle (plist-get p :away-kind)))
    (should (equal 5000.0 (lr-track-presence-test--get p :first-time)))
    (should (equal 5000.0 (lr-track-presence-test--get p :prev-time))))
  (let ((p (lr-track--presence-step (lr-track--presence-init)
                                    (lr-track-presence-test--sample 5000 1200 t))))
    (should (eq 'away (plist-get p :mode)))
    (should (eq 'locked (plist-get p :away-kind)))
    (should (plist-get p :saw-lock)))
  ;; the threshold is inclusive
  (should (eq 'away (plist-get (lr-track--presence-step
                                (lr-track--presence-init)
                                (lr-track-presence-test--sample 5000 900))
                               :mode)))
  (should (eq 'here (plist-get (lr-track--presence-step
                                (lr-track--presence-init)
                                (lr-track-presence-test--sample 5000 899))
                               :mode))))


;;;; the last input

(ert-deftest lr-track-presence-input-is-sample-minus-idle ()
  "Last input is the sample time minus HID idle, and only moves forward on
real new input (more than 1 s past the previous one, so probe jitter is not
input)."
  (let* ((s1 (lr-track-presence-test--sample 1000 40))
         (s2 (lr-track-presence-test--sample 1030 2.5))
         (s3 (lr-track-presence-test--sample 1060 32.5))   ; same input as s2
         (s4 (lr-track-presence-test--sample 1090 62))     ; 0.5 s of jitter
         (s5 (lr-track-presence-test--sample 1120 10))
         (p (lr-track--presence-step (lr-track--presence-init) s1)))
    (should (equal 960.0 (lr-track-presence-test--get p :last-input)))
    (setq p (lr-track--presence-step p s2))
    (should (equal 1027.5 (lr-track-presence-test--get p :last-input)))
    (setq p (lr-track--presence-step p s3))
    (should (equal 1027.5 (lr-track-presence-test--get p :last-input)))
    (setq p (lr-track--presence-step p s4))
    (should (equal 1027.5 (lr-track-presence-test--get p :last-input)))
    (setq p (lr-track--presence-step p s5))
    (should (equal 1110.0 (lr-track-presence-test--get p :last-input)))
    (should (equal 1120.0 (lr-track-presence-test--get p :prev-time)))
    (should (eq 'here (plist-get p :mode)))))

(ert-deftest lr-track-presence-input-while-locked-ignored ()
  "Input while the screen is locked (typing the password) is not his input.
A locked sample also marks the lock as seen, until real input clears it."
  (let ((p (lr-track-presence-test--run
            (list (lr-track-presence-test--sample 1000 10)
                  (lr-track-presence-test--sample 1030 40 t)))))
    (should (equal 990.0 (lr-track-presence-test--get p :last-input)))
    (should (plist-get p :saw-lock))
    ;; the password: 2 s of idle, but locked
    (setq p (lr-track--presence-step p (lr-track-presence-test--sample 1060 2 t)))
    (should (equal 990.0 (lr-track-presence-test--get p :last-input)))
    (should (plist-get p :saw-lock))
    ;; unlocked, typing: now it counts, and the lock mark clears
    (setq p (lr-track--presence-step p (lr-track-presence-test--sample 1090 5)))
    (should (equal 1085.0 (lr-track-presence-test--get p :last-input)))
    (should-not (plist-get p :saw-lock))
    (should (eq 'here (plist-get p :mode)))))

(ert-deftest lr-track-presence-unknown-lock-counts-as-unlocked ()
  "A garbled lock line reads as unlocked: idle still moves the last input,
and no lock is marked as seen."
  (let ((p (lr-track-presence-test--run
            (list (lr-track-presence-test--sample 1000 10)
                  (lr-track-presence-test--sample 1030 3 'unknown)))))
    (should (equal 1027.0 (lr-track-presence-test--get p :last-input)))
    (should-not (plist-get p :saw-lock))))


;;;; breaks

(ert-deftest lr-track-presence-gap-3-to-15m-is-break ()
  "An input gap of 3 to 15 min is a break inside the here-block.
It runs from the last input to the previous sample (the last one before he
came back), newest first, and starts no away."
  (let ((p (lr-track-presence-test--run
            (lr-track-presence-test--world
             :to 1800 :input '((0 . 600) (1000 . 1300) (1700 . 1800))))))
    (should (eq 'here (plist-get p :mode)))
    (should (equal '((1300.0 1680.0) (600.0 990.0))
                   (lr-track-presence-test--breaks p)))
    (should-not (plist-get p :segments))
    (should-not (plist-get p :return))
    (should-not (plist-get p :last-away))
    (should (equal 1800.0 (lr-track-presence-test--get p :last-input)))
    (should (= lr-track-presence-test--t0 (lr-track--presence-here-since p)))))

(ert-deftest lr-track-presence-gap-under-3m-is-nothing ()
  "A gap under 3 min leaves no trace: no break, no segment."
  (let ((p (lr-track-presence-test--run
            (lr-track-presence-test--world
             :to 900 :input '((0 . 600) (750 . 900))))))
    (should (eq 'here (plist-get p :mode)))
    (should-not (plist-get p :breaks))
    (should-not (plist-get p :segments))
    (should (equal 900.0 (lr-track-presence-test--get p :last-input)))))

(ert-deftest lr-track-presence-lock-under-15m-is-break ()
  "A lock shorter than 15 min is a break, not an away.
The password typed while locked does not end it; the first unlocked input does."
  (let ((p (lr-track-presence-test--run
            (lr-track-presence-test--world
             :to 2700 :input '((0 . 1800) (2390 . 2700)) :locked '((1810 . 2400))))))
    (should (eq 'here (plist-get p :mode)))
    (should (equal '((1800.0 2370.0)) (lr-track-presence-test--breaks p)))
    (should-not (plist-get p :segments))
    (should-not (plist-get p :last-away))
    (should-not (plist-get p :saw-lock))))


;;;; aways

(ert-deftest lr-track-presence-gap-15m-is-away-from-last-input ()
  "15 min without input starts an away, dated from the last input (not from
the sample that noticed it), and closes the here segment there."
  (let ((samples (lr-track-presence-test--world :to 3300 :input '((0 . 1800)))))
    (let ((p (lr-track-presence-test--upto samples 2670)))   ; 14.5 min idle
      (should (eq 'here (plist-get p :mode)))
      (should-not (plist-get p :segments)))
    (let ((p (lr-track-presence-test--upto samples 2700)))   ; 15 min idle
      (should (eq 'away (plist-get p :mode)))
      (should (equal 1800.0 (lr-track-presence-test--get p :away-from)))
      (should (eq 'idle (plist-get p :away-kind)))
      (should (equal '((here 0.0 1800.0 nil)) (lr-track-presence-test--segs p))))
    ;; staying away changes nothing more
    (let ((p (lr-track-presence-test--run samples)))
      (should (eq 'away (plist-get p :mode)))
      (should (equal 1800.0 (lr-track-presence-test--get p :away-from)))
      (should (equal '((here 0.0 1800.0 nil)) (lr-track-presence-test--segs p)))
      (should-not (plist-get p :last-away))
      (should-not (plist-get p :return)))))

(ert-deftest lr-track-presence-lock-15m-is-away-kind-locked ()
  "A lock of 15 min or more is an away of kind `locked', from the last input.
A lock seen since the last input also makes the kind `locked' when the screen
is already unlocked again (a watch unlock types nothing)."
  (let ((samples (lr-track-presence-test--world
                  :to 4300 :input '((0 . 1800) (3990 . 4300))
                  :locked '((1810 . 4000)))))
    (let ((p (lr-track-presence-test--upto samples 2700)))
      (should (eq 'away (plist-get p :mode)))
      (should (equal 1800.0 (lr-track-presence-test--get p :away-from)))
      (should (eq 'locked (plist-get p :away-kind))))
    (let ((p (lr-track-presence-test--run samples)))
      (should (eq 'here (plist-get p :mode)))
      (should (equal 3990.0 (lr-track-presence-test--get p :return)))
      (should (equal '(1800.0 3990.0 locked)
                     (lr-track-presence-test--away (plist-get p :last-away))))
      (should (equal '((away 1800.0 3990.0 locked) (here 0.0 1800.0 nil))
                     (lr-track-presence-test--segs p)))))
  (let ((p (lr-track-presence-test--run
            (lr-track-presence-test--world
             :to 2700 :input '((0 . 1800)) :locked '((1810 . 2100))))))
    (should (eq 'away (plist-get p :mode)))
    (should (eq 'locked (plist-get p :away-kind)))))

(ert-deftest lr-track-presence-wake-after-prev-sample-is-asleep-away ()
  "A wake time after the previous sample means the Mac slept in between: an
away of kind `asleep' from the last input, overriding idle when already away."
  ;; here when it slept: the away starts at the last input
  (let ((samples (lr-track-presence-test--world
                  :to 6030 :input '((0 . 1800)) :asleep '((1900 . 6000)))))
    (let ((p (lr-track-presence-test--upto samples 6000)))
      (should (eq 'away (plist-get p :mode)))
      (should (equal 1800.0 (lr-track-presence-test--get p :away-from)))
      (should (eq 'asleep (plist-get p :away-kind)))
      (should (equal 6000.0 (lr-track-presence-test--get p :wake)))
      (should (equal '((here 0.0 1800.0 nil)) (lr-track-presence-test--segs p))))
    ;; the next sample sees the same wake: still one away
    (let ((p (lr-track-presence-test--run samples)))
      (should (eq 'asleep (plist-get p :away-kind)))
      (should (equal '((here 0.0 1800.0 nil)) (lr-track-presence-test--segs p)))))
  ;; already away (idle) when it slept: the kind becomes asleep
  (let ((p (lr-track-presence-test--run
            (lr-track-presence-test--world
             :to 9000 :input '((0 . 1800)) :asleep '((3000 . 9000))))))
    (should (eq 'away (plist-get p :mode)))
    (should (equal 1800.0 (lr-track-presence-test--get p :away-from)))
    (should (eq 'asleep (plist-get p :away-kind)))
    (should (equal '((here 0.0 1800.0 nil)) (lr-track-presence-test--segs p))))
  ;; a wake at the previous sample time is not after it
  (let ((p (lr-track-presence-test--run
            (list (lr-track-presence-test--sample 1000 0 nil 500)
                  (lr-track-presence-test--sample 1030 30 nil 1000)))))
    (should (eq 'here (plist-get p :mode)))
    (should-not (plist-get p :segments))))

(ert-deftest lr-track-presence-wake-kept-when-sample-has-none ()
  "The state keeps the latest wake time; a sample without one does not erase it."
  (let ((p (lr-track-presence-test--run
            (list (lr-track-presence-test--sample 1000 5 nil 500)
                  (lr-track-presence-test--sample 1030 5 nil nil)))))
    (should (equal 500.0 (lr-track-presence-test--get p :wake)))))


;;;; returns

(ert-deftest lr-track-presence-return-is-prev-sample-time ()
  "An away ends at the previous sample time: the last moment he was surely
still away (at most one tick early).  The return clears the breaks of the
old here-block and opens a new one."
  (let ((samples (lr-track-presence-test--world
                  :to 3900 :input '((0 . 600) (1000 . 1800) (3610 . 3900)))))
    (should (equal '((600.0 990.0))
                   (lr-track-presence-test--breaks
                    (lr-track-presence-test--upto samples 2000))))
    (let ((p (lr-track-presence-test--upto samples 3630)))
      (should (eq 'here (plist-get p :mode)))
      (should (equal 3600.0 (lr-track-presence-test--get p :return)))
      (should (equal 3630.0 (lr-track-presence-test--get p :last-input)))
      (should (equal '(1800.0 3600.0 idle)
                     (lr-track-presence-test--away (plist-get p :last-away))))
      (should (equal '((away 1800.0 3600.0 idle) (here 0.0 1800.0 nil))
                     (lr-track-presence-test--segs p)))
      (should-not (plist-get p :breaks))
      (should-not (plist-get p :away-from))
      (should-not (plist-get p :away-kind))
      (should (= (lr-track-presence-test--at 3600) (lr-track--presence-here-since p))))
    (let ((p (lr-track-presence-test--run samples)))
      (should (equal 3600.0 (lr-track-presence-test--get p :return)))
      (should (equal 3900.0 (lr-track-presence-test--get p :last-input)))
      (should-not (plist-get p :breaks)))))

(ert-deftest lr-track-presence-return-after-sleep-is-wake-time ()
  "When the Mac woke after the previous sample, he came back at the wake time,
not at the sample before the sleep."
  (let ((p (lr-track-presence-test--run
            (lr-track-presence-test--world
             :to 6300 :input '((0 . 1800) (5990 . 6300)) :asleep '((1900 . 5990))))))
    (should (eq 'here (plist-get p :mode)))
    (should (equal 5990.0 (lr-track-presence-test--get p :return)))
    (should (equal '(1800.0 5990.0 asleep)
                   (lr-track-presence-test--away (plist-get p :last-away))))
    (should (equal '((away 1800.0 5990.0 asleep) (here 0.0 1800.0 nil))
                   (lr-track-presence-test--segs p))))
  ;; idle first, then asleep: one away, of kind asleep, back at the wake
  (let ((p (lr-track-presence-test--run
            (lr-track-presence-test--world
             :to 9300 :input '((0 . 1800) (8990 . 9300)) :asleep '((3000 . 8990))))))
    (should (eq 'here (plist-get p :mode)))
    (should (equal 8990.0 (lr-track-presence-test--get p :return)))
    (should (equal '(1800.0 8990.0 asleep)
                   (lr-track-presence-test--away (plist-get p :last-away))))
    (should (equal '((away 1800.0 8990.0 asleep) (here 0.0 1800.0 nil))
                   (lr-track-presence-test--segs p)))))


;;;; glances

(ert-deftest lr-track-presence-glance-under-3m-joins-away ()
  "A here-stretch under 3 min between two aways is merged into the away.
He leaves at 1800, comes back at 3600 for a 100 s glance, and leaves again:
one away from 1800, not two."
  (let ((samples (lr-track-presence-test--world
                  :to 6100 :input '((0 . 1800) (3610 . 3700) (6010 . 6100)))))
    ;; at first the glance is a real return
    (let ((p (lr-track-presence-test--upto samples 3690)))
      (should (eq 'here (plist-get p :mode)))
      (should (equal 3600.0 (lr-track-presence-test--get p :return)))
      (should (equal '(1800.0 3600.0 idle)
                     (lr-track-presence-test--away (plist-get p :last-away)))))
    ;; 15 min later the away starts, and the glance joins the first away
    (let ((p (lr-track-presence-test--upto samples 4700)))
      (should (eq 'away (plist-get p :mode)))
      (should (equal 1800.0 (lr-track-presence-test--get p :away-from)))
      (should (eq 'idle (plist-get p :away-kind)))
      (should-not (plist-get p :last-away))
      (should-not (plist-get p :return))
      (should (equal '((here 0.0 1800.0 nil)) (lr-track-presence-test--segs p)))
      (should (= lr-track-presence-test--t0 (lr-track--presence-here-since p))))
    ;; the real return closes ONE away from 1800
    (let ((p (lr-track-presence-test--run samples)))
      (should (eq 'here (plist-get p :mode)))
      (should (equal 6000.0 (lr-track-presence-test--get p :return)))
      (should (equal '(1800.0 6000.0 idle)
                     (lr-track-presence-test--away (plist-get p :last-away))))
      (should (equal '((away 1800.0 6000.0 idle) (here 0.0 1800.0 nil))
                     (lr-track-presence-test--segs p))))))

(ert-deftest lr-track-presence-here-over-3m-is-not-a-glance ()
  "A here-stretch of 3 min or more between two aways stays its own block."
  (let ((p (lr-track-presence-test--run
            (lr-track-presence-test--world
             :to 4900 :input '((0 . 1800) (3610 . 3910))))))
    (should (eq 'away (plist-get p :mode)))
    (should (equal 3910.0 (lr-track-presence-test--get p :away-from)))
    (should (eq 'idle (plist-get p :away-kind)))
    (should (equal '(1800.0 3600.0 idle)
                   (lr-track-presence-test--away (plist-get p :last-away))))
    (should (equal '((here 3600.0 3910.0 nil)
                     (away 1800.0 3600.0 idle)
                     (here 0.0 1800.0 nil))
                   (lr-track-presence-test--segs p)))))

(ert-deftest lr-track-presence-glance-keeps-night-one-away ()
  "The 3 a.m. glance stays inside the night.
Evening work until 22:00 (14400 s), screen locked for the night, a 90 s
look at 3:00 that ends unlocked, the screen locks again, he is back at 7:00.
The night is ONE away of kind `locked' (the glance keeps the night's kind
even though it ended without a lock)."
  (let ((samples (lr-track-presence-test--world
                  :to 47400
                  :input '((0 . 14400) (32390 . 32460) (46790 . 47400))
                  :locked '((14410 . 32400) (33600 . 46800)))))
    (let ((p (lr-track-presence-test--upto samples 15300)))
      (should (eq 'away (plist-get p :mode)))
      (should (equal 14400.0 (lr-track-presence-test--get p :away-from)))
      (should (eq 'locked (plist-get p :away-kind))))
    ;; the glance at first ends the night
    (let ((p (lr-track-presence-test--upto samples 32430)))
      (should (eq 'here (plist-get p :mode)))
      (should (equal 32370.0 (lr-track-presence-test--get p :return)))
      (should (equal '(14400.0 32370.0 locked)
                     (lr-track-presence-test--away (plist-get p :last-away)))))
    ;; 15 min after it, the night is whole again
    (let ((p (lr-track-presence-test--upto samples 40000)))
      (should (eq 'away (plist-get p :mode)))
      (should (equal 14400.0 (lr-track-presence-test--get p :away-from)))
      (should (eq 'locked (plist-get p :away-kind)))
      (should-not (plist-get p :last-away))
      (should-not (plist-get p :return))
      (should (equal '((here 0.0 14400.0 nil)) (lr-track-presence-test--segs p))))
    ;; morning: one away for the whole night
    (let ((p (lr-track-presence-test--run samples)))
      (should (eq 'here (plist-get p :mode)))
      (should (equal 46770.0 (lr-track-presence-test--get p :return)))
      (should (equal '(14400.0 46770.0 locked)
                     (lr-track-presence-test--away (plist-get p :last-away))))
      (should (equal '((away 14400.0 46770.0 locked) (here 0.0 14400.0 nil))
                     (lr-track-presence-test--segs p))))))


;;;; no information, retention, purity

(ert-deftest lr-track-presence-unknown-idle-changes-nothing ()
  "A sample whose idle is `unknown' carries no information: the state comes
back unchanged, whatever its lock and wake say, and the next real sample
still sees the sample before it as the previous one."
  (let* ((init (lr-track--presence-init))
         (mid (lr-track-presence-test--upto
               (lr-track-presence-test--world :to 3000 :input '((0 . 1800)))
               3000))
         (unknowns (list (list :time (lr-track-presence-test--at 5000)
                               :idle 'unknown :locked t
                               :wake (lr-track-presence-test--at 4000))
                         (list :time (lr-track-presence-test--at 5000)
                               :idle 'unknown :locked 'unknown :wake nil))))
    (should (eq 'away (plist-get mid :mode)))
    (dolist (u unknowns)
      (should (equal init (lr-track--presence-step init u)))
      (should (equal mid (lr-track--presence-step mid u)))))
  ;; the break after an unknown sample ends at the last KNOWN sample
  (let* ((input '((0 . 1800) (2110 . 2200)))
         (p (lr-track-presence-test--run
             (append (lr-track-presence-test--world :to 2070 :input input)
                     (list (lr-track-presence-test--sample 2100 'unknown))
                     (lr-track-presence-test--world :from 2130 :to 2130 :input input)))))
    (should (equal '((1800.0 2070.0)) (lr-track-presence-test--breaks p)))))

(ert-deftest lr-track-presence-prunes-breaks-and-segments ()
  "Breaks are kept 3 h and segments 36 h; older ones are dropped."
  ;; breaks: one at 600 to 990, one at 11000 to 11370
  (let ((samples (lr-track-presence-test--world
                  :to 12000 :input '((0 . 600) (1000 . 11000) (11400 . 12000)))))
    (should (equal '((600.0 990.0))
                   (lr-track-presence-test--breaks
                    (lr-track-presence-test--upto samples 11300))))
    (should (equal '((11000.0 11370.0))
                   (lr-track-presence-test--breaks
                    (lr-track-presence-test--run samples)))))
  ;; segments: an away at 1800 to 3600, then 36 h of steady input
  (let ((samples (lr-track-presence-test--world
                  :to 134000 :input '((0 . 1800) (3610 . 134000)))))
    (should (equal '((away 1800.0 3600.0 idle) (here 0.0 1800.0 nil))
                   (lr-track-presence-test--segs
                    (lr-track-presence-test--upto samples 129000))))
    (should-not (plist-get (lr-track-presence-test--run samples) :segments))))

(ert-deftest lr-track-presence-step-is-pure ()
  "The step never mutates its state or its sample, gives the same answer twice,
and never reads the clock.  Breaks, aways, returns, a glance, a lock, a sleep
and an unknown sample all go through it."
  (let ((clock-reads 0)
        (days (list (lr-track-presence-test--world
                     :to 3900 :input '((0 . 600) (1000 . 1800) (3610 . 3900)))
                    (lr-track-presence-test--world
                     :to 6100 :input '((0 . 1800) (3610 . 3700) (6010 . 6100)))
                    (lr-track-presence-test--world
                     :to 4300 :input '((0 . 1800) (3990 . 4300))
                     :locked '((1810 . 4000)))
                    (lr-track-presence-test--world
                     :to 9300 :input '((0 . 1800) (8990 . 9300))
                     :asleep '((3000 . 8990))))))
    (cl-letf (((symbol-function 'float-time)
               (lambda (&rest _) (cl-incf clock-reads) 0.0)))
      (dolist (samples days)
        (let ((p (lr-track--presence-init)))
          (dolist (x (append samples
                             (list (lr-track-presence-test--sample 99999 'unknown t))))
            (let* ((p-before (copy-tree p))
                   (x-before (copy-tree x))
                   (next (lr-track--presence-step p x)))
              (should (equal p-before p))
              (should (equal x-before x))
              (should (equal next (lr-track--presence-step p x)))
              (setq p next))))))
    (should (= 0 clock-reads))))

(ert-deftest lr-track-presence-here-since ()
  "Here-since is the return when there is one, else the first sample time.
Away-p is true only in mode `away'."
  (should (= 500.0 (lr-track--presence-here-since
                    (lr-track-presence-test--state :mode 'here :first-time 100.0
                                                   :return 500.0))))
  (should (= 100.0 (lr-track--presence-here-since
                    (lr-track-presence-test--state :mode 'here :first-time 100.0
                                                   :return nil))))
  (should (lr-track--presence-away-p (lr-track-presence-test--state :mode 'away)))
  (should-not (lr-track--presence-away-p (lr-track-presence-test--state :mode 'here)))
  (should-not (lr-track--presence-away-p (lr-track--presence-init)))
  ;; through the step: the first sample, then the return
  (let ((samples (lr-track-presence-test--world
                  :to 3700 :input '((0 . 1800) (3610 . 3700)))))
    (should (= lr-track-presence-test--t0
               (lr-track--presence-here-since
                (lr-track-presence-test--upto samples 1800))))
    (should (= (lr-track-presence-test--at 3600)
               (lr-track--presence-here-since
                (lr-track-presence-test--run samples))))))

(ert-deftest lr-track-presence-segments-never-run-backwards ()
  "Emacs starts while he is already idle (the first sample's last input is
before the sample) and he leaves without touching anything: the here segment
ends at that last input and so starts there too, never after its end."
  (let* ((p (lr-track-presence-test--run
             (list (lr-track-presence-test--sample 0 100)
                   (lr-track-presence-test--sample 900 1000))))
         (here (car (plist-get p :segments))))
    (should (lr-track--presence-away-p p))
    (should (eq (plist-get here :kind) 'here))
    (should (= (plist-get here :to) (lr-track-presence-test--at -100)))
    (should (<= (plist-get here :from) (plist-get here :to)))))


;;;; the module itself

(ert-deftest lr-track-presence-module-loads-standalone ()
  "`lr-track-presence' is pure with no deps: in a bare Emacs it loads without
lr-track or org, and parses and steps on its own."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (form `(progn
                  (require 'lr-track-presence)
                  (prin1 (list (featurep 'lr-track) (featurep 'org)
                               (plist-get (lr-track--parse-probe
                                           ,lr-track-presence-test--real-output)
                                          :wake)
                               (plist-get (lr-track--presence-step
                                           (lr-track--presence-init)
                                           '(:time 100.0 :idle 10.0 :locked nil :wake nil))
                                          :last-input)))))
         (err-file (make-temp-file "lr-track-presence-standalone" nil ".err"))
         (want '(nil nil 1791033656.0 90.0))
         (out (with-temp-buffer
                (call-process emacs nil (list t err-file) nil "-Q" "--batch"
                              "-L" lr-track-presence-test--modules
                              "--eval" (prin1-to-string form))
                (buffer-string)))
         (got (condition-case nil (car (read-from-string out)) (error nil)))
         (err (with-temp-buffer (insert-file-contents err-file) (buffer-string))))
    (ignore-errors (delete-file err-file))
    ;; on failure, show what the bare Emacs printed
    (should (equal want (if (equal want got) got (list :stdout out :stderr err))))))

(ert-deftest lr-track-presence-source-ascii-lexical-no-arrows ()
  "The module is lexical-binding, ASCII only, and has no arrows."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "lr-track-presence.el"
                                            lr-track-presence-test--modules))
    (goto-char (point-min))
    (should (string-match-p "lexical-binding: *t"
                            (buffer-substring (point-min) (line-end-position))))
    (should (cl-every (lambda (c) (< c 128)) (buffer-string)))
    ;; built from two chars so this file holds no arrow either
    (should-not (search-forward (string ?- ?>) nil t))))


;;;; regressions from the round 1 review

(ert-deftest lr-track-regression-presence-clock-step-back-reanchors ()
  "One sample dated a day ahead (a bogus wall clock), then the corrected
ones: presence must follow the corrected samples at once, not ignore every
sample until real time passes the bogus one.  A step back under 2 min is
still only a stale sample."
  (let* ((p (lr-track-presence-test--run
             (list (lr-track-presence-test--sample 0 5)
                   (lr-track-presence-test--sample 30 5)
                   (lr-track-presence-test--sample 86400 5))))
         (fixed (lr-track-presence-test--run
                 (list (lr-track-presence-test--sample 60 4)
                       (lr-track-presence-test--sample 90 4))
                 p)))
    (should (= (lr-track-presence-test--at 90) (plist-get fixed :prev-time)))
    (should (= (lr-track-presence-test--at 86) (plist-get fixed :last-input)))
    (should (eq 'here (plist-get fixed :mode))))
  (let* ((p (lr-track-presence-test--run
             (list (lr-track-presence-test--sample 0 5)
                   (lr-track-presence-test--sample 300 5))))
         (stale (lr-track--presence-step p (lr-track-presence-test--sample 200 1))))
    (should (equal p stale))))

;;;; regressions from the round 2 review

(ert-deftest lr-track-regression-presence-step-back-keeps-history ()
  "He was away 30 min and has been back 3 min when the wall clock steps back
3 min.  Presence follows the new clock at once (mode, last input, previous
sample from the new sample) and keeps its history: the return, the away he
came back from and the segments.  A due Now? stays due, and the away can
still be labelled.  Anything the old clock dated after the new sample is
dated at it."
  (let* ((input '((0 . 1800) (3610 . 3780)))
         (p (lr-track-presence-test--run
             (lr-track-presence-test--world :to 3780 :input input)))
         ;; the clock steps back 3 min: the next sample reads 3600
         (q (lr-track--presence-step p (lr-track-presence-test--sample 3600 0))))
    (should (eq 'here (plist-get p :mode)))
    (should (equal 3600.0 (lr-track-presence-test--get p :return)))
    (should (eq 'here (plist-get q :mode)))
    (should (equal 3600.0 (lr-track-presence-test--get q :prev-time)))
    (should (equal 3600.0 (lr-track-presence-test--get q :last-input)))
    (should (equal 3600.0 (lr-track-presence-test--get q :return)))
    (should (equal '(1800.0 3600.0 idle)
                   (lr-track-presence-test--away (plist-get q :last-away))))
    (should (equal '((away 1800.0 3600.0 idle) (here 0.0 1800.0 nil))
                   (lr-track-presence-test--segs q)))
    (should (equal 0.0 (lr-track-presence-test--get q :first-time)))
    ;; and it goes on from there: 10 min on, still the same return
    (let ((r (lr-track-presence-test--run
              (list (lr-track-presence-test--sample 3630 2)
                    (lr-track-presence-test--sample 4200 2))
              q)))
      (should (eq 'here (plist-get r :mode)))
      (should (equal 3600.0 (lr-track-presence-test--get r :return)))
      (should (equal 4198.0 (lr-track-presence-test--get r :last-input)))))
  ;; a step back of 10 min within the return: the return is dated at the
  ;; new sample, never left in the future
  (let* ((p (lr-track-presence-test--run
             (lr-track-presence-test--world :to 3660 :input '((0 . 1800) (3610 . 3660)))))
         (q (lr-track--presence-step p (lr-track-presence-test--sample 3100 1))))
    (should (equal 3100.0 (lr-track-presence-test--get q :return)))
    (should (equal '(1800.0 3100.0 idle)
                   (lr-track-presence-test--away (plist-get q :last-away)))))
  ;; away when the clock steps back: still away, of the same kind, from his
  ;; last input as the new clock reads it
  (let* ((p (lr-track-presence-test--run
             (lr-track-presence-test--world :to 3000 :input '((0 . 1800))
                                            :locked '((1810 . 9000)))))
         (q (lr-track--presence-step p (lr-track-presence-test--sample 2700 1000 t))))
    (should (eq 'away (plist-get p :mode)))
    (should (eq 'locked (plist-get p :away-kind)))
    (should (eq 'away (plist-get q :mode)))
    (should (eq 'locked (plist-get q :away-kind)))
    (should (equal 1700.0 (lr-track-presence-test--get q :away-from)))))

(ert-deftest lr-track-regression-presence-wake-after-sample-time ()
  "kern.waketime is stamped at the wake; a wall clock stepped back after it
leaves the wake later than every sample for a while.  That wake is not a new
sleep at each of those samples, nor when the clock catches up with it, and
a return is never dated after the sample that saw it."
  ;; the wake is seen right after it, then the clock steps back 10 min
  (let* ((w 7200)
         (p (lr-track-presence-test--run
             (append (cl-loop for s from 0 to 3600 by 60
                              collect (lr-track-presence-test--sample s 2 nil -86400))
                     (list (lr-track-presence-test--sample (+ w 20) 3 nil w)))))
         (gone (plist-get p :last-away)))
    (should (eq 'asleep (plist-get gone :kind)))
    (dotimes (k 31)
      (setq p (lr-track--presence-step
               p (lr-track-presence-test--sample (+ w 40 -600 (* 30 k)) 2 nil w))))
    (should (eq 'here (plist-get p :mode)))
    ;; the one night, not a fresh away per sample or at the catch-up
    (should (= 1 (seq-count (lambda (g) (eq (plist-get g :kind) 'away))
                            (plist-get p :segments))))
    (should (<= (plist-get p :return) (plist-get p :prev-time))))
  ;; the first sample after the wake is already on the stepped-back clock:
  ;; the sleep still counts, and he is back at that sample, not at the wake
  (let* ((w 7200)
         (p (lr-track-presence-test--run
             (append (cl-loop for s from 0 to 3600 by 60
                              collect (lr-track-presence-test--sample s 2 nil -86400))
                     (list (lr-track-presence-test--sample (- w 560) 3 nil w))))))
    (should (eq 'here (plist-get p :mode)))
    (should (equal '(3598.0 6640.0 asleep)
                   (lr-track-presence-test--away (plist-get p :last-away))))
    (should (equal 6640.0 (lr-track-presence-test--get p :return)))))

;;;; regressions from the round 3 review

(ert-deftest lr-track-regression-reanchor-keeps-running-away ()
  "A wall-clock step back of 3 min or more lands on the sample that ends a
locked lunch.  The away must not vanish: an unlocked sample with input ends
it at T - I on the new clock (never early), recorded as a return, so Now?
comes due and the lunch can be labelled.  A locked sample (the password
being typed) keeps the away going, and never dates his input."
  (let* ((lunch (append
                 (cl-loop for s from 0 to 10800 by 30
                          collect (lr-track-presence-test--sample s 2))
                 (cl-loop for s from 10860 to 12300 by 60
                          collect (lr-track-presence-test--sample
                                   s (- s 10798) t))))
         (p (lr-track-presence-test--run lunch)))
    (should (eq 'away (plist-get p :mode)))
    (should (equal 10798.0 (lr-track-presence-test--get p :away-from)))
    (should (eq 'locked (plist-get p :away-kind)))
    ;; the return sample, unlocked with input 20 s ago, read 200 s early
    (let ((q (lr-track--presence-step
              p (lr-track-presence-test--sample (- 12360 200) 20))))
      (should (eq 'here (plist-get q :mode)))
      (should (equal 12140.0 (lr-track-presence-test--get q :last-input)))
      (should (equal 12140.0 (lr-track-presence-test--get q :return)))
      (should (equal '(10798.0 12140.0 locked)
                     (lr-track-presence-test--away (plist-get q :last-away))))
      (should (equal '(away 10798.0 12140.0 locked)
                     (car (lr-track-presence-test--segs q)))))
    ;; the password sample, locked with 2 s of idle, read 200 s early
    (let ((q (lr-track--presence-step
              p (lr-track-presence-test--sample (- 12360 200) 2 t))))
      (should (eq 'away (plist-get q :mode)))
      (should (equal 10798.0 (lr-track-presence-test--get q :away-from)))
      (should (eq 'locked (plist-get q :away-kind)))
      (should (equal 10798.0 (lr-track-presence-test--get q :last-input)))
      ;; 30 s on he types, unlocked: the away ends there, as usual
      (let ((r (lr-track--presence-step
                q (lr-track-presence-test--sample (+ (- 12360 200) 30) 5))))
        (should (eq 'here (plist-get r :mode)))
        (should (equal 12160.0 (lr-track-presence-test--get r :return)))
        (should (equal '(10798.0 12160.0 locked)
                       (lr-track-presence-test--away (plist-get r :last-away))))))))

(ert-deftest lr-track-regression-held-glance-not-merged ()
  "A here-stretch under 3 min is a glance and joins the night, unless a line
started in it (:held is its return, set at a clock-in): then the night is
split at his line, and the away after it is its own, to be labelled."
  (let* ((samples (lr-track-presence-test--world
                   :to 47400
                   :input '((0 . 14400) (32390 . 32460) (46790 . 47400))
                   :locked '((14410 . 32400) (33600 . 46800))))
         (glance (lr-track-presence-test--upto samples 32430)))
    (should (equal 32370.0 (lr-track-presence-test--get glance :return)))
    ;; he answered during the glance: the clock-in marks the block held
    (let* ((held (plist-put (copy-sequence glance) :held
                            (plist-get glance :return)))
           (rest (cl-remove-if (lambda (x) (<= (plist-get x :time)
                                               (lr-track-presence-test--at 32430)))
                               samples))
           (p (lr-track-presence-test--run rest held)))
      (should (eq 'here (plist-get p :mode)))
      ;; no input 15 min after the glance, before the lock came: idle
      (should (equal '(32460.0 46770.0 idle)
                     (lr-track-presence-test--away (plist-get p :last-away))))
      (should (equal '(14400.0 32370.0 locked)
                     (lr-track-presence-test--away (plist-get p :prev-last-away))))
      (should (equal '((away 32460.0 46770.0 idle)
                       (here 32370.0 32460.0 nil)
                       (away 14400.0 32370.0 locked)
                       (here 0.0 14400.0 nil))
                     (lr-track-presence-test--segs p))))
    ;; unheld, the same night stays one away
    (let ((p (lr-track-presence-test--run samples)))
      (should (equal '(14400.0 46770.0 locked)
                     (lr-track-presence-test--away (plist-get p :last-away)))))))

;;;; regressions from the round 3.3 review

(ert-deftest lr-track-regression-glance-never-joins-unseen-away ()
  "Nothing saw 1:00 to 1:39 after the epoch (the probe failed): an `unseen'
stretch.  He is seen at the Mac for 2 min, locks, and every sample sees the
lock until 2:20.  That lock is an away the samples watched: it stays its own
away, kind `locked', and can be labelled.  Merged into the unseen stretch as
a glance, it would read as not seen, and never could be."
  (let* ((before (lr-track-presence-test--world
                  :from 0 :to 3600 :input '((0 . 3600))))
         ;; what the impure side steps for the stretch no sample saw
         ;; (`lr-track--blind-sample'): just before the input after it
         (blind (list :time (lr-track-presence-test--at 5939) :idle 2339.0
                      :locked nil :wake (lr-track-presence-test--at -86400)
                      :blind t))
         (after (lr-track-presence-test--world
                 :from 5940 :to 9000
                 :input '((0 . 3600) (5940 . 6060) (8400 . 9000))
                 :locked '((6090 . 8400))))
         (p (lr-track-presence-test--run (append before (list blind) after))))
    (should (eq 'here (plist-get p :mode)))
    (should (equal '(6060.0 8370.0 locked)
                   (lr-track-presence-test--away (plist-get p :last-away))))
    (should (equal '(3600.0 5939.0 unseen)
                   (lr-track-presence-test--away (plist-get p :prev-last-away))))))

(provide 'lr-track-presence-test)
;;; lr-track-presence-test.el ends here
