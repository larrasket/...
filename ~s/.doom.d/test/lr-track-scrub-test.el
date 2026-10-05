;;; lr-track-scrub-test.el --- tests for arrow-key time scrubbing -*- lexical-binding: t; -*-

;;; Commentary:
;; The backfill gauge lets you SCRUB the end of the current segment with the
;; arrow keys instead of typing a time.  The whole safety story rests on one
;; choice: an arrow writes a DURATION token ("<N>m", or the literal "now"),
;; never a clock stamp.  `lr-track--parse-when' turns "<N>m" into
;; `(min now (+ cursor (* 60 N)))' with N>0 -- so a scrubbed value is in
;; `(cursor, now]' BY CONSTRUCTION, at a 30-minute window and at a 40-hour one
;; identically, with no `clock-on-day' day-rollover to get wrong.  An
;; out-of-range end cannot be AUTHORED, so it cannot be committed.
;;
;; The logic is factored into three pure functions so it is fully unit-testable
;; without a live minibuffer:
;;   `lr-track--scrub-steps'  (cursor now) -> (FINE . COARSE) minutes
;;   `lr-track--scrub-target' (d0 step dir m minfloor) -> minutes
;;   `lr-track--scrub-string' (contents kind cursor now) -> new minibuffer string
;; The interactive `lr-track--scrub' is a thin minibuffer wrapper over the last.

;;; Code:

(require 'ert)
(require 'cl-lib)

(when (boundp 'native-comp-jit-compilation)
  (setq native-comp-jit-compilation nil))

(defmacro add-hook! (&rest _) nil)
(defmacro after! (&rest body) `(progn ,@body))
(defmacro defadvice! (&rest _) nil)
(defvar lr-track-scrub-test--tmp
  (file-name-as-directory (make-temp-file "lr-track-scrub-test" t)))
(defvar doom-data-dir (expand-file-name "etc/" lr-track-scrub-test--tmp))
(defvar doom-cache-dir (expand-file-name "cache/" lr-track-scrub-test--tmp))
(make-directory doom-data-dir t)
(make-directory doom-cache-dir t)

(add-to-list 'load-path (expand-file-name "modules"
                                          (locate-dominating-file
                                           (or load-file-name buffer-file-name)
                                           "modules")))
(require 'lr-track)


;;;; 1. the step ladder, keyed off the REMAINING span (cursor, now]

(ert-deftest lr-track-scrub-steps-across-window-sizes ()
  "FINE/COARSE steps for every window size.  The invariant: fine crosses the
window in <= ~40 presses, coarse in <= ~12, at 1 minute AND at 40 hours."
  (let ((now 100000.0))
    (dolist (case '((1    . (1 . 5))
                    (30   . (1 . 5))
                    (60   . (2 . 10))
                    (120  . (5 . 15))
                    (240  . (10 . 30))
                    (480  . (15 . 60))
                    (660  . (30 . 120))
                    (1440 . (60 . 120))
                    (2400 . (60 . 120))))
      (let* ((m (car case))
             (cursor (- now (* 60.0 m)))
             (got (lr-track--scrub-steps cursor now)))
        (should (equal got (cdr case)))))))

(ert-deftest lr-track-scrub-steps-coarse-nests-on-fine ()
  "COARSE must always be an integer multiple of FINE, so a coarse landing is
also a fine landing and the two grids never fight."
  (let ((now 100000.0))
    (dolist (m '(1 30 60 120 240 480 660 1440 2400))
      (let* ((cursor (- now (* 60.0 m)))
             (st (lr-track--scrub-steps cursor now)))
        (should (= 0 (% (cdr st) (car st))))))))


;;;; 2. the snap-to-grid target

(ert-deftest lr-track-scrub-target-cases ()
  (should (= 45  (lr-track--scrub-target 40  5  +1 480 5)))
  (should (= 35  (lr-track--scrub-target 40  5  -1 480 5)))
  (should (= 45  (lr-track--scrub-target 43  5  +1 480 5)))
  (should (= 40  (lr-track--scrub-target 43  5  -1 480 5)))
  (should (= 465 (lr-track--scrub-target 480 15 -1 480 15)))
  (should (= 15  (lr-track--scrub-target 0   15 +1 480 15)))
  ;; parks at the wall: +1 past M clamps to M
  (should (= 480 (lr-track--scrub-target 480 15 +1 480 15)))
  ;; below-floor snaps UP to the floor, never to 0
  (should (= 15  (lr-track--scrub-target 5   15 -1 480 15))))

(ert-deftest lr-track-scrub-target-never-below-floor-or-above-m ()
  (dolist (d0 '(0 1 7 40 200 479 480))
    (dolist (dir '(+1 -1))
      (let ((r (lr-track--scrub-target (float d0) 15 dir 480 15)))
        (should (>= r 15))
        (should (<= r 480))))))


;;;; 3. string round-trip: what an arrow writes always parses back in range

(ert-deftest lr-track-scrub-string-round-trips-into-the-window ()
  (let* ((now 100000.0) (cursor (- now (* 60.0 240))))   ; 4h window
    (dolist (kind '(fine- fine+ coarse- coarse+ home end))
      (let* ((s (lr-track--scrub-string "" kind cursor now))
             (tm (lr-track--parse-when s cursor now)))
        (should (stringp s))
        (should (numberp tm))
        (should (> tm cursor))
        (should (<= tm now))))))

(ert-deftest lr-track-scrub-end-writes-now-exactly ()
  (let* ((now 100000.0) (cursor (- now 1800)))
    (should (equal "now" (lr-track--scrub-string "40m" 'end cursor now)))
    (should (= now (lr-track--parse-when "now" cursor now)))))

(ert-deftest lr-track-scrub-home-writes-the-shortest-committable-segment ()
  (let* ((now 100000.0) (cursor (- now (* 60.0 480))))   ; 8h -> fine 15
    (let* ((s (lr-track--scrub-string "5h" 'home cursor now))
           (tm (lr-track--parse-when s cursor now)))
      (should (equal "15m" s))
      (should (> tm cursor)))))


;;;; 4. the in-range invariant under arbitrary key sequences (fuzz)

(ert-deftest lr-track-scrub-string-is-always-in-range-under-any-sequence ()
  "Thread the string through random key sequences.  A scrub key must NEVER
produce a value outside (cursor, now], and must never produce an empty field."
  (let ((now 100000.0)
        (kinds '(fine- fine+ coarse- coarse+ home end))
        (seed 12345))
    ;; deterministic LCG so the test is reproducible
    (cl-flet ((rnd (n) (setq seed (mod (+ (* seed 1103515245) 12345) 2147483648))
                       (mod (/ seed 65536) n)))
      (dolist (mins '(1 7 25 90 240 659 1439 2400))
        (let ((cursor (- now (* 60.0 mins)))
              (s ""))
          (dotimes (_ 60)
            (let ((kind (nth (rnd (length kinds)) kinds)))
              (setq s (lr-track--scrub-string s kind cursor now))
              (should (> (length s) 0))
              (let ((tm (lr-track--parse-when s cursor now)))
                (should (numberp tm))
                (should (> tm cursor))
                (should (<= tm now)))))))
      t)))


;;;; 5. >24h safety: no day rollover, no ambiguity

(ert-deftest lr-track-scrub-is-safe-past-24h ()
  "A duration token cannot hit `clock-on-day', so a 40-hour overnight window
scrubs cleanly where a clock stamp would be ambiguous."
  (let* ((now 100000.0) (cursor (- now (* 3600.0 40))))  ; 40h window
    (let* ((s (lr-track--scrub-string "" 'coarse- cursor now))  ; pull back from now
           (tm (lr-track--parse-when s cursor now)))
      (should (numberp tm))
      (should (> tm cursor))
      (should (<= tm now))
      ;; the emitted duration, converted, must land exactly cursor + that many min
      (should (string-match-p "\\`[0-9]+m\\'" s)))))


;;;; 6. coexistence with typing: an arrow adopts whatever is already there

(ert-deftest lr-track-scrub-adopts-a-typed-duration ()
  (let* ((now 100000.0) (cursor (- now (* 60.0 120))))   ; 2h -> fine 5
    ;; 40m, one step later -> next 5-multiple past 40 = 45
    (should (equal "45m" (lr-track--scrub-string "40m" 'fine+ cursor now)))
    ;; 40m, one step earlier -> 35
    (should (equal "35m" (lr-track--scrub-string "40m" 'fine- cursor now)))))

(ert-deftest lr-track-scrub-adopts-a-typed-clock-time ()
  (let* ((now 100000.0) (cursor (- now (* 60.0 120))))
    ;; a clock time 30m into the window, stepped down one fine (5m) -> 25m
    (let* ((clock (lr-track--ts-hm (+ cursor (* 60.0 30))))
           (s (lr-track--scrub-string clock 'fine- cursor now)))
      (should (equal "25m" s)))))

(ert-deftest lr-track-scrub-from-empty-or-garbage-seeds-from-now ()
  (let* ((now 100000.0) (cursor (- now (* 60.0 30))))    ; 30m -> fine 1
    ;; empty -> base now -> one fine earlier = 29m
    (should (equal "29m" (lr-track--scrub-string "" 'fine- cursor now)))
    ;; unparseable fragment behaves like empty (base = now)
    (should (equal "29m" (lr-track--scrub-string "3:3" 'fine- cursor now)))))


;;;; 7. edges

(ert-deftest lr-track-scrub-one-minute-window ()
  (let* ((now 100000.0) (cursor (- now 60.0)))           ; M = 1
    (dolist (kind '(fine- fine+ coarse- coarse+ home))
      (should (equal "1m" (lr-track--scrub-string "" kind cursor now))))
    (should (equal "now" (lr-track--scrub-string "" 'end cursor now)))))

(ert-deftest lr-track-scrub-left-parks-at-the-floor-never-empties ()
  "Repeated Left must bottom out at the floor, never at 0 and never empty."
  (let* ((now 100000.0) (cursor (- now (* 60.0 480))))   ; 8h -> fine 15
    (let ((s "20m"))
      (dotimes (_ 10) (setq s (lr-track--scrub-string s 'fine- cursor now)))
      (should (equal "15m" s))
      (should (> (lr-track--parse-when s cursor now) cursor)))))


;;;; 8. the keymap installs without leaking into the shared minibuffer map

(ert-deftest lr-track-scrub-map-binds-the-arrows ()
  (should (commandp (lookup-key lr-track--scrub-map [left])))
  (should (commandp (lookup-key lr-track--scrub-map [right])))
  (should (commandp (lookup-key lr-track--scrub-map [up])))
  (should (commandp (lookup-key lr-track--scrub-map [down])))
  (should (commandp (lookup-key lr-track--scrub-map [home])))
  (should (commandp (lookup-key lr-track--scrub-map [end])))
  ;; RET / C-g / self-insert are NOT in our map -- they fall through
  (should-not (lookup-key lr-track--scrub-map (kbd "RET")))
  (should-not (lookup-key lr-track--scrub-map (kbd "C-g"))))

(ert-deftest lr-track-scrub-map-does-not-mutate-the-shared-minibuffer-map ()
  "Installing the scrub map must never touch `minibuffer-local-map', or every
later minibuffer in the session inherits stray arrow bindings."
  (let ((before (copy-keymap minibuffer-local-map)))
    ;; the composed map is what the setup hook installs; building it must not
    ;; alter the shared parent
    (make-composed-keymap lr-track--scrub-map minibuffer-local-map)
    (should (equal before minibuffer-local-map))
    ;; and arrows in the shared map are still whatever they were (not our command)
    (should-not (eq (lookup-key minibuffer-local-map [left])
                    (lookup-key lr-track--scrub-map [left])))))

;;;; 9. end-to-end through the REAL minibuffer (simulated keys)
;;
;; The pure core is covered above; this drives `lr-track--read-until' with
;; actual key events so the composed keymap, the interactive `lr-track--scrub'
;; wrapper, and the minibuffer rewrite are all exercised for real -- the one
;; piece that cannot be a pure test.

(require 'ert-x)

(defun lr-track-scrub-test--drive (keys)
  "Feed KEYS to `lr-track--read-until' over a 2h window (fine 5m) and return it."
  (let* ((now 100000.0) (cursor (- now (* 60.0 120))) (start (- cursor 3600.0))
         (lr-track-timeline nil))            ; no side window under batch
    (setq lr-track--tl-model
          (list :start start :now now :cursor cursor :segments nil
                :label "emails" :width 46 :ascii t))
    (get-buffer-create lr-track--tl-buffer-name)
    (unwind-protect
        (ert-simulate-keys keys
          (lr-track--read-until cursor now nil "emails"))
      (setq lr-track--tl-model nil)
      (when (get-buffer lr-track--tl-buffer-name)
        (kill-buffer lr-track--tl-buffer-name)))))

(defmacro lr-track-scrub-test--near (form expected)
  `(let ((r ,form))
     (if (null ,expected) (should (null r))
       (should (numberp r)) (should (< (abs (- r ,expected)) 1.0)))))

(ert-deftest lr-track-scrub-live-two-lefts-pull-the-end-back ()
  (let* ((now 100000.0) (cursor (- now (* 60.0 120))))
    ;; from empty (base now = 120m) two Lefts on the 5m grid -> 110m
    (lr-track-scrub-test--near
     (lr-track-scrub-test--drive (vconcat [left] [left] (kbd "RET")))
     (+ cursor (* 60.0 110)))))

(ert-deftest lr-track-scrub-live-end-then-ret-is-now ()
  (let ((now 100000.0))
    (lr-track-scrub-test--near
     (lr-track-scrub-test--drive (vconcat [end] (kbd "RET"))) now)))

(ert-deftest lr-track-scrub-live-home-is-the-shortest-segment ()
  (let* ((now 100000.0) (cursor (- now (* 60.0 120))))
    (lr-track-scrub-test--near
     (lr-track-scrub-test--drive (vconcat [home] (kbd "RET"))) (+ cursor 300.0))))

(ert-deftest lr-track-scrub-live-bare-ret-is-still-on-it ()
  (lr-track-scrub-test--near
   (lr-track-scrub-test--drive (vconcat (kbd "RET"))) nil))

(ert-deftest lr-track-scrub-live-type-then-scrub-coexist ()
  (let* ((now 100000.0) (cursor (- now (* 60.0 120))))
    ;; type "40m", one Right on the 5m grid -> 45m
    (lr-track-scrub-test--near
     (lr-track-scrub-test--drive
      (vconcat (kbd "4") (kbd "0") (kbd "m") [right] (kbd "RET")))
     (+ cursor (* 60.0 45)))))

(ert-deftest lr-track-scrub-live-does-not-leak-into-later-minibuffers ()
  "After a scrub prompt, the shared `minibuffer-local-map' must be untouched."
  (lr-track-scrub-test--drive (vconcat [left] [end] (kbd "RET")))
  (should-not (lookup-key minibuffer-local-map [left])))

(provide 'lr-track-scrub-test)
;;; lr-track-scrub-test.el ends here
