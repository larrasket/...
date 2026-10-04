;;; lr-track-presence.el --- Presence from the lock, HID idle and wake -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Whether he is at this Mac, and nothing more.  Presence is the ONLY thing
;; lr-track senses: the screen lock, the HID idle time, and kern.waketime (did
;; the Mac sleep since the last look).  Never what he did: no app, window,
;; buffer or process text gets past `lr-track--parse-probe' (I16).
;;
;; Two pure pieces with no deps beyond cl-lib, so this file loads without org
;; or lr-track.  lr-track.el requires it and owns everything impure: the probe
;; process, the tick, `lr-track--presence', state.eld.
;;
;;   - `lr-track--parse-probe' reads the probe's three lines into
;;     (:idle IDLE :locked LOCKED :wake WAKE).  A failure is `unknown' (idle,
;;     lock) or nil (wake), never 0: a 0 idle reads as input right now and
;;     would make every away unreachable.
;;   - `lr-track--presence-step' folds one sample into the presence state and
;;     returns the next state.  It never mutates and never reads the clock, so
;;     a scripted day replays exactly.
;;
;; THE MODEL (spec v3 3.1)
;;   last input L  the sample time minus HID idle, while unlocked.  Typing the
;;                 password on the lock screen is not his input.
;;   break         an input gap of 3 to 15 min, or a lock shorter than 15 min.
;;                 It stays inside the here-block; a machine clock bridges it.
;;   away          15 min without input (kind `idle'), a lock that passes
;;                 15 min (`locked'), or a wake after the previous sample
;;                 (`asleep', which wins over the other two).  It starts at
;;                 L, never at the sample that noticed it, so a late tick
;;                 (App Nap) never moves a departure.  A stretch of 15 min
;;                 or more that no sample saw (no tick ran, or the probe
;;                 failed) is kind `unseen': its sample says :blind, and
;;                 nothing says he was gone, only that nothing saw him.
;;   return R      the first input after an away, dated at the previous
;;                 sample (the last moment he was surely still gone, at most
;;                 one tick early), or at the wake when the Mac slept.
;;   glance        a here-stretch under 3 min between two aways.  It joins
;;                 the away, so the 3 a.m. glance stays inside the night,
;;                 unless a line started in it or an answer was written in
;;                 it (:held), or the away before it is `unseen'.
;;
;; The state keeps the current here-block's breaks (3 h) and the closed here
;; and away segments (36 h), newest first.  Nothing here is written to disk.

;;; Code:

(require 'cl-lib)

;;;; thresholds

(defconst lr-track-presence-break-seconds 180.0
  "An input gap this long or longer, short of an away, is a break.")

(defconst lr-track-presence-away-seconds 900.0
  "An input gap or a lock this long or longer is an away.")

(defconst lr-track-presence-glance-seconds 180.0
  "A here-stretch shorter than this between two aways joins the away.")

(defconst lr-track-presence-keep-breaks-seconds 10800.0
  "Breaks that ended longer ago than this (3 h) are dropped.")

(defconst lr-track-presence-keep-segments-seconds 129600.0
  "Segments that ended longer ago than this (36 h) are dropped.")

(defconst lr-track-presence-clock-step-seconds 120.0
  "A sample this far or further before the previous one means the wall clock
was stepped back (or the previous one was dated in a bogus future): presence
re-anchors on that sample instead of waiting for time to catch up.")

;;;; probe parsing

(defconst lr-track--hid-idle-max-seconds 2592000.0
  "30-day sanity bound on parsed HID idle.")

(defun lr-track--parse-hid-idle (output)
  "Parse HIDIdleTime (nanoseconds) out of ioreg OUTPUT.
Return seconds or `unknown'.  Every failure mode returns `unknown', NEVER 0
\(0 means input-now and would make `away' permanently unreachable) and NEVER a
wild value."
  (if (not (stringp output))
      'unknown
    (if (not (string-match
              "^[ \t]*\"HIDIdleTime\"[ \t]*=[ \t]*\\([0-9]+\\)[ \t]*$" output))
        'unknown
      (let ((secs (/ (string-to-number (match-string 1 output)) 1e9)))
        (if (and (>= secs 0.0) (< secs lr-track--hid-idle-max-seconds)) secs 'unknown)))))

(defun lr-track--parse-probe (output)
  "Parse the presence probe OUTPUT into (:idle IDLE :locked LOCKED :wake WAKE).
IDLE is HID idle seconds by `lr-track--parse-hid-idle', or `unknown'.  LOCKED
is t for an IOConsoleLocked of Yes, nil for No, `unknown' when the line is
missing or garbled.  WAKE is kern.waketime's whole seconds as a float, or nil.
Those three keys are ALL that survive: no other line or field of the output,
whatever it carries, gets into the result (I16)."
  (if (not (stringp output))
      (list :idle 'unknown :locked 'unknown :wake nil)
    (let ((idle (lr-track--parse-hid-idle output))
          (case-fold-search nil))
      (list :idle idle
            :locked (if (string-match
                         "^[ \t]*\"IOConsoleLocked\"[ \t]*=[ \t]*\\(Yes\\|No\\)[ \t]*$"
                         output)
                        (string= (match-string 1 output) "Yes")
                      'unknown)
            ;; `{ sec = N, usec = U } DATE': anchored on the brace, so the
            ;; usec field can never be read as the seconds
            :wake (and (string-match "^[ \t]*{[ \t]*sec[ \t]*=[ \t]*\\([0-9]+\\)"
                                     output)
                       (float (string-to-number (match-string 1 output))))))))

;;;; the presence step (pure)

(defun lr-track--presence-init ()
  "A fresh presence state: nothing seen yet, mode `unknown'.
See `lr-track--presence-step' for the fields."
  (list :mode 'unknown :last-input nil :prev-time nil :wake nil :first-time nil
        :away-from nil :away-kind nil :saw-lock nil :return nil
        :last-away nil :breaks nil :segments nil))

(defun lr-track--presence-here-since (p)
  "Start of P's current here-block: the last return, else the first sample."
  (or (plist-get p :return) (plist-get p :first-time)))

(defun lr-track--presence-away-p (p)
  "Non-nil when presence state P is away."
  (eq (plist-get p :mode) 'away))

(defun lr-track--presence-prune (items since)
  "ITEMS (plists with a `:to') without those that ended before SINCE."
  (cl-remove-if (lambda (x)
                  (let ((to (plist-get x :to)))
                    (and (numberp to) (< to since))))
                items))

(defun lr-track--presence-leave (q kind)
  "Start an away of KIND in Q, a here state the caller owns, and return Q.
The away starts at the last input L and closes the here segment there.  When
the here-stretch since the last return is shorter than
`lr-track-presence-glance-seconds', it was a glance: drop it and the away
before it, and continue that away instead (its start and its kind), with the
last away and the return put back to the ones before it.  Not when the
here-stretch is held (:held is its return): a line started in it, or he
answered in it, so it splits the away, and the away after it can be
labelled on its own.  Nor when the away before it is `unseen': no sample
saw him leave then, and the away now starting was seen, so it stays its
own, of its own kind, and can be labelled; the unseen stretch never reads
as an away he took."
  (let* ((l (plist-get q :last-input))
         (r (plist-get q :return))
         (last (plist-get q :last-away))
         (to (plist-get last :to))
         (segs (plist-get q :segments)))
    (if (and (numberp l) (numberp r) (numberp to) (= to r)
             (not (eql (plist-get q :held) r))
             (not (eq (plist-get last :kind) 'unseen))
             (< (- l r) lr-track-presence-glance-seconds))
        (let ((top (car segs)))
          ;; the away segment the return pushed is still on top
          (when (and (eq (plist-get top :kind) 'away)
                     (equal (plist-get top :to) r))
            (setq segs (cdr segs)))
          (setq q (plist-put q :segments segs)
                q (plist-put q :away-from (plist-get last :from))
                q (plist-put q :away-kind (plist-get last :kind))
                q (plist-put q :last-away (plist-get q :prev-last-away))
                q (plist-put q :return (plist-get q :prev-return))
                ;; the ones before those are not kept; the next return sets
                ;; both again before another glance can need them
                q (plist-put q :prev-last-away nil)
                q (plist-put q :prev-return nil)))
      (setq q (plist-put q :segments
                         (cons (list :kind 'here
                                     ;; never after L: with no input since
                                     ;; the first sample, L is before it
                                     :from (min l (or r (plist-get q :first-time)
                                                      l))
                                     :to l :detail nil)
                               segs))
            q (plist-put q :away-from l)
            q (plist-put q :away-kind kind)))
    (plist-put q :mode 'away)))

(defun lr-track--presence-reanchor (p sample)
  "P after the wall clock stepped back to SAMPLE's time; P is not mutated.
What says where he is now starts over from SAMPLE, as a first sample sets it
\(rule 2): the mode, the last input (T - I holds on any clock, since HID idle
is a span), the previous sample and the wake.  The history is carried: the
first time, the return, the latest away and the two a glance needs, the
segments and the held return, so a due Now? stays due and that away can
still be labelled.  A carried time past SAMPLE's is dated at it: the old
clock may have been ahead.

A locked sample never moves his last input later (the password is typed
there): it stays P's, or T - I when that is earlier.  An away P was in goes
on across the step: a locked sample, or 15 min or more of idle, keeps it,
with its start and its kind; input in an unlocked sample ends it at T - I,
the latest that input can be dated on the new time line, and records it as
rule 4 does."
  (let* ((now (plist-get sample :time))
         (idle (plist-get sample :idle))
         (locked (eq (plist-get sample :locked) t))
         (q (lr-track--presence-step (lr-track--presence-init) sample))
         (clamp (lambda (x) (if (numberp x) (min x now) x)))
         (span (lambda (a)
                 (and a (let ((b (copy-sequence a)))
                          (setq b (plist-put b :from
                                             (funcall clamp (plist-get b :from))))
                          (plist-put b :to (funcall clamp (plist-get b :to)))))))
         ;; HID idle is a span, so T - I dates the last HID event on the new
         ;; time line; on a locked sample that may be the password, so it
         ;; only ever bounds a carried time from above, never sets one later
         (input (- now idle))
         (last (let ((l (funcall clamp (plist-get p :last-input))))
                 (if (numberp l) (min l input) input)))
         (from (let ((f (funcall clamp (plist-get p :away-from))))
                 (if (numberp f) (min f input) last))))
    (when (numberp (plist-get p :first-time))
      (setq q (plist-put q :first-time
                         (min (plist-get p :first-time)
                              (or (plist-get q :first-time) now)))))
    (setq q (plist-put q :return (funcall clamp (plist-get p :return))))
    (setq q (plist-put q :last-away (funcall span (plist-get p :last-away))))
    (setq q (plist-put q :segments
                       (mapcar span (plist-get p :segments))))
    (dolist (k '(:prev-last-away :prev-return :held))
      (when (plist-member p k)
        (setq q (plist-put q k (if (eq k :prev-last-away)
                                   (funcall span (plist-get p k))
                                 (funcall clamp (plist-get p k)))))))
    (cond
     ;; an away goes on: locked, or still no input
     ((and (eq (plist-get p :mode) 'away)
           (or locked (>= idle lr-track-presence-away-seconds)))
      (setq q (plist-put q :mode 'away)
            q (plist-put q :away-from from)
            q (plist-put q :away-kind (or (plist-get p :away-kind)
                                          (if locked 'locked 'idle)))
            q (plist-put q :last-input (min last from))
            q (plist-put q :saw-lock (or locked (plist-get p :saw-lock)))))
     ;; his input ends the away, never early: at T - I
     ((eq (plist-get p :mode) 'away)
      (let* ((r (max from input))
             (away (list :from from :to r
                         :kind (or (plist-get p :away-kind) 'idle))))
        (setq q (plist-put q :segments
                           (cons (list :kind 'away :from from :to r
                                       :detail (plist-get away :kind))
                                 (plist-get q :segments)))
              q (plist-put q :prev-last-away (plist-get q :last-away))
              q (plist-put q :prev-return (plist-get q :return))
              q (plist-put q :last-away away)
              q (plist-put q :return r)
              q (plist-put q :breaks nil)
              q (plist-put q :mode 'here)
              q (plist-put q :away-from nil)
              q (plist-put q :away-kind nil)
              q (plist-put q :last-input input)
              q (plist-put q :saw-lock nil))))
     ;; here, and a locked sample: the lock screen's idle is not his input
     (locked
      (setq q (plist-put q :mode 'here)
            q (plist-put q :away-from nil)
            q (plist-put q :away-kind nil)
            q (plist-put q :last-input last)
            q (plist-put q :saw-lock t))))
    q))

(defun lr-track--presence-step (p sample)
  "Return the presence state after SAMPLE, from state P.  P is never mutated.
SAMPLE is (:time T :idle I :locked K :wake W): T the time it was taken, I HID
idle seconds or `unknown', K t, nil or `unknown' (read as nil), W the latest
kern.waketime or nil.  A sample the impure side made up for a stretch no
sample saw also says :blind t.  The state, from `lr-track--presence-init':

  :mode        `here', `away', or `unknown' before the first usable sample
  :last-input  L, the latest real input (T - I, only while unlocked)
  :prev-time   the previous usable sample;  :first-time  the first one
  :wake        the latest wake seen
  :away-from   start of the current away (L when input stopped), else nil
  :away-kind   `locked', `idle', `asleep' or `unseen'
  :saw-lock    non-nil when a locked sample was seen since the last input
  :return      R, start of the current here-block, nil before any return
  :last-away   the latest completed away (:from F :to F :kind K)
  :breaks      breaks in the current here-block, newest first (:from :to)
  :segments    closed segments, newest first
               (:kind here or away :from F :to F :detail KIND or nil)
  :prev-last-away, :prev-return  the ones before, set at each return, for
               the glance (absent until the first return)
  :held        a return whose here-block a line started in, or an answer
               was written in (set by the impure side); that block is
               never a glance

The rules, in order:
 1. An `unknown' idle carries no information: P comes back as it is.  So
    does a sample that is not after the previous one, unless it is
    `lr-track-presence-clock-step-seconds' or more before it: the wall
    clock moved, and presence re-anchors on this sample, keeping its
    history (`lr-track--presence-reanchor').
 2. The first usable sample sets L = T - I and the first time, and starts
    here, or away (kind `idle', `locked' when K) when I is 15 min or more.
 3. A wake after the previous sample: the Mac slept, an away of kind
    `asleep'.  It starts at L when he was here (a glance joins the away
    before it, as in 5), and the kind wins over a running away's kind.  A
    wake later than T (the wall clock stepped back after it) counts as at
    T, and a wake already seen is never a new one.
 4. New input while unlocked (T - I more than 1 s past L): an away ends at
    R, the latest of the previous sample, the wake (at most T) when it is
    after that sample, and the away's start; in a here-block, a gap of
    3 min or more at the previous sample is a break from L to that sample.
    Then L moves to T - I and the lock mark clears.
 5. Here, and 15 min since L: an away starts at L, of kind `unseen' when
    the sample is :blind (nothing saw the stretch), else `locked' when K or
    a lock was seen since L, else `idle'.  A here-stretch under 3 min since
    the last return was a glance and joins the away before it, unless
    that away is `unseen' (`lr-track--presence-leave').
 6. A locked sample marks the lock as seen, until the next input.
 7. The previous time becomes T; a numeric W becomes the wake.
 8. Breaks that ended over 3 h and segments that ended over 36 h before T
    are dropped."
  (let ((now (plist-get sample :time))
        (idle (plist-get sample :idle))
        (prev (plist-get p :prev-time)))
    (cond
     ((or (not (numberp now)) (not (numberp idle))) p)
     ;; the wall clock stepped back by minutes, or the previous sample was
     ;; dated in a bogus future: everything in P is on another time line,
     ;; and waiting for time to pass it would freeze presence that long
     ((and (numberp prev)
           (<= now (- prev lr-track-presence-clock-step-seconds)))
      (lr-track--presence-reanchor p sample))
     ;; a stale or duplicate sample, or a small step back: nothing new, and
     ;; the state must never run backwards
     ((and (numberp prev) (<= now prev)) p)
     (t
      (let* ((locked (eq (plist-get sample :locked) t))
             (wake (plist-get sample :wake))
             ;; a wake stamped after this sample (the clock stepped back
             ;; since) counts at the sample, and one seen before is old news
             (wake-at (and (numberp wake) (min wake now)))
             (woke (and wake-at (numberp prev) (> wake-at prev)
                        (not (eql wake (plist-get p :wake)))))
             (input (- now idle))
             ;; a private copy: every write below lands on Q's own conses, and
             ;; the lists in it only ever get new conses in front
             (q (copy-sequence p)))
        (cl-flet ((val (k) (plist-get q k))
                  (put-val (k v) (setq q (plist-put q k v))))
          ;; 2. the first usable sample
          (unless (memq (val :mode) '(here away))
            (put-val :last-input input)
            (put-val :first-time now)
            (if (< idle lr-track-presence-away-seconds)
                (put-val :mode 'here)
              (put-val :mode 'away)
              (put-val :away-from input)
              (put-val :away-kind (if locked 'locked 'idle))))
          ;; 3. the Mac slept since the previous sample
          (when woke
            (when (eq (val :mode) 'here)
              (setq q (lr-track--presence-leave q 'asleep)))
            (put-val :away-kind 'asleep))
          ;; 4. new input (the password typed on the lock screen is not his)
          (let ((last (val :last-input)))
            (when (and (not locked)
                       (or (not (numberp last)) (> input (+ last 1.0))))
              (if (eq (val :mode) 'away)
                  (let* ((from (val :away-from))
                         (kind (val :away-kind))
                         (cands (delq nil (list prev (and woke wake-at) from)))
                         (r (if cands (apply #'max cands) now)))
                    (put-val :segments (cons (list :kind 'away :from from :to r
                                                   :detail kind)
                                             (val :segments)))
                    (put-val :prev-last-away (val :last-away))
                    (put-val :prev-return (val :return))
                    (put-val :last-away (list :from from :to r :kind kind))
                    (put-val :return r)
                    (put-val :breaks nil)
                    (put-val :mode 'here)
                    (put-val :away-from nil)
                    (put-val :away-kind nil))
                ;; at the previous sample he had been idle 3 min or more
                (when (and (numberp prev) (numberp last)
                           (>= (- prev last) lr-track-presence-break-seconds))
                  (put-val :breaks (cons (list :from last :to prev)
                                         (val :breaks)))))
              (put-val :last-input input)
              (put-val :saw-lock nil)))
          ;; 5. 15 min without input
          (when (and (eq (val :mode) 'here)
                     (numberp (val :last-input))
                     (>= (- now (val :last-input))
                         lr-track-presence-away-seconds))
            (setq q (lr-track--presence-leave
                     q (cond ((plist-get sample :blind) 'unseen)
                             ((or locked (val :saw-lock)) 'locked)
                             (t 'idle)))))
          ;; 6. the lock, until the next input
          (when locked (put-val :saw-lock t))
          ;; 7.
          (put-val :prev-time now)
          (when (numberp wake) (put-val :wake wake))
          ;; 8. retention
          (put-val :breaks (lr-track--presence-prune
                            (val :breaks)
                            (- now lr-track-presence-keep-breaks-seconds)))
          (put-val :segments (lr-track--presence-prune
                              (val :segments)
                              (- now lr-track-presence-keep-segments-seconds)))
          q))))))

(provide 'lr-track-presence)
;;; lr-track-presence.el ends here
