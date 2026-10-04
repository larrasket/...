;;; lr-track.el --- World-class attention tracking + accountability coach -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A background coach that watches what you are actually doing and keeps your
;; clock honest.  Every tick it senses:
;;
;;   - system idle       (macOS HID, via ioreg -r)   : at the machine?
;;   - Emacs idle        (current-idle-time)          : in Emacs?
;;   - frame focus                                    : Emacs frontmost?
;;   - a wall-clock gap between ticks                 : did it sleep?
;;
;; and classifies you into one of: engaged / reading / elsewhere / away / slept.
;; "elsewhere" = at the machine but not in Emacs; "away" = not at the machine;
;; "slept" = the laptop was asleep.  The distinction between elsewhere and away
;; is the whole point of reading HID idle separately from Emacs idle.
;;
;; WHAT IT GIVES YOU
;;   - a modeline badge of your current state + minutes in it
;;   - `lr-track-status' (SPC d s): a plain-language read of what you are doing
;;   - an ORG activity log (`lr-track-activity-file'): each engaged / elsewhere /
;;     away / asleep episode is a native CLOCK line under a per-state heading, so
;;     `org-clock-report' / a clocktable answers "how engaged was I this week?"
;;   - TRUSTWORTHY CLOCKS: if you go away or the machine sleeps while a clock is
;;     running, the clock is auto-closed at the moment you actually left (idle
;;     time subtracted), never at "now"; a clock running past 4h is capped.
;;   - NUDGES (accountability coach): if you are clocked into one task but have
;;     been out of Emacs for a while, it tells you, escalating modeline to
;;     banner, with a daily interruption budget so it never spams you.
;;
;; WHERE STATE LIVES.  Facts about your work are org: the activity episodes are
;; CLOCK lines in `lr-track-activity-file'; your task time is your real task
;; clocks.  Everything else (the probe cache, the current-state, the nudge
;; budget) is in-memory only and is *meant* to be recomputed each session, so
;; this module writes NO sidecar file at all.
;;
;; The CLOCK-mutating code (auto clock-out) carries the exact defenses that make
;; the previous sidecar prototype's clock honest: a 60s floor and whole-interval
;; cancel so a close can never round to 0:00 and get DELETED by
;; `org-clock-out-remove-zero-time-clocks'; float-only clamping; a close time
;; that is never "now"; a post-condition check so a silent abort is reported as
;; failed; and it never saves the buffer (a save would run this config's three
;; before-save rewriters and push into iCloud).
;;
;; V3 STAGE 1: PRESENCE AND THE HONEST CLOCK.  The probe reads three facts every
;; tick, clocked or not: HID idle, the console lock and kern.waketime
;; (`lr-track--probe-command').  lr-track-presence.el folds them into here or
;; away (`lr-track--presence'), and the running clock earns time from presence
;; alone, through the pure `lr-track--clock-step':
;;
;;   machine (and either, for now)  the line ends at his last input.  An away
;;       (15 min without input, a lock past 15 min, the Mac asleep) PAUSES it
;;       at that last input, and only his own key ever continues it.
;;   away (practice, life, sleep)   the line ends at presence's newest sample
;;       while he is away; 15 min of activity at the Mac pauses it where that
;;       activity began.
;;
;; A pause only stops the line growing: org still clocks the task.  His own
;; clock-out of a paused clock (O, SPC c o, the switch inside `org-clock-in')
;; ends it at the pause, through a filter on `org-clock-out', with a note under
;; the line.  The old check-in and banners stay in this file until Stage 2, but
;; are off by default (S0).

;;; Code:

(require 'cl-lib)
(require 'seq)
;; Presence (pure, no deps): the probe parsers, including the HID idle one the
;; sentinel below uses, and the presence step.
(require 'lr-track-presence)

;; Soft deps: read live, never required at load, so enabling the mode never
;; forces org to load at startup.
(declare-function org-clocking-p "org-clock" ())
(declare-function org-clock-out "org-clock" (&optional switch-to-state fail-quietly at-time))
(declare-function org-clock-cancel "org-clock" ())
(declare-function org-log-beginning "org" (&optional create))
(declare-function org-back-to-heading "org" (&optional invisible-ok))
;; used by the live clock line (`lr-track--advance-clock-line')
(declare-function org-clock-update-time-maybe "org-clock" ())
(declare-function org-end-of-subtree "org" (&optional invisible-ok to-heading))
(declare-function org-entry-end-position "org" ())
(declare-function org-time-string-to-seconds "org" (s))
(declare-function org-time-string-to-time "org" (s))
;; used by the honest clock (places, the paused string, the ended note)
(declare-function org-entry-get "org" (epom property &optional inherit literal-nil))
(declare-function org-clock-update-mode-line "org-clock" (&optional refresh))
(declare-function lr-track-ask-refresh-header "lr-track-ask" ())
(declare-function org-base-buffer "org-macs" (buffer))
(declare-function org-get-heading "org" (&optional no-tags no-todo no-priority no-comment))
(declare-function org-remove-empty-drawer-at "org" (pos))
(defvar org-clock-marker)
(defvar org-clock-out-time)
(defvar org-clock-hd-marker)
(defvar org-clock-start-time)
(defvar org-clock-current-task)
(defvar org-clock-out-remove-zero-time-clocks)
(defvar org-clock-out-removed-last-clock)
(defvar org-log-into-drawer)
(defvar org-clock-idle-time)
(defvar org-clock-out-switch-to-state)
(defvar org-clock-rounding-minutes)
(defvar org-log-note-clock-out)

(defgroup lr-track nil
  "Attention tracking and accountability coaching."
  :group 'org :prefix "lr-track-")

;;;; thresholds

(defcustom lr-track-activity-file "~/roam/main/activity.org"
  "Org file where engaged/elsewhere/away/asleep episodes accrue as CLOCK lines."
  :type 'file)

(defcustom lr-track-away-seconds 600.0
  "System (HID) idle seconds at/above which you count as `away' from the machine."
  :type 'number)

(defcustom lr-track-elsewhere-emacs-idle 180.0
  "Emacs idle seconds at/above which, with low system idle, you are `elsewhere'."
  :type 'number)

(defcustom lr-track-elsewhere-sys-idle 120.0
  "System idle must be below this for `elsewhere' (you are at the machine)."
  :type 'number)

(defcustom lr-track-engaged-emacs-idle 60.0
  "Emacs idle below this counts as `engaged'."
  :type 'number)

(defcustom lr-track-slept-tick-multiple 3.0
  "A wall-clock gap larger than this many tick intervals means the machine slept."
  :type 'number)

(defcustom lr-track-autoout-floor-seconds 60.0
  "Minimum length of an auto-closed clock interval.
Load-bearing: `org-clock-out-remove-zero-time-clocks' is t and `org-clock-out'
writes minute resolution, so a sub-minute close rounds to 0:00 and DELETES the
CLOCK line.  This floor makes that unreachable."
  :type 'number)

(defcustom lr-track-max-clock-seconds 14400.0
  "A running clock longer than this (4h) is auto-capped (D1b ceiling)."
  :type 'number)

(defcustom lr-track-auto-clock-out nil
  "When non-nil, auto-close the clock on away/slept/4h-ceiling (idle subtracted).
Default nil: the clock is closed ONLY by you (explicitly, or via the check-in when
you say where you were).  The coach still NOTIFIES about away/slept/4h and the
check-in still asks on return, but it NEVER touches the CLOCK line on its own.
Set this non-nil to opt back into automatic clock surgery.

You almost certainly want `lr-track-live-clock-line' instead: it removes the
REASON for clock surgery rather than automating it."
  :type 'boolean)

(defcustom lr-track-live-clock-line t
  "Keep the running clock's CLOCK line CLOSED at all times.

org writes `CLOCK: [start]' and leaves it open until something closes it.
Forget once -- or lose the session -- and that line is a claim with no end.
That is how 299 intervals here came to hold 48.9% of all clocked hours.

With this on, the tick advances the line's TRAILING stamp while you are at the
Mac, so it always reads `CLOCK: [start]--[last-input] =>  H:MM'.  Leave and it
pauses, at your last input; only your own key continues it.

The point is what it does NOT do.  It never closes the clock, never rewrites a
stamp backwards, never asks you anything, and saves only as
`lr-track-autosave-clock' says.  There is simply no dangling line left to
forget, so no surgery is needed to fix one, which is why this is the answer to
\"shit should be explicit\" rather than a violation of it.  You still clock out
yourself: a live clock's explicit clock-out extends the line to the real end
time (verified against org, not assumed), a paused one's ends at the pause.

nil restores plain org behaviour."
  :type 'boolean)

(defcustom lr-track-autosave-clock t
  "Save the clocked org file after the live clock line is advanced.

Without this the advanced line lives only in the buffer until your next manual
save, so a crash loses it and the on-disk line is stale -- which undercuts the
whole point of `lr-track-live-clock-line'.  With it on, an advance is flushed
to disk at most once per `lr-track-autosave-interval', at once when the clock
pauses, and once more when Emacs is killed.  The save is silent: no Wrote line.

The save goes through `lr-track--save-buffer', which NEUTRALISES this config's
three before-save rewriters (toc-org-insert-toc, vulpea-project-update-tag,
org-roam-link-replace-all): it is a clean write of the clock advance, never a
reformat, and never runs an org-roam reindex on a timer.  Only the one clocked
buffer is ever saved; no other buffer is touched.

This deliberately reverses the usual \"never save an org buffer from a timer\"
stance.  That is safe here because only this machine writes `~/roam' (no iCloud
merge to lose) and the write is a neutralised flush.  nil restores the
save-only-by-you behaviour."
  :type 'boolean)

(defcustom lr-track-autosave-interval 300.0
  "Seconds between two autosaves of the clocked buffer for a plain advance.
A pause and kill-emacs save at once, whatever this says."
  :type 'number)

(defcustom lr-track-away-nudge-seconds 0.0
  "Seconds after you're detected AWAY (machine idle) before the coach checks in.
0 = check in as soon as you cross the away threshold (already ~10 min idle)."
  :type 'number)

(defcustom lr-track-away-close-seconds 300.0
  "Seconds you must stay AWAY before the clock is auto-closed (when
`lr-track-auto-clock-out'), closing at when you actually left."
  :type 'number)

(defcustom lr-track-elsewhere-checkin-seconds nil
  "If set, banner-check-in while clocked but OUTSIDE Emacs this long.
Off by default: the in-Emacs check-in on return (see `lr-track-checkin-on-return')
is the real interaction; this is the naggier banner version.  A number enables it."
  :type '(choice (const :tag "Disabled" nil) number))

;; S0 (v3 Stage 1): the old prompts are silenced by default.  Nothing opens from
;; a timer or a hook any more; the Now? question in the agenda replaces them,
;; and Stage 2 deletes the code below.
(defcustom lr-track-checkin-on-return nil
  "When you come back to Emacs after being away/out of it a while, POP an
in-Emacs check-in: \"still on X? / switch to what you're doing / clock out\",
and offer to clock into whatever you name.  Off by default since v3 Stage 1."
  :type 'boolean)

(defcustom lr-track-checkin-after-seconds 180.0
  "Minimum seconds out of Emacs (unfocused) before a return triggers the check-in."
  :type 'number)

(defcustom lr-track-checkin-cooldown 300.0
  "Minimum seconds between return check-ins, so refocusing doesn't nag."
  :type 'number)

(defcustom lr-track-checkin-on-startup nil
  "When Emacs starts (after a prior session), pop the check-in so you account for
where you've been and clock into what you're doing now.  Off by default since v3
Stage 1."
  :type 'boolean)

(defcustom lr-track-log-activity nil
  "When non-nil, log raw attention episodes (engaged/elsewhere/away/asleep) as
CLOCK lines in `lr-track-activity-file'.  OFF by default: time belongs on your
real tasks (via the check-in), not dumped into a synthetic activity file."
  :type 'boolean)

(defcustom lr-track-backfill-max-seconds 86400.0
  "Longest away gap (default 24h) the return/startup check-in offers to break
down.  Beyond this it just offers a plain clock-in, e.g. back after a week."
  :type 'number)

(defcustom lr-track-snooze-seconds 900.0
  "How long `lr-track-resolve's snooze silences nudges AND auto-close."
  :type 'number)

(defcustom lr-track-interval-clocked 30.0 "Tick seconds while a clock runs." :type 'number)
(defcustom lr-track-interval-idle 120.0 "Tick seconds while no clock runs." :type 'number)
(defcustom lr-track-degraded-interval 300.0 "Backoff tick seconds after repeated phase failures." :type 'number)
(defcustom lr-track-interval-here 30.0
  "Tick seconds while presence is here (he is at the Mac)."
  :type 'number)
(defcustom lr-track-interval-away 60.0
  "Tick seconds while presence is away."
  :type 'number)

(defcustom lr-track-daily-banner-budget 0
  "Maximum coach banners per day (modeline nudges are free and uncounted).
0 since v3 Stage 1: no banner ever interrupts."
  :type 'integer)

(defconst lr-track--s0-old-defaults
  '((lr-track-checkin-on-return . t)
    (lr-track-checkin-on-startup . t)
    (lr-track-daily-banner-budget . 14))
  "The defaults S0 changed, as they were before v3 Stage 1.")

(defun lr-track--s0-silence-old-defaults ()
  "Give each S0 option still at its old default its new one.
Deploy is by `load' over the running session (I13), and `defcustom' keeps a
live value, so the old check-in and banners would stay on until a restart.
An option customized through Custom, or set to anything else, is his: it is
left alone."
  (dolist (c lr-track--s0-old-defaults)
    (let ((sym (car c)))
      (when (and (boundp sym)
                 (equal (default-value sym) (cdr c))
                 (not (get sym 'saved-value))
                 (not (get sym 'customized-value)))
        (set-default sym (eval (car (get sym 'standard-value)) t))))))

(lr-track--s0-silence-old-defaults)

(defcustom lr-track-category-cooldown 300.0
  "Minimum seconds between banners of the same category."
  :type 'number)

(defcustom lr-track-timeline t
  "When non-nil, the backfill check-in shows a live timeline gauge in a side window
so you can SEE the untracked stretch, what is already filled, where the cursor is,
how much time is left, and a preview of the segment as you type \"until when\"."
  :type 'boolean)

(defcustom lr-track-timeline-ascii nil
  "When non-nil, draw the backfill timeline with ASCII glyphs even when the unicode
block characters are displayable.  (It already falls back to ASCII automatically
when a glyph cannot be shown.)"
  :type 'boolean)

;; The gauge glyphs live as integer codepoints so this source file stays pure ASCII;
;; they are turned into characters only at render time.  Density ramp full > dark >
;; medium > light encodes done > preview > empty even with every face stripped.
(defconst lr-track--tl-glyphs
  '((:fill . #x2588) (:fill2 . #x2593) (:preview . #x2592) (:empty . #x2591)
    (:hour . #x2503) (:half . #x2502))
  "Unicode codepoints for the backfill gauge (FULL/DARK/MEDIUM/LIGHT block, heavy/light bar).")
(defconst lr-track--tl-ascii
  '((:fill . ?#) (:fill2 . ?%) (:preview . ?=) (:empty . ?.) (:hour . ?|) (:half . ?:))
  "Pure-ASCII fallback glyphs, same keys as `lr-track--tl-glyphs'.")

(defface lr-track-backfill-fill '((t :inherit success))
  "Committed (already logged) time in the backfill gauge." :group 'lr-track)
(defface lr-track-backfill-fill-alt
  '((((class color) (min-colors 88)) :foreground "turquoise")
    (((class color)) :foreground "cyan")
    (t :inherit success))
  "Alternate committed segment, so two adjacent logged runs never merge." :group 'lr-track)
(defface lr-track-backfill-preview '((t :inherit warning :weight bold))
  "The tentative segment you are typing at the until-when prompt." :group 'lr-track)
(defface lr-track-backfill-empty '((t :inherit shadow))
  "Unfilled time still to account for in the backfill gauge." :group 'lr-track)
(defface lr-track-backfill-cursor '((t :inherit (bold warning)))
  "The cursor and tentative-end carets in the backfill gauge." :group 'lr-track)
(defface lr-track-backfill-tick-hour '((t :weight bold))
  "Hour tick and its label on the gauge axis." :group 'lr-track)
(defface lr-track-backfill-tick-half '((t :inherit shadow))
  "Half-hour tick and its label on the gauge axis." :group 'lr-track)
(defface lr-track-backfill-task '((t :inherit warning :weight bold))
  "The task name this prompt is filling (the one word that answers what am I filling)."
  :group 'lr-track)
(defface lr-track-backfill-invalid '((t :inherit error))
  "Shown when the typed until-when does not parse or is out of range." :group 'lr-track)

;; probe cache tuning
(defconst lr-track--probe-command
  (list "/bin/sh" "-c"
        (concat "ioreg -r -c IOHIDSystem -d 1 -w 0 | grep -m1 HIDIdleTime;"
                " ioreg -n Root -d 1 | grep -m1 IOConsoleLocked;"
                " sysctl -n kern.waketime"))
  "The presence probe: one shell, three facts, one line each.
HID idle, the console lock and the last wake, and nothing else (I16): no app,
window or process name is ever read.  The `-r' on the first ioreg is
MANDATORY: without it ioreg prints only the registry root, HIDIdleTime never
appears, and `away' silently becomes unreachable.  Do not \"tidy\" it.")
(defconst lr-track--probe-skip-seconds 30.0
  "Emacs input this recent already proves he is here: no process is spawned.")
;; `lr-track--hid-idle-max-seconds' lives in lr-track-presence.el, with the parser.
(defvar lr-track--sense-respawn-seconds 25.0 "Do not re-probe if the cached HID sample is younger than this.")
(defvar lr-track--sense-stale-seconds 60.0
  "A cached HID sample older than this reads as `unknown'.
Deliberately > respawn AND > the clocked tick interval: the probe is async, so
each tick classifies on the PREVIOUS tick's sample; a single threshold would
collapse every classification to unknown.")
(defvar lr-track--sense-timeout-seconds 5.0 "Watchdog: kill a probe that has not exited within this.")
(defvar lr-track--sense-max-failures 3 "Consecutive probe failures before the probe is deemed unhealthy.")

;;;; module state

(defvar lr-track--interval lr-track-interval-idle "Current tick interval (seconds).")
(defvar lr-track--last-tick nil "`float-time' the current interval was armed at.")
(defvar lr-track--generation 0 "Monotonic token; a stale timer no-ops when its generation differs.")
(defvar lr-track--timer nil "The single tick timer.")

(defvar lr-track--sys-idle-value 'unknown "Last HID idle sample (float seconds) or `unknown'.")
(defvar lr-track--sys-idle-stamp nil "`float-time' the HID sample was captured.")
(defvar lr-track--sys-idle-process nil "The one in-flight probe, or nil.")
(defvar lr-track--sys-idle-watchdog nil "Watchdog timer for the in-flight probe.")
(defvar lr-track--sense-failures 0 "Consecutive probe failures.")

(defvar lr-track--focused-p t "Non-nil when some frame has focus; defaults t (a shrug counts as focused).")
(defvar lr-track--unfocused-since nil "Float time Emacs last lost focus, for the return check-in.")
(defvar lr-track--last-checkin 0.0 "Float time of the last return check-in (cooldown).")
(defvar lr-track--checkin-since nil "Float time you left, for the check-in's retroactive backfill.")

(defvar lr-track--tick-attention '(:state unknown) "This tick's raw classification.")
(defvar lr-track--last-state nil "Previous tick's raw state (for hysteresis).")
(defvar lr-track--stable-state 'unknown "Confirmed (2-tick) state, or a transient class.")
(defvar lr-track--stable-since nil "`float-time' the stable state began.")
(defvar lr-track--last-engaged nil "`float-time' of the last engaged/reading tick.")

(defvar lr-track--episode nil "Current activity episode: (:state SYM :start FLOAT).")
(defvar lr-track--incidents nil "Alist of active coach incidents: (KEY :since FLOAT :level N).")
(defvar lr-track--budget-day nil "Day index the banner budget was last reset for.")
(defvar lr-track--budget-spent 0 "Banners delivered today.")
(defvar lr-track--cooldowns nil "Alist (CATEGORY . last-float).")
(defvar lr-track--modeline-cache "" "O(1) modeline string, written only by the modeline phase.")
(defvar lr-track--phase-failures nil "Alist (PHASE . consecutive-failures).")

(defconst lr-track--phases
  '(sense presence live-clock heartbeat activity nudge modeline header)
  "Ordered tick phases; order is behavioral.
`presence' steps the newest probe sample straight after `sense', and
`live-clock' runs right after it, so the clock line moves on the presence this
tick just stepped, and BEFORE `nudge', so a nudge that reads the clock sees the
line already up to date.  `header' runs last, on everything the tick changed.")
(defconst lr-track--phase-failure-limit 3 "Consecutive failures before a phase is disabled + tick backs off.")
(defvar lr-track--modeline-form '(:eval (lr-track--modeline-string)) "Literal appended to `global-mode-string'.")

(defvar lr-track--state nil "In-memory mirror of the heartbeat sidecar plist.")
(defvar lr-track--state-prev nil "Sidecar plist as read at enable (for crash recovery).")
(defvar lr-track--recovered nil "Non-nil once startup crash recovery has run this session.")

;; Presence and the honest clock (v3 Stage 1).  Every one is a `defvar': deploy
;; is by `load' mid-day (I13), and a reload must keep the live values.
(defvar lr-track--last-sample nil
  "The newest presence sample: (:time T :idle I :locked K :wake W).
Written by the probe's sentinel, or synthesized when Emacs saw input under
`lr-track--probe-skip-seconds' ago.  Stepped once by the `presence' phase.")
(defvar lr-track--presence (lr-track--presence-init)
  "Presence state, from `lr-track--presence-step'.  `lr-track--enable' starts it
fresh unless it is still current (`lr-track--presence-fresh-p'); a plain
`load' keeps it.")
(defvar lr-track--presence-stepped nil
  "The sample last stepped into `lr-track--presence', so none goes in twice.")
(defvar lr-track--clock-pause nil
  "nil, or (:start F :paused-at F :why SYM) for the clock whose
`org-clock-start-time' is :start.  Any other clock is not paused.")
(defvar lr-track--internal nil
  "Non-nil around lr-track's own clock-outs; the clock-out filter skips those.")
(defvar lr-track--clock-out-ended nil
  "What the clock-out filter changed, for the `org-clock-out-hook' note: nil or
\(:task S :paused-at F :start-stamp S :hd MARKER).")
(defvar lr-track--last-clock-end nil
  "Float time the most recent clock ended at, this session (any clock-out).")
(defvar lr-track--ask-exit-fn nil
  "Exit function of the Now? one-key map while it is up (set by lr-track-ask).
Focus loss calls it, so the map never outlives his attention.")
(defvar-local lr-track--autosaved-at nil
  "`float-time' this buffer was last autosaved by `lr-track--maybe-autosave'.")
(put 'lr-track--autosaved-at 'permanent-local t)
(defvar-local lr-track--advanced-tick nil
  "`buffer-chars-modified-tick' right after the last advance of the clock line.
While the buffer's tick still equals it, nothing but that line is unsaved.")
(put 'lr-track--advanced-tick 'permanent-local t)
(defvar lr-track--blind-since nil
  "Float time the probe first failed since the last usable sample was stepped,
or nil.  A usable sample after 3 min or more of this cannot vouch for the
stretch before it (`lr-track--step-sample').")

;;;; sensing

;; `lr-track--parse-hid-idle' (and `lr-track--parse-probe') live in
;; lr-track-presence.el, so the pure presence module needs nothing from here.

(defun lr-track-emacs-idle-seconds ()
  "Seconds since Emacs last read input.
Must be (float-time (or (current-idle-time) 0)); (car (current-idle-time)) is 0
even at real idle because the seconds live in the low-order slot."
  (float-time (or (current-idle-time) 0)))

(defconst lr-track--return-confirm-seconds 1.5
  "Focus must still be on Emacs this long after a return before it counts.")

(defvar lr-track--resolving)  ; defined with the check-in below

(defun lr-track--focus-change (&rest _)
  "Recompute `lr-track--focused-p' (`unknown' counts as focused) and, when Emacs
regains focus after a real absence, trigger the return check-in."
  (let ((was lr-track--focused-p))
    (setq lr-track--focused-p (lr-track--frame-focused-now-p))
    ;; The Now? one-key map never outlives his attention: losing focus closes
    ;; it, exactly like any other key would.
    (when (and (not lr-track--focused-p) (functionp lr-track--ask-exit-fn))
      (condition-case err (funcall lr-track--ask-exit-fn)
        (error (lr-track--log 'ask-exit err))))
    (cond
     ((and was (not lr-track--focused-p))          ; just left Emacs
      ;; Keep the EARLIEST unanswered departure: a pass-through visit must not
      ;; reset when you actually left, or the next real return under-asks.
      (unless lr-track--unfocused-since
        (setq lr-track--unfocused-since (float-time)))
      ;; Leaving while a check-in is open drops it, exactly like C-g, so it can
      ;; never sit in a minibuffer nobody is looking at and swallow the next
      ;; keystrokes (one was found open for 34 min on 2026-10-03).
      (when (and lr-track--resolving (> (minibuffer-depth) 0))
        (run-at-time 0 nil #'lr-track--drop-unseen-checkin)))
     ((and (not was) lr-track--focused-p)           ; just came back to Emacs
      ;; Only a return that HOLDS counts.  AeroSpace workspace hops pass through
      ;; Emacs for under a second; confirming after a beat means they never
      ;; open a prompt into the void.
      (run-at-time lr-track--return-confirm-seconds nil #'lr-track--confirm-return)))))

(defun lr-track--frame-focused-now-p ()
  "Non-nil when some frame has focus (`unknown' counts as focused)."
  (and (seq-some (lambda (f) (memq (frame-focus-state f) '(t unknown))) (frame-list)) t))

(defun lr-track--confirm-return ()
  "Second half of a return: if Emacs still has focus, ask about the gap (when it
is long enough) and clear the departure.  A pass-through that already left does
nothing, so the departure survives for the next real return."
  (when (lr-track--frame-focused-now-p)
    (when (and lr-track-checkin-on-return
               lr-track--unfocused-since
               (>= (- (float-time) lr-track--unfocused-since)
                   lr-track-checkin-after-seconds))
      (setq lr-track--checkin-since lr-track--unfocused-since)  ; remember when you left
      (lr-track--trigger-checkin 'return))
    (setq lr-track--unfocused-since nil)))

(defun lr-track--drop-unseen-checkin ()
  "Abort an open check-in once Emacs has lost focus (the C-g path, nothing written)."
  (when (and lr-track--resolving
             (> (minibuffer-depth) 0)
             (not (lr-track--frame-focused-now-p)))
    (abort-recursive-edit)))

(defun lr-track-sense-healthy-p ()
  (< lr-track--sense-failures lr-track--sense-max-failures))

(defun lr-track--sys-idle-age ()
  (if lr-track--sys-idle-stamp (- (float-time) lr-track--sys-idle-stamp) 1.0e+INF))

(defun lr-track--sense-note-success (seconds)
  (setq lr-track--sys-idle-value seconds
        lr-track--sys-idle-stamp (float-time)
        lr-track--sense-failures 0)
  seconds)

(defun lr-track--sense-note-failure (_reason)
  (setq lr-track--sys-idle-value 'unknown
        lr-track--sys-idle-stamp (float-time)
        lr-track--sense-failures (1+ lr-track--sense-failures))
  ;; presence is blind from here until a usable sample is stepped
  (unless lr-track--blind-since
    (setq lr-track--blind-since lr-track--sys-idle-stamp))
  nil)

(defun lr-track--sys-idle-watchdog-fire ()
  "Kill a probe that overran.  Clears the slot BEFORE `delete-process' (which runs
the sentinel synchronously) so one timeout is not counted twice.  The
sentinel then no longer owns the probe, so its buffer is killed here."
  (let* ((proc lr-track--sys-idle-process)
         (buf (and (processp proc) (process-get proc 'lr-track-buf))))
    (setq lr-track--sys-idle-process nil
          lr-track--sys-idle-watchdog nil)
    (when (process-live-p proc) (delete-process proc))
    (when (buffer-live-p buf) (kill-buffer buf))
    (lr-track--sense-note-failure 'timeout)))

(defun lr-track--sys-idle-sentinel (proc _event)
  "Read the finished presence probe PROC into the caches.
The idle seconds keep feeding the old classifier's cache, and the whole sample,
stamped with the time of the parse, becomes `lr-track--last-sample'."
  (when (memq (process-status proc) '(exit signal))
    ;; only the owner of the slot may write the cache
    (when (eq proc lr-track--sys-idle-process)
      (setq lr-track--sys-idle-process nil)
      (when (timerp lr-track--sys-idle-watchdog)
        (cancel-timer lr-track--sys-idle-watchdog)
        (setq lr-track--sys-idle-watchdog nil))
      (let* ((buf (process-get proc 'lr-track-buf))
             (out (and (buffer-live-p buf)
                       (with-current-buffer buf (buffer-string))))
             (parsed (lr-track--parse-probe out))
             (idle (plist-get parsed :idle)))
        (when (buffer-live-p buf) (kill-buffer buf))
        ;; Only the three parsed facts and the time get past this point (I16).
        (setq lr-track--last-sample
              (list :time (float-time) :idle idle
                    :locked (plist-get parsed :locked)
                    :wake (plist-get parsed :wake)))
        (if (eq idle 'unknown) (lr-track--sense-note-failure 'parse)
          (lr-track--sense-note-success idle))))))

(defun lr-track--sys-idle-probe ()
  "Spawn one async presence probe if the slot is free and the shell exists."
  (when (and (eq system-type 'darwin)
             (file-executable-p (car lr-track--probe-command))
             (not (process-live-p lr-track--sys-idle-process)))
    (let ((buf nil))
      (condition-case err
          (let ((proc (progn
                        (setq buf (generate-new-buffer " *lr-track-probe*" t))
                        (make-process
                         :name "lr-track-probe" :buffer buf :noquery t
                         :connection-type 'pipe
                         :command lr-track--probe-command
                         :sentinel #'lr-track--sys-idle-sentinel))))
            (process-put proc 'lr-track-buf buf)
            (setq lr-track--sys-idle-process proc
                  lr-track--sys-idle-watchdog
                  (run-with-timer lr-track--sense-timeout-seconds nil
                                  #'lr-track--sys-idle-watchdog-fire)))
        (error
         ;; no process owns the buffer: one per failed spawn would pile up
         (when (buffer-live-p buf) (kill-buffer buf))
         (lr-track--sense-note-failure 'spawn)
         (lr-track--log 'probe err))))))

(defun lr-track--tick-probe ()
  "Sense presence for this tick, clock or no clock (spec v3 3.1).
Emacs input under `lr-track--probe-skip-seconds' ago already proves he is here
and unlocked, so no process is spawned: the sample is synthesized from Emacs
idle with the last known wake.  Two guards keep that honest.  A probe sample
that arrived since the last tick is stepped first, so the synthesized one never
overwrites it unseen (it may be the very sample that starts an away).  And
after a stalled tick (over `lr-track-slept-tick-multiple' intervals late, as
after a sleep) the real probe runs instead, since only it can read the new
wake that tells presence the Mac slept."
  (let* ((e (lr-track-emacs-idle-seconds))
         (now (float-time)))
    (if (or (>= e lr-track--probe-skip-seconds) (lr-track--tick-stalled-p now))
        (lr-track--sys-idle-probe)
      ;; contained: a bad sample must never cost the classifier its tick
      (condition-case err (lr-track--step-sample lr-track--last-sample)
        (error (lr-track--log 'presence err)))
      (setq lr-track--last-sample (lr-track--emacs-sample now e)
            ;; the old classifier's cache too, so it spawns nothing itself
            lr-track--sys-idle-value e
            lr-track--sys-idle-stamp now))))

(defun lr-track-system-idle-seconds ()
  "Return the cached HID idle sample (float) or `unknown'; kick an async refresh.
NON-BLOCKING by contract, a wedged ioreg must never freeze Emacs."
  (let ((age (lr-track--sys-idle-age)))
    (when (>= age lr-track--sense-respawn-seconds) (lr-track--sys-idle-probe))
    (if (< age lr-track--sense-stale-seconds) lr-track--sys-idle-value 'unknown)))

(defun lr-track-sense-cleanup ()
  "Tear down the probe without counting a failure (a deliberate teardown is not one)."
  (when (timerp lr-track--sys-idle-watchdog) (cancel-timer lr-track--sys-idle-watchdog))
  (when (process-live-p lr-track--sys-idle-process) (delete-process lr-track--sys-idle-process))
  (setq lr-track--sys-idle-watchdog nil lr-track--sys-idle-process nil))

;;;; classifier

(defun lr-track--reading-mode-p (mode)
  (memq mode '(pdf-view-mode nov-mode doc-view-mode)))

(defun lr-track--classify (e s gap focused mode interval)
  "PURE classifier.  E=Emacs idle, S=system idle (num or `unknown'), GAP=now-last,
FOCUSED bool, MODE major-mode symbol, INTERVAL current tick seconds.
Returns a plist with :state, :gap, :outside.  Rule order is load-bearing."
  ;; Step 0: structural clamp, a one-tick-old S can exceed a fresh E; without
  ;; this, `away' fires while you are actively typing and a clock is closed at
  ;; now-700s.  Never let S exceed E.
  (when (and (numberp s) (numberp e)) (setq s (min s e)))
  (let ((outside (if (numberp s) (max 0.0 (- e s)) nil)))
    (cond
     ((and (numberp gap) (< gap 0))
      (list :state 'unknown :cause 'clock-step :gap gap :outside nil))
     ((and (numberp gap) (> gap (* lr-track-slept-tick-multiple interval)))
      (list :state 'slept :gap gap :outside nil))
     ((not (numberp s))                 ; S unknown: E-only, away bar raised
      (cond
       ((>= e 1800.0) (list :state 'away :gap gap :outside nil))
       ((< e lr-track-engaged-emacs-idle) (list :state 'engaged :gap gap :outside nil))
       ((and (>= e 900.0) (lr-track--reading-mode-p mode) focused)
        (list :state 'reading :gap gap :outside nil))
       (focused (list :state 'engaged :gap gap :outside nil))   ; focused = present
       (t (list :state 'unknown :gap gap :outside nil))))
     ((>= s lr-track-away-seconds) (list :state 'away :gap gap :outside outside))
     ;; recent Emacs input means engaged, focus irrelevant
     ((< e lr-track-engaged-emacs-idle) (list :state 'engaged :gap gap :outside outside))
     ;; at the machine, idle in Emacs, and NOT looking at Emacs means elsewhere
     ((and (not focused) (>= e lr-track-elsewhere-emacs-idle) (< s lr-track-elsewhere-sys-idle))
      (list :state 'elsewhere :gap gap :outside outside))
     ((and (>= e 900.0) (lr-track--reading-mode-p mode) focused)
      (list :state 'reading :gap gap :outside outside))
     ;; at the machine, focused on Emacs, just paused means engaged (present)
     (focused (list :state 'engaged :gap gap :outside outside))
     (t (list :state 'unknown :gap gap :outside outside)))))

(defun lr-track--probe-worthwhile-p ()
  "Only spend a subprocess when it can change a decision."
  (and (lr-track--clocking-p)
       (or (>= (lr-track-emacs-idle-seconds) 60.0)
           (not lr-track--focused-p)
           (let ((s lr-track--sys-idle-value))
             (and (numberp s) (>= s lr-track-away-seconds))))))

(defun lr-track-attention-state ()
  "Gather signals and classify.  Returns the classifier plist."
  (let* ((e (lr-track-emacs-idle-seconds))
         (s (if (lr-track--probe-worthwhile-p)
                (lr-track-system-idle-seconds)
              ;; not worthwhile: treat as fully present at machine (0 sys idle)
              0.0))
         (now (float-time))
         (gap (if lr-track--last-tick (- now lr-track--last-tick) 0.0)))
    ;; MODE nil: the buffer he looks at is never read (spec v3 3.1), so the
    ;; old classifier has no `reading' state left.  It is inert since S0.
    (lr-track--classify e s gap lr-track--focused-p nil lr-track--interval)))

;;;; org: clock

(defun lr-track--clocking-p ()
  (and (fboundp 'org-clocking-p) (ignore-errors (org-clocking-p))))

(defun lr-track--clock-task ()
  "Plain (unpropertized) title of the currently clocked task, or nil."
  (when (and (lr-track--clocking-p) (boundp 'org-clock-current-task) org-clock-current-task)
    (substring-no-properties org-clock-current-task)))

(defun lr-track--clock-elapsed ()
  "Seconds the current clock has been running, or nil."
  (when (and (lr-track--clocking-p) (boundp 'org-clock-start-time) org-clock-start-time)
    (- (float-time) (float-time org-clock-start-time))))

(defmacro lr-track--with-pristine-org-globals (&rest body)
  "Run BODY with this config's mutated org-clock globals bound to safe values."
  (declare (indent 0) (debug t))
  `(let ((org-log-into-drawer "LOGBOOK")
         (org-clock-out-switch-to-state nil)
         (org-clock-rounding-minutes 0)
         (org-clock-idle-time nil)
         (org-log-note-clock-out nil))
     ,@body))

(defconst lr-track--clock-line-re
  "^[ \t]*CLOCK: \\(\\[[^]\n]*\\]\\)\\(?:--\\[[^]\n]*\\]\\)?.*$"
  "The running clock's line, whether still open or already closed by us.
Group 1 is the START stamp, which is the only part we ever preserve verbatim.")

(defun lr-track--advance-clock-line (end-float)
  "Rewrite the running clock's CLOCK line so it ends at END-FLOAT.  Return t if
the line changed.

This is the whole live-clock mechanism.  It does exactly one thing: move the
trailing stamp of the line org is already maintaining, forward, in the buffer.

What it deliberately does NOT do:
  - close the clock (`org-clocking-p' stays t, `org-clock-marker' stays valid);
  - move a stamp BACKWARDS (a late tick, an NTP step back, or a stale caller
    must never shorten an interval that is already recorded);
  - write an inverted or zero-length interval (END-FLOAT at or before the start
    leaves the line untouched);
  - save the buffer.  This config runs three rewriters on `before-save-hook'
    and the tree is iCloud-synced; a 30-second save loop would run all three
    every tick.  The line is left dirty for the owner's own save, exactly as
    org's own clock-in already does.

Verified against real org before this was written: rewriting the line in place
keeps the clock live, is idempotent across ticks, and a later explicit
`org-clock-out' EXTENDS the line to the real end rather than keeping our stamp."
  (when (and lr-track-live-clock-line
             (lr-track--clocking-p)
             (numberp end-float)
             (markerp org-clock-marker)
             (marker-buffer org-clock-marker))
    (let ((start (and (boundp 'org-clock-start-time)
                      org-clock-start-time
                      (float-time org-clock-start-time))))
      (when (and start (> end-float start))
        (with-current-buffer (marker-buffer org-clock-marker)
          (unless buffer-read-only
            (save-excursion
              (save-restriction
                (widen)
                ;; Locate the line by its START STAMP: the line at
                ;; `org-clock-marker' when it carries that stamp, else the first
                ;; one in the clocked ENTRY (`lr-track--goto-running-line').
                ;; Never by the marker's position alone: before the rewrite
                ;; below put it back, anchoring on it made the SECOND tick
                ;; replace at the wrong position and clobber the heading
                ;; (observed: `CLOCK: ...:LOGBOOK: =>  0:10' with the heading
                ;; gone).
                (let* ((start-stamp (format-time-string
                                     (org-time-stamp-format t t)
                                     (seconds-to-time start)))
                       (end-stamp (format-time-string
                                   (org-time-stamp-format t t)
                                   (seconds-to-time end-float)))
                       ;; Duration from the STAMPS (minute resolution), exactly as
                       ;; `org-clock-out' computes it, so the line is always
                       ;; internally consistent.  Deliberately NOT
                       ;; `org-clock-update-time-maybe': that goes through
                       ;; `org-timestamp-change', which deletes and reinserts BOTH
                       ;; stamps and so destroys point sitting inside them.
                       (secs (max 0 (round (- (org-time-string-to-seconds end-stamp)
                                              (org-time-string-to-seconds start-stamp)))))
                       (new-tail (format "--%s => %2d:%02d" end-stamp
                                         (floor secs 3600) (floor (mod secs 3600) 60))))
                  (when (lr-track--goto-running-line start-stamp)
                    (let ((current-end (lr-track--clock-line-end)))
                      ;; Only ever move FORWARD.
                      (when (or (null current-end) (> end-float current-end))
                        (when (looking-at
                               (concat "^[ \t]*CLOCK: " (regexp-quote start-stamp)
                                       "\\(.*\\)$"))
                          (let ((tail-beg (match-beginning 1))
                                (old-tail (match-string 1)))
                            ;; Skip an identical rewrite (ticks within the same
                            ;; MINUTE), and never write a zero-length interval --
                            ;; the float guard above does not stop a same-minute
                            ;; end stamp.  Both would dirty the buffer and push
                            ;; undo entries for no change at all.
                            (unless (or (equal old-tail new-tail)
                                        (equal end-stamp start-stamp)
                                        ;; Never edit a buffer whose file
                                        ;; changed on disk since it was read:
                                        ;; the first change would ask whether
                                        ;; to edit it anyway, and the save
                                        ;; whether to overwrite the disk, from
                                        ;; a timer.  The line waits.
                                        (and (not (lr-track--buffer-current-p
                                                   (current-buffer)))
                                             (progn (lr-track--log
                                                     'advance-stale (buffer-name))
                                                    t)))
                              (let ((inhibit-field-text-motion t))
                                ;; Rewrite only the TAIL after the start stamp.
                                ;; Whole-line `replace-match' destroyed leading
                                ;; indentation (life.org has 475 indented CLOCK
                                ;; lines), clobbered any text being typed on the
                                ;; line, and dragged point to column 0.
                                (delete-region tail-beg (line-end-position))
                                (goto-char tail-beg)
                                (insert new-tail)
                                ;; Put org's marker back where org itself keeps it
                                ;; (right after the START stamp).  Without this the
                                ;; marker collapses to column 0 and
                                ;; `org-clock-cancel' SILENTLY FAILS, leaving a
                                ;; fabricated, plausible, closed interval behind --
                                ;; the banned clock surgery by another route.
                                (move-marker org-clock-marker tail-beg
                                             (buffer-base-buffer))
                                ;; what only our line made unsaved, for the
                                ;; kill-emacs save (`lr-track--on-kill-emacs')
                                (setq lr-track--advanced-tick
                                      (buffer-chars-modified-tick))
                                ;; Flush the advance to disk so a crash never
                                ;; loses it and the on-disk line stays current.
                                ;; `lr-track--save-buffer' neutralises the three
                                ;; before-save rewriters, so this is a clean,
                                ;; silent write, not a reformat, and touches
                                ;; only this one buffer.  Only reached on a REAL
                                ;; advance, and throttled to one save per
                                ;; `lr-track-autosave-interval' (a pause and
                                ;; kill-emacs force one).  A failing save (a
                                ;; save hook's error, a full disk) is logged:
                                ;; it must not abort the step that called
                                ;; this before its pause is recorded.
                                (condition-case err
                                    (lr-track--maybe-autosave (current-buffer) nil)
                                  (error (lr-track--log 'autosave err)))
                                t))))))))))))))))

(defun lr-track--clock-line-end ()
  "Float time of the end stamp on the clock line at point, or nil when open.
Point must already be at the beginning of the line."
  (save-excursion
    (when (looking-at "^[ \t]*CLOCK: \\[[^]\n]*\\]--\\(\\[[^]\n]*\\]\\)")
      (ignore-errors
        (float-time (org-time-string-to-time (match-string 1)))))))

(defun lr-track--running-line-end ()
  "Float time of the end stamp on the running clock's line, or nil while open.
The line is found the way `lr-track--advance-clock-line' finds it: by its start
stamp, inside the clocked entry only."
  (when (and (lr-track--clocking-p)
             (boundp 'org-clock-start-time) org-clock-start-time
             (markerp org-clock-marker) (marker-buffer org-clock-marker))
    (with-current-buffer (marker-buffer org-clock-marker)
      (save-excursion
        (save-restriction
          (widen)
          (when (lr-track--goto-running-line
                 (format-time-string (org-time-stamp-format t t)
                                     org-clock-start-time))
            (lr-track--clock-line-end)))))))

(defun lr-track--goto-running-line (start-stamp)
  "Move point to the start of the running clock's line and return non-nil.
START-STAMP is that line's start stamp.  The line at `org-clock-marker' wins
when it carries the stamp: that is the line org runs, even when an answer
wrote a closed line with the same start into the same entry.  Otherwise the
first line with the stamp inside the clocked ENTRY (`org-end-of-subtree'
would span children, and a descendant with the same start stamp would get
its real record rewritten).  nil, point unspecified, when there is none.
Call it in the clocked buffer, widened."
  (let ((re (concat "^[ \t]*CLOCK: " (regexp-quote start-stamp))))
    (or (and (markerp org-clock-marker)
             (eq (marker-buffer org-clock-marker) (current-buffer))
             (progn (goto-char org-clock-marker)
                    (beginning-of-line)
                    (looking-at re)))
        (let ((hd (and (markerp org-clock-hd-marker)
                       (eq (marker-buffer org-clock-hd-marker) (current-buffer))
                       org-clock-hd-marker)))
          (when (or hd (and (markerp org-clock-marker)
                            (eq (marker-buffer org-clock-marker)
                                (current-buffer))))
            (goto-char (or hd org-clock-marker))
            (beginning-of-line)
            (when (re-search-forward
                   re (save-excursion
                        (or (ignore-errors (org-entry-end-position)) (point-max)))
                   t)
              (beginning-of-line)
              t))))))

(defun lr-track--buffer-current-p (buffer)
  "Non-nil unless BUFFER visits a file that changed on disk since it was read.
A timer must never edit or save such a buffer: Emacs would ask, on the first
change, whether to edit it anyway, and on the save whether to overwrite."
  (with-current-buffer buffer
    (or (not buffer-file-name)
        (verify-visited-file-modtime buffer))))

;;;; the honest clock: places, the pure step, the pause

(defconst lr-track--clock-default-max 36000.0
  "A clock's maximum when no TRACK_MAX is set on or above its heading: 10 h.")

(defun lr-track--parse-max (s)
  "Seconds for a TRACK_MAX string S in H:MM, like \"10:00\"; nil if not one."
  (when (and (stringp s)
             (string-match "\\`[ \t]*\\([0-9]+\\):\\([0-5][0-9]\\)[ \t]*\\'" s))
    (let ((secs (+ (* 3600.0 (string-to-number (match-string 1 s)))
                   (* 60.0 (string-to-number (match-string 2 s))))))
      (and (> secs 0) secs))))

(defun lr-track--clock-place (marker)
  "The place and maximum of the heading at MARKER, as (PLACE . MAX-SECONDS).
PLACE is `machine', `either' or `away', from the nearest TRACK_PLACE on the
heading or an ancestor; MAX-SECONDS from the nearest TRACK_MAX (H:MM).  Without
them: `machine' and `lr-track--clock-default-max'.  Never signals: no marker,
a dead buffer, a buffer not in org, or any error answers that default."
  (let ((place 'machine) (cap lr-track--clock-default-max))
    (condition-case err
        (when (and (markerp marker) (buffer-live-p (marker-buffer marker)))
          (with-current-buffer (marker-buffer marker)
            (when (derived-mode-p 'org-mode)
              (save-excursion
                (save-restriction
                  (widen)
                  (goto-char marker)
                  (let ((p (org-entry-get nil "TRACK_PLACE" t))
                        (m (lr-track--parse-max
                            (org-entry-get nil "TRACK_MAX" t))))
                    (when (stringp p)
                      (let ((sym (intern (downcase (string-trim p)))))
                        (when (memq sym '(machine either away))
                          (setq place sym))))
                    (when m (setq cap m))))))))
      (error (lr-track--log 'clock-place err)
             (setq place 'machine cap lr-track--clock-default-max)))
    (cons place cap)))

(defun lr-track--away-why (kind)
  "Why a clock paused at an away of KIND: the kind, `idle' when none, and
`unknown' for an `unseen' one, a stretch no sample saw."
  (pcase kind
    ('nil 'idle)
    ('unseen 'unknown)
    (_ kind)))

(defun lr-track--clock-step (clock p now)
  "PURE: what the running CLOCK earns from presence P at NOW.
CLOCK is (:start F :end F :place SYM :max SECS :paused-at F :why SYM), :end
the running line's current end stamp (nil while open), :paused-at and :why
nil while it is live.  Return (:advance-to F :paused-at F :why SYM):
:advance-to is the new end to write, or nil to hold, and is always past both
:end and :start; :paused-at and :why are set when the clock is paused.

  already paused  hold and keep the pause: a clock never resumes itself.
  machine/either  here: end at the last input L (a break under 15 min is
                  bridged, because L jumps on his return).  away: pause at
                  the away's start (never before :start), why its kind;
                  but hold while P's newest sample is older than a minute
                  past :start, since a clock started while presence still
                  says away is his own key, and the sample that sees it is
                  still to come.  unknown: hold.  Here again, but P's
                  latest away ended past the line's end and over a minute
                  past :start: no step saw it, as when the first sample
                  after a wake already carries his input, or he clocked in
                  just before the lid closed.  Pause at its start (never
                  before :start), why its kind, exactly as if a step had.
                  An `unseen' away (no sample saw it) pauses why
                  `unknown': it never says he left.
                  And whatever the mode, when P's first sample came 3 min
                  or more after the line's end, no sample saw that span:
                  presence started over (the mode toggled, a deploy by
                  `load') or was blind until then.  Pause where the line
                  reads, why `unknown'.  An open line is exempt: it is his
                  own clock-in, or an open line org resumed at his word,
                  and a pause at its start would delete it.
  away            away: end at P's newest sample, never past it (NOW at
                  most): a blind probe or a late tick vouches for nothing
                  after it.  here: let A be his return, or :start
                  when that is later; once his last input is 15 min or more
                  past A, pause at A, why `activity'.  Shorter activity
                  holds, and is bridged when he leaves again.  unknown: hold.
  any place       a would-be end at :start + :max or later pauses there
                  instead, why `max'.  A pause never lands before the line's
                  current end (a glance merged into an earlier away, or a
                  lowered max, can put its target there): his key ends the
                  line where it already reads, never earlier."
  (let ((start (plist-get clock :start))
        (end (plist-get clock :end))
        (paused (plist-get clock :paused-at)))
    (if paused
        (list :advance-to nil :paused-at paused :why (plist-get clock :why))
      (let* ((cap (or (plist-get clock :max) lr-track--clock-default-max))
             (mode (plist-get p :mode))
             (last (plist-get p :last-input))
             ;; (TARGET . WHY): the would-be end, and why the clock pauses
             ;; there (nil for a plain advance); nil to hold
             (verdict
              (if (eq (plist-get clock :place) 'away)
                  (pcase mode
                    ;; what presence observed: its newest sample said away
                    ('away (cons (min now (or (plist-get p :prev-time) now)) nil))
                    ('here
                     (let ((a (max start (or (lr-track--presence-here-since p)
                                             start))))
                       ;; the activity's own span, never NOW minus A: presence
                       ;; stays here 15 min past his last input, so a 2 min
                       ;; glance would otherwise end the stream 13 min later
                       (and (numberp last)
                            (>= (- last a) lr-track-presence-away-seconds)
                            (cons a 'activity)))))
                (pcase (if (let ((first (plist-get p :first-time)))
                             ;; presence started 3 min or more after the
                             ;; line's end: no sample saw that span.  Only
                             ;; a line with an end: an open one is his own
                             ;; clock-in, or org resuming an open line he
                             ;; chose to keep, and a pause at its start
                             ;; would delete it at his clock-out
                             (and (numberp first) (numberp end)
                                  (>= (- first end)
                                      lr-track-presence-break-seconds)))
                           'unseen
                         mode)
                  ('unseen (cons end 'unknown))
                  ('here
                   (let* ((gone (plist-get p :last-away))
                          (from (plist-get gone :from))
                          (to (plist-get gone :to)))
                     (if (and (numberp from) (numberp to)
                              ;; it ended on this clock's watch and the line
                              ;; never got past it.  It may have begun
                              ;; before :start: presence dates it from the
                              ;; last input it saw, and a clock-in just
                              ;; before the lid closed is never sampled.
                              ;; A line backdated to the return starts in
                              ;; the return's minute, so that away is not
                              ;; its own.
                              (> to (+ start 60.0))
                              (or (null end) (> to end)))
                         (cons (max start from)
                               (lr-track--away-why (plist-get gone :kind)))
                       (and (numberp last) (cons (min last now) nil)))))
                  ('away
                   (let ((seen (plist-get p :prev-time)))
                     ;; presence not yet sampled past the clock-in: the
                     ;; clock is his key, so this away may already be
                     ;; over; the next sample says.  A start floored to
                     ;; its minute is why the minute.
                     (unless (and (numberp seen) (< seen (+ start 60.0)))
                       (cons (max start (or (plist-get p :away-from) last start))
                             (lr-track--away-why (plist-get p :away-kind))))))))))
        (when (and verdict (>= (- (car verdict) start) cap))
          (setq verdict (cons (+ start cap) 'max)))
        (let* ((target (car verdict))
               (why (cdr verdict)))
          (list :advance-to (and target (> target start)
                                 (or (null end) (> target end))
                                 target)
                ;; never before the line's current end: his key then ends
                ;; the line where it already reads, never earlier (I7)
                :paused-at (and why target
                                (if (numberp end) (max target end) target))
                :why why))))))

(defun lr-track--sync-running-start ()
  "Follow his edit of the running line's start; non-nil when it moved.
Org's `org-clock-update-time-maybe' does this for an open line only, and the
live line is closed in form.  So after his S-up or S-down on its start,
`org-clock-start-time' still named the old one: the line was no longer found
by its start (it froze), and a pause before the new start ended it inverted.
The line at `org-clock-marker' is read, only when the marker sits past its
start stamp (a deleted line leaves it at the next line's start), and never
inside org's own clock resolution.  When that stamp names another minute,
`org-clock-start-time' becomes it.  A pause at or after the new start is
kept for it; one before it is dropped, and the clock step decides again.
Never signals."
  (condition-case err
      (when (and (lr-track--clocking-p)
                 (not (lr-track--clock-stood-in-p))
                 (boundp 'org-clock-start-time) org-clock-start-time
                 (markerp org-clock-marker)
                 (buffer-live-p (marker-buffer org-clock-marker)))
        (let ((stamp
               (with-current-buffer (marker-buffer org-clock-marker)
                 (save-excursion
                   (save-restriction
                     (widen)
                     (goto-char org-clock-marker)
                     (let ((m (point)))
                       (beginning-of-line)
                       (and (looking-at "[ \t]*CLOCK: \\(\\[[^]\n]*\\]\\)")
                            (>= m (match-end 1))
                            (match-string-no-properties 1))))))))
          (when stamp
            (let ((new (float-time (org-time-string-to-time stamp)))
                  (old (float-time org-clock-start-time))
                  (pause lr-track--clock-pause))
              (unless (= (floor new 60) (floor old 60))
                (setq org-clock-start-time (seconds-to-time new)
                      lr-track--clock-pause
                      (and pause
                           (eql (plist-get pause :start) old)
                           (numberp (plist-get pause :paused-at))
                           (>= (plist-get pause :paused-at) new)
                           (plist-put (copy-sequence pause) :start new)))
                (lr-track--log 'start-moved (list :from old :to new))
                t)))))
    (error (lr-track--log 'sync-start err) nil)))

(defun lr-track--sync-paused-end (pause)
  "PAUSE, the current clock's, after his edit of the paused line's end.
A pause is where his key ends the line, so it follows the end as the line
reads now.  It records the end the line read when it paused (:end).  When
the line now reads another minute, he moved it (S-up, S-down, or typed):
the pause becomes his end, so his clock-out, a switch or y y keeps it,
never puts the line back at the old pause.  An end later than the pause
is taken too, recorded or not: a pause is never before the line's end
\(I7).  Otherwise PAUSE as it is: a line still short of its pause (an
advance a stale or read-only buffer skipped) ends at the pause.  An open,
missing or inverted line keeps PAUSE.  Never signals."
  (condition-case err
      (let* ((end (lr-track--running-line-end))
             (recorded (plist-get pause :end))
             (at (plist-get pause :paused-at)))
        (if (and (numberp end) (numberp at)
                 (> end (plist-get pause :start))
                 (or (> end at)
                     (and (numberp recorded)
                          (/= (floor end 60) (floor recorded 60)))))
            (progn
              (lr-track--log 'end-moved (list :from at :to end))
              (setq lr-track--clock-pause
                    (plist-put (plist-put (copy-sequence pause) :paused-at end)
                               :end end)))
          pause))
    (error (lr-track--log 'sync-end err) pause)))

(defun lr-track--current-pause ()
  "The pause recorded for the CURRENT clock, or nil.
A pause recorded for any other clock (another start, or none running) is stale
and is dropped here, so it can never apply to the wrong line.  His edit of
the running line's start is followed first (`lr-track--sync-running-start'),
and his edit of its end then (`lr-track--sync-paused-end')."
  (lr-track--sync-running-start)
  (let ((pause lr-track--clock-pause))
    (when pause
      (if (and (lr-track--clocking-p)
               (boundp 'org-clock-start-time) org-clock-start-time
               (numberp (plist-get pause :start))
               (= (plist-get pause :start) (float-time org-clock-start-time)))
          (lr-track--sync-paused-end pause)
        (setq lr-track--clock-pause nil)))))

(defun lr-track--paused-at ()
  "The time the current clock paused at, or nil when it is live (or none runs)."
  (plist-get (lr-track--current-pause) :paused-at))

(defun lr-track--forget-pause ()
  "Clock hooks: the running clock changed, so no pause applies any more.
Not when org stands a dangling line in for it (`lr-track--clock-stood-in-p'):
its cancel in `org-resolve-clocks' (C) ends that line, not the clock org
runs, whose pause still holds."
  (unless (lr-track--clock-stood-in-p)
    (setq lr-track--clock-pause nil)))

(defun lr-track--maybe-autosave (buffer &optional force)
  "Save BUFFER, the clocked one, when FORCE or when a save is due.
Due means its last autosave was `lr-track-autosave-interval' or more ago, or
never happened.  A pause and kill-emacs pass FORCE.  Nothing at all when
`lr-track-autosave-clock' is nil.  Non-nil when a save was made or due."
  (when (and lr-track-autosave-clock (buffer-live-p buffer))
    (let ((now (float-time))
          (last (buffer-local-value 'lr-track--autosaved-at buffer)))
      (when (or force
                (not (numberp last))
                (>= (- now last) lr-track-autosave-interval)
                (< now last))           ; the wall clock stepped back
        (with-current-buffer buffer (setq lr-track--autosaved-at now))
        (lr-track--save-buffer buffer)
        t))))

(defun lr-track--tick-live-clock ()
  "Tick phase: move the running clock's line as far as presence allows.

The pure `lr-track--clock-step' decides, from the clock's place, its line, its
pause and `lr-track--presence'; this only carries the verdict out.  An advance
goes through `lr-track--advance-clock-line' (tail only, forward only, a
same-minute no-op).  A new pause is recorded for this clock and saved at once.
The old classifier plays no part: presence decides.  Walk away and the line
stops at your last input, with no clock surgery, no prompt and nothing to undo."
  (when (and lr-track-live-clock-line
             (lr-track--clocking-p)
             ;; never the dangling line org stands in while resolving it
             (not (lr-track--clock-stood-in-p))
             (boundp 'org-clock-start-time) org-clock-start-time
             (markerp org-clock-marker) (marker-buffer org-clock-marker))
    ;; his S-up on the line's start moves the clock's start with it
    (lr-track--sync-running-start)
    (let* ((start (float-time org-clock-start-time))
           (place (lr-track--clock-place
                   (and (boundp 'org-clock-hd-marker) org-clock-hd-marker)))
           (pause (lr-track--current-pause))
           (step (lr-track--clock-step
                  (list :start start :end (lr-track--running-line-end)
                        :place (car place) :max (cdr place)
                        :paused-at (plist-get pause :paused-at)
                        :why (plist-get pause :why))
                  lr-track--presence (float-time)))
           (advance (plist-get step :advance-to))
           (paused-at (plist-get step :paused-at)))
      (when advance (lr-track--advance-clock-line advance))
      (when (and paused-at (not pause))
        ;; with the end the line reads now, so his later edit of it shows
        ;; (`lr-track--sync-paused-end')
        (setq lr-track--clock-pause
              (list :start start :paused-at paused-at :why (plist-get step :why)
                    :end (lr-track--running-line-end)))
        ;; the line is final until his own key: put it on disk now
        (lr-track--maybe-autosave (marker-buffer org-clock-marker) t)
        ;; and say paused in the modeline now, not at org's next minute
        (when (fboundp 'org-clock-update-mode-line)
          (ignore-errors (org-clock-update-mode-line)))))))

(defun lr-track--settle-clock ()
  "Bring presence and the running clock up to the newest sample, now.
The probe's sample can land after the tick that spawned it, and the next
tick may be a minute away.  Right after a wake that sample is the sleep:
until it is stepped, presence still says here and the clock is live, so his
clock-out, a switch or an answer in that window would book the whole night.
So the sample is stepped and the live-clock phase runs on it here, and any
pause it implies is recorded before anything reads the pause.  Idempotent: a
sample is stepped once and a pause is never undone.  Never signals."
  (condition-case err
      (progn (lr-track--step-sample lr-track--last-sample)
             (lr-track--tick-live-clock))
    (error (lr-track--log 'settle err))))

(defun lr-track--tick-stalled-p (now)
  "Non-nil when the tick is late at NOW: over `lr-track-slept-tick-multiple'
intervals since the last one, as after a sleep or App Nap."
  (and (numberp lr-track--last-tick)
       (> (- now lr-track--last-tick)
          (* lr-track-slept-tick-multiple lr-track--interval))))

(defun lr-track--emacs-sample (now idle)
  "The sample Emacs input makes at NOW, IDLE seconds after it.
Unlocked (Emacs took the input) with the last known wake: what
`lr-track--tick-probe' steps instead of a probe while he types."
  (list :time now :idle idle :locked nil
        :wake (or (plist-get lr-track--last-sample :wake)
                  (plist-get lr-track--presence :wake))))

(defun lr-track--settle-at-key (&optional now)
  "Settle the clock for his key at NOW (default now): the key is input too.
First `lr-track--settle-clock'.  Then the sample his key in Emacs makes
\(`lr-track--emacs-sample') is stepped, and the clock step runs on it: a
return no tick has sampled yet (he pressed y 25 s after unlocking) is then
stepped before anything reads presence, so his answer starts at that
return and `a' names the away he just ended, not the one before.  The
rule is `lr-track--tick-probe's: Emacs input under
`lr-track--probe-skip-seconds' ago (a key is), and not after a stalled tick
\(`lr-track--tick-stalled-p'): only the probe reads the wake that says the
Mac slept, and a sample with the old wake would hide the sleep.
Idempotent; never signals."
  (lr-track--settle-clock)
  (condition-case err
      (let ((now (or now (float-time)))
            (e (lr-track-emacs-idle-seconds))
            (prev (plist-get lr-track--presence :prev-time)))
        ;; the rule of `lr-track--tick-probe': Emacs input under
        ;; `lr-track--probe-skip-seconds' ago, and no stalled tick.  Only a
        ;; sample newer than presence's newest: it becomes the newest sample
        ;; (`lr-track--last-sample'), so it is never stepped again, and an
        ;; older one never reads as the wall clock stepping back
        (unless (or (>= e lr-track--probe-skip-seconds)
                    (lr-track--tick-stalled-p now)
                    (and (numberp prev) (<= now prev)))
          (let ((sample (lr-track--emacs-sample now e)))
            (setq lr-track--last-sample sample)
            (when (lr-track--step-sample sample)
              (lr-track--tick-live-clock)))))
    (error (lr-track--log 'settle-key err))))

;;;; clock-out at the pause, and the paused clock string

(defun lr-track--drop-ended ()
  "Forget what the clock-out filter recorded, freeing its markers."
  (let ((hd (plist-get lr-track--clock-out-ended :hd))
        (note (cdr (plist-get lr-track--clock-out-ended :note))))
    (when (markerp hd) (set-marker hd nil))
    (when (markerp note) (set-marker note nil)))
  (setq lr-track--clock-out-ended nil))

(defconst lr-track--note-tags
  '("now" "declared" "continued" "ended" "away" "sleep" "restarted" "picked")
  "The TAGs of the notes lr-track writes, `- lr TAG: TEXT'.")

(defconst lr-track--note-re
  (concat "[ \t]*- lr " (regexp-opt lr-track--note-tags) ": [^\n]*$")
  "A note lr-track wrote: its own grammar, never a list item of his that
merely starts with `- lr'.")

(defun lr-track--note-below-running-line ()
  "The `- lr' note right below the running clock's line, or nil.
As (TEXT . MARKER), MARKER at the note's line start: lr-track wrote it when
the line started (`lr-track-ask'), and it must not outlive the line.  Only a
line in lr-track's own grammar (`lr-track--note-re') is one: an entry with
no clock drawer puts the CLOCK line right above his own text."
  (when (and (markerp org-clock-marker) (marker-buffer org-clock-marker))
    (with-current-buffer (marker-buffer org-clock-marker)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char org-clock-marker)
          ;; only below the running line itself, never below a stale marker
          (when (and (save-excursion (beginning-of-line)
                                     (looking-at "[ \t]*CLOCK: "))
                     (= 0 (forward-line 1))
                     (looking-at lr-track--note-re))
            (cons (match-string-no-properties 0) (copy-marker (point)))))))))

(defun lr-track--delete-note-line (note)
  "Delete NOTE, (TEXT . MARKER) from `lr-track--note-below-running-line'.
Only when the line at MARKER still reads TEXT exactly; a LOGBOOK it leaves
empty goes too, as `org-clock-cancel' does.  The marker is freed.  Non-nil
when the line was deleted."
  (let ((m (cdr note))
        (done nil))
    (when (and (markerp m) (buffer-live-p (marker-buffer m)))
      (with-current-buffer (marker-buffer m)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char m)
            (when (and (bolp) (> (point) (point-min))
                       (looking-at (concat (regexp-quote (car note)) "$")))
              (delete-region (1- (point)) (line-end-position))
              (ignore-errors (org-remove-empty-drawer-at (point)))
              (setq done t))))))
    (when (markerp m) (set-marker m nil))
    done))

(defun lr-track--clock-stood-in-p ()
  "Non-nil while org stands another clock in for the running one.
`org-with-clock' let-binds `org-clock-marker' and the start time to a
dangling line (`org-resolve-clocks', `org-clock-clock-out'): whatever reads
the clock globals then reads that line, not the clock org runs."
  (and (boundp 'org-clock-marker)
       (not (eq org-clock-marker (default-toplevel-value 'org-clock-marker)))))

(defun lr-track--org-resolving-p ()
  "Non-nil inside org's own clock resolution.
That is `org-resolve-clocks' and its idle prompt, or any clock org stands in
\(`lr-track--clock-stood-in-p').  His answer there (k, K, g, G, s, C) names
its own end, so lr-track keeps out: no settle, no pause, no note."
  (or (bound-and-true-p org-clock-resolving-clocks)
      (lr-track--clock-stood-in-p)))

(defun lr-track--org-ends-it-p ()
  "Non-nil when org's own clock resolution names the end of this clock-out.
That is a clock org stands in (`lr-track--clock-stood-in-p'), or one it
clocks out while it resolves, outside a clock-in.  Not the clock-out of his
real clock inside the clock-in a resolution makes (his k, t, g or s for a
dangling line clocks that task in again): that one interrupts his clock as
any switch of his does, so it ends at the pause."
  (or (lr-track--clock-stood-in-p)
      (and (bound-and-true-p org-clock-resolving-clocks)
           (not (bound-and-true-p org-clock-clocking-in)))))

(defun lr-track--clock-out-args (args)
  "`org-clock-out' :filter-args: his clock-out of a paused clock ends there.
ARGS are (SWITCH-TO-STATE FAIL-QUIETLY AT-TIME).  Only his own clock-outs
\(`lr-track--internal' nil) are touched, and never one whose end org's own
clock resolution names (`lr-track--org-ends-it-p'): his key there does.
With no AT-TIME the clock is first settled (`lr-track--settle-clock'), so a
sample that came after the last tick counts; then, when the current clock is
paused, AT-TIME becomes the pause, never before the line's start as it reads
now (`lr-track--sync-running-start').  That covers agenda O, SPC c o and the
switch inside `org-clock-in', none of which pass one.  An explicit AT-TIME
always wins.  The `- lr' note under the running line is remembered for every
clock-out of his, so the hook can drop it when org removes the line as zero
time."
  (lr-track--drop-ended)
  (if (or lr-track--internal (lr-track--org-ends-it-p))
      args
    (lr-track--sync-running-start)
    (unless (nth 2 args) (lr-track--settle-clock))
    (let* ((pause (and (null (nth 2 args))
                       (condition-case nil (lr-track--current-pause)
                         (error nil))))
           (paused-at (and (plist-get pause :paused-at)
                           (max (plist-get pause :paused-at)
                                (float-time org-clock-start-time))))
           (note (condition-case nil (lr-track--note-below-running-line)
                   (error nil))))
      (setq lr-track--clock-out-ended
            (if paused-at
                (list :task (lr-track--clock-task)
                      :paused-at paused-at
                      :why (plist-get pause :why)
                      :start-stamp (format-time-string
                                    (org-time-stamp-format t t)
                                    org-clock-start-time)
                      :hd (and (markerp org-clock-hd-marker)
                               (marker-buffer org-clock-hd-marker)
                               (copy-marker org-clock-hd-marker))
                      :note note)
              (and note (list :note note))))
      (if (not paused-at)
          args
        (append (list (nth 0 args) (nth 1 args) (seconds-to-time paused-at))
                (nthcdr 3 args))))))

(defun lr-track--note-under-clock-line (start-stamp hd note)
  "Put NOTE on its own line right after the closed CLOCK line from START-STAMP.
The note takes that line's indentation.  The line is the one at point, where
`org-clock-out' leaves it, else the one inside HD's entry.  Non-nil when the
note was written."
  (let ((re (concat "^\\([ \t]*\\)CLOCK: " (regexp-quote start-stamp) "--")))
    (cl-flet ((write-here ()
                (when (looking-at re)
                  (let ((indent (match-string 1)))
                    (end-of-line)
                    (insert "\n" indent note)
                    t))))
      (or (save-excursion
            (save-restriction
              (widen)
              (beginning-of-line)
              (write-here)))
          (and (markerp hd) (buffer-live-p (marker-buffer hd))
               (with-current-buffer (marker-buffer hd)
                 (save-excursion
                   (save-restriction
                     (widen)
                     (goto-char hd)
                     (when (re-search-forward
                            re (save-excursion
                                 (or (ignore-errors (org-entry-end-position))
                                     (point-max)))
                            t)
                       (beginning-of-line)
                       (write-here))))))))))

(defun lr-track--zero-time-why (why)
  "Why a clock paused at its start (WHY, the pause's) earned nothing."
  (pcase why
    ('activity "you stayed at the Mac, so it earned nothing")
    ('unknown "nothing was seen after it started")
    (_ "you left right after clocking in")))

(defun lr-track--on-clock-out ()
  "`org-clock-out-hook': a clock ended; finish one that ended at its pause.
Every clock-out clears the pause and records `lr-track--last-clock-end'.  When
org removed the line as zero time, the `- lr' note that was under it goes
too, whatever ended it.  When the filter moved AT-TIME to the pause, the
ended note goes under the closed line and the echo says where it ended; or,
when the line was removed, that no time was recorded, and why.  A clock org
stood in for the running one (`lr-track--clock-stood-in-p', its own
resolution of a dangling line) changes none of that: the running clock, its
pause and its end are not the ones that ended."
  (if (lr-track--clock-stood-in-p)
      (lr-track--drop-ended)
    (lr-track--clock-ended)))

(defun lr-track--clock-ended ()
  "`lr-track--on-clock-out' for the clock org runs."
  (let ((ended lr-track--clock-out-ended))
    (setq lr-track--clock-out-ended nil
          lr-track--clock-pause nil)
    (when (and (boundp 'org-clock-out-time) org-clock-out-time)
      (setq lr-track--last-clock-end (float-time org-clock-out-time)))
    (when ended
      (condition-case err
          (let ((removed (bound-and-true-p org-clock-out-removed-last-clock))
                (task (or (plist-get ended :task) "The task"))
                (hm (and (plist-get ended :paused-at)
                         (lr-track--ts-hm (plist-get ended :paused-at)))))
            ;; a note must not outlive its line
            (when (and removed (plist-get ended :note))
              (lr-track--delete-note-line (plist-get ended :note)))
            (cond
             ((not hm))
             (removed
              (message "%s: no time recorded (%s)."
                       task (lr-track--zero-time-why (plist-get ended :why))))
             (t
              (lr-track--note-under-clock-line
               (plist-get ended :start-stamp) (plist-get ended :hd)
               (concat "- lr ended: where it paused " hm))
              (message "Clocked out of %s at %s, where it paused." task hm))))
        (error (lr-track--log 'clock-out err)))
      (let ((hd (plist-get ended :hd))
            (note (cdr (plist-get ended :note))))
        (when (markerp hd) (set-marker hd nil))
        (when (markerp note) (set-marker note nil))))))

(defun lr-track--clock-string (orig &rest args)
  ":around `org-clock-get-clock-string': a paused clock says so in the modeline.
TASK (cut to 20 characters) paused HH:MM, in the `warning' face, and within
org's MAX-LENGTH (the first of ARGS) when that is positive.  A live clock keeps
ORIG's own string, ARGS passed through."
  (let ((paused-at (condition-case nil (lr-track--paused-at) (error nil)))
        (task (and (boundp 'org-clock-current-task) org-clock-current-task))
        (limit (car args)))
    (if (and paused-at (stringp task))
        (let ((s (format "%s paused %s"
                         (substring-no-properties task 0 (min 20 (length task)))
                         (lr-track--ts-hm paused-at))))
          (when (and (integerp limit) (> limit 0) (> (length s) limit))
            (setq s (substring s 0 limit)))
          (propertize s 'face 'warning))
      (apply orig args))))

(defun lr-track--cancel-takes-line-p ()
  "Non-nil when `org-clock-cancel' will delete the running line.
Org's own test: the text before `org-clock-marker' on its line is a CLOCK
line.  A marker at the line's start (the line retyped in place) fails it,
and org then only says Clock gone and keeps the line."
  (and (markerp org-clock-marker) (marker-buffer org-clock-marker)
       (with-current-buffer (marker-buffer org-clock-marker)
         (save-excursion
           (save-restriction
             (widen)
             (goto-char org-clock-marker)
             (looking-back "^[ \t]*CLOCK:.*" (line-beginning-position)))))))

(defun lr-track--clock-cancel-note (&rest _)
  ":before `org-clock-cancel': his cancel takes the running line's `- lr' note.
It goes first, so a LOGBOOK the cancel then empties still goes, as org
leaves it: a note must not outlive its line.  Only when org will delete the
line (`lr-track--cancel-takes-line-p'): a line org keeps keeps its note.
lr-track's own cancels (`lr-track--internal') handle their notes themselves.
Never signals."
  (unless lr-track--internal
    (condition-case err
        (let ((note (and (lr-track--cancel-takes-line-p)
                         (lr-track--note-below-running-line))))
          (when note (lr-track--delete-note-line note)))
      (error (lr-track--log 'cancel-note err)))))

(defun lr-track--hold-here-block ()
  "`org-clock-in-hook': a line starts in this here-block, so it is no glance.
Presence then never merges the block into the aways around it
\(`lr-track--presence-leave'): the night stays split at his line, and the
away after it can be labelled on its own.  An answer written in the block
\(`lr-track--execute-plan', a label too) holds it the same way."
  (let ((p lr-track--presence))
    (when (and (eq (plist-get p :mode) 'here)
               (numberp (plist-get p :return)))
      (setq lr-track--presence
            (plist-put (copy-sequence p) :held (plist-get p :return))))))

(defun lr-track--clock-in-paused-task (&optional select _start-time)
  ":before `org-clock-in': his clock-in on the paused task itself starts anew.
Org would only say Clock continues and leave it paused, so the time after his
explicit clock-in would never be earned and his clock-out would end it at the
old pause.  So the paused clock is clocked out first, which the filter ends
at its pause with its note, and org then starts a new line, exactly as when
he switches to another task.  Only his own clock-in (`lr-track--internal'
nil), outside org's clock resolution (`lr-track--org-resolving-p'), with no
task prompt (SELECT nil or the default-task prefix), on the very heading org
clocks.  The clock is settled first
\(`lr-track--settle-clock'), so a pause the newest sample implies counts.
Point stays in the heading's entry: a clock-out at the line's start removes
the 0:00 line, its note and the drawer they emptied, and point there would
fall onto the next heading, which org would then clock.  Never signals: on
any error org runs as it would."
  (condition-case err
      (when (and (not lr-track--internal)
                 (not (lr-track--org-resolving-p))
                 (progn (lr-track--settle-clock) t)
                 (or (null select) (equal select '(16)))
                 (derived-mode-p 'org-mode)
                 (lr-track--paused-at)
                 (markerp org-clock-hd-marker)
                 (eq (marker-buffer org-clock-hd-marker)
                     (org-base-buffer (current-buffer)))
                 (stringp org-clock-current-task)
                 (save-excursion
                   (org-back-to-heading t)
                   (and (= (point) (marker-position org-clock-hd-marker))
                        (equal org-clock-current-task
                               (org-get-heading t t t t)))))
        (let ((hd (save-excursion (org-back-to-heading t) (point-marker))))
          (unwind-protect
              (progn
                (org-clock-out nil t)
                (unless (ignore-errors
                          (= (save-excursion (org-back-to-heading t) (point))
                             hd))
                  (goto-char hd)))
            (set-marker hd nil))))
    (error (lr-track--log 'clock-in-paused err))))

;; Installed at load, never by the mode: deploy is by `load' (I13), and each of
;; these is idempotent, so a reload adds nothing twice.  Every one also works
;; before org-clock loads.
(advice-add 'org-clock-in :before #'lr-track--clock-in-paused-task)
(advice-add 'org-clock-out :filter-args #'lr-track--clock-out-args)
(advice-add 'org-clock-get-clock-string :around #'lr-track--clock-string)
(advice-add 'org-clock-cancel :before #'lr-track--clock-cancel-note)
(add-hook 'org-clock-in-hook #'lr-track--forget-pause)
(add-hook 'org-clock-in-hook #'lr-track--hold-here-block)
(add-hook 'org-clock-cancel-hook #'lr-track--forget-pause)
(add-hook 'org-clock-out-hook #'lr-track--on-clock-out)

(defun lr-track--autoout (at-float cause)
  "Close the running clock at AT-FLOAT (float seconds) for CAUSE.
Returns (:action clock-out|cancel|failed :at :elapsed-s :line-removed) or nil.
NEVER signals, NEVER saves the buffer, and can never write a 0:00 (deletable)
or inverted interval.  See the module commentary for why each guard exists."
  (require 'org-clock)
  (when (lr-track--clocking-p)
    (condition-case err
        (let* ((start (float-time org-clock-start-time))
               (target (max (or (and (numberp at-float) at-float) 0.0)
                            (+ start lr-track-autoout-floor-seconds))))
          (cond
           ;; whole interval was idle, so cancel (remove the line), never invert
           ((and (numberp at-float) (<= at-float start))
            (lr-track--with-pristine-org-globals (org-clock-cancel))
            (list :action 'cancel :cause cause :at at-float :elapsed-s 0.0 :line-removed t))
           (t
            (let ((removed nil))
              (lr-track--with-pristine-org-globals
                (let ((org-clock-out-removed-last-clock nil)
                      (lr-track--internal t))
                  (org-clock-out nil t (seconds-to-time target))
                  (setq removed org-clock-out-removed-last-clock)))
              ;; post-condition: fail-quietly may have silently aborted
              (if (lr-track--clocking-p)
                  (list :action 'failed :cause cause :at target
                        :elapsed-s (- target start) :line-removed nil)
                (list :action 'clock-out :cause cause :at target
                      :elapsed-s (- target start) :line-removed removed))))))
      (error (lr-track--log 'autoout err)
             (list :action 'failed :cause cause :line-removed nil)))))

;;;; sidecar: heartbeat + crash

;; The ONE machine-local file (outside ~/roam): a heartbeat + running-clock
;; pointer, so a clock a crashed/killed session left OPEN on disk can be closed
;; at the last known-alive moment on the next start, instead of silently
;; accruing hours.  It holds no fact about your work, only operational
;; scaffolding that is fine to lose.

(declare-function org-find-open-clocks "org-clock" (file))
(declare-function org-clock-clock-out "org-clock" (clock &optional fail-quietly at-time))
(declare-function org-clock-clock-cancel "org-clock" (&optional clock))
(declare-function org-is-active-clock "org-clock" (clock))
(declare-function org-clocking-buffer "org-clock" ())
(declare-function org-read-date "org" (&optional with-time to-time from-string prompt default-time default-input inactive))

(defun lr-track--cache-dir ()
  (expand-file-name "lr-track/" (or (bound-and-true-p doom-data-dir) user-emacs-directory)))
(defun lr-track--state-file () (expand-file-name "state.eld" (lr-track--cache-dir)))

(defun lr-track--read-state ()
  (let ((f (lr-track--state-file)))
    (when (file-exists-p f)
      (ignore-errors (with-temp-buffer (insert-file-contents f) (read (current-buffer)))))))

(defun lr-track--write-state (plist)
  (ignore-errors
    (make-directory (lr-track--cache-dir) t)
    (let ((tmp (make-temp-file (expand-file-name "st" (lr-track--cache-dir)))))
      (with-temp-file tmp (let ((print-length nil) (print-level nil)) (prin1 plist (current-buffer))))
      (rename-file tmp (lr-track--state-file) t))))

(defun lr-track--state-put (&rest kvs)
  "Update the in-memory state with KVS (a plist) and flush it to disk."
  (while kvs (setq lr-track--state (plist-put lr-track--state (pop kvs) (pop kvs))))
  (lr-track--write-state lr-track--state))

(defun lr-track--clock-file ()
  (when (and (fboundp 'org-clocking-buffer) (org-clocking-buffer))
    (buffer-file-name (org-clocking-buffer))))

(defun lr-track--tick-heartbeat ()
  "Stamp a heartbeat every tick (so a restart knows how long Emacs was off) and,
while clocked, the running-clock pointer (for crash recovery) and its pause."
  (if (lr-track--clocking-p)
      (lr-track--state-put
       :heartbeat (float-time) :pid (emacs-pid)
       :clock (list :file (lr-track--clock-file)
                    :task (lr-track--clock-task)
                    :started (and (boundp 'org-clock-start-time) org-clock-start-time
                                  (float-time org-clock-start-time)))
       :clock-pause (lr-track--current-pause))
    (lr-track--state-put :heartbeat (float-time) :clock nil :clock-pause nil)))

(defun lr-track--recover-prompt (task at)
  "Startup-safe: never steal input; time out to `?n' (leave alone) after 60s.
It runs from an idle timer, so with no Emacs frame focused it never asks: a
question nobody sees would take the keys he types once he comes back."
  (if (or noninteractive (active-minibuffer-window) executing-kbd-macro
          defining-kbd-macro (> (recursion-depth) 0)
          (not (lr-track--frame-focused-now-p)))
      ?n
    (condition-case nil
        (with-timeout (60 ?n)
          (read-char-choice
           (format "lr-track: a clock was left running on \"%s\", close at %s? [y]es [e]dit-time [n]o: "
                   (or task "a task") (lr-track--ts at))
           '(?y ?e ?n)))
      (quit ?n))))

(defun lr-track--recover-close (clock at)
  "Close CLOCK (a (MARKER . START) cons) at time AT with the CLOCK-safety defenses.
Return `closed', `removed', or `cancel'."
  (let* ((start (float-time (cdr clock)))
         (at-float (float-time at))
         (target (max at-float (+ start lr-track-autoout-floor-seconds))))
    (if (<= at-float start)
        (progn (lr-track--with-pristine-org-globals (org-clock-clock-cancel clock)) 'cancel)
      (let ((removed nil))
        (lr-track--with-pristine-org-globals
          (let ((org-clock-out-removed-last-clock nil)
                (lr-track--internal t))
            (org-clock-clock-out clock nil (seconds-to-time target))
            (setq removed org-clock-out-removed-last-clock)))
        (if removed 'removed 'closed)))))

(defun lr-track--recover-file (file task hb-float)
  "Close any clock a crashed session left OPEN in FILE, at HB-FLOAT (last heartbeat).
Returns the list of outcomes.  Opened buffers do not get the save-time rewriters
\(org-mode-hook bound nil); the closed buffer is saved via `lr-track--save-buffer'."
  (require 'org-clock)
  (let ((at (seconds-to-time hb-float))
        (org-mode-hook nil)
        (outcomes nil))
    (dolist (clock (org-find-open-clocks file))
      (unless (and (fboundp 'org-is-active-clock) (org-is-active-clock clock))
        (let ((choice (lr-track--recover-prompt task at)))
          (pcase choice
            (?n (push 'left outcomes))
            (?e (let ((picked (org-read-date t t nil "Close the dangling clock at: ")))
                  (push (if picked (lr-track--recover-close clock picked) 'left) outcomes)))
            (?y (push (lr-track--recover-close clock at) outcomes)))
          (when (memq choice '(?y ?e))
            (let ((buf (marker-buffer (car clock))))
              (when (buffer-live-p buf)
                (lr-track--save-buffer buf)
                (display-buffer buf)))))))
    (nreverse outcomes)))

(defun lr-track--recover ()
  "Once per session, close any clock a crashed previous session left open."
  (unless lr-track--recovered
    (setq lr-track--recovered t)
    (let* ((prev lr-track--state-prev)
           (clean (plist-get prev :clean))
           (clock (plist-get prev :clock))
           (hb (plist-get prev :heartbeat)))
      (when (and prev (not clean) clock hb)
        (let ((file (plist-get clock :file)) (task (plist-get clock :task)))
          (when (and file (file-exists-p file))
            (require 'org)
            (condition-case e (lr-track--recover-file file task hb)
              (error (lr-track--log 'recover e)))))))))

(defun lr-track--recover-quietly ()
  "The idle timer `lr-track--enable' arms: `lr-track--recover', never raising.
A named function, not a lambda, so `lr-track--reap-timers' finds it."
  (condition-case e (lr-track--recover)
    (error (lr-track--log 'recover e))))

;;;; org: activity log

(defun lr-track--activity-file () (expand-file-name lr-track-activity-file))

(defconst lr-track--activity-seed "\
#+title: Activity
#+filetags: :activity:
#+STARTUP: overview

# Managed by lr-track.el; plain org.  Each engaged/elsewhere/away/asleep episode
# is a CLOCK line under its state heading, so C-c C-x C-r here totals how your
# attention actually went, per day/week.

* Engaged
* Elsewhere
* Away
* Asleep
")

(defun lr-track--activity-buffer ()
  (let ((file (lr-track--activity-file)))
    (unless (file-exists-p file)
      (make-directory (file-name-directory file) t)
      (with-temp-file file (insert lr-track--activity-seed)))
    (find-file-noselect file)))

(defun lr-track--save-buffer (buffer)
  "Save BUFFER with this config's three before-save rewriters neutralised.
A bare `let'-bind of the buffer-local hook cannot suppress them; only overriding
the symbol-functions reaches an installed buffer-local entry.  Silent: no Wrote
line, in the echo area or in *Messages*.  A buffer whose file changed on disk
since it was read is not saved, nor one whose file is write-protected (a
chmod, or the Finder lock, leaves the modification time alone), nor one with
no file at all: the save would ask whether to overwrite it, to try anyway,
or for a file name, and this runs from timers, his keys and kill-emacs.  All
three are logged; the line stays in the buffer."
  (with-current-buffer buffer
    (when (and (buffer-modified-p)
               (or (lr-track--buffer-current-p buffer)
                   (progn (lr-track--log 'save-stale (buffer-name)) nil))
               ;; no file: `save-buffer' would ask for one
               (or buffer-file-name
                   (progn (lr-track--log 'save-no-file (buffer-name)) nil))
               (or (file-writable-p buffer-file-name)
                   (progn (lr-track--log 'save-write-protected (buffer-name))
                          nil)))
      (let* ((names (seq-filter #'fboundp '(toc-org-insert-toc
                                            vulpea-project-update-tag
                                            org-roam-link-replace-all)))
             (saved (mapcar (lambda (n) (cons n (symbol-function n))) names)))
        (unwind-protect
            (progn (dolist (n names) (fset n #'ignore))
                   (let ((save-silently t) (inhibit-message t))
                     (save-buffer)))
          (dolist (p saved) (fset (car p) (cdr p))))))))

(defun lr-track--ts (time) (format-time-string (org-time-stamp-format t t) time))
(defun lr-track--ts-hm (x) (format-time-string "%H:%M" (if (numberp x) (seconds-to-time x) x)))

(defun lr-track--fmt-dur (mins)
  "Compact human duration for MINS minutes: \"0m\", \"40m\", \"2h\", \"1h15m\"."
  (let* ((m (max 0 (round mins))) (h (/ m 60)) (r (% m 60)))
    (cond ((= m 0) "0m")
          ((= h 0) (format "%dm" r))
          ((= r 0) (format "%dh" h))
          (t (format "%dh%dm" h r)))))

(defun lr-track--activity-log (state start-float end-float)
  "Append a CLOCK line for the STATE episode START..END under its heading.
Episodes shorter than a minute are dropped, they round to 0:00 and are just
state flap, not signal."
  (require 'org)
  (when (and start-float end-float (>= (- end-float start-float) 60.0))
    (let ((heading (capitalize (symbol-name state)))
          (buf (lr-track--activity-buffer)))
      (with-current-buffer buf
        (org-with-wide-buffer
         (goto-char (point-min))
         (when (re-search-forward (format "^\\* %s[ \t]*$" (regexp-quote heading)) nil t)
           (let* ((mins (/ (- end-float start-float) 60.0))
                  (line (format "CLOCK: %s--%s =>  %s"
                                (lr-track--ts (seconds-to-time start-float))
                                (lr-track--ts (seconds-to-time end-float))
                                (org-duration-from-minutes mins)))
                  (org-log-into-drawer "LOGBOOK"))
             (goto-char (org-log-beginning t))
             (insert line "\n"))))
        (lr-track--save-buffer buf)))))

;;;; nudge core

(defun lr-track--day-index ()
  "Integer day index honoring `org-extend-today-until' (read live, never cached)."
  (let ((cut (or (and (boundp 'org-extend-today-until) org-extend-today-until) 0)))
    (floor (/ (- (float-time) (* cut 3600)) 86400))))

(defun lr-track--budget-reset-if-needed ()
  (let ((d (lr-track--day-index)))
    (unless (equal d lr-track--budget-day)
      (setq lr-track--budget-day d lr-track--budget-spent 0 lr-track--cooldowns nil))))

(defun lr-track--may-interrupt-p (category)
  "PURE predicate: may a banner in CATEGORY fire now?  Consumes nothing."
  (lr-track--budget-reset-if-needed)
  (and (< lr-track--budget-spent lr-track-daily-banner-budget)
       (let ((last (cdr (assq category lr-track--cooldowns))))
         (or (null last) (>= (- (float-time) last) lr-track-category-cooldown)))))

(defun lr-track--charge (category)
  (setq lr-track--budget-spent (1+ lr-track--budget-spent))
  (setf (alist-get category lr-track--cooldowns) (float-time)))

(defun lr-track-notify (level category msg)
  "Deliver a nudge.  LEVEL <=1 is modeline-only (the modeline already reflects
the condition, so this is a no-op); LEVEL >=2 is a macOS banner, gated by the
daily budget + per-category cooldown and charged only when actually delivered."
  (when (>= level 2)
    (when (lr-track--may-interrupt-p category)
      (lr-track--charge category)
      (run-with-timer
       0 nil
       (lambda ()
         (condition-case e
             (if (require 'alert nil t)
                 (alert msg :title "lr-track coach" :style 'osx-notifier)
               (message "lr-track: %s" msg))
           (error (lr-track--log 'notify e))))))))

;;;; detectors

(defun lr-track--incident (key) (assoc key lr-track--incidents))
(defun lr-track--incident-open (key)
  (or (lr-track--incident key)
      (car (push (list key :since (float-time) :level 0) lr-track--incidents))))
(defun lr-track--incident-close (key)
  (setq lr-track--incidents (assq-delete-all key lr-track--incidents)))

(defvar lr-track--away-pending nil "When set (a float), an away stretch awaits your decision on return.")
(defvar lr-track--snooze-until nil "Float time until which nudges AND auto-close are paused.")
(defvar lr-track--last-close nil "Most recent auto-close, for review: (:task :marker :at).")
(defvar lr-track--resolving nil "Non-nil while a check-in prompt is up (reentrancy guard).")

(defun lr-track--snoozed-p () (and lr-track--snooze-until (< (float-time) lr-track--snooze-until)))

(defun lr-track--safe-to-prompt-p ()
  (not (or noninteractive (active-minibuffer-window) executing-kbd-macro
           defining-kbd-macro (> (recursion-depth) 0) lr-track--resolving)))

;; DESIGN: HID (are you at the machine?) is the source of truth for presence,
;; NEVER Emacs focus.  You often work outside Emacs, so "elsewhere" (at the
;; machine, not in Emacs) is PRESUMED WORKING, it is never treated as a
;; distraction.  The clock is only defended when you've actually left the
;; machine (away) or it slept.  "elsewhere" earns only a gentle, occasional
;; "still on it?" check-in, because Emacs genuinely cannot see your other apps.

(defun lr-track--close-running-clock (state)
  "Auto-close the running clock because you are away/slept.  Close at WHEN YOU
LEFT (away: now minus system idle; slept: now minus gap), never at now.  Notifies."
  (let* ((now (float-time))
         (att lr-track--tick-attention)
         (task (or (lr-track--clock-task) "your task"))
         (marker (and (boundp 'org-clock-hd-marker) (markerp org-clock-hd-marker)
                      (marker-buffer org-clock-hd-marker) (copy-marker org-clock-hd-marker)))
         (at (pcase state
               ('slept (let ((g (plist-get att :gap))) (and (numberp g) (- now g))))
               ('away  (let ((s (lr-track-system-idle-seconds))) (and (numberp s) (- now s)))))))
    (when at
      (let ((res (lr-track--autoout at (if (eq state 'slept) 'auto_slept 'auto_idle))))
        (when (and res (memq (plist-get res :action) '(clock-out cancel)))
          (setq lr-track--last-close (list :task task :marker marker :at (plist-get res :at))
                lr-track--checkin-since (plist-get res :at)))   ; the gap starts where the clock ended
        (when res
          (lr-track-notify
           2 'clock
           (pcase (plist-get res :action)
             ('clock-out (format "Auto-closed \"%s\", you were %s. Stopped at %s."
                                 task state (lr-track--ts (seconds-to-time (plist-get res :at)))))
             ('cancel    (format "Auto-closed \"%s\", whole interval idle; CLOCK line removed." task))
             (_          (format "Auto-close of \"%s\" failed (%s); clock left as-is." task state)))))
        res))))

(defun lr-track--check-away ()
  "Coach the clock when you've LEFT THE MACHINE (away) or it slept.
Away escalates: a one-time check-in banner, then an auto-close (if enabled)
after you stay away, closing at when you left."
  (when (lr-track--clocking-p)
    (pcase lr-track--stable-state
      ('slept
       (if lr-track-auto-clock-out
           (progn (lr-track--close-running-clock 'slept)   ; records the gap in `lr-track--last-close'
                  (lr-track--incident-close 'away))
         ;; keeping the clock running: remember when sleep began, so the return
         ;; check-in can attribute the whole gap to this same task or split it out
         (let ((g (plist-get lr-track--tick-attention :gap)))
           (when (numberp g) (setq lr-track--away-pending (- (float-time) g))))))
      ('away
       (let* ((inc (lr-track--incident-open 'away))
              (age (- (float-time) (plist-get (cdr inc) :since)))
              (level (plist-get (cdr inc) :level)))
         (setq lr-track--away-pending (plist-get (cdr inc) :since))  ; offer a decision on return
         (unless (plist-get (cdr inc) :keep)                         ; "keep" from resolve pauses this
           (cond
            ;; sustained away, so auto-close at when you left
            ((and lr-track-auto-clock-out (>= age lr-track-away-close-seconds))
             (lr-track--close-running-clock 'away)
             (lr-track--incident-close 'away))
            ;; otherwise a one-time check-in banner
            ((and (< level 2) (>= age lr-track-away-nudge-seconds))
             (setf (plist-get (cdr inc) :level) 2)
             (lr-track-notify 2 'away
                              (format "Clocked into \"%s\" but away from your machine. Still on it? (SPC d j)"
                                      (or (lr-track--clock-task) "a task"))))))))
      (_ (lr-track--incident-close 'away)))))

(defun lr-track--check-ceiling ()
  "A clock running past the 4h ceiling is capped at your last-active moment."
  (when (lr-track--clocking-p)
    (let ((elapsed (lr-track--clock-elapsed)))
      (when (and elapsed (> elapsed lr-track-max-clock-seconds))
        (let* ((inc (lr-track--incident-open 'ceiling))
               (at (or lr-track--last-engaged
                       (+ (float-time org-clock-start-time) lr-track-autoout-floor-seconds))))
          (when lr-track-auto-clock-out (lr-track--autoout at 'auto_ceiling))
          (unless (eq (plist-get (cdr inc) :level) 1)
            (setf (plist-get (cdr inc) :level) 1)
            (lr-track-notify 2 'ceiling
                             (if lr-track-auto-clock-out
                                 "Clock ran past 4h, capped at your last active moment."
                               "Clock has been running over 4h. Is it still real?")))
          (unless (lr-track--clocking-p) (lr-track--incident-close 'ceiling)))))))

(defun lr-track--check-elsewhere ()
  "Gently check in when you're clocked but working OUTSIDE Emacs for a while.
Emacs cannot see your other apps, so this NEVER accuses, it only asks, and it
re-arms so you get at most one check-in per `lr-track-elsewhere-checkin-seconds'."
  (if (and lr-track-elsewhere-checkin-seconds
           (lr-track--clocking-p) (eq lr-track--stable-state 'elsewhere))
      (let* ((inc (lr-track--incident-open 'elsewhere))
             (age (- (float-time) (plist-get (cdr inc) :since))))
        (when (>= age lr-track-elsewhere-checkin-seconds)
          (setf (plist-get (cdr inc) :since) (float-time))   ; re-arm, don't nag every tick
          (lr-track-notify 2 'elsewhere
                           (format "Still on \"%s\"? You've been working outside Emacs a while."
                                   (or (lr-track--clock-task) "your task")))))
    (lr-track--incident-close 'elsewhere)))

(defun lr-track--maybe-prompt-on-return ()
  "Tick-side backup to the focus trigger: when you are present in Emacs again and
there is an unaccounted stretch (an away/slept pending, or a clock the coach just
auto-closed), run the return check-in so no minute is lost."
  (when (and lr-track-checkin-on-return
             (memq lr-track--stable-state '(engaged reading))
             (or lr-track--away-pending lr-track--last-close))
    (when lr-track--away-pending
      (setq lr-track--checkin-since lr-track--away-pending))   ; remember when you left
    (lr-track--trigger-checkin 'return)))

(defun lr-track--nudge-check ()
  "Run detectors, each contained.  Snooze pauses everything; no clock clears all."
  (cond
   ((not (lr-track--clocking-p))
    (setq lr-track--incidents nil lr-track--away-pending nil)
    (lr-track--maybe-prompt-on-return))   ; a clock the coach just auto-closed still owes a check-in
   ((lr-track--snoozed-p) nil)          ; you asked for quiet, no nudges, no auto-close
   (t
    (dolist (fn '(lr-track--check-away lr-track--check-ceiling lr-track--check-elsewhere))
      (condition-case e (funcall fn) (error (lr-track--log 'nudge (cons fn e)))))
    (lr-track--maybe-prompt-on-return))))

;;;; check-in (the coach)

(defun lr-track--trigger-checkin (context)
  "Schedule the in-Emacs check-in (CONTEXT is `return' or nil), once per cooldown
and only when safe to prompt.  Runs on a short idle timer so it never fights a
command in flight."
  (when (and (>= (- (float-time) lr-track--last-checkin) lr-track-checkin-cooldown)
             (lr-track--safe-to-prompt-p))
    (setq lr-track--last-checkin (float-time)
          lr-track--away-pending nil)
    (run-with-idle-timer
     1.0 nil (lambda () (lr-track--open-checkin-if-seen context)))))

(defun lr-track--open-checkin-if-seen (context)
  "Open the check-in only if someone is looking.  Emacs is idle while unfocused,
so this idle timer fires after a pass-through too; opening then would leave a
prompt in the void.  Instead re-arm: the cooldown resets and the departure
rewinds to the earliest unanswered gap, so the next real return asks it all."
  (if (lr-track--frame-focused-now-p)
      (when (lr-track--safe-to-prompt-p)
        (ignore-errors (lr-track-checkin context)))
    (setq lr-track--last-checkin 0.0
          lr-track--unfocused-since
          (let ((a lr-track--checkin-since) (b lr-track--unfocused-since))
            (cond ((and a b) (min a b)) (a a) (b b) (t (float-time)))))))

(defcustom lr-track-task-cache-ttl 60.0
  "Seconds the check-in memoizes the agenda task list, so repeated prompts are instant."
  :type 'number)

(defvar lr-track--task-cache nil
  "Cons (FLOAT-TIME . CANDIDATES) memoizing the agenda task list.")

(defun lr-track--agenda-files ()
  "The existing agenda files to search (refresh the vulpea-computed set if we can)."
  (when (fboundp 'vulpea-agenda-files-update) (ignore-errors (vulpea-agenda-files-update)))
  (seq-filter #'file-exists-p
              (cond ((listp org-agenda-files) org-agenda-files)
                    ((fboundp 'org-agenda-files) (org-agenda-files))
                    (t nil))))

(defun lr-track--format-candidate (heading file tags width)
  "A citar-style completion string: HEADING left aligned to WIDTH, then the FILE
and TAGS dimmed on the right.  It is all one string, so completion (vertico or
orderless) can filter by task text, file name, or tag."
  (concat heading
          (make-string (max 2 (- width (string-width heading))) ?\s)
          (propertize (file-name-nondirectory (or file "?")) 'face 'completions-annotations)
          (if (and tags (> (length tags) 0)) (concat "  " (propertize tags 'face 'org-tag)) "")))

(defun lr-track--collect-tasks ()
  "List of (HEADING FILE TAGS MARKER): recently clocked first, then every TODO
across the agenda files (via org-ql), de-duplicated by file+heading."
  (require 'org)
  (let (out seen)
    (cl-flet ((add (h f tags mk)
                (when (and h (stringp h) (markerp mk))
                  (setq h (substring-no-properties (string-trim h)))
                  (when (> (length h) 0)
                    (let ((key (cons f h)))
                      (unless (member key seen)
                        (push key seen)
                        (push (list h f (substring-no-properties (or tags "")) mk) out)))))))
      (dolist (m (and (boundp 'org-clock-history) org-clock-history))
        (when (and (markerp m) (marker-buffer m))
          (org-with-point-at m
            (add (org-get-heading t t t t) (buffer-file-name)
                 (org-make-tag-string (org-get-tags)) m))))
      (when (require 'org-ql nil t)
        (dolist (item (ignore-errors
                        (org-ql-select (lr-track--agenda-files) '(todo)
                          :action (lambda () (list (org-get-heading t t t t) (buffer-file-name)
                                                   (org-make-tag-string (org-get-tags))
                                                   (point-marker))))))
          (apply #'add item))))
    (nreverse out)))

(defun lr-track--task-candidates ()
  "Memoized alist of (DISPLAY . (HEADING . MARKER)) for the task picker."
  (let ((now (float-time)))
    (if (and lr-track--task-cache (< (- now (car lr-track--task-cache)) lr-track-task-cache-ttl))
        (cdr lr-track--task-cache)
      (let* ((raw (lr-track--collect-tasks))
             (width (if raw (min 60 (apply #'max 20 (mapcar (lambda (r) (string-width (car r))) raw))) 20))
             (cands (mapcar (lambda (r)
                              (pcase-let ((`(,h ,f ,tags ,mk) r))
                                (cons (lr-track--format-candidate h f tags width) (cons h mk))))
                            raw)))
        (setq lr-track--task-cache (cons now cands))
        cands))))

(defun lr-track--create-task (text)
  "Create a `* TODO TEXT' in the todo file, invalidate the cache, return its marker."
  (require 'org-clock)
  (let ((file (expand-file-name
               (or (and (boundp '+org-capture-todo-file) +org-capture-todo-file)
                   (and (boundp 'org-default-notes-file) org-default-notes-file)
                   "~/roam/main/life.org"))))
    (setq lr-track--task-cache nil)
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-max)) (unless (bolp) (insert "\n"))
       (let ((pt (point))) (insert "* TODO " text "\n") (copy-marker pt))))))

(defconst lr-track--checkin-specials
  '(("asleep" . :asleep) ("away / break" . :away))
  "Non-work picks in the check-in that log to activity.org, not a task file.")

(defun lr-track--special-kw (text)
  "Map free text like \"sleep\"/\"away\" to a :asleep/:away keyword, else nil."
  (pcase (downcase (string-trim text))
    ((or "sleep" "asleep" "slept" "sleeping" "nap") :asleep)
    ((or "away" "afk" "break" "out") :away)
    (_ nil)))

(defun lr-track--pick-task (prompt &optional no-specials)
  "Prompt for a task with completion over recent tasks plus all agenda TODOs
(filter by task text or file name).  Return (LABEL . TARGET): a MARKER for an
existing task, a :asleep/:away keyword for sleep/away (unless NO-SPECIALS), or,
only on explicit confirmation, a newly created TODO.  A non-matching typo never
silently appends a task."
  (let* ((cands (lr-track--task-candidates))
         (all (if no-specials (mapcar #'car cands)
                (append (mapcar #'car lr-track--checkin-specials) (mapcar #'car cands))))
         (pick (string-trim (completing-read prompt all nil nil))))
    (when (> (length pick) 0)
      (let ((sp (unless no-specials
                  (or (cdr (assoc pick lr-track--checkin-specials)) (lr-track--special-kw pick))))
            (hit (cdr (assoc pick cands))))
        (cond
         (sp (cons pick sp))
         (hit (cons (car hit) (cdr hit)))
         ((y-or-n-p (format "No matching task. Create a new TODO \"%s\"? " pick))
          (cons pick (lr-track--create-task pick)))
         (t nil))))))

;;;###autoload
(defun lr-track-clock-in ()
  "Clock into a task chosen by searching across all your agenda files.
Filter the list by task text or by file name.  Selecting a match clocks into it;
typing a name that matches nothing offers to create a TODO first.  This is the
same search interface the check-in uses."
  (interactive)
  (require 'org-clock)
  (let ((task (lr-track--pick-task "Clock in: " t)))
    (cond
     ((null task) (message "lr-track: nothing selected."))
     ((keywordp (cdr task)) (message "\"%s\" is not a task to clock into." (car task)))
     (t (org-with-point-at (cdr task) (org-clock-in))
        (message "Clocked into %s." (car task))))))

(defun lr-track--left-since ()
  "Best float estimate of when you left: the check-in start, else now minus idle."
  (or lr-track--checkin-since
      (let ((s (lr-track-system-idle-seconds)))
        (if (numberp s) (- (float-time) s) (- (float-time) (lr-track-emacs-idle-seconds))))))

(defun lr-track--gap-start ()
  "Authoritative start of the currently-unaccounted stretch.  Prefer the moment the
coach last auto-closed a clock (survives laptop sleep, where the idle counters
reset to ~0 on wake and would otherwise report no gap at all); else fall back to
when you left."
  (or (and lr-track--last-close
           (numberp (plist-get lr-track--last-close :at))
           (plist-get lr-track--last-close :at))
      (lr-track--left-since)))

(defun lr-track--clock-task-from (task since)
  "Clock into TASK (LABEL . MARKER) as if it had started at SINCE (float) -
i.e. a running clock backdated to when you actually started.  A :asleep/:away
TARGET is logged closed instead (you can't be currently asleep and clocking)."
  (require 'org-clock)
  (let ((marker (cdr task)))
    (cond
     ((keywordp marker) (lr-track--log-task-interval task since (float-time)))
     ((and marker (marker-buffer marker))
      (org-with-point-at marker (org-clock-in))
      (let ((start (seconds-to-time since)))
        (setq org-clock-start-time start)
        (when (and (markerp org-clock-marker) (marker-buffer org-clock-marker))
          (with-current-buffer (marker-buffer org-clock-marker)
            (org-with-wide-buffer
             (goto-char org-clock-marker) (beginning-of-line)
             (when (re-search-forward "\\(CLOCK: \\)\\(\\[[^]]+\\]\\)" (line-end-position) t)
               (replace-match (format-time-string (org-time-stamp-format t t) start) t t nil 2))))))
      (message "Clocked into %s, running since %s (%dm so far)." (car task)
               (lr-track--ts-hm since) (max 0 (round (/ (- (float-time) since) 60.0))))))))

(defun lr-track--log-task-interval (task start-float end-float)
  "Log a completed interval START..END for TASK (LABEL . TARGET).  A marker TARGET
gets a CLOCK line in its LOGBOOK; a :asleep/:away keyword logs to activity.org -
its one legitimate use, since sleep/away time has no task home."
  (require 'org)
  (let ((target (cdr task))
        (mins (max 0 (round (/ (- end-float start-float) 60.0)))))
    (cond
     ((memq target '(:asleep :away))
      (lr-track--activity-log (if (eq target :asleep) 'asleep 'away) start-float end-float)
      (message "Logged %s to activity: %s to %s (%dm)." (car task)
               (lr-track--ts-hm start-float) (lr-track--ts-hm end-float) mins))
     ((and (markerp target) (marker-buffer target))
      (with-current-buffer (marker-buffer target)
        (org-with-wide-buffer
         (goto-char target)
         (let ((line (format "CLOCK: %s--%s =>  %s"
                             (lr-track--ts (seconds-to-time start-float))
                             (lr-track--ts (seconds-to-time end-float))
                             (org-duration-from-minutes (/ (- end-float start-float) 60.0))))
               (org-log-into-drawer "LOGBOOK"))
           (goto-char (org-log-beginning t))
           (insert line "\n"))))
      (message "Logged %s: %s to %s (%dm)." (car task)
               (lr-track--ts-hm start-float) (lr-track--ts-hm end-float) mins)))))

(defun lr-track--pick-and-clock-from (prompt since)
  "Pick a task and clock into it starting at SINCE (float; nil or ~now = start now).
A :asleep/:away pick is logged closed, never left as a running clock."
  (let ((task (lr-track--pick-task prompt)))
    (when task
      (cond
       ((keywordp (cdr task))
        (if (and since (< since (- (float-time) 30.0)))
            (lr-track--log-task-interval task since (float-time))
          (message "%s isn't something to clock right now." (car task))))
       ((and since (< since (- (float-time) 30.0)))
        (lr-track--clock-task-from task since))
       (t (org-with-point-at (cdr task) (org-clock-in))
          (message "Clocked into %s." (car task)))))))

(defun lr-track--pick-and-clock ()
  "Pick a task and clock into it now."
  (lr-track--pick-and-clock-from "What are you working on? (pick recent or type new): " nil))

(defun lr-track--checkin-clocked (_context)
  "You are back with a clock running.  Account for the time you were away: it was
this task, or something else.  Every minute is attributed, none dropped.  The
split is the moment you left, so the task and the next activity never overlap."
  (let* ((task (or (lr-track--clock-task) "your task"))
         (gap-start (lr-track--left-since))
         (gap-min (max 0 (round (/ (- (float-time) gap-start) 60.0))))
         (choice (car (read-multiple-choice
                       (format "The last %dm (since %s): still \"%s\"? "
                               gap-min (lr-track--ts-hm gap-start) task)
                       '((?y "yes, this task" "keep those minutes on this task, stay clocked in")
                         (?e "something else" "end this task here, then say what you were doing")
                         (?o "this task, and stop" "keep those minutes on it, then clock out now")
                         (?z "ask later" "remind me in a bit"))))))
    (setq lr-track--away-pending nil lr-track--checkin-since nil)
    (pcase choice
      (?y (let ((inc (or (lr-track--incident 'away) (lr-track--incident-open 'away))))
            (setf (plist-get (cdr inc) :keep) t))
          (message "Kept %dm on %s." gap-min task))
      (?e (lr-track--autoout gap-start 'manual)     ; end this task the moment you left
          (lr-track--backfill gap-start))            ; account every minute since then
      ;; "keep those minutes on it": his explicit now, never the pause
      (?o (require 'org-clock)
          (let ((lr-track--internal t)) (org-clock-out))
          (message "Clocked out of %s." task))
      (?z (setq lr-track--snooze-until (+ (float-time) lr-track-snooze-seconds))
          (message "Will ask again in %d min." (round (/ lr-track-snooze-seconds 60.0))))
      (_ nil))))

(defun lr-track--parse-clock (s)
  "Parse S as a wall-clock time (needs a colon or an am/pm suffix) into (H . MM),
or nil.  Bare numbers are NOT clock times here, they are durations."
  (let (h mm ap)
    (cond
     ((string-match "\\`\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\s-*\\(am\\|pm\\)?\\'" s)
      (setq h (string-to-number (match-string 1 s))
            mm (string-to-number (match-string 2 s))
            ap (match-string 3 s)))
     ((string-match "\\`\\([0-9]\\{1,2\\}\\)\\s-*\\(am\\|pm\\)\\'" s)
      (setq h (string-to-number (match-string 1 s)) mm 0 ap (match-string 2 s))))
    (when h
      (when ap
        (setq h (cond ((and (equal ap "pm") (< h 12)) (+ h 12))
                      ((and (equal ap "am") (= h 12)) 0)
                      (t h))))
      (when (and (<= 0 h 23) (<= 0 mm 59)) (cons h mm)))))

(defun lr-track--clock-on-day (h mm cursor now)
  "Float time for H:MM on CURSOR's day, rolled to the next day if that would land at
or before CURSOR.  Returns nil unless it sits in (CURSOR, NOW]."
  (let ((d (decode-time (seconds-to-time cursor))))
    (setf (nth 0 d) 0 (nth 1 d) mm (nth 2 d) h)
    (let ((t0 (float-time (encode-time d))))
      (when (<= t0 cursor) (setq t0 (+ t0 86400.0)))
      (and (> t0 cursor) (<= t0 (+ now 90.0)) (min t0 now)))))

(defun lr-track--duration-minutes (s)
  "Minutes (float) for a duration like 90, 90m, 1h, 1h30, 1h30m, 1.5h, 2:15; else nil."
  (let ((s (replace-regexp-in-string "[+ \t]" "" s)))
    (cond
     ;; H:MM written as a duration (2:15 = 135m); clock times are routed away earlier
     ((string-match "\\`\\([0-9]+\\):\\([0-9]\\{2\\}\\)\\'" s)
      (+ (* 60.0 (string-to-number (match-string 1 s))) (string-to-number (match-string 2 s))))
     ;; NhMM / NhMMm / N.Nh: hours with optional trailing minutes
     ((string-match "\\`\\([0-9]*\\.?[0-9]+\\)h\\([0-9]*\\)m?\\'" s)
      (+ (* 60.0 (string-to-number (match-string 1 s)))
         (if (> (length (match-string 2 s)) 0) (string-to-number (match-string 2 s)) 0)))
     ;; Nm
     ((string-match "\\`\\([0-9]+\\)m\\'" s) (float (string-to-number (match-string 1 s))))
     ;; bare number = minutes
     ((string-match "\\`[0-9]*\\.?[0-9]+\\'" s) (float (string-to-number s)))
     (t nil))))

(defun lr-track--parse-when (input cursor now)
  "Turn INPUT into an absolute float time.  A wall-clock time (\"15:30\", \"3:30pm\",
\"3pm\") is read on CURSOR's day; anything else is a duration from CURSOR (\"90\",
\"90m\", \"1h\", \"1h30\", \"1.5h\").  \"now\" is now.  Returns a float or nil."
  (let ((s (downcase (string-trim input))))
    (cond
     ((string-empty-p s) nil)
     ((member s '("now" "n")) now)
     ((lr-track--parse-clock s)
      (let ((hm (lr-track--parse-clock s))) (lr-track--clock-on-day (car hm) (cdr hm) cursor now)))
     (t (let ((mins (lr-track--duration-minutes s)))
          (and mins (> mins 0) (min now (+ cursor (* 60.0 mins)))))))))

;;;; arrow-key scrubbing (pure core)
;;
;; The arrows author the SAME minibuffer string you could type, so preview,
;; parsing and commit are all unchanged.  The one deliberate choice: an arrow
;; writes a DURATION token ("<N>m", or the literal "now"), NEVER a clock stamp.
;; `lr-track--parse-when' turns "<N>m" into (min now (+ cursor (* 60 N))) with
;; N>0, so a scrubbed value is in (CURSOR, NOW] by construction -- it never
;; reaches `lr-track--clock-on-day', so there is no day-rollover to get wrong
;; and no >24h ambiguity.  An out-of-range end cannot be AUTHORED.

(defconst lr-track--scrub-ladder '(1 2 5 10 15 30 60 120)
  "Step sizes (minutes).  Every element two rungs up is an integer multiple of
the one below (1|5, 2|10, 5|15, 10|30, 15|60, 30|120), so the coarse grid nests
on the fine grid and a coarse landing is always a fine landing.")

(defun lr-track--scrub-steps (cursor now)
  "Return (FINE . COARSE) step minutes for the REMAINING span (CURSOR, NOW].
Keyed off `now - cursor', NOT the whole gauge window, so fine control does not
collapse on a late segment.  Fine crosses the span in <= ~40 presses at every
width from 1 minute to 40 hours; coarse in <= ~12."
  (let* ((m (max 1.0 (/ (- now cursor) 60.0)))
         (last (1- (length lr-track--scrub-ladder)))
         (fi (or (cl-position-if (lambda (s) (<= (/ m s) 40))
                                 lr-track--scrub-ladder)
                 last)))
    (cons (nth fi lr-track--scrub-ladder)
          (nth (min last (+ fi 2)) lr-track--scrub-ladder))))

(defun lr-track--scrub-target (d0 step dir m minfloor)
  "The next STEP multiple strictly past duration D0 in DIR (+1/-1), clamped to
[MINFLOOR, M].  Below the floor snaps UP to the floor (never to 0); past M
parks at M."
  (let* ((d0 (float d0))                 ; never integer-divide (43/5 -> 8, not 8.6)
         (d (if (> dir 0)
                (* step (1+ (floor (/ d0 step))))
              (* step (1- (ceiling (/ d0 step)))))))
    (max minfloor (min m (float d)))))

(defun lr-track--scrub-string (contents kind cursor now)
  "Given the current minibuffer CONTENTS, return the new minibuffer string after
a scrub key KIND (`fine-' `fine+' `coarse-' `coarse+' `home' `end') in the
window (CURSOR, NOW].

Pure: no minibuffer, no side effects.  Result is always exactly one canonical
token -- \"<N>m\" with N>=1, or \"now\" -- so it round-trips through
`lr-track--parse-when' back into (CURSOR, NOW]."
  (let* ((m (max 1.0 (/ (- now cursor) 60.0)))
         (st (lr-track--scrub-steps cursor now))
         (fine (car st)) (coarse (cdr st))
         (minf (min (float fine) m))
         (s (string-trim (or contents "")))
         ;; adopt whatever is already typed; empty/unparseable seeds from `now'
         (base (or (and (> (length s) 0) (lr-track--parse-when s cursor now)) now))
         (d0 (max 0.0 (min m (/ (- base cursor) 60.0)))))
    (if (eq kind 'end)
        "now"
      (let ((d1 (pcase kind
                  ('home    minf)
                  ('fine-   (lr-track--scrub-target d0 fine   -1 m minf))
                  ('fine+   (lr-track--scrub-target d0 fine   +1 m minf))
                  ('coarse- (lr-track--scrub-target d0 coarse -1 m minf))
                  ('coarse+ (lr-track--scrub-target d0 coarse +1 m minf))
                  (_        d0))))
        (format "%dm" (max 1 (round d1)))))))

;;;; backfill timeline gauge
;;
;; A bottom side window that visualizes the untracked stretch while you answer
;; "until when".  It shows the whole window [gap-start .. now], the runs you have
;; already logged, the cursor (where the next segment starts), the time left, and
;; a LIVE preview of the tentative segment as you type (repainted from a buffer
;; local post-command-hook in the minibuffer).  The answer is still read by the
;; ordinary `read-string', so point never leaves the minibuffer.  Everything here
;; is display-only: only a parse that satisfies (> tm cursor) ever logs anything.

(defvar lr-track--tl-buffer-name " *lr-track backfill*")
(defvar lr-track--tl-model nil
  "Plist for the active gauge: :start :now :cursor :segments :label :width :ascii.
:segments is a list of (LABEL START END) oldest first.  nil when no gauge is up.")
(defvar lr-track--tl-last-sig nil "Debounce token: the last preview state painted.")

(defun lr-track--tl-glyph (key ascii)
  "Character for glyph KEY, from the ASCII set when ASCII, else the unicode set."
  (cdr (assq key (if ascii lr-track--tl-ascii lr-track--tl-glyphs))))

(defun lr-track--tl-use-ascii ()
  "Whether to draw the gauge in ASCII (toggle on, or a glyph is not displayable)."
  (or lr-track-timeline-ascii
      (not (cl-every (lambda (c) (char-displayable-p (cdr c))) lr-track--tl-glyphs))))

(defun lr-track--tl-col (tm start total width)
  "Column (0..WIDTH) for absolute time TM in the window [START, START+TOTAL]."
  (if (<= total 0) 0
    (max 0 (min width (round (* (/ (- tm start) (float total)) width))))))

(defun lr-track--tl-width (win)
  "Bar width: the side window body minus a margin, clamped to keep it one line."
  (max 24 (min 48 (- (if (window-live-p win) (window-body-width win) 62) 2))))

(defun lr-track--tl-window () (get-buffer-window lr-track--tl-buffer-name t))

;; --- rows ---------------------------------------------------------------

(defun lr-track--tl-bar (model preview-end)
  "The proportional bar row: filled runs, an optional PREVIEW-END run, remaining."
  (let* ((start (plist-get model :start)) (now (plist-get model :now))
         (cursor (plist-get model :cursor)) (segs (plist-get model :segments))
         (w (plist-get model :width)) (ascii (plist-get model :ascii))
         (total (- now start))
         (cur-col (lr-track--tl-col cursor start total w))
         (cells (make-vector w nil)))
    (dotimes (i w) (aset cells i (cons (lr-track--tl-glyph :empty ascii) 'lr-track-backfill-empty)))
    ;; filled segments as boundary columns B[0..n] tiling [0, cur-col) exactly, then a
    ;; 1-column floor so a tiny segment never vanishes (it borrows a column from the
    ;; run before it), applied backward and then re-clamped monotone.
    (let* ((n (length segs)) (b (make-vector (1+ n) 0)))
      (aset b n cur-col)
      (cl-loop for i from 1 below n
               do (aset b i (min cur-col (max (aref b (1- i))
                                              (lr-track--tl-col (nth 2 (nth (1- i) segs)) start total w)))))
      (cl-loop for i from n downto 1
               for seg = (nth (1- i) segs)
               when (and (> (nth 2 seg) (nth 1 seg)) (< (- (aref b i) (aref b (1- i))) 1))
               do (aset b (1- i) (max 0 (1- (aref b i)))))
      (cl-loop for i from 1 to n do (aset b i (max (aref b i) (aref b (1- i)))))
      (cl-loop for i from 0 below n
               for evenp = (cl-evenp i)
               for ch = (lr-track--tl-glyph (if evenp :fill :fill2) ascii)
               for face = (if evenp 'lr-track-backfill-fill 'lr-track-backfill-fill-alt)
               do (cl-loop for c from (aref b i) below (aref b (1+ i))
                           do (aset cells c (cons ch face)))))
    (when (and preview-end (> preview-end cursor))
      (let ((pcol (min w (max cur-col (lr-track--tl-col preview-end start total w))))
            (ch (lr-track--tl-glyph :preview ascii)))
        (cl-loop for i from cur-col below pcol do (aset cells i (cons ch 'lr-track-backfill-preview)))))
    (cl-loop for cell across cells
             concat (propertize (char-to-string (car cell)) 'face (cdr cell)))))

(defun lr-track--tl-caret-row (model preview-end)
  "The caret row: a ^ under the cursor time, plus (typing) one under the preview end."
  (let* ((start (plist-get model :start)) (now (plist-get model :now))
         (cursor (plist-get model :cursor)) (w (plist-get model :width))
         (total (- now start)) (len (+ w 12)) (vec (make-vector len ?\s)) (spans nil))
    (cl-flet ((place (tm)
                (let ((col (min (1- len) (lr-track--tl-col tm start total w)))
                      (lbl (lr-track--ts-hm tm)))
                  (aset vec col ?^)
                  (cl-loop for i from 0 below (length lbl)
                           when (< (+ col 2 i) len) do (aset vec (+ col 2 i) (aref lbl i)))
                  (push (cons col (min len (+ col 2 (length lbl)))) spans))))
      (place cursor)
      (when (and preview-end (> preview-end cursor)) (place preview-end)))
    (let ((row (concat vec)))
      (dolist (sp spans) (put-text-property (car sp) (cdr sp) 'face 'lr-track-backfill-cursor row))
      (string-trim-right row))))

(defun lr-track--tl-header (model preview-end invalid)
  "Header row: the window, total, and a live logged / typing / left readout."
  (let* ((start (plist-get model :start)) (now (plist-get model :now))
         (cursor (plist-get model :cursor))
         (previewing (and preview-end (> preview-end cursor)))
         (left (if previewing (- now preview-end) (- now cursor))))
    (concat
     (format "Backfill %s to %s (%s)   " (lr-track--ts-hm start) (lr-track--ts-hm now)
             (lr-track--fmt-dur (/ (- now start) 60.0)))
     (propertize (concat "logged " (lr-track--fmt-dur (/ (- cursor start) 60.0)))
                 'face 'lr-track-backfill-fill)
     (cond (previewing (concat ", " (propertize (concat "typing " (lr-track--fmt-dur (/ (- preview-end cursor) 60.0)))
                                                'face 'lr-track-backfill-preview)))
           (invalid (concat ", " (propertize "typing ?" 'face 'lr-track-backfill-invalid)))
           (t ""))
     "   "
     (propertize (concat "left " (lr-track--fmt-dur (/ left 60.0)))
                 'face (if previewing 'lr-track-backfill-preview 'lr-track-backfill-empty)))))

(defun lr-track--tl-tick-step (total w)
  "Minutes between axis ticks so adjacent labels never collide (need >= 6 columns)."
  (let ((per-col (/ total (float w))))
    (or (seq-find (lambda (m) (>= (/ (* m 60.0) per-col) 6.0)) '(30 60 120 180 360)) 360)))

(defun lr-track--tl-ticks (start now step-min)
  "List of (TIME . HOURP) axis ticks at STEP-MIN spacing within [START, NOW]."
  (let* ((res nil) (d (decode-time (seconds-to-time start))))
    (setf (nth 0 d) 0)
    (let* ((mod (+ (* 60 (nth 2 d)) (nth 1 d)))
           (rem (% mod step-min))
           (bump (if (zerop rem) 0 (- step-min rem)))
           (t0 (+ (float-time (encode-time d)) (* bump 60))))
      (while (<= t0 (+ now 1))
        (when (>= t0 start)
          (push (cons t0 (zerop (nth 1 (decode-time (seconds-to-time t0))))) res))
        (setq t0 (+ t0 (* step-min 60)))))
    (nreverse res)))

(defun lr-track--tl-axis (model)
  "The two axis rows (labels, ticks), or nil when the window is too narrow."
  (let* ((start (plist-get model :start)) (now (plist-get model :now))
         (w (plist-get model :width)) (ascii (plist-get model :ascii)) (total (- now start)))
    (when (>= w 40)
      (let* ((step (lr-track--tl-tick-step total w))
             (ticks (lr-track--tl-ticks start now step))
             (lvec (make-vector w ?\s)) (tvec (make-vector w ?\s)) (ls nil) (ts nil))
        (dolist (tk ticks)
          (let* ((tm (car tk)) (hourp (cdr tk))
                 (col (min (1- w) (lr-track--tl-col tm start total w)))
                 (lbl (lr-track--ts-hm tm))
                 (lstart (max 0 (min (- w (length lbl)) col))))
            (aset tvec col (lr-track--tl-glyph (if hourp :hour :half) ascii))
            (push (list col hourp) ts)
            (when (cl-loop for i from lstart below (+ lstart (length lbl))
                           always (eq (aref lvec i) ?\s))
              (dotimes (i (length lbl)) (aset lvec (+ lstart i) (aref lbl i)))
              (push (list lstart (+ lstart (length lbl)) hourp) ls))))
        (let ((lrow (concat lvec)) (trow (concat tvec)))
          (dolist (sp ts) (put-text-property (car sp) (1+ (car sp))
                                             'face (if (cadr sp) 'lr-track-backfill-tick-hour 'lr-track-backfill-tick-half) trow))
          (dolist (sp ls) (put-text-property (nth 0 sp) (nth 1 sp)
                                             'face (if (nth 2 sp) 'lr-track-backfill-tick-hour 'lr-track-backfill-tick-half) lrow))
          (list (string-trim-right lrow) (string-trim-right trow)))))))

(defun lr-track--tl-legend (model)
  "The done-legend row listing each logged segment in bar order, oldest first."
  (let ((segs (plist-get model :segments)))
    (if (null segs) (propertize "done: nothing yet" 'face 'shadow)
      (let* ((parts (mapcar (lambda (seg)
                              (format "%s %s to %s %s" (nth 0 seg)
                                      (lr-track--ts-hm (nth 1 seg)) (lr-track--ts-hm (nth 2 seg))
                                      (lr-track--fmt-dur (/ (- (nth 2 seg) (nth 1 seg)) 60.0))))
                            segs))
             (maxw (max 20 (- (plist-get model :width) 0)))
             (dropped 0))
        (while (and (> (length parts) 1)
                    (> (length (string-join parts ", ")) (* 2 maxw)))
          (setq parts (cdr parts) dropped (1+ dropped)))
        (concat (propertize "done: " 'face 'shadow)
                (when (> dropped 0) (propertize (format "(+%d more) " dropped) 'face 'shadow))
                (string-join parts ", "))))))

(defun lr-track--tl-nowline (model preview-end invalid)
  "The now line: what THIS prompt is filling, and where the typed answer lands."
  (let ((label (plist-get model :label)) (cursor (plist-get model :cursor)))
    (concat
     (propertize "now: " 'face 'shadow)
     (if label (concat (propertize label 'face 'lr-track-backfill-task) " ") "")
     (cond
      ((and preview-end (> preview-end cursor))
       (format "%s to %s (%s)" (lr-track--ts-hm cursor) (lr-track--ts-hm preview-end)
               (lr-track--fmt-dur (/ (- preview-end cursor) 60.0))))
      (invalid (propertize "that time is not in range" 'face 'lr-track-backfill-invalid))
      (label (format "from %s, until when?" (lr-track--ts-hm cursor)))
      (t (format "pick what you were doing from %s onward" (lr-track--ts-hm cursor)))))))

(defun lr-track--tl-footer (model)
  "Scrub help row for the until-when prompt: the LIVE fine/coarse step sizes and
the keys.  Kept plain ASCII and arrow-free on purpose (the whole file is)."
  (let* ((cursor (plist-get model :cursor)) (now (plist-get model :now))
         (st (lr-track--scrub-steps cursor now)))
    (propertize
     (format "Left/Right %s   Up/Down %s   Home shortest   End now   RET log"
             (lr-track--fmt-dur (car st)) (lr-track--fmt-dur (cdr st)))
     'face 'shadow)))

;; --- window + render ----------------------------------------------------

(defun lr-track--tl-render (model preview-end invalid)
  "Repaint the gauge buffer from MODEL, with an optional PREVIEW-END and INVALID flag."
  (let ((buf (get-buffer lr-track--tl-buffer-name)))
    (when (buffer-live-p buf)
      (let* ((axis (lr-track--tl-axis model))
             (rows (delq nil (list (lr-track--tl-header model preview-end invalid)
                                   (nth 0 axis) (nth 1 axis)
                                   (lr-track--tl-bar model preview-end)
                                   (lr-track--tl-caret-row model preview-end)
                                   (lr-track--tl-legend model)
                                   (lr-track--tl-nowline model preview-end invalid)
                                   ;; scrub help only on the until-when prompt
                                   (and (plist-get model :label)
                                        (lr-track--tl-footer model))))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert " " (string-join rows "\n ") "\n"))
          (goto-char (point-min)))
        (let ((win (lr-track--tl-window)))
          (when (window-live-p win) (ignore-errors (fit-window-to-buffer win 10 4))))))))

(defun lr-track--tl-show ()
  "Create and display the gauge side window (nil if it cannot be shown).
Uses a dedicated slot so it never reuses a Doom popup's bottom side window, and
marks ONLY a window it actually creates (`lr-track-gauge'), so teardown can never
delete a pre-existing side window it merely happened to land in."
  (with-demoted-errors "lr-track timeline: %S"
    (let ((buf (get-buffer-create lr-track--tl-buffer-name)))
      (with-current-buffer buf
        (setq buffer-read-only t truncate-lines t mode-line-format nil)
        (setq-local cursor-type nil))
      (let* ((before (window-list nil 'no-minibuf))
             (win (or (ignore-errors
                        (display-buffer-in-side-window
                         buf '((side . bottom) (slot . -100) (window-height . 9)
                               (window-parameters . ((no-other-window . t) (no-delete-other-windows . t))))))
                      (ignore-errors (display-buffer-at-bottom buf '((window-height . 9)))))))
        (when (and (window-live-p win) (not (memq win before)))
          (set-window-parameter win 'lr-track-gauge t)))   ; mark it as OURS to delete
      buf)))

(defun lr-track--tl-begin (start now)
  "Open the gauge for the window [START, NOW].  No-op when the gauge is disabled."
  (when lr-track-timeline
    (let ((buf (lr-track--tl-show)))
      (when (and buf (lr-track--tl-window))
        (setq lr-track--tl-model
              (list :start start :now now :cursor start :segments nil :label nil
                    :width (lr-track--tl-width (lr-track--tl-window))
                    :ascii (lr-track--tl-use-ascii))
              lr-track--tl-last-sig nil)
        (lr-track--tl-render lr-track--tl-model nil nil)))))

(defun lr-track--tl-update (cursor segments label)
  "Advance the gauge to CURSOR with SEGMENTS logged and LABEL as the current task."
  (when lr-track--tl-model
    (setq lr-track--tl-model
          (plist-put (plist-put (plist-put (plist-put lr-track--tl-model :cursor cursor)
                                           :segments segments)
                                :label label)
                     :width (lr-track--tl-width (lr-track--tl-window)))
          lr-track--tl-last-sig nil)
    (lr-track--tl-render lr-track--tl-model nil nil)))

(defun lr-track--tl-end ()
  "Tear down the gauge.  Delete ONLY a window this module created; if we ever landed
in a foreign window, restore its previous buffer instead of deleting it."
  (let ((win (lr-track--tl-window)))
    (when (window-live-p win)
      (if (window-parameter win 'lr-track-gauge)
          (ignore-errors (delete-window win))
        (ignore-errors (quit-restore-window win 'bury)))))
  (when (get-buffer lr-track--tl-buffer-name) (kill-buffer lr-track--tl-buffer-name))
  (setq lr-track--tl-model nil lr-track--tl-last-sig nil))

(defun lr-track--tl-post-command ()
  "Buffer-local minibuffer hook: reparse the input and repaint the live preview."
  (when (and lr-track--tl-model (minibufferp) (buffer-live-p (get-buffer lr-track--tl-buffer-name)))
    (with-demoted-errors "lr-track timeline: %S"
      (let* ((s (string-trim (minibuffer-contents)))
             (cursor (plist-get lr-track--tl-model :cursor))
             (now (plist-get lr-track--tl-model :now))
             (tm (and (> (length s) 0) (lr-track--parse-when s cursor now)))
             (valid (and (numberp tm) (> tm cursor)))
             (invalid (and (> (length s) 0) (not valid)))
             (sig (cond ((= (length s) 0) 'idle) (valid (round tm)) (t 'invalid))))
        (unless (equal sig lr-track--tl-last-sig)
          (setq lr-track--tl-last-sig sig)
          (lr-track--tl-render lr-track--tl-model (and valid tm) invalid))))))

;;;; the until-when prompt (with the live gauge)

(defun lr-track--scrub (kind)
  "Rewrite the minibuffer with the scrubbed end for KIND.  Thin wrapper over the
pure `lr-track--scrub-string'; the existing post-command hook repaints."
  (when (and lr-track--tl-model (minibufferp))
    (let ((new (lr-track--scrub-string (minibuffer-contents)
                                       kind
                                       (plist-get lr-track--tl-model :cursor)
                                       (plist-get lr-track--tl-model :now))))
      (delete-minibuffer-contents)
      (insert new))))

(defconst lr-track--scrub-map
  (let ((km (make-sparse-keymap)))
    ;; right/up = later, left/down = earlier; home = shortest, end = now.
    ;; Installed via a COMPOSED map (never by mutating the shared
    ;; `minibuffer-local-map'), so nothing leaks into later minibuffers.
    ;;
    ;; These reach `lr-track--scrub' because this config leaves evil OUT of the
    ;; minibuffer: `evil-want-minibuffer' and `evil-collection-setup-minibuffer'
    ;; are both nil, so `evil-initialize' skips `evil-local-mode' here
    ;; (evil-core.el) and there is no `evil-motion-state-map' to shadow the
    ;; arrows.  It is NOT the key encoding that saves us -- evil DOES bind the
    ;; arrow EVENTS in motion state (evil-maps.el).  If evil-in-the-minibuffer
    ;; is ever enabled, ESC into normal/motion state would revert these to evil
    ;; motions (insert state still falls through to scrub, and typing always
    ;; works, so there is no data risk -- only dead scrub keys until you type).
    (dolist (b '(([left]  fine-)  ([right] fine+)
                 ([down]  coarse-)([up]    coarse+)
                 ([home]  home)   ([end]   end)))
      (let ((kind (cadr b)))
        (define-key km (car b) (lambda () (interactive) (lr-track--scrub kind)))))
    km)
  "Minibuffer overlay for the until-when prompt.  Composed OVER the live map, so
RET, C-g, self-insert, C-a/C-e/C-k and history (M-p/M-n) all fall through.")

(defun lr-track--read-until (cursor now &optional segments label)
  "Read when an activity that started at CURSOR ended: a wall-clock time or a
duration, both in (CURSOR, NOW].  Empty input returns nil, meaning you are STILL
doing it now.  Re-prompts on anything unparseable or out of range.  When the gauge
is up, SEGMENTS (logged so far) and LABEL (the current task) drive the live view."
  (let ((prompt (format "  until when? (%s, 40m, or arrows to scrub; RET = still on it) "
                        (lr-track--ts-hm now))))
    (when lr-track--tl-model (lr-track--tl-update cursor segments label))
    (catch 'ok
      (while t
        (let ((s (string-trim
                  (minibuffer-with-setup-hook
                      (lambda ()
                        (when lr-track--tl-model
                          (add-hook 'post-command-hook #'lr-track--tl-post-command nil t)
                          ;; Overlay the scrub keys OVER the live minibuffer map,
                          ;; per-minibuffer.  `use-local-map' + `make-composed-keymap'
                          ;; never mutates the shared `minibuffer-local-map', so the
                          ;; bindings die with this minibuffer and never leak.
                          (use-local-map
                           (make-composed-keymap lr-track--scrub-map
                                                 (current-local-map)))))
                    (read-string prompt)))))
          (if (string-empty-p s) (throw 'ok nil)
            (let ((tm (lr-track--parse-when s cursor now)))
              (if (and (numberp tm) (> tm cursor))
                  (throw 'ok tm)
                (message "Give a clock time like %s, a duration like 40m, or RET if still on it."
                         (lr-track--ts-hm now))
                (sit-for 1.3)))))))))

(defun lr-track--backfill (since &optional _total-mins)
  "Walk the untracked stretch from SINCE to now, oldest first, with NO minute math.
For each activity you name, say UNTIL WHEN it ran (a clock time like 15:30, or a
duration like 40m); press RET to mean you are STILL doing it now, which leaves a
running clock (backdated to when it started) and ends the walk.  Each closed
segment is logged and the cursor advances, so segments are contiguous and never
overlap.  A live gauge in a side window shows the whole stretch, what is filled,
and a preview as you type.  Type a new name at any prompt to create a fresh TODO."
  (let ((cursor since) (now (float-time)) (segments nil))
    (unwind-protect
        (progn
          (lr-track--tl-begin since now)
          (catch 'done
            (while (< cursor (- now 30.0))      ; stop once the gap is essentially filled
              (lr-track--tl-update cursor (reverse segments) nil)   ; label nil = choosing
              (let ((task (lr-track--pick-task
                           (format "From %s onward, what were you doing? (empty = stop) "
                                   (lr-track--ts-hm cursor)))))
                (unless task (throw 'done nil))
                (let ((end (lr-track--read-until cursor now (reverse segments) (car task))))
                  (if (null end)                ; RET = still doing it now
                      (progn (lr-track--clock-task-from task cursor) (throw 'done t))
                    (lr-track--log-task-interval task cursor end)
                    (push (list (car task) cursor end) segments)
                    (setq cursor end)))))))
      (lr-track--tl-end))))

(defun lr-track--checkin-idle (context)
  "Check-in flow when no clock is running.  If the coach just auto-closed a clock
\(away/slept), the stretch since then is UNACCOUNTED: ask where you were and put
every minute somewhere (the same task across the gap, a split, or explicitly off
the clock), never dropping it.  Otherwise, on a real return offer a retroactive
backfill; failing that, just clock in."
  (let* ((since (lr-track--gap-start))
         (mins (max 0 (round (/ (- (float-time) since) 60.0)))))
    (setq lr-track--away-pending nil lr-track--checkin-since nil)
    (cond
     ;; a clock the coach auto-closed leaves a real gap we must account for
     ((and lr-track--last-close (>= mins 1))
      (let ((task (plist-get lr-track--last-close :task))
            (marker (plist-get lr-track--last-close :marker)))
        (setq lr-track--last-close nil)
        (pcase (car (read-multiple-choice
                     (format "The last %dm (since %s) had no clock (was \"%s\"). "
                             mins (lr-track--ts-hm since) task)
                     '((?s "same task" "you kept doing that task across the gap; clock it")
                       (?e "something else" "say what you were doing, oldest first")
                       (?o "off the clock" "genuine downtime, leave it unaccounted")
                       (?d "ask later" "dismiss for now"))))
          (?s (if (and (markerp marker) (marker-buffer marker))
                  (lr-track--clock-task-from (cons task marker) since)
                (lr-track--backfill since)))
          (?e (lr-track--backfill since))
          (?o (message "Left %dm (since %s) off the clock." mins (lr-track--ts-hm since)))
          (_ nil))))
     ;; stale pointer with no real gap: nothing to account, just clock in
     (lr-track--last-close (setq lr-track--last-close nil) (lr-track--pick-and-clock))
     ((and (memq context '(return startup)) (>= mins 1)
           (<= (- (float-time) since) lr-track-backfill-max-seconds))
      (lr-track--backfill since))
     (t (lr-track--pick-and-clock)))))

;;;###autoload
(defun lr-track-checkin (&optional context)
  "The coach's in-Emacs check-in.  Pops on return to Emacs after an absence, or
call it (SPC d j) anytime.  If clocked: still-on-it / switch (name what you're
doing and it clocks you in) / clock-out / snooze.  If not: clock into something."
  (interactive)
  (require 'org-clock)
  (let ((lr-track--resolving t))
    (if (lr-track--clocking-p)
        (lr-track--checkin-clocked context)
      (lr-track--checkin-idle context))))

;;;; modeline

(defun lr-track--modeline-string () lr-track--modeline-cache)

(defun lr-track--state-glyph (state)
  (pcase state
    ((or 'engaged 'reading) "*")
    ('elsewhere "+")
    ('away "o")
    ('slept "z")
    (_ "-")))

(defun lr-track--modeline-refresh ()
  "Write the O(1) modeline cache.  The ONLY writer, run from the modeline phase.
Quiet by design: while he is here (or nothing is known) it shows NOTHING, so
org-clock owns the modeline.  It speaks only while a clock runs and presence
says he is away: away for the minutes since his last input, or slept when
the Mac slept.  It reads `lr-track--presence', as the clock does, so it
never names another departure or length than the paused clock beside it."
  (let* ((p lr-track--presence)
         (from (plist-get p :away-from))
         (mins (and (numberp from)
                    (max 0 (floor (/ (- (float-time) from) 60.0)))))
         (ago (if (and mins (> mins 0)) (format " %dm" mins) "")))
    (setq lr-track--modeline-cache
          (if (and (eq (plist-get p :mode) 'away) (lr-track--clocking-p))
              (if (eq (plist-get p :away-kind) 'asleep)
                  " z slept "
                (format " o away%s " ago))
            ""))))

;;;; tick engine

(defun lr-track--next-interval ()
  "Seconds to the next tick: by presence, else by whether a clock runs."
  (pcase (plist-get lr-track--presence :mode)
    ('here lr-track-interval-here)
    ('away lr-track-interval-away)
    (_ (if (lr-track--clocking-p) lr-track-interval-clocked lr-track-interval-idle))))

(defun lr-track--schedule (interval)
  "Arm the next tick.  Single choke point: stamps `lr-track--last-tick' to now at
the same moment it sets the interval, so gap is always measured from here."
  (setq lr-track--interval interval
        lr-track--last-tick (float-time))
  (when (timerp lr-track--timer) (cancel-timer lr-track--timer))
  (let ((gen lr-track--generation))
    (setq lr-track--timer (run-with-timer interval nil #'lr-track--tick gen))))

(defun lr-track--transient-p (att)
  (or (eq (plist-get att :state) 'slept) (eq (plist-get att :cause) 'clock-step)))

(defun lr-track--tick-sense ()
  "Tick phase: probe presence (every tick, clock or not), then classify."
  (lr-track--tick-probe)
  (let ((att (lr-track-attention-state)))
    (setq lr-track--tick-attention att)
    (let ((state (plist-get att :state)))
      (cond
       ((lr-track--transient-p att)
        ;; transient classes prove themselves in one tick; bypass hysteresis
        (setq lr-track--stable-state state
              lr-track--stable-since (float-time)
              lr-track--last-state state))
       ((eq state lr-track--last-state)
        (unless (eq lr-track--stable-state state)
          (setq lr-track--stable-state state lr-track--stable-since (float-time))))
       (t (setq lr-track--last-state state)))
      (when (memq state '(engaged reading))
        (setq lr-track--last-engaged (float-time))))))

(defun lr-track--presence-compact (p)
  "The part of presence state P that state.eld keeps."
  (list :mode (plist-get p :mode)
        :last-input (plist-get p :last-input)
        :away-from (plist-get p :away-from)
        :away-kind (plist-get p :away-kind)
        :return (plist-get p :return)
        :last-away (plist-get p :last-away)))

(defun lr-track--blind-sample (p sample)
  "The sample to step before SAMPLE after a blind stretch, or nil.
P is the presence state.  The last usable sample is 3 min or more before
SAMPLE, which carries new input 15 min or more after P's last input, and
nothing vouches for the stretch between: the probe failed
\(`lr-track--blind-since'), or no tick ran at all (App Nap, a blocked Emacs)
and no wake says the Mac slept (a wake is rule 3's sleep, which the step
handles).  HID idle only dates the LAST input, so the whole stretch would
read as continuous input and its absence would be credited.  A gap under
15 min hides an away too: 10 min of idle the samples saw, then 10 min no
sample saw, may be 16 min away.  A sample just before that input, with no
input since P's, comes first.  It starts an away at P's last input (a
here-block) or keeps one going, and the return is dated at the new input,
never at the stale previous sample.  It says :blind, so an away it starts
is of kind `unseen': nothing saw him leave, and the header and the label
must not say he did.  If he did work in another app all along, the clock
pauses where it need not, and his key continues it: the safe way to be
wrong."
  (let* ((now (plist-get sample :time))
         (idle (plist-get sample :idle))
         (prev (plist-get p :prev-time))
         (last (plist-get p :last-input))
         (wake (plist-get sample :wake))
         (input (and (numberp now) (numberp idle) (- now idle)))
         (at (and input (- input 1.0))))
    (when (and input (numberp prev) (numberp last)
               (or lr-track--blind-since
                   ;; no wake new since P's: the gap is not a sleep
                   (not (and (numberp wake) (> wake prev)
                             (not (eql wake (plist-get p :wake))))))
               (memq (plist-get p :mode) '(here away))
               (not (eq (plist-get sample :locked) t))
               ;; more than a tick or two went unsampled
               (>= (- now prev) lr-track-presence-break-seconds)
               (>= (- at last) lr-track-presence-away-seconds)
               (> at prev))
      (list :time at :idle (- at last) :locked nil
            :wake (plist-get sample :wake) :blind t))))

(defun lr-track--step-sample (sample)
  "Fold SAMPLE into `lr-track--presence', unless it already was.
Non-nil when it was stepped now.  Each sample is stepped exactly once.  A
usable sample after a blind stretch is preceded by `lr-track--blind-sample'.
A return it steps is held when the running line began in that block
\(`lr-track--hold-early-line')."
  (when (and sample (not (eq sample lr-track--presence-stepped)))
    (let* ((p (or lr-track--presence (lr-track--presence-init)))
           (r0 (plist-get p :return))
           (blind (and (numberp (plist-get sample :idle))
                       (lr-track--blind-sample p sample))))
      (when blind
        (lr-track--log 'presence-blind
                       (list :since lr-track--blind-since :to (plist-get blind :time)))
        (setq p (lr-track--presence-step p blind)))
      ;; a sample with no idle is the probe failing too (the sentinel also
      ;; marks it); a usable one ends the blind stretch
      (if (numberp (plist-get sample :idle))
          (setq lr-track--blind-since nil)
        (unless lr-track--blind-since
          (setq lr-track--blind-since (or (plist-get sample :time) (float-time)))))
      (setq lr-track--presence-stepped sample
            lr-track--presence (lr-track--presence-step p sample))
      (lr-track--hold-early-line r0))
    t))

(defun lr-track--hold-early-line (old-return)
  "Hold the here-block a step just opened when the running line began in it.
OLD-RETURN is presence's return before the step.  His key on sitting down
\(SPC d N, y then a digit, SPC c i) often comes before the sample that sees
him back: the clock-in hook (`lr-track--hold-here-block') then finds
presence away and holds nothing, and a short block would merge as a glance
into the aways around it, his line inside.  So when the step sets a new
return R, presence is here, and a clock runs that started after R or at
most 60 s before it (a start floored to its minute, or the tick before R),
the block is held, as the hook would have held it."
  (let* ((p lr-track--presence)
         (r (plist-get p :return)))
    (when (and (numberp r) (not (eql r old-return))
               (eq (plist-get p :mode) 'here)
               (lr-track--clocking-p)
               (not (lr-track--clock-stood-in-p))
               (boundp 'org-clock-start-time) org-clock-start-time
               (>= (float-time org-clock-start-time) (- r 60.0)))
      (setq lr-track--presence (plist-put (copy-sequence p) :held r)))))

(defun lr-track--tick-presence ()
  "Tick phase: step `lr-track--presence' with the newest sample, once each.
A compact copy goes into `lr-track--state', for state.eld.  It is written at
once only when more than the last input moved (a mode change, an away, a
return): the heartbeat phase of this same tick flushes the state anyway, so
typing does not cost a second write every tick."
  (lr-track--step-sample lr-track--last-sample)
  (let ((compact (lr-track--presence-compact lr-track--presence))
        (old (plist-get lr-track--state :presence)))
    (unless (equal compact old)
      (if (equal (plist-put (copy-sequence compact) :last-input nil)
                 (and old (plist-put (copy-sequence old) :last-input nil)))
          (setq lr-track--state (plist-put lr-track--state :presence compact))
        (lr-track--state-put :presence compact)))))

(defun lr-track--tick-header ()
  "Tick phase: refresh the agenda's Now? header, when lr-track-ask is loaded.
lr-track never requires lr-track-ask (config.el does), so without it this is a
quiet no-op.  So is it before org loads: reading time.org would load org from a
timer, and the header only shows in an agenda, whose build loads org and
refreshes the header itself.  An error is logged here, never counted as a
phase failure: the header is display only, and a failing time.org must not
back the whole tick (presence, the clock line) off to 5 min."
  (when (and (fboundp 'lr-track-ask-refresh-header) (featurep 'org))
    (condition-case err (lr-track-ask-refresh-header)
      (error (lr-track--log 'header err)))))

(defun lr-track--tick-activity ()
  (if (not lr-track-log-activity)
      (setq lr-track--episode nil)
    (let* ((now (float-time))
           (new (pcase lr-track--stable-state
                  ((or 'engaged 'reading) 'engaged)
                  ('elsewhere 'elsewhere) ('away 'away) ('slept 'asleep)
                  (_ nil))))
      (when (and new (not (eq new (plist-get lr-track--episode :state))))
        (when (plist-get lr-track--episode :state)
          (lr-track--activity-log (plist-get lr-track--episode :state)
                                  (plist-get lr-track--episode :start) now))
        (setq lr-track--episode (list :state new :start now))))))

(defun lr-track--run-phase (phase)
  "Run PHASE contained.  Return non-nil ONLY when it has failed past the limit
\(i.e. the tick should back off); a successful phase returns nil."
  (condition-case e
      (progn
        (pcase phase
          ('sense (lr-track--tick-sense))
          ('presence (lr-track--tick-presence))
          ('live-clock (lr-track--tick-live-clock))
          ('heartbeat (lr-track--tick-heartbeat))
          ('activity (lr-track--tick-activity))
          ('nudge (lr-track--nudge-check))
          ('modeline (lr-track--modeline-refresh))
          ('header (lr-track--tick-header)))
        (setf (alist-get phase lr-track--phase-failures) 0)
        nil)                            ; success means not degraded
    (error
     (lr-track--log (intern (format "phase-%s" phase)) e)
     (let ((n (1+ (or (alist-get phase lr-track--phase-failures) 0))))
       (setf (alist-get phase lr-track--phase-failures) n)
       (>= n lr-track--phase-failure-limit)))))

(defun lr-track--tick (generation)
  "Run one tick, then reschedule, but only if this is the live generation."
  (when (= generation lr-track--generation)
    (let ((degraded nil))
      (setq lr-track--tick-attention '(:state unknown))
      (dolist (phase lr-track--phases)
        (when (lr-track--run-phase phase) (setq degraded t)))
      (when (bound-and-true-p lr-track-mode)
        (lr-track--schedule
         (if degraded lr-track-degraded-interval
           (condition-case _ (lr-track--next-interval) (error lr-track-degraded-interval))))))))

;;;; logging (mem)

(defvar lr-track--log-buffer " *lr-track-log*")
(defun lr-track--log (tag payload)
  "Record an operational error to an in-memory buffer (never `display-warning',
which is invisible here, and never org)."
  (ignore-errors
    (with-current-buffer (get-buffer-create lr-track--log-buffer)
      (goto-char (point-max))
      (insert (format-time-string "[%H:%M:%S] ") (format "%s: %S\n" tag payload))
      (when (> (count-lines (point-min) (point-max)) 500)
        (goto-char (point-min)) (forward-line 100) (delete-region (point-min) (point))))))

;;;; status

;;;###autoload
(defun lr-track-status ()
  "Show, in plain language, what the coach thinks you are doing right now."
  (interactive)
  (let* ((state lr-track--stable-state)
         (e (lr-track-emacs-idle-seconds))
         (s (lr-track-system-idle-seconds))
         (task (lr-track--clock-task))
         (elapsed (lr-track--clock-elapsed))
         (since lr-track--stable-since))
    (with-current-buffer (get-buffer-create "*lr-track*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Right now\n\n" 'face 'bold))
        (insert (format "  state        %s%s\n" (lr-track--state-glyph state)
                        (concat " " (symbol-name state))))
        (when since (insert (format "  in state     %d min\n" (max 0 (floor (/ (- (float-time) since) 60.0))))))
        (insert (format "  at machine   %s (system idle %s)\n"
                        (if (and (numberp s) (< s lr-track-away-seconds)) "yes" "no/unknown")
                        (if (numberp s) (format "%ds" (round s)) "unknown")))
        (insert (format "  in Emacs     %s (Emacs idle %ds, %s)\n"
                        (if (< e lr-track-engaged-emacs-idle) "yes" "no")
                        (round e) (if lr-track--focused-p "focused" "unfocused")))
        (if task
            (insert (format "  clocked into %s%s\n" task
                            (if elapsed (format "  (%d min)" (round (/ elapsed 60.0))) "")))
          (insert "  clocked into (nothing)\n"))
        (cond
         ((and task (eq state 'away))
          (insert (propertize "\n  ! clocked but away from your machine.\n" 'face 'warning)))
         ((and task (eq state 'elsewhere))
          (insert "\n  | working outside Emacs, presumed on-task.\n")))
        (insert (format "\n  probe %s | banners today %d/%d\n"
                        (if (lr-track-sense-healthy-p) "healthy" "DEGRADED")
                        lr-track--budget-spent lr-track-daily-banner-budget))
        (insert (format "\n  Activity log: %s  (C-c C-x C-r for a clocktable)\n"
                        (abbreviate-file-name (lr-track--activity-file)))))
      (goto-char (point-min))
      (view-mode 1)
      (display-buffer (current-buffer)))))

;;;###autoload
(defun lr-track-doctor ()
  "Live self-check of the sensing seams."
  (interactive)
  (lr-track--sys-idle-probe)
  (sit-for 0.3)
  (message "lr-track: mode=%s ioreg=%s hid=%s emacs-idle=%.0fs focus=%s clocking=%s"
           (if (bound-and-true-p lr-track-mode) "on" "off")
           (if (executable-find "ioreg") "found" "MISSING")
           (let ((s lr-track--sys-idle-value)) (if (numberp s) (format "%.0fs" s) s))
           (lr-track-emacs-idle-seconds)
           lr-track--focused-p (lr-track--clocking-p)))

;;;; the mode

(defun lr-track--flush-episode ()
  "Write the in-progress activity episode so nothing is lost on mode-off."
  (when (and lr-track-log-activity (plist-get lr-track--episode :state))
    (ignore-errors
      (lr-track--activity-log (plist-get lr-track--episode :state)
                              (plist-get lr-track--episode :start) (float-time)))
    (setq lr-track--episode nil)))

(defun lr-track--on-kill-emacs ()
  "On a clean Emacs exit, mark the session clean (so next start won't recover),
flush the in-progress episode, and save the clocked buffer once more (the
autosave throttle may be holding its last advance).  That save happens only
while nothing but the clock line's own advance is unsaved: by now he has
answered `save-buffers-kill-emacs' about the file, and his no to his own
unsaved text stands.  Narrow, never signals."
  (ignore-errors (lr-track--state-put :clean t :heartbeat (float-time)))
  (ignore-errors
    (when (and (lr-track--clocking-p) (markerp org-clock-marker))
      (let ((buf (marker-buffer org-clock-marker)))
        (when (buffer-live-p buf)
          (if (eql (buffer-local-value 'lr-track--advanced-tick buf)
                   (with-current-buffer buf (buffer-chars-modified-tick)))
              (lr-track--maybe-autosave buf t)
            (when (buffer-modified-p buf)
              (lr-track--log 'kill-save-skipped (buffer-name buf))))))))
  (lr-track--flush-episode))

(defun lr-track--reap-timers ()
  "Cancel any timer whose function name starts with lr-track (belt + braces)."
  (dolist (tm (append timer-list timer-idle-list))
    (let ((fn (timer--function tm)))
      (when (and (symbolp fn) (string-prefix-p "lr-track" (symbol-name fn)))
        (ignore-errors (cancel-timer tm))))))

(defun lr-track--install-seams ()
  (add-function :after after-focus-change-function #'lr-track--focus-change)
  (unless (member lr-track--modeline-form global-mode-string)
    (setq global-mode-string (append (or global-mode-string '("")) (list lr-track--modeline-form))))
  (add-hook 'kill-emacs-hook #'lr-track--on-kill-emacs)
  (when (boundp 'doom-before-reload-hook)
    (add-hook 'doom-before-reload-hook #'lr-track--teardown)))

(defun lr-track--remove-seams ()
  (remove-function after-focus-change-function #'lr-track--focus-change)
  (setq global-mode-string (delete lr-track--modeline-form global-mode-string))
  (remove-hook 'kill-emacs-hook #'lr-track--on-kill-emacs)
  (when (boundp 'doom-before-reload-hook)
    (remove-hook 'doom-before-reload-hook #'lr-track--teardown)))

(defun lr-track--presence-fresh-p (p now)
  "Non-nil when presence P still vouches for NOW: its newest sample is at
most `lr-track-presence-break-seconds' old.  SPC d t twice while he works
then keeps what presence saw, instead of a fresh start whose first sample
would leave the running line unseen and pause it (`lr-track--clock-step')."
  (let ((prev (plist-get p :prev-time)))
    (and (memq (plist-get p :mode) '(here away))
         (numberp prev)
         (<= 0.0 (- now prev) lr-track-presence-break-seconds))))

(defun lr-track--enable ()
  (setq lr-track--generation (1+ lr-track--generation))
  (lr-track--reap-timers)
  (lr-track--focus-change)
  ;; crash detection: snapshot the previous session's state, then arm ours
  ;; (:clean nil means "this session has not exited cleanly yet").
  (setq lr-track--state-prev (lr-track--read-state)
        lr-track--recovered nil
        lr-track--state (list :clean nil :pid (emacs-pid)
                              :session-id (format "%d-%d" (emacs-pid) (floor (float-time)))
                              :heartbeat (float-time) :clock nil))
  (lr-track--write-state lr-track--state)
  (lr-track--install-seams)
  (setq lr-track--stable-state 'unknown lr-track--stable-since (float-time)
        lr-track--last-state nil lr-track--episode nil lr-track--incidents nil
        lr-track--away-pending nil lr-track--snooze-until nil lr-track--last-close nil)
  ;; Presence starts fresh here, and only here: a plain `load' keeps it (I13).
  ;; The clock's pause is NOT reset: it is a fact about the running line.
  (unless (lr-track--presence-fresh-p lr-track--presence (float-time))
    (setq lr-track--presence (lr-track--presence-init)))
  (setq lr-track--presence-stepped nil
        lr-track--last-sample nil
        lr-track--blind-since nil)
  ;; close any clock a crashed session left open, after the first frame is up.
  ;; A named function, so `lr-track--reap-timers' cancels it at teardown.
  (run-with-idle-timer 5 nil #'lr-track--recover-quietly)
  ;; startup check-in: prompt for what you're doing (and account for the gap since
  ;; Emacs was last alive).  Only after a real prior session; a quick reload has a
  ;; fresh heartbeat, so its ~0 gap just offers a plain clock-in.
  (when lr-track-checkin-on-startup
    (let ((prev-hb (and lr-track--state-prev (plist-get lr-track--state-prev :heartbeat))))
      (when prev-hb
        (run-with-idle-timer
         7 nil
         (lambda ()
           (when (and (lr-track--safe-to-prompt-p) (not (lr-track--clocking-p)))
             (setq lr-track--checkin-since prev-hb)
             (ignore-errors (lr-track-checkin 'startup))))))))
  (lr-track--schedule 5.0))

(defun lr-track--teardown ()
  (setq lr-track--generation (1+ lr-track--generation))
  (when (timerp lr-track--timer) (cancel-timer lr-track--timer) (setq lr-track--timer nil))
  (lr-track--reap-timers)
  (lr-track--remove-seams)
  (lr-track-sense-cleanup)
  (lr-track--flush-episode)
  ;; deliberate disable is not a crash: drop the clock pointer so a later crash
  ;; (while the mode is off) can't trigger recovery of a stale clock.
  (when lr-track--state (ignore-errors (lr-track--state-put :clock nil)))
  (setq lr-track--modeline-cache "" lr-track--focused-p t))

;;;###autoload
(define-minor-mode lr-track-mode
  "Background attention tracking + accountability coach."
  :global t :group 'lr-track
  (if lr-track-mode (lr-track--enable) (lr-track--teardown)))

(provide 'lr-track)
;;; lr-track.el ends here
