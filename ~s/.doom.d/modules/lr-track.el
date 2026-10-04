;;; lr-track.el --- deterministic time tracking on org clocks -*- lexical-binding: t; -*-

;;; Commentary:
;; The core of the tracker.  Deterministic by rule: every time it writes is NOW
;; (to the minute), a time he typed, or one of his own boundaries (a clock's
;; start, his last stop).  Nothing about this laptop is read: no input idle, no
;; screen lock, no sleep, no focus.  He works in other places and away from the
;; laptop, so a clock runs from his start to his stop, wherever he is.
;;
;; This file:
;;   - keeps the running clock's line on disk reading its start to now (the
;;     live line), forward only, so a crash never leaves it open;
;;   - autosaves that one buffer, silently, at most every 5 minutes;
;;   - remembers in a small machine-local file (state.eld) his last stop, the
;;     clock that was running (so a restart can say "it was running when Emacs
;;     quit"), and when he last confirmed the running clock;
;;   - provides the clock primitives lr-track-ask.el writes with.
;; The questions, the keys and every write he asks for live in lr-track-ask.el.

;;; Code:

(require 'cl-lib)
(require 'seq)
(eval-when-compile (require 'org-macs nil t))
;; Soft deps: read live, never required at load, so enabling the mode never
(declare-function org-clocking-p "org-clock" ())
(declare-function org-clock-out "org-clock" (&optional switch-to-state fail-quietly at-time))
(declare-function org-clock-cancel "org-clock" ())
(declare-function org-log-beginning "org" (&optional create))
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-clock-update-time-maybe "org-clock" ())
(declare-function org-end-of-subtree "org" (&optional invisible-ok to-heading))
(declare-function org-entry-end-position "org" ())
(declare-function org-time-string-to-seconds "org" (s))
(declare-function org-time-string-to-time "org" (s))
(declare-function org-entry-get "org" (epom property &optional inherit literal-nil))
(declare-function org-clock-update-mode-line "org-clock" (&optional refresh))
(declare-function lr-track-ask-refresh-header "lr-track-ask" ())
(declare-function org-base-buffer "org-macs" (buffer))
(declare-function org-get-heading "org" (&optional no-tags no-todo no-priority no-comment))
(declare-function org-remove-empty-drawer-at "org" (pos))
(declare-function org-clock-in "org-clock" (&optional select start-time))
(declare-function org-get-outline-path "org" (&optional with-self use-cache))
(declare-function org-duration-from-minutes "org-duration" (minutes &optional fmt canonical))
(declare-function org-time-stamp-format "org" (&optional with-time inactive custom))
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
  "Deterministic time tracking on org clocks."
  :group 'org)

(defcustom lr-track-live-clock-line t
  "Keep the running clock's line on disk reading its start to now.
Every tick moves the line's trailing stamp forward to the current minute, so a
crash or a kill never leaves a line open and the file always says how long the
clock has run.  Deterministic: it depends on the clock only, never on this
laptop.  It never closes the clock and never moves a stamp backwards; your own
clock-out still ends it, at now or at the time you give.  nil leaves the line
open until you clock out, as plain org does."
  :type 'boolean)

(defcustom lr-track-autosave-clock t
  "Save the clocked org file after its line is advanced.
At most once per `lr-track-autosave-interval', and once more when Emacs is
killed.  The save goes through `lr-track--save-buffer', which neutralises this
config's three before-save rewriters and saves only that one buffer,
silently.  nil leaves saving to you."
  :type 'boolean)

(defcustom lr-track-autosave-interval 300.0
  "Seconds between two autosaves of the clocked buffer.
Kill-emacs saves at once."
  :type 'number)

(defcustom lr-track-interval 60.0
  "Seconds between two ticks."
  :type 'number)

(defvar lr-track--interval nil "The interval the current tick was armed with.")
(defvar lr-track--last-tick nil "`float-time' the current tick was armed at.")

(defvar lr-track--internal nil
  "Non-nil around lr-track's own clock-ins and clock-outs.
`lr-track-ask' uses it to tell its own writes from his.")

(defvar lr-track--last-clock-end nil
  "His last stop: the float time the most recent clock line ended at.
Set when any clock is clocked out and when a closed line is written, persisted
in state.eld as :last-stop, and read back at enable.  Never moves backwards
except when an undo restores it.")

(defcustom lr-track-autoout-floor-seconds 60.0
  "Minimum length of an auto-closed clock interval.
Load-bearing: `org-clock-out-remove-zero-time-clocks' is t and `org-clock-out'
writes minute resolution, so a sub-minute close rounds to 0:00 and DELETES the
CLOCK line.  This floor makes that unreachable."
  :type 'number)

(defvar lr-track--generation 0 "Monotonic token; a stale timer no-ops when its generation differs.")

(defvar lr-track--timer nil "The single tick timer.")

(defvar lr-track--phase-failures nil "Alist (PHASE . consecutive-failures).")

(defconst lr-track--phase-failure-limit 3 "Consecutive failures before a phase is disabled + tick backs off.")

(defvar lr-track--state nil "In-memory mirror of the heartbeat sidecar plist.")

(defvar-local lr-track--autosaved-at nil
  "`float-time' this buffer was last autosaved by `lr-track--maybe-autosave'.")

(defvar-local lr-track--advanced-tick nil
  "`buffer-chars-modified-tick' right after the last advance of the clock line.
While the buffer's tick still equals it, nothing but that line is unsaved.")

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

(defconst lr-track--note-tags
  '("now" "since" "gap" "split" "resumed" "restarted" "ended"
    "declared" "continued" "away" "sleep" "picked")
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

(defconst lr-track--clock-default-max 36000.0
  "A clock's maximum when no TRACK_MAX is set on or above its heading: 10 h.")

(defun lr-track--parse-max (s)
  "Seconds for a TRACK_MAX string S in H:MM, like \"10:00\"; nil if not one."
  (when (and (stringp s)
             (string-match "\\`[ \t]*\\([0-9]+\\):\\([0-5][0-9]\\)[ \t]*\\'" s))
    (let ((secs (+ (* 3600.0 (string-to-number (match-string 1 s)))
                   (* 60.0 (string-to-number (match-string 2 s))))))
      (and (> secs 0) secs))))

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

(defun lr-track--clock-task-from (task since)
  "Clock into TASK (LABEL . MARKER) as if it had started at SINCE (float) -
i.e. a running clock backdated to when you actually started."
  (require 'org-clock)
  (let ((marker (cdr task)))
    (cond
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
  "Log a completed interval START..END for TASK (LABEL . MARKER): a closed
CLOCK line at the top of the heading's LOGBOOK."
  (require 'org)
  (let ((target (cdr task))
        (mins (max 0 (round (/ (- end-float start-float) 60.0)))))
    (cond
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
  "Float time for H:MM on CURSOR's day, rolled to the next day if that
would land at or before CURSOR.  Returns nil unless it sits in (CURSOR, NOW]."
  (let ((d (decode-time (seconds-to-time cursor))))
    (setf (nth 0 d) 0 (nth 1 d) mm (nth 2 d) h)
    (let ((t0 (float-time (encode-time d))))
      (when (<= t0 cursor) (setq t0 (+ t0 86400.0)))
      (and (> t0 cursor) (<= t0 (+ now 90.0)) (min t0 now)))))

(defun lr-track--duration-minutes (s)
  "Minutes (float) for a duration like 90, 90m, 1h, 1h30, 1.5h or 2:15.
nil for anything else."
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

(defun lr-track--schedule (interval)
  "Arm the next tick.  Single choke point: stamps `lr-track--last-tick' to now at
the same moment it sets the interval, so gap is always measured from here."
  (setq lr-track--interval interval
        lr-track--last-tick (float-time))
  (when (timerp lr-track--timer) (cancel-timer lr-track--timer))
  (let ((gen lr-track--generation))
    (setq lr-track--timer (run-with-timer interval nil #'lr-track--tick gen))))

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

(defun lr-track--reap-timers ()
  "Cancel any timer whose function name starts with lr-track (belt + braces)."
  (dolist (tm (append timer-list timer-idle-list))
    (let ((fn (timer--function tm)))
      (when (and (symbolp fn) (string-prefix-p "lr-track" (symbol-name fn)))
        (ignore-errors (cancel-timer tm))))))


;;;; the running clock's start

(defun lr-track--sync-running-start ()
  "When he moved the running line's start (S-up or S-down on its stamp), make
`org-clock-start-time' follow it.  Org's `org-clock-update-time-maybe' does
this for an open line only, and the live line is closed in form.  Only the
line at `org-clock-marker' is read, only when the marker sits past its start
stamp, and never inside org's own clock resolution.  Never signals."
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
                  (old (float-time org-clock-start-time)))
              (unless (= (floor new 60) (floor old 60))
                (setq org-clock-start-time (seconds-to-time new))
                (lr-track--log 'start-moved (list :from old :to new))
                t)))))
    (error (lr-track--log 'sync-start err) nil)))

;;;; his last stop and the running clock (state.eld)

(defun lr-track--set-last-stop (at &optional exact)
  "Make AT his last stop, persisted.  It only moves forward, unless EXACT
\(an undo putting back what was there before)."
  (when (or exact
            (and (numberp at)
                 (or (null lr-track--last-clock-end)
                     (> at lr-track--last-clock-end))))
    (setq lr-track--last-clock-end at)
    (lr-track--state-put :last-stop at)))

(defun lr-track--running-info ()
  "The running clock as (:task S :file F :start F :at F), or nil.
:olp is its outline path, to find it again; :at is where its line ends on
disk (the live line), or now when it is open."
  (when (and (lr-track--clocking-p)
             (not (lr-track--clock-stood-in-p))
             (markerp org-clock-marker)
             (buffer-live-p (marker-buffer org-clock-marker))
             (boundp 'org-clock-start-time) org-clock-start-time)
    (list :task (lr-track--clock-task)
          :file (buffer-file-name (or (buffer-base-buffer (marker-buffer org-clock-marker))
                                      (marker-buffer org-clock-marker)))
          :olp (ignore-errors
                 (org-with-point-at org-clock-hd-marker (org-get-outline-path t)))
          :start (float-time org-clock-start-time)
          :at (or (lr-track--running-line-end) (float-time)))))

(defun lr-track--on-clock-in ()
  "`org-clock-in-hook': the new clock is the running one, unconfirmed."
  (condition-case err
      (unless (lr-track--clock-stood-in-p)
        (lr-track--state-put :running (lr-track--running-info) :confirmed nil))
    (error (lr-track--log 'clock-in-hook err))))

(defun lr-track--on-clock-out ()
  "`org-clock-out-hook': his last stop is where the clock ended; nothing runs."
  (condition-case err
      (unless (lr-track--clock-stood-in-p)
        (let ((out (and (boundp 'org-clock-out-time) org-clock-out-time
                        (float-time org-clock-out-time))))
          (lr-track--state-put :running nil :confirmed nil)
          (when out (lr-track--set-last-stop out))))
    (error (lr-track--log 'clock-out-hook err))))

(defun lr-track--on-clock-cancel ()
  "`org-clock-cancel-hook': the cancelled clock never ran."
  (condition-case err
      (unless (lr-track--clock-stood-in-p)
        (lr-track--state-put :running nil :confirmed nil))
    (error (lr-track--log 'clock-cancel-hook err))))

(defun lr-track--on-kill-emacs ()
  "`kill-emacs-hook': bring the running line to now, save it, remember it."
  (condition-case err
      (when (lr-track--clocking-p)
        (lr-track--tick-live-clock)
        (when (and (markerp org-clock-marker) (marker-buffer org-clock-marker))
          (lr-track--maybe-autosave (marker-buffer org-clock-marker) t))
        (lr-track--state-put :running (lr-track--running-info)))
    (error (lr-track--log 'kill-emacs err))))

;;;; the tick

(defconst lr-track--phases '(live-clock heartbeat header)
  "What one tick does, in order; each phase is contained by `lr-track--run-phase'.")

(defun lr-track--tick-live-clock ()
  "Tick phase: the running clock's line reads its start to now.
Forward only, a same-minute no-op, never the dangling line org stands in while
it resolves one, and nothing about this laptop is read."
  (when (and lr-track-live-clock-line
             (lr-track--clocking-p)
             (not (lr-track--clock-stood-in-p)))
    (lr-track--sync-running-start)
    (lr-track--advance-clock-line (float-time))))

(defun lr-track--tick-heartbeat ()
  "Tick phase: remember the running clock (state.eld :running)."
  (let ((info (lr-track--running-info)))
    (when info (lr-track--state-put :running info))))

(defun lr-track--tick-header ()
  "Tick phase: refresh the agenda's Time line, when lr-track-ask is loaded."
  (when (fboundp 'lr-track-ask-refresh-header)
    (lr-track-ask-refresh-header)))

(defun lr-track--run-phase (phase)
  "Run PHASE contained.  Non-nil only when it has failed past the limit."
  (condition-case e
      (progn
        (pcase phase
          ('live-clock (lr-track--tick-live-clock))
          ('heartbeat (lr-track--tick-heartbeat))
          ('header (lr-track--tick-header)))
        (setf (alist-get phase lr-track--phase-failures) 0)
        nil)
    (error
     (lr-track--log (intern (format "phase-%s" phase)) e)
     (let ((n (1+ (or (alist-get phase lr-track--phase-failures) 0))))
       (setf (alist-get phase lr-track--phase-failures) n)
       (>= n lr-track--phase-failure-limit)))))

(defun lr-track--tick (generation)
  "Run one tick, then arm the next, but only for the live GENERATION."
  (when (= generation lr-track--generation)
    (dolist (phase lr-track--phases)
      (lr-track--run-phase phase))
    (when (bound-and-true-p lr-track-mode)
      (lr-track--schedule lr-track-interval))))

;;;; the mode

(defun lr-track--enable ()
  "Start: read state.eld, install the clock hooks, arm the tick."
  (lr-track--teardown)
  (setq lr-track--state (or (lr-track--read-state) nil)
        lr-track--last-clock-end (or lr-track--last-clock-end
                                     (plist-get lr-track--state :last-stop)))
  (add-hook 'org-clock-in-hook #'lr-track--on-clock-in)
  (add-hook 'org-clock-out-hook #'lr-track--on-clock-out)
  (add-hook 'org-clock-cancel-hook #'lr-track--on-clock-cancel)
  (add-hook 'kill-emacs-hook #'lr-track--on-kill-emacs)
  (when (boundp 'doom-before-reload-hook)
    (add-hook 'doom-before-reload-hook #'lr-track--teardown))
  (cl-incf lr-track--generation)
  (lr-track--schedule lr-track-interval))

(defun lr-track--teardown ()
  "Stop: cancel the tick and remove the hooks.  Never touches a clock."
  (cl-incf lr-track--generation)
  (when (timerp lr-track--timer) (cancel-timer lr-track--timer))
  (setq lr-track--timer nil)
  (lr-track--reap-timers)
  (remove-hook 'org-clock-in-hook #'lr-track--on-clock-in)
  (remove-hook 'org-clock-out-hook #'lr-track--on-clock-out)
  (remove-hook 'org-clock-cancel-hook #'lr-track--on-clock-cancel)
  (remove-hook 'kill-emacs-hook #'lr-track--on-kill-emacs)
  (when (boundp 'doom-before-reload-hook)
    (remove-hook 'doom-before-reload-hook #'lr-track--teardown)))

;;;###autoload
(define-minor-mode lr-track-mode
  "Deterministic time tracking on org clocks (see lr-track.el's commentary)."
  :global t
  :group 'lr-track
  (if lr-track-mode (lr-track--enable) (lr-track--teardown)))

;;;; leftovers of the presence-based version, removed at load
;; A `load' over a running session keeps whatever the old file installed.
;; Each removal is a no-op when nothing is installed, so this is safe on a
;; fresh start too.

(dolist (a '((org-clock-in . lr-track--clock-in-paused-task)
             (org-clock-out . lr-track--clock-out-args)
             (org-clock-get-clock-string . lr-track--clock-string)
             (org-clock-cancel . lr-track--clock-cancel-note)))
  (advice-remove (car a) (cdr a)))
(dolist (h '((org-clock-in-hook . lr-track--forget-pause)
             (org-clock-in-hook . lr-track--hold-here-block)
             (org-clock-cancel-hook . lr-track--forget-pause)
             (org-clock-out-hook . lr-track--on-clock-out-v3)
             (kill-emacs-hook . lr-track--flush-episode)))
  (remove-hook (car h) (cdr h)))
(remove-function after-focus-change-function 'lr-track--focus-change)
(setq global-mode-string
      (delete '(:eval (lr-track--modeline-string)) (bound-and-true-p global-mode-string)))

(provide 'lr-track)
;;; lr-track.el ends here
