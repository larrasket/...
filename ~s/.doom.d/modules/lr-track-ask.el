;;; lr-track-ask.el --- Now? in the agenda: streams, answers, undo -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; v3 Stage 1, the question side.  lr-track.el senses presence and keeps the
;; running clock honest; this file asks what the time was, only when he looks,
;; and writes exactly the answer he gave.
;;
;;   STREAMS  time.org (`lr-track-time-file'), created by SPC d M and nothing
;;            else: nine plain headings, each with a key, a place and a max.
;;   CONTEXT  `lr-track--ask-context' is the one door from live state.  Every
;;            function after it is pure over that plist (tests build it by
;;            hand): the state, the answer start, the header, the echo and the
;;            plan of each key.
;;   HEADER   the f agenda's header line: the clock, or Now? when something is
;;            due.  Display only, read from a cache the tick refreshes.
;;   ASK      y in the f agenda (or SPC d j) shows in the echo area what each
;;            key would write, then takes ONE key through a transient map that
;;            any other key, focus loss, a minibuffer or 15 s idle closes.  The
;;            key recomputes its plan and refuses when it is not the one shown
;;            (I18).
;;   WRITES   `lr-track--execute-plan' clocks in backdated, ends clocks and logs
;;            closed lines, each with a `- lr TAG:' note under its CLOCK line,
;;            saves silently and pushes ONE undo record that matches by exact
;;            text (`lr-track-undo').
;;
;; Nothing in this file reads the minibuffer, displays a buffer, selects a
;; window or captures a key, except `lr-track-ask', its away map and
;; `lr-track-setup'.  A test scans the source for exactly that.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lr-track)

(declare-function org-entry-get "org" (epom property &optional inherit literal-nil))
(declare-function org-get-heading "org" (&optional no-tags no-todo no-priority no-comment))
(declare-function org-clock-cancel "org-clock" ())
(declare-function org-clock-in "org-clock" (&optional select start-time))
(declare-function org-duration-from-minutes "org-duration" (minutes &optional fmt canonical))
(declare-function evil-define-minor-mode-key "evil-core" (state mode key def &rest bindings))
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function evil-normalize-keymaps "evil-core" (&optional state))
(defvar org-clock-marker)
(defvar org-clock-hd-marker)
(defvar org-clock-start-time)
(defvar org-clock-auto-clock-resolution)
(defvar org-clock-in-switch-to-state)
(defvar org-clock-leftover-time)
(defvar org-clock-in-resume)
;; bound by `lr-track--with-pristine-org-globals': special here too, or a
;; compiled `let' would bind them lexically and org would never see them
(defvar org-log-into-drawer)
(defvar org-clock-out-switch-to-state)
(defvar org-clock-rounding-minutes)
(defvar org-clock-idle-time)
(defvar org-log-note-clock-out)
(defvar evil-state)
(defvar evil-local-mode)
(defvar doom-leader-map)

;;;; settings and constants

(defcustom lr-track-time-file "~/roam/main/time.org"
  "Org file of the streams: top-level headings carrying a numeric TRACK_KEY.
Only `lr-track-setup' creates it.  Answers add CLOCK lines and notes under its
streams; nothing else here ever writes it."
  :type 'file :group 'lr-track)

(defconst lr-track--stream-seeds
  '((1 "avey" machine "10:00") (2 "study" machine "10:00")
    (3 "build" machine "10:00") (4 "writing" machine "10:00")
    (5 "reading" either "8:00") (6 "practice" away "4:00")
    (7 "life" away "4:00") (8 "leisure" either "8:00")
    (9 "sleep" away "16:00"))
  "The streams SPC d M creates, as (KEY NAME PLACE MAX).")

(defconst lr-track-ask-timeout 15
  "Seconds of idle after which the one-key map closes by itself.")
(defconst lr-track--ask-back-seconds 120.0
  "Here this long since his return before a paused clock asks Now?.")
(defconst lr-track--ask-open-seconds 600.0
  "Here this long with nothing running before the header asks Now?.")
(defconst lr-track--answer-window 5400.0
  "How far back a now answer may start: 90 min.  Anything older stays open.")
(defconst lr-track--header-essential-width 90
  "Columns the header's essential part (before the key hints) must fit in.")
(defconst lr-track--note-max 70
  "Longest note line, `- lr TAG: ' included.")
(defconst lr-track--undo-max 10
  "Undo records kept; the oldest beyond this can no longer be undone.")
(defconst lr-track--ask-undo-recent-seconds 600.0
  "The u key after y undoes only a write this recent, and its echo names it.
SPC d u reaches the whole stack: it is a command of its own.")
(defconst lr-track--echo-width 186
  "Columns of his frame (spec v3 2): every echo line fits in it.")
(defconst lr-track--echo-task-width 24
  "Columns a task name gets where an echo line names it twice.")
(defconst lr-track--covered-max 20
  "Spans kept in `lr-track--covered-spans'.")
(defconst lr-track--agenda-buffer "*Org Agenda(f)*"
  "The one agenda buffer that gets the header and `lr-track-agenda-mode'.")
(defconst lr-track--agenda-sticky-commands
  '(salih/org-agenda-no-full-f salih/toggle-agenda-late)
  "Commands that rebuild the sticky f agenda without running its finalize.")
(defconst lr-track--header-line-form '(:eval (lr-track--header-cached))
  "The `header-line-format' the f agenda gets.")

;; Arabic letters and digits as codepoints, so this source stays ASCII.  Each
;; letter is the one on the same physical key under the Arabic PC layout.
(defconst lr-track--ar-ghain #x63a "Arabic ghain, the y key.")
(defconst lr-track--ar-sheen #x634 "Arabic sheen, the a key.")
(defconst lr-track--ar-ain #x639 "Arabic ain, the u key.")
(defconst lr-track--ar-teh #x62a "Arabic teh, the j key.")
(defconst lr-track--ar-noon #x646 "Arabic noon, the k key.")
(defconst lr-track--ar-yeh #x64a "Arabic yeh, the d key.")
(defconst lr-track--ar-khah #x62e "Arabic khah, the o key.")
(defconst lr-track--ar-digit-zeros '(#x660 #x6f0)
  "Arabic-Indic and Extended Arabic-Indic zero: digit N is the zero plus N.")

;;;; state

(defvar lr-track--streams-cache nil
  "(FILE MODTIME TICK STREAMS): the streams last read, by file modification
time and the visiting buffer's `buffer-chars-modified-tick'.")
(defvar lr-track--header-cache nil
  "The header text, written by `lr-track-ask-refresh-header' only.")
(defvar lr-track--ask-shown nil
  "While a one-key map is up: (STATE . ALIST), each key's plan signature as
the echo showed it.  `lr-track--ask-on-exit' clears it.")
(defvar lr-track--ask-map nil
  "The one-key map currently installed by `lr-track-ask', or nil.")
(defvar lr-track--undo-stack nil
  "Undo records, newest first, at most `lr-track--undo-max'.")
(defvar lr-track--covered-spans nil
  "Spans a line already books, newest first, as (FROM . TO): every clock line
that ended this session and every away he labelled.  An away inside one is
not offered for a label again (`lr-track--ask-away-covered-p').  At most
`lr-track--covered-max'.")
(defvar lr-track--ask-echo-shown nil
  "The echo text a one-key map of ours showed, while that map is up.")
(defvar lr-track--ask-echo-seen nil
  "Non-nil when the key being read came while the echo area still showed
the one-key map's echo (`lr-track--ask-note-echo').  Cleared each time an
echo is shown, so a key after another message, or after the echo area was
emptied, finds it nil.")
(defvar lr-track--ask-where nil
  "While a one-key map is up: (WINDOW BUFFER STATE), where its echo was
shown and the evil state there.  A key of the map answers only there
\(`lr-track--ask-here-p').")
(defvar lr-track--ask-display-only nil
  "Non-nil while the context is read for display only (the header).
The clock is then not settled: a header refresh runs from clock hooks and
right after an undo, where moving the running line would change what was
just written.  The next tick, or his next key, settles it.")
(defvar-local lr-track--setup-file nil
  "In the *Time setup* preview: the file RET would create.")

;;;; small helpers

(defun lr-track--ask-say (text)
  "Echo TEXT without logging it in *Messages*."
  (let ((message-log-max nil))
    (message "%s" text)))

(defun lr-track--ask-minute (f)
  "Float time F floored to the minute, the resolution of a CLOCK stamp."
  (* 60.0 (floor f 60)))

(defun lr-track--ask-dur (secs)
  "SECS as a compact duration: 25m, 2h, 1h12m."
  (lr-track--fmt-dur (/ (max 0.0 secs) 60.0)))

(defun lr-track--ask-cut (s n)
  "S cut to at most N columns."
  (if (> (string-width s) n) (truncate-string-to-width s n) s))

(defun lr-track--ask-name (s)
  "S, a name as he wrote it, safe inside a left-to-right line.
A name in a right-to-left script (an Arabic heading) gets a mark after it,
so the hint that follows keeps its place (`bidi-string-mark-left-to-right').
Any other name comes back as it is."
  (bidi-string-mark-left-to-right s))

(defun lr-track--ask-cut-words (s n)
  "S cut to at most N columns, at a word boundary when one is near."
  (if (<= (string-width s) n)
      s
    (let* ((cut (truncate-string-to-width s n))
           (sp (string-match-p " [^ ]*\\'" cut)))
      (if (and sp (>= sp (/ n 2))) (substring cut 0 sp) cut))))

(defun lr-track--ask-kind-text (kind)
  "How an away of KIND reads: locked, no input, Mac asleep or not seen."
  (pcase kind
    ('locked "locked")
    ('asleep "Mac asleep")
    ('unseen "not seen")
    (_ "no input")))

(defun lr-track--ask-ascii (s)
  "S with every character outside printable ASCII dropped, spaces folded."
  (string-trim (replace-regexp-in-string
                "[ \t]+" " "
                (replace-regexp-in-string "[^ -~]+" " " (or s "")))))

(defun lr-track--ask-task-ascii (task)
  "TASK as an ASCII name for a note: at most 30 characters, never empty."
  (let ((a (lr-track--ask-ascii task)))
    (if (string-match-p "[[:alnum:]]" a)
        (string-trim-right (substring a 0 (min 30 (length a))))
      "the last task")))

(defun lr-track--note-text (tag text)
  "TEXT as the body of a note tagged TAG: ASCII, the whole line at most
`lr-track--note-max' characters."
  (let* ((body (lr-track--ask-ascii text))
         (room (- lr-track--note-max (length (format "- lr %s: " tag)))))
    (if (> (length body) room)
        (string-trim-right (substring body 0 (max 0 room)))
      body)))

(defun lr-track--note-line (tag text)
  "The note line for TAG and TEXT, without indentation."
  (format "- lr %s: %s" tag text))

;;;; streams and time.org

(defun lr-track--time-file ()
  "`lr-track-time-file', expanded."
  (expand-file-name lr-track-time-file))

(defun lr-track--time-file-text ()
  "The text SPC d M writes: the preamble and one heading per seed."
  (concat
   "#+title: Time\n"
   "#+startup: overview\n"
   "# Managed by lr-track. Streams are plain headings (no TODO keyword),"
   " so this file stays out\n"
   "# of the agenda. Edit the properties freely.\n"
   "\n"
   (mapconcat
    (lambda (s)
      (format (concat "* %s\n:PROPERTIES:\n:TRACK_KEY:   %d\n"
                      ":TRACK_PLACE: %s\n:TRACK_MAX:   %s\n:END:\n")
              (nth 1 s) (nth 0 s) (nth 2 s) (nth 3 s)))
    lr-track--stream-seeds "\n")))

(defun lr-track--time-buffer (file)
  "A buffer visiting FILE, current with the disk, never displayed.
An unmodified buffer whose file changed on disk is reverted without asking
\(a header refresh runs on a timer, where a question would be a prompt from
a timer); one with unsaved edits is read as it stands.  For the same reason
the first visit applies only safe local variables (never the question about
the others), never asks about the file's size, and says nothing (NOWARN): no
auto-save-data warning and the `sit-for' after it, no write-protect note."
  (let ((buf (find-buffer-visiting file)))
    (cond
     ((null buf)
      (let ((enable-local-variables :safe)
            (enable-dir-local-variables nil)
            (large-file-warning-threshold nil))
        (find-file-noselect file t)))
     ((or (verify-visited-file-modtime buf) (buffer-modified-p buf)) buf)
     (t (with-current-buffer buf (revert-buffer t t t))
        buf))))

(defun lr-track--read-streams (file)
  "The streams in FILE, sorted by key (see `lr-track--streams').
Only keys 1 to 9 are streams, since only those have a key to answer with,
and only the first heading with a key: the rest are logged and left out."
  (require 'org)
  (let ((buf (lr-track--time-buffer file))
        (streams nil)
        (skipped nil))
    (with-current-buffer buf
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (re-search-forward "^\\* " nil t)
            (let* ((pos (line-beginning-position))
                   (raw (org-entry-get pos "TRACK_KEY")))
              (when (and (stringp raw)
                         (string-match "\\`[ \t]*\\([0-9]+\\)[ \t]*\\'" raw))
                (let ((key (string-to-number (match-string 1 raw))))
                  (if (or (not (<= 1 key 9))
                          (seq-find (lambda (x) (eql (plist-get x :key) key))
                                    streams))
                      (push (cons key pos) skipped)
                    (let* ((m (copy-marker pos))
                           (place (lr-track--clock-place m)))
                      (push (list :key key
                                  :name (save-excursion
                                          (goto-char pos)
                                          (substring-no-properties
                                           (org-get-heading t t t t)))
                                  :place (car place)
                                  :max (cdr place)
                                  :marker m)
                            streams))))))))))
    (when skipped
      (lr-track--log 'streams-skipped (nreverse skipped)))
    (sort streams (lambda (a b) (< (plist-get a :key) (plist-get b :key))))))

(defun lr-track--time-buffer-tick (file)
  "`buffer-chars-modified-tick' of the buffer visiting FILE, or nil."
  (let ((buf (find-buffer-visiting file)))
    (and buf (buffer-chars-modified-tick buf))))

(defun lr-track--streams ()
  "The streams of `lr-track-time-file', sorted by key, or nil without it.
Each is (:key N :name S :place SYM :max SECS :marker M), one per top-level
heading with a TRACK_KEY of 1 to 9; place and max follow
`lr-track--clock-place'.  The file is visited with `find-file-noselect', never
displayed, and only when it exists.  Cached until its modification time or
its buffer's text changes: an unsaved move or delete of a heading must not
leave a key on the wrong one."
  (let ((file (lr-track--time-file)))
    (when (file-exists-p file)
      (let ((mtime (file-attribute-modification-time (file-attributes file)))
            (tick (lr-track--time-buffer-tick file))
            (c lr-track--streams-cache))
        (if (and c (equal (nth 0 c) file) (equal (nth 1 c) mtime)
                 tick (eql (nth 2 c) tick)
                 (seq-every-p (lambda (s)
                                (buffer-live-p
                                 (marker-buffer (plist-get s :marker))))
                              (nth 3 c)))
            (nth 3 c)
          (let ((streams (lr-track--read-streams file)))
            ;; the old markers are dead weight in the buffer from now on
            (dolist (old (nth 3 c))
              (let ((m (plist-get old :marker)))
                (when (markerp m) (set-marker m nil))))
            (setq lr-track--streams-cache
                  (list file
                        (file-attribute-modification-time (file-attributes file))
                        (lr-track--time-buffer-tick file)
                        streams))
            streams))))))

(defun lr-track--stream-checked (n)
  "Stream N, or an error when time.org no longer has it where it was read.
The marker must sit on a top-level heading whose TRACK_KEY is still N: a
write never lands on a heading the echo did not name."
  (let* ((stream (or (lr-track--stream-by-key n)
                     (error "No stream %d in %s" n (lr-track--time-file))))
         (m (plist-get stream :marker))
         (ok (and (markerp m) (buffer-live-p (marker-buffer m))
                  (with-current-buffer (marker-buffer m)
                    (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char m)
                        (and (bolp) (looking-at "\\* ")
                             (equal (string-trim
                                     (or (org-entry-get (point) "TRACK_KEY") ""))
                                    (number-to-string n)))))))))
    (unless ok
      (setq lr-track--streams-cache nil)
      (error "time.org changed (stream %d moved), nothing written; y again" n))
    stream))

(defun lr-track--stream-by-key (n)
  "The stream whose key is N, or nil."
  (seq-find (lambda (s) (eql (plist-get s :key) n)) (lr-track--streams)))

(defun lr-track--stream-of-marker (m)
  "The stream whose subtree holds marker M, or nil."
  (when (and (markerp m) (buffer-live-p (marker-buffer m)))
    (let* ((buf (marker-buffer m))
           (mine (seq-filter (lambda (s)
                               (eq (marker-buffer (plist-get s :marker)) buf))
                             (lr-track--streams))))
      (when mine
        (let ((top (with-current-buffer buf
                     (save-excursion
                       (save-restriction
                         (widen)
                         (goto-char m)
                         (beginning-of-line)
                         (if (looking-at "\\* ")
                             (point)
                           (and (re-search-backward "^\\* " nil t) (point))))))))
          (and top
               (seq-find (lambda (s) (= (plist-get s :marker) top)) mine)))))))

(defun lr-track--ask-stream (ctx n)
  "The stream keyed N among CTX's streams, or nil."
  (seq-find (lambda (s) (eql (plist-get s :key) n)) (plist-get ctx :streams)))

;;;; SPC d M: the setup preview

(defun lr-track--setup-text (file)
  "The preview of creating FILE with the seeds.
The first row's cells are three spaces apart; each cell of the second row
starts in the column of the cell above it."
  (let* ((cells (mapcar (lambda (s) (format "%d %s (%s)" (nth 0 s) (nth 1 s) (nth 2 s)))
                        lr-track--stream-seeds))
         (top (seq-take cells 5))
         (cols (let ((col 4) (out nil))
                 (dolist (c top (nreverse out))
                   (push col out)
                   (setq col (+ col (length c) 3)))))
         (row2 (let ((line ""))
                 (cl-loop for c in (seq-drop cells 5)
                          for col in cols
                          do (setq line (concat (string-pad
                                                 line (if (string-empty-p line) col
                                                        (max col (+ 2 (length line)))))
                                                c)))
                 line)))
    (concat
     "Time setup   nothing is written until RET; q cancels\n"
     (format "  create %s with %d streams (no TODO keywords, so it stays out of the agenda):\n"
             (abbreviate-file-name file) (length lr-track--stream-seeds))
     "    " (mapconcat #'identity top "   ") "\n"
     row2 "\n"
     "  your 20 old buckets in life.org stay where they are until a later stage moves them\n"
     "  RET create   q cancel\n")))

(defun lr-track--setup-exists-text (file)
  "The refusal when FILE already exists.
When it holds no stream, it says so and where to add them: SPC d M never
writes over his file, so the way on is in the file."
  (if (condition-case nil (lr-track--streams) (error nil))
      (format "%s already exists, so its streams stay as they are. Nothing written."
              (abbreviate-file-name file))
    (format "%s Nothing written." (lr-track--nostreams-file-text file))))

(defun lr-track--nostreams-file-text (file)
  "What to do when FILE, time.org, exists but holds no stream."
  (format "%s has no streams (top-level headings with TRACK_KEY 1 to 9): add them there."
          (abbreviate-file-name file)))

(defun lr-track--setup-close ()
  "Close the *Time setup* preview and kill it."
  (let* ((buf (get-buffer "*Time setup*"))
         (win (and buf (get-buffer-window buf))))
    (cond (win (quit-window t win))
          (buf (kill-buffer buf)))))

(defun lr-track--setup-create ()
  "RET in the preview: write time.org with the seeds, unless it exists."
  (interactive)
  (let ((file (or lr-track--setup-file (lr-track--time-file))))
    (if (file-exists-p file)
        (lr-track--ask-say (lr-track--setup-exists-text file))
      (make-directory (file-name-directory file) t)
      (let ((create-lockfiles nil))
        ;; `excl': never over a file that appeared since the check
        (write-region (lr-track--time-file-text) nil file nil 'quiet nil 'excl))
      (lr-track--setup-close)
      (lr-track--ask-say (format "Created %s with %d streams. y in your agenda now asks Now?"
                                 (abbreviate-file-name file)
                                 (length lr-track--stream-seeds))))))

(defun lr-track--setup-cancel ()
  "q in the preview: close it, write nothing."
  (interactive)
  (lr-track--setup-close)
  (lr-track--ask-say "Time setup closed. Nothing written."))

(defvar lr-track-setup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'lr-track--setup-create)
    (define-key map [return] #'lr-track--setup-create)
    (define-key map "q" #'lr-track--setup-cancel)
    map)
  "Keys of the *Time setup* preview: RET creates, q cancels.")

(define-derived-mode lr-track-setup-mode special-mode "Time-Setup"
  "The SPC d M preview.  Nothing is written until RET; q cancels.")

(with-eval-after-load 'evil
  (evil-set-initial-state 'lr-track-setup-mode 'motion)
  (evil-define-key* '(motion normal) lr-track-setup-mode-map
    (kbd "RET") #'lr-track--setup-create
    [return] #'lr-track--setup-create
    "q" #'lr-track--setup-cancel))

(defun lr-track-setup ()
  "SPC d M: preview creating `lr-track-time-file' with the nine streams.
The preview writes nothing; RET in it creates the file, q closes it.  An
existing file is his: this refuses with an echo and opens nothing."
  (interactive)
  (let ((file (lr-track--time-file)))
    (if (file-exists-p file)
        (lr-track--ask-say (lr-track--setup-exists-text file))
      (let ((buf (get-buffer-create "*Time setup*")))
        (with-current-buffer buf
          (lr-track-setup-mode)
          (setq lr-track--setup-file file)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (lr-track--setup-text file)))
          (goto-char (point-min)))
        (pop-to-buffer buf)))))

;;;; the context (the one door from live state)

(defun lr-track--ask-clock ()
  "The running clock for the context, or nil when none runs."
  (when (and (lr-track--clocking-p)
             (boundp 'org-clock-start-time) org-clock-start-time)
    (let* ((hd (and (boundp 'org-clock-hd-marker) (markerp org-clock-hd-marker)
                    (marker-buffer org-clock-hd-marker) org-clock-hd-marker))
           (pause (lr-track--current-pause))
           (stream (and hd (lr-track--stream-of-marker hd))))
      (list :task (or (lr-track--clock-task) "the clock")
            :marker hd
            :stream-key (plist-get stream :key)
            :start (float-time org-clock-start-time)
            :end (lr-track--running-line-end)
            :paused-at (plist-get pause :paused-at)
            :why (plist-get pause :why)
            :place (car (lr-track--clock-place hd))))))

(defun lr-track--ask-undo-label (now)
  "What u after y undoes at NOW: the newest undo record's label, when that
write is under `lr-track--ask-undo-recent-seconds' old; else nil."
  (let* ((rec (car lr-track--undo-stack))
         (at (plist-get rec :at)))
    (and rec (numberp at) (numberp now)
         (<= 0.0 (- now at) lr-track--ask-undo-recent-seconds)
         (plist-get rec :label))))

(defun lr-track--ask-context (&optional now)
  "The context every Now? function reads: live state at NOW (default now).
\(:now F :presence P :streams STREAMS :clock CLOCK :last-end F :quiet nil
 :covered SPANS :undo LABEL :time-file FILE).  :undo is what u would undo,
or nil (`lr-track--ask-undo-label'); :time-file is time.org when it exists
but holds no stream.  First the clock is settled at his key
\(`lr-track--settle-at-key'): a probe sample that arrived since the last
tick is stepped and the clock step runs on it, then the sample his key
itself is, at NOW.  He may answer right after a wake, before the tick that
would step it, and the old presence would date his answer and leave the
night on a live clock; or 25 s after unlocking, before any tick saw him
back.  Inside lr-track's own writes, and for the header
\(`lr-track--ask-display-only'), only the probe's sample is stepped."
  (if (or lr-track--internal lr-track--ask-display-only)
      (condition-case err (lr-track--step-sample lr-track--last-sample)
        (error (lr-track--log 'presence err)))
    (lr-track--settle-at-key now))
  (let ((now (or now (float-time)))
        (streams (lr-track--streams)))
    (list :now now
          :presence lr-track--presence
          :streams streams
          :clock (lr-track--ask-clock)
          :last-end lr-track--last-clock-end
          :quiet nil
          :covered lr-track--covered-spans
          :undo (lr-track--ask-undo-label now)
          :time-file (and (null streams)
                          (let ((f (lr-track--time-file)))
                            (and (file-exists-p f) f))))))

;;;; state, start, header, echo (PURE given a context)

(defun lr-track--ask-activity-pause-p (clock)
  "Non-nil when CLOCK is an away-place clock paused by his activity."
  (and (plist-get clock :paused-at) (eq (plist-get clock :why) 'activity)))

(defun lr-track--ask-no-continue-p (clock)
  "Non-nil when CLOCK is paused and has no y y: an away-place stream.
It earns while he is away, so it never continues from his return at the
Mac, whether it paused for his activity there or at its maximum (a line
from his return would pause at its own start).  The digits are the way on:
an away stream's digit starts from now."
  (and (plist-get clock :paused-at)
       (or (lr-track--ask-activity-pause-p clock)
           (eq (plist-get clock :place) 'away))))

(defun lr-track--ask-back-at (ctx)
  "When he came back, for a paused clock of CTX, or nil.
His return.  With none this session, for a pause by his activity or one
where nothing was seen (why `unknown'): the start of his here-block, the
first time presence saw him.  For an activity pause never before the
clock's start: a return before it is not one from this clock."
  (let* ((p (plist-get ctx :presence))
         (clock (plist-get ctx :clock))
         (start (plist-get clock :start))
         (back (or (plist-get p :return)
                   (and (plist-get clock :paused-at)
                        (memq (plist-get clock :why) '(activity unknown))
                        (lr-track--presence-here-since p)))))
    (if (and (numberp back) (numberp start)
             (lr-track--ask-activity-pause-p clock))
        (max back start)
      back)))

(defun lr-track--ask-stayed-p (ctx)
  "Non-nil when CTX's clock paused for his activity and he never left: he was
here since before it started, so its pause is at its start, not a return."
  (let ((since (lr-track--presence-here-since (plist-get ctx :presence)))
        (clock (plist-get ctx :clock)))
    (and (lr-track--ask-activity-pause-p clock)
         (numberp since) (numberp (plist-get clock :start))
         (< since (plist-get clock :start)))))

(defun lr-track--ask-state (ctx)
  "What CTX asks: `nostreams', `paused-back', `open-back', `live' or `calm'.
A clock paused by his activity at the Mac (an away place) is back at once:
that pause is his return, so it can never be after it."
  (let* ((now (plist-get ctx :now))
         (p (plist-get ctx :presence))
         (clock (plist-get ctx :clock))
         (paused (plist-get clock :paused-at))
         (here (eq (plist-get p :mode) 'here))
         (r (lr-track--ask-back-at ctx))
         (since (lr-track--presence-here-since p))
         (last-end (plist-get ctx :last-end)))
    (cond
     ((null (plist-get ctx :streams)) 'nostreams)
     ((and clock paused here (numberp r)
           (or (> r paused) (lr-track--ask-activity-pause-p clock))
           (>= (- now r) lr-track--ask-back-seconds))
      'paused-back)
     ((and (null clock) here (numberp since)
           (>= (- now (max since (or last-end 0.0))) lr-track--ask-open-seconds))
      'open-back)
     ((and clock (not paused)) 'live)
     (t 'calm))))

(defun lr-track--answer-start-why (ctx)
  "(START . WHY): `lr-track--answer-start' and what it is.
WHY is `return', `seen' (the return from a stretch no sample saw, kind
`unseen': nothing says he left), `first' (first input this session),
`break', `now', `last-end' or `paused'."
  (let* ((now (plist-get ctx :now))
         (p (plist-get ctx :presence))
         ;; He is answering, so he is back; presence away only has not stepped
         ;; his return yet.  Its here-since and breaks are then the block
         ;; BEFORE this away, and a start from them would claim the away.
         (gone (lr-track--presence-away-p p))
         (r (and (not gone) (lr-track--presence-here-since p)))
         (floor-at (- now lr-track--answer-window))
         (ends (and (not gone)
                    (seq-filter (lambda (x) (and (numberp x) (>= x floor-at) (<= x now)))
                                (mapcar (lambda (b) (plist-get b :to))
                                        (plist-get p :breaks)))))
         (pick (cond
                ((and (numberp r) (<= r now) (<= (- now r) lr-track--answer-window))
                 (cons r (cond ((not (plist-get p :return)) 'first)
                               ((eq (plist-get (plist-get p :last-away) :kind)
                                    'unseen)
                                'seen)
                               (t 'return))))
                (ends (cons (apply #'min ends) 'break))
                (t (cons now 'now))))
         (last-end (plist-get ctx :last-end))
         (paused (plist-get (plist-get ctx :clock) :paused-at)))
    (when (and (numberp last-end) (> last-end (car pick)))
      (setq pick (cons last-end 'last-end)))
    (when (and (numberp paused) (> paused (car pick)))
      (setq pick (cons paused 'paused)))
    (cons (lr-track--ask-minute (min (car pick) now)) (cdr pick))))

(defun lr-track--answer-start (ctx)
  "The start a now answer in CTX writes, floored to the minute.
His return (or first input) when at most 90 min ago; else the earliest break
end in the last 90 min; else now.  Now, too, while presence still says away
\(his return is not stepped yet).  Never before the last clock's end nor the
paused clock's pause."
  (car (lr-track--answer-start-why ctx)))

(defun lr-track--start-phrase (why)
  "What a start of kind WHY is, in a few words."
  (pcase why
    ('return "your return")
    ('seen "seen again")
    ('first "first input this session")
    ('break "end of a break")
    ('last-end "end of your last clock")
    ('paused "where it paused")
    (_ "now")))

(defun lr-track--ask-keys (ctx)
  "The digits CTX's streams answer to, ascending (each of 1 to 9)."
  (sort (delq nil (mapcar (lambda (s) (let ((k (plist-get s :key)))
                                        (and (integerp k) (<= 1 k 9) k)))
                          (plist-get ctx :streams)))
        #'<))

(defun lr-track--ask-keys-text (keys)
  "KEYS, digits, as the echo names them: 1-9 for a run from 1, else 2 5 7."
  (let ((ks (sort (copy-sequence keys) #'<)))
    (if (and (cdr ks) (equal ks (number-sequence 1 (length ks))))
        (format "1-%d" (length ks))
      (mapconcat #'number-to-string ks " "))))

(defun lr-track--ask-overlap (a-from a-to b-from b-to)
  "Seconds the span A-FROM to A-TO shares with B-FROM to B-TO."
  (max 0.0 (- (min a-to b-to) (max a-from b-from))))

(defun lr-track--ask-away-covered-p (ctx away)
  "Non-nil when a line already books AWAY in CTX, for a minute or more.
That is the running clock's line (to its pause, its end, or now while still
open), and CTX's `:covered' spans: lines that ended this session and aways
already labelled.  A label there would count the time twice."
  (let* ((from (plist-get away :from))
         (to (plist-get away :to))
         (clock (plist-get ctx :clock))
         (start (plist-get clock :start))
         (spans (append (and (numberp start)
                             (list (cons start (or (plist-get clock :paused-at)
                                                   (plist-get clock :end)
                                                   (plist-get ctx :now)))))
                        (plist-get ctx :covered))))
    (seq-some (lambda (sp)
                (and (numberp (car sp)) (numberp (cdr sp))
                     (>= (lr-track--ask-overlap from to (car sp) (cdr sp))
                         60.0)))
              spans)))

(defun lr-track--ask-labelable-away (ctx)
  "CTX's latest completed away when it can be labelled: 15 min or more, and
no line books it yet (`lr-track--ask-away-covered-p').  A stretch no sample
saw (kind `unseen') is not one: nothing says he was away.  Nor is any while
presence still says away: he is answering, so the away he means is the one
not stepped yet, never the one before it."
  (let* ((p (plist-get ctx :presence))
         (a (plist-get p :last-away))
         (from (plist-get a :from))
         (to (plist-get a :to)))
    (and (numberp from) (numberp to)
         (not (lr-track--presence-away-p p))
         (not (eq (plist-get a :kind) 'unseen))
         (>= (- to from) lr-track-presence-away-seconds)
         (not (lr-track--ask-away-covered-p ctx a))
         a)))

(defun lr-track--ask-away-text (away)
  "AWAY as HH:MM to HH:MM (D, KIND)."
  (let ((from (lr-track--ask-minute (plist-get away :from)))
        (to (lr-track--ask-minute (plist-get away :to))))
    (format "%s to %s (%s, %s)" (lr-track--ts-hm from) (lr-track--ts-hm to)
            (lr-track--ask-dur (- to from))
            (lr-track--ask-kind-text (plist-get away :kind)))))

(defun lr-track--ask-paused-why (why)
  "Why a clock paused, as the header says it."
  (pcase why
    ('max "(its maximum)")
    ('activity "(your return)")
    ('unknown "(nothing seen after)")
    (_ "(your last input)")))

(defun lr-track--ask-in-start-minute-p (at start)
  "Non-nil when AT reads as START's minute or earlier, as a CLOCK stamp does.
A line from START ending there is 0:00 (org removes it) or inverted, so it is
cancelled, never stretched to a minute he did not earn.  Floats both."
  (< at (+ (lr-track--ask-minute start) 60.0)))

(defun lr-track--ask-paused-empty-p (clock)
  "Non-nil when CLOCK paused in its own start's minute: its line never earned
a minute (`lr-track--ask-in-start-minute-p').  Ending it there removes the
line, as his own clock-out does, so the echo and the messages say that
instead of a line that stays."
  (let ((paused (plist-get clock :paused-at))
        (start (plist-get clock :start)))
    (and (numberp paused) (numberp start)
         (lr-track--ask-in-start-minute-p paused start))))

(defun lr-track--ask-nostreams-text (ctx)
  "The way on when CTX has no stream: SPC d M without a time.org, the file
itself when it exists but holds none (SPC d M never writes over it)."
  (let ((f (plist-get ctx :time-file)))
    (if f
        (lr-track--nostreams-file-text f)
      "No streams yet: SPC d M sets them up.")))

(defun lr-track--ask-fit-task (task fixed)
  "TASK cut so that FIXED more columns still fit the essential width.
His heading is shown as he wrote it (`lr-track--ask-name')."
  (lr-track--ask-name
   (lr-track--ask-cut (or task "the clock")
                      (max 8 (- lr-track--header-essential-width fixed)))))

(defun lr-track--ask-live-switch-at (ctx)
  "Where a switch ends CTX's live clock, floored to the minute.
Now; but an away-place clock he is back from (presence here) ends at his
return, or at its start when that is later.  It earns his absence only, and
the activity since is held, never earned, unless he leaves again."
  (let* ((now (lr-track--ask-minute (plist-get ctx :now)))
         (clock (plist-get ctx :clock))
         (start (plist-get clock :start))
         (p (plist-get ctx :presence)))
    (if (and (eq (plist-get clock :place) 'away)
             (not (plist-get clock :paused-at))
             (eq (plist-get p :mode) 'here)
             (numberp start))
        (min now (lr-track--ask-minute
                  (max start (or (lr-track--presence-here-since p) start))))
      now)))

(defun lr-track--ask-digit-from (ctx state)
  "Where a digit for a machine or either stream starts in CTX in STATE."
  (if (eq state 'live)
      (lr-track--ask-live-switch-at ctx)
    (lr-track--answer-start ctx)))

(defun lr-track--ask-away-clause (ctx state)
  "The echo's word on away-place digits in CTX in STATE, or nil.
Those streams always start now (I am leaving for this), so when the other
digits start earlier the echo says so: `6 7 9: from 16:41 (now, ...)'."
  (let* ((now (lr-track--ask-minute (plist-get ctx :now)))
         (running (and (eq state 'live)
                       (plist-get (plist-get ctx :clock) :stream-key)))
         (keys (delq nil (mapcar (lambda (s)
                                   (let ((k (plist-get s :key)))
                                     (and (eq (plist-get s :place) 'away)
                                          (integerp k) (<= 1 k 9)
                                          (not (eql k running))
                                          k)))
                                 (plist-get ctx :streams)))))
    (when (and keys (< (lr-track--ask-digit-from ctx state) now))
      (format "%s: from %s (now, for when you step away)"
              (mapconcat #'number-to-string (sort keys #'<) " ")
              (lr-track--ts-hm now)))))

(defun lr-track--header (ctx)
  "The f agenda's header line for CTX: one ASCII line, `Time' first.
Everything before the key hints fits in `lr-track--header-essential-width'."
  (let* ((state (lr-track--ask-state ctx))
         (now (plist-get ctx :now))
         (p (plist-get ctx :presence))
         (clock (plist-get ctx :clock))
         (task (plist-get clock :task))
         (hm #'lr-track--ts-hm)
         (s (lr-track--answer-start ctx))
         (digits (lr-track--ask-keys-text (lr-track--ask-keys ctx)))
         (text
          (pcase state
            ('nostreams
             (if (plist-get ctx :time-file)
                 (format "Time  %s has no streams: add top-level headings with TRACK_KEY 1 to 9"
                         (file-name-nondirectory (plist-get ctx :time-file)))
               "Time  no streams yet: SPC d M sets them up"))
            ('live
             (let* ((start (plist-get clock :start))
                    (d (lr-track--ask-dur (- (or (plist-get clock :end) now) start)))
                    (tail (format " %s since %s" d (funcall hm start))))
               (format "Time  %s%s  |  y: switch"
                       (lr-track--ask-fit-task task (+ 6 (length tail))) tail)))
            ('paused-back
             (let* ((paused (plist-get clock :paused-at))
                    (r (lr-track--ask-back-at ctx))
                    (away (plist-get p :last-away))
                    (d (lr-track--ask-dur
                        (if away
                            (- (lr-track--ask-minute (plist-get away :to))
                               (lr-track--ask-minute (plist-get away :from)))
                          (- r paused))))
                    (tail (cond
                           ;; he never left: no return to name
                           ((lr-track--ask-stayed-p ctx)
                            (format " paused %s (you stayed at the Mac)"
                                    (funcall hm paused)))
                           ;; nothing seen, then presence started or saw him
                           ;; again after a stretch no sample saw: not an away
                           ((and (eq (plist-get clock :why) 'unknown)
                                 (or (null (plist-get p :return))
                                     (eq (plist-get away :kind) 'unseen)))
                            (format " paused %s %s, seen again %s"
                                    (funcall hm paused)
                                    (lr-track--ask-paused-why 'unknown)
                                    (funcall hm r)))
                           (t
                            (format " paused %s %s, back %s after %s away"
                                    (funcall hm paused)
                                    (lr-track--ask-paused-why (plist-get clock :why))
                                    (funcall hm r) d))))
                    (name (lr-track--ask-fit-task task (+ 12 (length tail)))))
               (if (lr-track--ask-no-continue-p clock)
                   ;; an away stream does not continue: no y y
                   (format "Time  Now?  %s%s  |  y then %s: another"
                           name tail digits)
                 (format "Time  Now?  %s%s  |  y y: %s again from %s   y then %s: another"
                         name tail name (funcall hm s) digits))))
            ('open-back
             (let ((away (plist-get p :last-away))
                   (r (plist-get p :return)))
               (cond
                ;; a stretch no sample saw: he was not seen, not away
                ((and away (numberp r) (eq (plist-get away :kind) 'unseen))
                 (format (concat "Time  Now?  seen again %s after %s not seen, nothing running"
                                 "  |  y then %s: what you do now, from %s")
                         (funcall hm r)
                         (lr-track--ask-dur
                          (- (lr-track--ask-minute (plist-get away :to))
                             (lr-track--ask-minute (plist-get away :from))))
                         digits
                         (funcall hm s)))
                ((and away (numberp r))
                 (format (concat "Time  Now?  back %s after %s away (%s), nothing running"
                                 "  |  y then %s: what you do now, from %s%s")
                         (funcall hm r)
                         (lr-track--ask-dur
                          (- (lr-track--ask-minute (plist-get away :to))
                             (lr-track--ask-minute (plist-get away :from))))
                         (lr-track--ask-kind-text (plist-get away :kind))
                         digits
                         (funcall hm s)
                         (if (lr-track--ask-labelable-away ctx) "   y a: the away" "")))
                (t
                 (format (concat "Time  Now?  here since %s, nothing running"
                                 "  |  y then %s: what you do now, from %s")
                         (funcall hm (lr-track--presence-here-since p))
                         digits
                         (funcall hm s))))))
            (_
             (if (plist-get clock :paused-at)
                 (let ((tail (format " paused %s %s"
                                     (funcall hm (plist-get clock :paused-at))
                                     (lr-track--ask-paused-why (plist-get clock :why)))))
                   (format "Time  %s%s  |  y: switch"
                           (lr-track--ask-fit-task task (+ 6 (length tail))) tail))
               "Time  nothing running  |  y: start something")))))
    (propertize text 'face (if (memq state '(paused-back open-back)) 'warning 'shadow))))

(defun lr-track--header-cached ()
  "The header text as last refreshed, ready for `header-line-format'.
A percent sign is doubled, so a task name never reads as a mode-line
construct."
  (let ((h (or lr-track--header-cache "Time")))
    (if (string-search "%" h) (string-replace "%" "%%" h) h)))

(defun lr-track-ask-refresh-header ()
  "Recompute the header from live state; redraw only when its text changed."
  (let ((h (lr-track--header (let ((lr-track--ask-display-only t))
                               (lr-track--ask-context)))))
    (unless (equal h lr-track--header-cache)
      (setq lr-track--header-cache h)
      (force-mode-line-update t))
    h))

(defun lr-track--ask-refresh-quietly (&rest _)
  "Refresh the header from a clock hook; an error is logged, never raised.
Not while org stands a dangling line in for the running clock
\(`lr-track--clock-stood-in-p'): the header would describe that line."
  (unless (lr-track--clock-stood-in-p)
    (condition-case err (lr-track-ask-refresh-header)
      (error (lr-track--log 'header err)))))

(defun lr-track--ask-stream-list (ctx)
  "CTX's streams as the echo names them: 1 avey  2 study ...
Only streams with a key of 1 to 9: no other key can answer."
  (mapconcat (lambda (s) (format "%d %s" (plist-get s :key)
                                 (lr-track--ask-name (plist-get s :name))))
             (seq-filter (lambda (s) (let ((k (plist-get s :key)))
                                       (and (integerp k) (<= 1 k 9))))
                         (plist-get ctx :streams))
             "  "))

(defun lr-track--ask-keys-tail (ctx before)
  "The echo's closing key hints for CTX, on a line that starts with BEFORE.
u names what it would undo, when that write is recent (CTX's :undo, see
`lr-track--ask-undo-label'); no recent write, no u.  Then other keys: not
now.  The undo text is cut so the line fits `lr-track--echo-width'."
  (let ((label (plist-get ctx :undo))
        (rest "other keys: not now"))
    (if (not (stringp label))
        rest
      (let ((room (- lr-track--echo-width (string-width before)
                     (length "u: undo ()    ") (length rest))))
        (format "u: undo (%s)    %s" (lr-track--ask-cut label (max 16 room))
                rest)))))

(defun lr-track--ask-paused-clauses (ctx state)
  "Line 1 of the echo for CTX's paused clock in STATE: y, then the digits.
The task is named in both, cut to `lr-track--echo-task-width' so the line
fits the frame.  A line that paused at its own start never earned a minute:
both say it goes, never that it stays."
  (let* ((clock (plist-get ctx :clock))
         (task (lr-track--ask-name
                (lr-track--ask-cut-words (or (plist-get clock :task) "the clock")
                                         lr-track--echo-task-width)))
         (hm #'lr-track--ts-hm)
         (s (lr-track--answer-start ctx))
         (digits (lr-track--ask-keys-text (lr-track--ask-keys ctx)))
         (start (funcall hm (plist-get clock :start)))
         (paused (funcall hm (plist-get clock :paused-at)))
         (empty (lr-track--ask-paused-empty-p clock)))
    (concat
     (if (and (eq state 'paused-back)
              (not (lr-track--ask-no-continue-p clock)))
         (if empty
             (format "y: %s again from %s (its empty %s line goes)    "
                     task (funcall hm s) start)
           (format "y: %s again from %s (its %s to %s line stays)    "
                   task (funcall hm s) start paused))
       "")
     (if empty
         (format "%s: another stream from %s, the empty %s line of %s goes"
                 digits (funcall hm s) start task)
       (format "%s: another stream from %s, %s stays ended at %s"
               digits (funcall hm s) task paused)))))

(defun lr-track--ask-echo (ctx)
  "The echo `y' shows for CTX: what each key would write, two lines at most.
Every key it names is bound in the one-key map and no other key is.  Every
line fits `lr-track--echo-width'."
  (let* ((state (lr-track--ask-state ctx))
         (clock (plist-get ctx :clock))
         (task (lr-track--ask-name
                (lr-track--ask-cut (or (plist-get clock :task) "the clock") 40)))
         (hm #'lr-track--ts-hm)
         (sw (lr-track--answer-start-why ctx))
         (s (car sw))
         (digits (lr-track--ask-keys-text (lr-track--ask-keys ctx)))
         (away (lr-track--ask-labelable-away ctx))
         (a-part (and away (format "a then %s: the away %s was that" digits
                                   (lr-track--ask-away-text away))))
         (clause (lr-track--ask-away-clause ctx state))
         (lead (mapconcat (lambda (x) (concat x "    "))
                          (delq nil (list clause a-part)) ""))
         (line2 (concat lead (lr-track--ask-keys-tail ctx lead))))
    (pcase state
      ('nostreams (lr-track--ask-nostreams-text ctx))
      ('live
       (let* ((at (lr-track--ask-live-switch-at ctx))
              (main (format "%s: switch now (%s %s to %s, the new one from %s)"
                            digits task (funcall hm (plist-get clock :start))
                            (funcall hm at) (funcall hm at))))
         (if (or a-part clause)
             (concat main "\n" line2)
           (let ((lead1 (concat main "    ")))
             (concat lead1 (lr-track--ask-keys-tail ctx lead1))))))
      ((guard (plist-get clock :paused-at))
       (concat (lr-track--ask-paused-clauses ctx state) "\n" line2))
      (_
       (concat
        (if (eq (cdr sw) 'now)
            (format "Now, from %s:  " (funcall hm s))
          (format "Now, from %s (%s, %s ago):  " (funcall hm s)
                  (lr-track--start-phrase (cdr sw))
                  (lr-track--ask-dur (- (plist-get ctx :now) s))))
        (lr-track--ask-stream-list ctx)
        "\n" line2)))))

;;;; plans (PURE given a context)

(defun lr-track--refuse (text)
  "A plan that writes nothing and says TEXT."
  (list :refuse text))

(defun lr-track--start-op (n from tag text)
  "The operation that clocks into stream N from FROM, with its note."
  (list :start n :from from :tag tag :text (lr-track--note-text tag text)))

(defun lr-track--plan-resume (ctx)
  "The paused clock of CTX again, from the answer start."
  (let* ((clock (plist-get ctx :clock))
         (sw (lr-track--answer-start-why ctx))
         (s (car sw))
         (task (plist-get clock :task))
         (hm #'lr-track--ts-hm))
    (list :ops (list (list :resume :from s :tag "continued"
                           :text (lr-track--note-text
                                  "continued"
                                  (format "from %s, %s" (funcall hm s)
                                          (lr-track--start-phrase (cdr sw))))))
          :message (if (lr-track--ask-paused-empty-p clock)
                       (format "%s again from %s, running. Its empty %s line is removed.  SPC d u undoes."
                               task (funcall hm s) (funcall hm (plist-get clock :start)))
                     (format "%s again from %s, running. Its %s to %s line stays.  SPC d u undoes."
                             task (funcall hm s) (funcall hm (plist-get clock :start))
                             (funcall hm (plist-get clock :paused-at)))))))

(defun lr-track--plan-switch (ctx n)
  "The plan of digit N while CTX's clock is live: it ends, N starts.
Both at now, except after an away-place clock he is back from: that one
ends at his return (`lr-track--ask-live-switch-at') and a machine or either
stream starts there, since the time since is his at the Mac."
  (let* ((stream (lr-track--ask-stream ctx n))
         (name (plist-get stream :name))
         (clock (plist-get ctx :clock))
         (task (plist-get clock :task))
         (now (lr-track--ask-minute (plist-get ctx :now)))
         (at (lr-track--ask-live-switch-at ctx))
         (from (if (eq (plist-get stream :place) 'away) now at))
         (hm #'lr-track--ts-hm))
    (list :ops (list (list :end-at at)
                     (if (= from now)
                         (lr-track--start-op n now "declared"
                                             (format "from %s, switched from %s"
                                                     (funcall hm now)
                                                     (lr-track--ask-task-ascii task)))
                       (lr-track--start-op n from "now"
                                           (format "from %s, back from %s"
                                                   (funcall hm from)
                                                   (lr-track--ask-task-ascii task)))))
          :message (format "%s from %s%s; %s %s to %s.  SPC d u undoes."
                           name (funcall hm from) (if (= from now) " (now)" "")
                           task (funcall hm (plist-get clock :start))
                           (funcall hm at)))))

(defun lr-track--plan-digit (ctx state n)
  "The plan of digit N in CTX, whose state is STATE."
  (let* ((stream (lr-track--ask-stream ctx n))
         (name (plist-get stream :name))
         (clock (plist-get ctx :clock))
         (task (plist-get clock :task))
         (paused (plist-get clock :paused-at))
         (now (lr-track--ask-minute (plist-get ctx :now)))
         (sw (lr-track--answer-start-why ctx))
         (s (car sw))
         (hm #'lr-track--ts-hm)
         (away-place (eq (plist-get stream :place) 'away))
         (from (if away-place now s))
         (start-op (if away-place
                       (lr-track--start-op n now "declared"
                                           (format "from %s, for when you step away"
                                                   (funcall hm now)))
                     (lr-track--start-op n s "now"
                                         (format "from %s, %s" (funcall hm s)
                                                 (lr-track--start-phrase (cdr sw)))))))
    (cond
     ((null stream)
      (lr-track--refuse (format "No stream %d in time.org. Nothing written." n)))
     ((eq state 'live)
      (if (eql n (plist-get clock :stream-key))
          (lr-track--refuse (format "%s is already running (since %s). Nothing written."
                                    task (funcall hm (plist-get clock :start))))
        (lr-track--plan-switch ctx n)))
     (paused
      ;; every digit does what the echo says, its own stream's too: the
      ;; paused line stays ended at P and N runs from the answer start (y is
      ;; the one key that continues the task itself)
      (list :ops (list (list :end-at paused) start-op)
            :message (if (lr-track--ask-paused-empty-p clock)
                         (format "%s from %s, running; the empty %s line of %s is removed.  SPC d u undoes."
                                 name (funcall hm from)
                                 (funcall hm (plist-get clock :start)) task)
                       (format "%s from %s, running; %s stays ended at %s.  SPC d u undoes."
                               name (funcall hm from) task (funcall hm paused)))))
     (clock
      (lr-track--refuse (format "%s is running. Nothing written." task)))
     (t
      (list :ops (list start-op)
            :message (format "%s from %s, running (time.org).  SPC d u undoes."
                             name (funcall hm from)))))))

(defun lr-track--plan-away (ctx n)
  "The plan that labels CTX's latest away as stream N."
  (let ((stream (lr-track--ask-stream ctx n))
        (away (lr-track--ask-labelable-away ctx)))
    (cond
     ((null stream)
      (lr-track--refuse (format "No stream %d in time.org. Nothing written." n)))
     ((null away)
      (lr-track--refuse
       "No away of 15 min or more left to label (none, or a line already has it). Nothing written."))
     (t
      (let* ((from (lr-track--ask-minute (plist-get away :from)))
             (to (lr-track--ask-minute (plist-get away :to)))
             (kind (lr-track--ask-kind-text (plist-get away :kind))))
        (list :ops (list (list :log n :from from :to to :tag "away"
                               :text (lr-track--note-text
                                      "away" (format "%s %s" kind
                                                     (lr-track--ask-dur (- to from))))))
              :message (format "%s %s to %s (%s) written (time.org).  SPC d u undoes."
                               (plist-get stream :name) (lr-track--ts-hm from)
                               (lr-track--ts-hm to) (lr-track--ask-dur (- to from)))))))))

(defun lr-track--answer-plan (ctx key)
  "What KEY writes in CTX: (:refuse STRING) or (:ops OPS :message STRING).
KEY is a digit 1 to 9, `default' (y), (away . N) or `undo'.  OPS, run in
order by `lr-track--execute-plan':
  (:end-at F)                       end the running clock at F
  (:start N :from F :tag T :text X) clock into stream N, running since F
  (:resume :from F :tag T :text X)  end the paused clock at its pause, then
                                    the same heading again since F
  (:log N :from F :to F :tag T :text X)  a closed line under stream N
  (:undo)                           `lr-track-undo'"
  (let ((state (lr-track--ask-state ctx)))
    (cond
     ((eq key 'undo)
      (list :ops (list (list :undo)) :message "Undo the last tracker write."))
     ((eq state 'nostreams)
      (lr-track--refuse (concat (lr-track--ask-nostreams-text ctx) " Nothing written.")))
     ((and (consp key) (eq (car key) 'away) (integerp (cdr key)))
      (lr-track--plan-away ctx (cdr key)))
     ((eq key 'default)
      (cond
       ((not (eq state 'paused-back))
        (lr-track--refuse "Nothing to continue now. Nothing written."))
       ((lr-track--ask-activity-pause-p (plist-get ctx :clock))
        (lr-track--refuse
         (format "%s paused at your return: an away stream does not continue. Nothing written."
                 (plist-get (plist-get ctx :clock) :task))))
       ((lr-track--ask-no-continue-p (plist-get ctx :clock))
        (lr-track--refuse
         (format "%s is an away stream: it does not continue from your return. Nothing written."
                 (plist-get (plist-get ctx :clock) :task))))
       (t (lr-track--plan-resume ctx))))
     ((and (integerp key) (<= 1 key 9))
      (lr-track--plan-digit ctx state key))
     (t (lr-track--refuse "Not an answer. Nothing written.")))))

(defun lr-track--now-plan (ctx n)
  "The plan of SPC d N in CTX: stream N from now, never backdated.
A live away-place clock he is back from ends at his return, not now (see
`lr-track--ask-live-switch-at')."
  (let* ((stream (lr-track--ask-stream ctx n))
         (name (plist-get stream :name))
         (clock (plist-get ctx :clock))
         (task (plist-get clock :task))
         (paused (plist-get clock :paused-at))
         (now (lr-track--ask-minute (plist-get ctx :now)))
         (hm #'lr-track--ts-hm))
    (cond
     ((null (plist-get ctx :streams))
      (lr-track--refuse (lr-track--ask-nostreams-text ctx)))
     ((null stream)
      (lr-track--refuse (format "No stream %d in time.org. Nothing written." n)))
     ((and clock (not paused) (eql n (plist-get clock :stream-key)))
      (lr-track--refuse (format "%s is already running (since %s). Nothing written."
                                task (funcall hm (plist-get clock :start)))))
     ((and clock (not paused))
      (let ((at (lr-track--ask-live-switch-at ctx)))
        (list :ops (list (list :end-at at)
                         (lr-track--start-op n now "declared"
                                             (format "from %s, switched from %s"
                                                     (funcall hm now)
                                                     (lr-track--ask-task-ascii task))))
              :message (format "%s from %s (now); %s %s to %s.  SPC d u undoes."
                               name (funcall hm now) task
                               (funcall hm (plist-get clock :start)) (funcall hm at)))))
     ((lr-track--ask-paused-empty-p clock)
      (let ((start (funcall hm (plist-get clock :start))))
        (list :ops (list (list :end-at paused)
                         (lr-track--start-op n now "declared"
                                             (format "from %s, the empty %s line of %s removed"
                                                     (funcall hm now) start
                                                     (lr-track--ask-task-ascii task))))
              :message (format "%s from %s (now); the empty %s line of %s is removed.  SPC d u undoes."
                               name (funcall hm now) start task))))
     (clock
      (list :ops (list (list :end-at paused)
                       (lr-track--start-op n now "declared"
                                           (format "from %s, %s stayed ended at %s"
                                                   (funcall hm now)
                                                   (lr-track--ask-task-ascii task)
                                                   (funcall hm paused))))
            :message (format "%s from %s (now); %s stays ended at %s.  SPC d u undoes."
                             name (funcall hm now) task (funcall hm paused))))
     (t
      (list :ops (list (lr-track--start-op n now "declared"
                                           (format "from %s, said with SPC d %d"
                                                   (funcall hm now) n)))
            :message (format "%s from %s (now), running.  SPC d u undoes."
                             name (funcall hm now)))))))

(defun lr-track--plan-signature (plan)
  "What PLAN writes, comparable with `equal'; nil when it writes nothing."
  (copy-tree (plist-get plan :ops)))

;;;; execution

(defun lr-track--entry-region (pos)
  "(BEG . END) of the entry whose heading is at or above POS: its heading
line to the next heading of any level."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (unless (looking-at "\\*+ ")
      (re-search-backward "^\\*+ " nil 'move))
    (let ((beg (point)))
      (end-of-line)
      (cons beg (if (re-search-forward "^\\*+ " nil t)
                    (match-beginning 0)
                  (point-max))))))

(defun lr-track--text-diff (old new)
  "(OFFSET OLD-MID NEW-MID): how NEW differs from OLD.
The common prefix (backed up to a line start) and the common suffix are cut;
OFFSET is where both middles start."
  (let* ((c (compare-strings old nil nil new nil nil))
         (p (if (eq c t) (length old) (1- (abs c))))
         (nl (and (> p 0) (cl-position ?\n old :end p :from-end t)))
         (p (if nl (1+ nl) 0))
         (lo (length old))
         (ln (length new))
         (cap (- (min lo ln) p))
         (s 0))
    (while (and (< s cap) (eq (aref old (- lo s 1)) (aref new (- ln s 1))))
      (setq s (1+ s)))
    (list p (substring old p (- lo s)) (substring new p (- ln s)))))

(defun lr-track--replace-action (beg old)
  "The undo action for the entry starting at BEG, which read OLD before.
It puts OLD's text back where the entry now differs from it."
  (let* ((region (lr-track--entry-region beg))
         (new (buffer-substring-no-properties (car region) (cdr region)))
         (d (lr-track--text-diff old new))
         (from (+ (car region) (nth 0 d)))
         (b (copy-marker from t))
         (e (copy-marker (+ from (length (nth 2 d))))))
    (list :type 'replace :buffer (current-buffer) :beg b :end e
          :new (nth 2 d) :old (nth 1 d))))

(defun lr-track--ask-clock-in (name marker from note)
  "Clock into NAME's heading at MARKER, its line running since FROM, with the
NOTE line right below it.  Return (UNDO-ACTION . BUFFER)."
  (when (lr-track--clocking-p)
    (error "%s is still running" (lr-track--clock-task)))
  (lr-track--clock-task-from (cons name marker) from)
  (unless (and (lr-track--clocking-p)
               (markerp org-clock-marker) (marker-buffer org-clock-marker)
               (= (float-time org-clock-start-time) from))
    (error "Clocking into %s did not take" name))
  (let ((buf (marker-buffer org-clock-marker))
        (stamp (lr-track--ts (seconds-to-time from)))
        (line nil))
    (with-current-buffer buf
      (save-excursion
        (save-restriction
          (widen)
          (goto-char org-clock-marker)
          (beginning-of-line)
          (unless (looking-at (concat "\\([ \t]*\\)CLOCK: " (regexp-quote stamp)))
            (error "The running line of %s is not where org keeps it" name))
          (setq line (concat (match-string 1) note))
          (end-of-line)
          (insert "\n" line))))
    (cons (list :type 'cancel :buffer buf :start from :stamp stamp :note line
                :name name)
          buf)))

(defun lr-track--note-under-running-line (text)
  "Put the note TEXT on its own line right below the running clock's line,
with that line's indentation."
  (with-current-buffer (marker-buffer org-clock-marker)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char org-clock-marker)
        (beginning-of-line)
        (looking-at "[ \t]*")
        (let ((indent (match-string 0)))
          (end-of-line)
          (insert "\n" indent text))))))

(defun lr-track--op-start (n from tag text)
  "Run (:start N :from FROM :tag TAG :text TEXT); return (ACTION . BUFFER)."
  (let ((stream (lr-track--stream-checked n)))
    (lr-track--ask-clock-in (plist-get stream :name) (plist-get stream :marker)
                            from (lr-track--note-line tag text))))

(defun lr-track--op-end-at (at)
  "Run (:end-at AT): end the running clock at AT; return (ACTION . BUFFER).
The action clocks the task in again from its start, its old end and pause
restored, after deleting the closed line by exact text.  When AT is in the
start's minute or before it (`lr-track--ask-in-start-minute-p'), the line is
cancelled instead, as his own clock-out removes a 0:00 line, and the `- lr'
note under it goes first (the action puts it back): a note must not outlive
its line."
  (unless (lr-track--clocking-p) (error "No clock runs to end"))
  (lr-track--sync-running-start)
  (let* ((buf (marker-buffer org-clock-marker))
         (hd (copy-marker org-clock-hd-marker))
         (name (or (lr-track--clock-task) "the task"))
         (start (float-time org-clock-start-time))
         (stamp (lr-track--ts org-clock-start-time))
         (end (lr-track--running-line-end))
         (pause (lr-track--current-pause))
         (bol (with-current-buffer buf
                (save-excursion
                  (save-restriction
                    (widen)
                    (goto-char org-clock-marker)
                    (copy-marker (line-beginning-position))))))
         ;; the cancel path of `lr-track--autoout', which takes an AT at or
         ;; before the start (one later in its minute would be stretched to
         ;; a whole minute): the note goes first, so a LOGBOOK left empty
         ;; goes with the line, as org cancels it
         (empty (lr-track--ask-in-start-minute-p at start))
         (note (and empty (lr-track--note-below-running-line)))
         (note-text (and note (lr-track--delete-note-line note)
                         (string-trim-left (car note))))
         (r (lr-track--autoout (if empty (min at start) at) 'answer))
         (line nil))
    (when (or (not r) (eq (plist-get r :action) 'failed) (lr-track--clocking-p))
      (set-marker bol nil)
      (when (and note-text (lr-track--clocking-p))
        (lr-track--note-under-running-line note-text))
      (error "Could not end %s at %s" name (lr-track--ts-hm at)))
    (when (and (eq (plist-get r :action) 'clock-out)
               (not (plist-get r :line-removed)))
      (with-current-buffer buf
        (save-excursion
          (save-restriction
            (widen)
            (goto-char bol)
            (when (looking-at (concat "[ \t]*CLOCK: " (regexp-quote stamp) "--.*$"))
              (setq line (match-string-no-properties 0)))))))
    (unless line (set-marker bol nil))
    (cons (list :type 'reopen :hd hd :name name :start start :end end
                :pause pause :bol (and line bol) :line line
                :note note-text :ended-at (plist-get r :at))
          buf)))

(defun lr-track--op-resume (from tag text)
  "Run (:resume :from FROM ...): end the paused clock at its pause, then its
heading again from FROM.  Return (ACTION . BUFFERS)."
  (unless (lr-track--clocking-p) (error "No paused clock to continue"))
  (lr-track--sync-running-start)
  (let* ((paused (or (lr-track--paused-at) (error "The clock is not paused")))
         (start (float-time org-clock-start-time))
         (hd (copy-marker org-clock-hd-marker))
         (name (or (lr-track--clock-task) "the task"))
         (buf (marker-buffer org-clock-marker))
         ;; paused in its start's minute, the line never earned a minute and
         ;; the end cancels it (an AT at the start takes `lr-track--autoout's
         ;; cancel): its `- lr' note goes first, as `lr-track--op-end-at'
         ;; does, so no note outlives its line
         (empty (lr-track--ask-in-start-minute-p paused start))
         (note (and empty (lr-track--note-below-running-line)))
         (note-text (and note (lr-track--delete-note-line note)
                         (string-trim-left (car note))))
         (r (lr-track--autoout (if empty (min paused start) paused) 'answer)))
    (when (or (not r) (eq (plist-get r :action) 'failed) (lr-track--clocking-p))
      (when (and note-text (lr-track--clocking-p))
        (lr-track--note-under-running-line note-text))
      (error "Could not end %s at its pause" name))
    (let ((res (lr-track--ask-clock-in name hd from (lr-track--note-line tag text))))
      (set-marker hd nil)
      ;; undo removes the new line only: the old one stays ended at its
      ;; pause, or stays removed when it was empty
      (cons (append (car res)
                    (list :label
                          (if empty
                              (format "%s from %s removed; its empty %s line stays removed"
                                      name (lr-track--ts-hm from)
                                      (lr-track--ts-hm start))
                            (format "%s from %s removed; it stays ended at %s"
                                    name (lr-track--ts-hm from)
                                    (lr-track--ts-hm paused)))))
            (list buf (cdr res))))))

(defun lr-track--op-log (n from to tag text)
  "Run (:log N :from FROM :to TO ...): a closed line and its note under
stream N.  Return (ACTION . BUFFER)."
  (let* ((stream (lr-track--stream-checked n))
         (m (plist-get stream :marker))
         (buf (marker-buffer m))
         (name (plist-get stream :name))
         (re (concat "^\\([ \t]*\\)CLOCK: "
                     (regexp-quote (lr-track--ts (seconds-to-time from))) "--"
                     (regexp-quote (lr-track--ts (seconds-to-time to))) ".*$")))
    (with-current-buffer buf
      (save-excursion
        (save-restriction
          (widen)
          (let* ((region (lr-track--entry-region m))
                 (old (buffer-substring-no-properties (car region) (cdr region))))
            (lr-track--log-task-interval (cons name m) from to)
            ;; the new line is inside the changed part of the entry
            (let* ((region2 (lr-track--entry-region m))
                   (d (lr-track--text-diff
                       old (buffer-substring-no-properties (car region2) (cdr region2)))))
              (goto-char (+ (car region2) (nth 0 d)))
              (unless (re-search-forward re (cdr region2) t)
                (error "The closed line for %s is missing" name))
              (beginning-of-line)
              ;; the drawer's own indentation, as an indented LOGBOOK has it
              (let ((indent (match-string 1)))
                (when (and (equal indent "")
                           (save-excursion
                             (forward-line -1)
                             (looking-at "\\([ \t]+\\):LOGBOOK:[ \t]*$")))
                  (setq indent (match-string 1))
                  (insert indent))
                (end-of-line)
                (insert "\n" indent (lr-track--note-line tag text))))
            (lr-track--cover-span from to)
            (cons (append (lr-track--replace-action (car region) old)
                          (list :label (format "%s %s to %s removed" name
                                               (lr-track--ts-hm from)
                                               (lr-track--ts-hm to))
                                :span (cons from to)))
                  buf)))))))

(defun lr-track--execute-op (op)
  "Run plan operation OP; return (ACTION . BUFFER-OR-BUFFERS)."
  (pcase (car op)
    (:end-at (lr-track--op-end-at (nth 1 op)))
    (:start (let ((p (cddr op)))
              (lr-track--op-start (nth 1 op) (plist-get p :from)
                                  (plist-get p :tag) (plist-get p :text))))
    (:resume (let ((p (cdr op)))
               (lr-track--op-resume (plist-get p :from) (plist-get p :tag)
                                    (plist-get p :text))))
    (:log (let ((p (cddr op)))
            (lr-track--op-log (nth 1 op) (plist-get p :from) (plist-get p :to)
                              (plist-get p :tag) (plist-get p :text))))
    (_ (error "Unknown operation %S" (car op)))))

(defun lr-track--action-label (a)
  "What undoing action A does, in a few words."
  (or (plist-get a :label)
      (pcase (plist-get a :type)
        ('cancel (format "%s from %s removed" (plist-get a :name)
                         (lr-track--ts-hm (plist-get a :start))))
        ('reopen (format "%s runs again from %s" (plist-get a :name)
                         (lr-track--ts-hm (plist-get a :start))))
        (_ "a write removed"))))

(defun lr-track--save-touched (buffers)
  "Save every live buffer in BUFFERS through `lr-track--save-buffer'."
  (dolist (b (delete-dups (delq nil (flatten-tree buffers))))
    (when (buffer-live-p b)
      (condition-case err (lr-track--save-buffer b)
        (error (lr-track--log 'ask-save err))))))

(defmacro lr-track--with-answer-globals (&rest body)
  "Run BODY as an answer writes: pristine org globals, no clock resolution,
no TODO state change, lr-track's own clock-outs, and quiet inner messages.
`org-clock-in-resume' is nil: with it (Doom sets it) a clock-in takes over an
open line left in the entry, rewrites its start and undo then deletes it."
  (declare (indent 0) (debug t))
  `(lr-track--with-pristine-org-globals
     (let ((org-clock-auto-clock-resolution nil)
           (org-clock-in-resume nil)
           (org-clock-in-switch-to-state nil)
           (org-clock-leftover-time nil)
           (lr-track--internal t)
           (inhibit-message t)
           (message-log-max nil))
       ,@body)))

(defun lr-track--push-undo (record)
  "Push RECORD on the undo stack, dropping (and freeing) the oldest past 10."
  (push record lr-track--undo-stack)
  (when (> (length lr-track--undo-stack) lr-track--undo-max)
    (dolist (old (nthcdr lr-track--undo-max lr-track--undo-stack))
      (lr-track--undo-release old))
    (setq lr-track--undo-stack (seq-take lr-track--undo-stack lr-track--undo-max))))

(defun lr-track--execute-plan (plan)
  "Write PLAN: run its ops in order, notes included, save what they touched,
push ONE undo record for all of it, and echo its message (not logged).
The here-block he answered in is held (`lr-track--hold-here-block').
A refusal is only echoed.  A failing op stops the plan; what was written
before it stays, and is undoable."
  (let ((ops (plist-get plan :ops)))
    (cond
     ((plist-get plan :refuse) (lr-track--ask-say (plist-get plan :refuse)))
     ((null ops) (lr-track--ask-say "Nothing to write."))
     ((equal ops '((:undo))) (lr-track-undo))
     (t
      (require 'org-clock)
      (let ((actions nil) (touched nil) (err nil)
            (last-end lr-track--last-clock-end))
        (lr-track--with-answer-globals
          (condition-case e
              (progn
                ;; every stream the plan writes to is checked first, so a
                ;; time.org that changed under it stops the plan before its
                ;; first write, not halfway
                (dolist (op ops)
                  (when (memq (car op) '(:start :log))
                    (lr-track--stream-checked (nth 1 op))))
                (dolist (op ops)
                  (let ((r (lr-track--execute-op op)))
                    (push (car r) actions)
                    (push (cdr r) touched))))
            (error (setq err e))))
        (lr-track--save-touched touched)
        (when actions
          ;; he answered in this here-block, a label too: it is no glance,
          ;; so it never merges into the aways around it and takes his
          ;; label's away with it (`lr-track--hold-here-block')
          (lr-track--hold-here-block)
          ;; an end this plan wrote must not outlive its undo: the next
          ;; answer start is never before `lr-track--last-clock-end'
          (lr-track--push-undo
           (list :actions actions
                 :at (float-time)
                 :label (mapconcat #'lr-track--action-label actions "; ")
                 :last-end-before last-end
                 :last-end-after lr-track--last-clock-end)))
        (lr-track--ask-refresh-quietly)
        (if err
            (progn
              (lr-track--log 'execute err)
              (lr-track--ask-say
               (format "Stopped: %s.%s" (error-message-string err)
                       (if actions "  What was written: SPC d u undoes it." ""))))
          (lr-track--ask-say (plist-get plan :message))))))))

;;;; undo

(defun lr-track--undo-release (record)
  "Free the markers RECORD holds."
  (dolist (a (plist-get record :actions))
    (dolist (k '(:beg :end :hd :bol))
      (let ((m (plist-get a k)))
        (when (markerp m) (set-marker m nil))))))

(defun lr-track--undo-line-at (bol)
  "The text of the line starting at marker BOL, or nil when it is gone."
  (when (and (markerp bol) (buffer-live-p (marker-buffer bol)))
    (with-current-buffer (marker-buffer bol)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char bol)
          (and (bolp)
               (buffer-substring-no-properties (point) (line-end-position))))))))

(defun lr-track--undo-cancel-ok (a)
  "Non-nil when cancel action A still finds its running line and note."
  (and (lr-track--clocking-p)
       (markerp org-clock-marker)
       (eq (marker-buffer org-clock-marker) (plist-get a :buffer))
       (= (float-time org-clock-start-time) (plist-get a :start))
       (with-current-buffer (plist-get a :buffer)
         (save-excursion
           (save-restriction
             (widen)
             (goto-char org-clock-marker)
             (beginning-of-line)
             (and (looking-at (concat "[ \t]*CLOCK: " (regexp-quote (plist-get a :stamp))))
                  (= 0 (forward-line 1))
                  (equal (plist-get a :note)
                         (buffer-substring-no-properties (point) (line-end-position)))))))))

(defun lr-track--undo-refusal (record)
  "Why RECORD cannot be undone as it stands, or nil when it can."
  (let ((cancels (seq-filter (lambda (a) (eq (plist-get a :type) 'cancel))
                             (plist-get record :actions))))
    (catch 'why
      (dolist (a (plist-get record :actions))
        (pcase (plist-get a :type)
          ('replace
           (let ((b (plist-get a :beg)) (e (plist-get a :end)))
             (unless (and (buffer-live-p (plist-get a :buffer))
                          (markerp b) (marker-buffer b) (markerp e) (marker-buffer e)
                          (<= b e)
                          (equal (plist-get a :new)
                                 (with-current-buffer (plist-get a :buffer)
                                   (save-restriction
                                     (widen)
                                     (buffer-substring-no-properties b e)))))
               (throw 'why "the lines it wrote were changed since"))))
          ('cancel
           (unless (lr-track--undo-cancel-ok a)
             (throw 'why (format "%s from %s is no longer the running clock as written"
                                 (plist-get a :name)
                                 (lr-track--ts-hm (plist-get a :start))))))
          ('reopen
           (unless (and (markerp (plist-get a :hd)) (marker-buffer (plist-get a :hd)))
             (throw 'why (format "%s is gone" (plist-get a :name))))
           (when (and (plist-get a :line)
                      (not (equal (plist-get a :line)
                                  (lr-track--undo-line-at (plist-get a :bol)))))
             (throw 'why (format "the ended line of %s was changed since"
                                 (plist-get a :name))))
           (when (and (lr-track--clocking-p)
                      (not (seq-some (lambda (c)
                                       (= (plist-get c :start)
                                          (float-time org-clock-start-time)))
                                     cancels)))
             (throw 'why (format "another clock runs (%s)" (lr-track--clock-task)))))))
      nil)))

(defconst lr-track--open-clock-re
  "^[ \t]*CLOCK: \\[[^]\n]*\\][ \t]*$"
  "A CLOCK line with a start and no end: the shape org resumes.")

(defun lr-track--undo-reopen-in-place (a)
  "Run reopen action A's line again where it stands; non-nil when it runs.
Its closed line, still at A's :bol as written, goes back to its open form,
CLOCK: [START], and org resumes that very line (`org-clock-in-resume'): it
keeps its place in the LOGBOOK and the note under it, so write then undo
gives the bytes back.  nil, with nothing changed, when there is no such
line, or when another open line in the entry is the one org would resume."
  (let ((bol (plist-get a :bol))
        (line (plist-get a :line))
        (hd (plist-get a :hd)))
    (when (and (markerp bol) (buffer-live-p (marker-buffer bol))
               (markerp hd) (eq (marker-buffer hd) (marker-buffer bol))
               (stringp line)
               (string-match "\\`[ \t]*CLOCK: \\[[^]\n]*\\]" line))
      (let ((open (match-string 0 line))
            (ready nil))
        (with-current-buffer (marker-buffer bol)
          (save-excursion
            (save-restriction
              (widen)
              (let ((region (lr-track--entry-region hd)))
                (goto-char (car region))
                (when (and (<= (car region) bol (cdr region))
                           (not (re-search-forward lr-track--open-clock-re
                                                   (cdr region) t)))
                  (goto-char bol)
                  (when (looking-at (concat (regexp-quote line) "$"))
                    (delete-region (+ (point) (length open)) (line-end-position))
                    (setq ready t)))))))
        (when ready
          (let ((org-clock-in-resume t))
            (with-current-buffer (marker-buffer hd)
              (save-excursion
                (save-restriction
                  (widen)
                  (goto-char hd)
                  (org-clock-in)))))
          (unless (and (lr-track--clocking-p)
                       (eq (marker-buffer org-clock-marker) (marker-buffer bol))
                       (= (marker-position bol)
                          (with-current-buffer (marker-buffer bol)
                            (save-excursion
                              (save-restriction
                                (widen)
                                (goto-char org-clock-marker)
                                (line-beginning-position))))))
            (error "%s did not resume its own line" (plist-get a :name)))
          ;; the stamp holds the minute; the clock keeps its own start
          (setq org-clock-start-time (seconds-to-time (plist-get a :start)))
          t)))))

(defun lr-track--undo-apply (a)
  "Apply undo action A; return the buffers it touched."
  (pcase (plist-get a :type)
    ('replace
     (with-current-buffer (plist-get a :buffer)
       (save-excursion
         (save-restriction
           (widen)
           (let ((b (marker-position (plist-get a :beg))))
             (delete-region b (plist-get a :end))
             (goto-char b)
             (insert (plist-get a :old))))))
     ;; the label is gone, so its away may be labelled again
     (when (plist-get a :span)
       (setq lr-track--covered-spans
             (delete (plist-get a :span) lr-track--covered-spans)))
     (list (plist-get a :buffer)))
    ('cancel
     (with-current-buffer (plist-get a :buffer)
       (save-excursion
         (save-restriction
           (widen)
           (goto-char org-clock-marker)
           ;; the note line first, so an emptied drawer goes with the clock
           (delete-region (line-end-position)
                          (save-excursion (forward-line 1) (line-end-position))))))
     (org-clock-cancel)
     (list (plist-get a :buffer)))
    ('reopen
     (let ((bol (plist-get a :bol))
           (start (plist-get a :start))
           (pause (plist-get a :pause)))
       (unless (lr-track--undo-reopen-in-place a)
         (when (and (plist-get a :line) (markerp bol))
           (with-current-buffer (marker-buffer bol)
             (save-excursion
               (save-restriction
                 (widen)
                 (goto-char bol)
                 (delete-region (point) (min (point-max) (1+ (line-end-position))))))))
         (lr-track--clock-task-from (cons (plist-get a :name) (plist-get a :hd)) start))
       (unless (and (lr-track--clocking-p) (= (float-time org-clock-start-time) start))
         (error "%s did not start again" (plist-get a :name)))
       ;; the note the cancel took with the line comes back under it
       (when (plist-get a :note)
         (lr-track--note-under-running-line (plist-get a :note)))
       (let ((end (plist-get a :end)))
         (when (and (numberp end) (> end start))
           (lr-track--advance-clock-line end)))
       (when pause
         (setq lr-track--clock-pause
               (list :start start :paused-at (plist-get pause :paused-at)
                     :why (plist-get pause :why)
                     :end (lr-track--running-line-end))))
       (list (marker-buffer org-clock-marker))))))

(defun lr-track-undo ()
  "Undo the last tracker write (SPC d u, or u after y), by exact text.
A line it wrote is removed, a clock it started is cancelled, a clock it ended
runs again from its own start.  When he changed what it wrote, it refuses and
changes nothing."
  (interactive)
  (let ((rec (car lr-track--undo-stack)))
    (if (null rec)
        (lr-track--ask-say "Nothing to undo: no tracker write left this session.")
      (setq lr-track--undo-stack (cdr lr-track--undo-stack))
      (let ((why (lr-track--undo-refusal rec)))
        (if why
            (progn
              (lr-track--undo-release rec)
              (lr-track--ask-say (format "Undo refused: %s. Nothing changed." why)))
          (require 'org-clock)
          (let ((touched nil) (err nil))
            (lr-track--with-answer-globals
              (condition-case e
                  (dolist (a (plist-get rec :actions))
                    (push (lr-track--undo-apply a) touched))
                (error (setq err e))))
            (lr-track--save-touched touched)
            (lr-track--undo-release rec)
            (when (and (not err)
                       (equal lr-track--last-clock-end (plist-get rec :last-end-after)))
              (setq lr-track--last-clock-end (plist-get rec :last-end-before)))
            (lr-track--ask-refresh-quietly)
            (if err
                (progn (lr-track--log 'undo err)
                       (lr-track--ask-say (format "Undo stopped: %s."
                                                  (error-message-string err))))
              (lr-track--ask-say (format "Undone: %s." (plist-get rec :label))))))))))

(defun lr-track--cover-span (from to)
  "Remember that a line books FROM to TO (floats), newest first."
  (when (and (numberp from) (numberp to) (> to from))
    (push (cons from to) lr-track--covered-spans)
    (setq lr-track--covered-spans
          (seq-take lr-track--covered-spans lr-track--covered-max))))

(defun lr-track--ask-on-clock-out ()
  "`org-clock-out-hook': the line that just ended books its span.
A line org removed as zero time books nothing."
  (condition-case err
      (when (and (boundp 'org-clock-start-time) org-clock-start-time
                 (boundp 'org-clock-out-time) org-clock-out-time
                 (not (bound-and-true-p org-clock-out-removed-last-clock)))
        (lr-track--cover-span (float-time org-clock-start-time)
                              (float-time org-clock-out-time)))
    (error (lr-track--log 'cover err))))

;;;; SPC d N

(defun lr-track--now (n)
  "I am doing stream N now: it runs from now; a running other clock ends now."
  (lr-track--execute-plan (lr-track--now-plan (lr-track--ask-context) n)))

(defmacro lr-track--define-now-commands ()
  "Define `lr-track-now-1' to `lr-track-now-9'."
  `(progn
     ,@(mapcar
        (lambda (n)
          `(defun ,(intern (format "lr-track-now-%d" n)) ()
             ,(format "I am doing stream %d now (SPC d %d).
It runs from now, never backdated; a running other stream ends now." n n)
             (interactive)
             (lr-track--now ,n)))
        (number-sequence 1 9))))

(lr-track--define-now-commands)

;;;; y: the question and its one-key maps

(defun lr-track--ask-refusal ()
  "Why the one-key map must not go up now, or a key of it answer, or nil.
Typing is evil's insert, replace or emacs state: there a digit is text."
  (cond
   ((and (boundp 'evil-state) (memq evil-state '(insert replace emacs)))
    "you are typing")
   ((or executing-kbd-macro defining-kbd-macro) "a keyboard macro is running")
   ((or (minibufferp) (active-minibuffer-window)) "a minibuffer is open")
   ((not (lr-track--frame-focused-now-p)) "no Emacs frame has focus")))

(defun lr-track--ask-signatures (ctx)
  "Each answer key of CTX with its plan signature, as an alist."
  (mapcar (lambda (k) (cons k (lr-track--plan-signature (lr-track--answer-plan ctx k))))
          (append (number-sequence 1 9) '(default)
                  (mapcar (lambda (n) (cons 'away n)) (number-sequence 1 9)))))

(defun lr-track--ask-answer (key shown now)
  "Answer KEY: recompute its plan at NOW, the time the echo was shown; write
it only when it is the plan SHOWN had for KEY, else refuse (I18)."
  (let* ((plan (lr-track--answer-plan (lr-track--ask-context now) key))
         (was (cdr (assoc key (cdr shown)))))
    (cond
     ((not (equal (lr-track--plan-signature plan) was))
      (lr-track--ask-say
       (concat "Changed since shown: a clock or your presence moved, so that key"
               " would write something else. Nothing written; y again.")))
     ((plist-get plan :refuse) (lr-track--ask-say (plist-get plan :refuse)))
     (t (lr-track--execute-plan plan)))))

(defun lr-track--ask-note-echo ()
  "`echo-area-clear-hook': note whether the key comes while our echo shows.
Emacs runs it when the read of a key clears the echo area, before the key is
looked up, and `current-message' still reads what he saw.  The filter of a
one-key binding runs later, once the key has cleared it, so it cannot ask
itself.  When the echo area was already empty the hook does not run, and
the mark stays as `lr-track-ask' left it: nil."
  (setq lr-track--ask-echo-seen
        (and lr-track--ask-echo-shown
             (equal (current-message) lr-track--ask-echo-shown)
             t)))

(defun lr-track--ask-here-p ()
  "Non-nil when a key now is meant for the one-key map.
The key came while the echo area showed the echo naming it
\(`lr-track--ask-echo-seen'), the selected window and its buffer are where
the echo was shown, evil is in the state it was shown in, and nothing that
refuses `lr-track-ask' holds (typing, a macro, a minibuffer, no focus).  A
timer's or a process's message over the echo, a timer that selects another
window, a server or a frame switch that shows another buffer, or a hook that
puts evil in another state: the key there is his text or his command."
  (pcase-let ((`(,win ,buf ,state) lr-track--ask-where))
    (and lr-track--ask-echo-seen
         (window-live-p win)
         (eq (selected-window) win)
         (eq (window-buffer win) buf)
         (eq (current-buffer) buf)
         (eq (bound-and-true-p evil-state) state)
         (not (lr-track--ask-refusal)))))

(defun lr-track--ask-key (cmd)
  "CMD as a one-key map binding that answers only where the echo was shown.
Anywhere else the key is not ours (`lr-track--ask-here-p'): the lookup goes
on to the key's own binding, so it runs as he meant it, and the map closes
as it does for any other key."
  (list 'menu-item "" cmd
        :filter (lambda (c) (and (lr-track--ask-here-p) c))))

(defun lr-track--ask-where-now ()
  "Where a one-key map goes up now, for `lr-track--ask-where'."
  (list (selected-window) (current-buffer) (bound-and-true-p evil-state)))

(defun lr-track--ask-bind-digits (map fn keys)
  "Bind each digit N of KEYS in MAP, its Arabic twins too, to (FN N).
KEYS are the streams' keys: a digit with no stream is never taken, so it
leaves the map and runs as it would."
  (dolist (n keys)
    (when (and (integerp n) (<= 1 n 9))
      (let ((cmd (lr-track--ask-key (lambda () (interactive) (funcall fn n)))))
        (define-key map (vector (+ ?0 n)) cmd)
        (dolist (zero lr-track--ar-digit-zeros)
          (define-key map (vector (+ zero n)) cmd))))))

(defun lr-track--ask-keymap (ctx shown now)
  "The one-key map for CTX: exactly the keys its echo names.
SHOWN and NOW are what each answer is checked against."
  (let ((map (make-sparse-keymap))
        (default (cdr (assq 'default (cdr shown)))))
    (lr-track--ask-bind-digits map (lambda (n) (lr-track--ask-answer n shown now))
                               (lr-track--ask-keys ctx))
    (when (and default (eq (car shown) 'paused-back))
      (let ((cmd (lr-track--ask-key
                  (lambda () (interactive) (lr-track--ask-answer 'default shown now)))))
        (define-key map "y" cmd)
        (define-key map (vector lr-track--ar-ghain) cmd)))
    (when (lr-track--ask-labelable-away ctx)
      (let ((cmd (lr-track--ask-key
                  (lambda () (interactive) (lr-track--ask-away-prompt shown now)))))
        (define-key map "a" cmd)
        (define-key map (vector lr-track--ar-sheen) cmd)))
    ;; u only while the echo names what it undoes (a recent write)
    (let ((label (plist-get ctx :undo)))
      (when (stringp label)
        (let ((cmd (lr-track--ask-key
                    (lambda () (interactive) (lr-track--ask-undo label)))))
          (define-key map "u" cmd)
          (define-key map (vector lr-track--ar-ain) cmd))))
    map))

(defun lr-track--ask-undo (label)
  "u after y: undo the write the echo named as LABEL, and only that one.
Refuses when the newest undo record is no longer it (I18)."
  (if (equal (plist-get (car lr-track--undo-stack) :label) label)
      (lr-track-undo)
    (lr-track--ask-say
     (concat "Changed since shown: the last tracker write is another one now."
             " Nothing undone; y again."))))

(defun lr-track--ask-on-exit ()
  "A one-key map closed: forget it, unless a newer one of ours is up.
Its echo goes too when it is still showing: after the timeout or focus loss
it would go on naming keys that now run their own commands.  (Any other key
replaces the echo anyway.)"
  (unless (and lr-track--ask-map
               (memq lr-track--ask-map overriding-terminal-local-map))
    (when (and lr-track--ask-echo-shown
               (equal (current-message) lr-track--ask-echo-shown))
      (let ((message-log-max nil)) (message nil)))
    (setq lr-track--ask-exit-fn nil
          lr-track--ask-shown nil
          lr-track--ask-map nil
          lr-track--ask-echo-shown nil
          lr-track--ask-echo-seen nil
          lr-track--ask-where nil)))

(defun lr-track--ask-focus-change (&rest _)
  "`after-focus-change-function': focus loss closes a one-key map of ours.
So does focus on another Emacs frame: Emacs keeps focus, but the frame the
echo was shown in lost it, or is no longer the selected one.  Installed at
load, so it holds with `lr-track-mode' off too."
  (when (and (functionp lr-track--ask-exit-fn)
             (or (not (lr-track--frame-focused-now-p))
                 (let* ((w (car lr-track--ask-where))
                        (f (and (window-live-p w) (window-frame w))))
                   (and f (or (not (eq f (selected-frame)))
                              (not (memq (frame-focus-state f)
                                         '(t unknown))))))))
    (condition-case err (funcall lr-track--ask-exit-fn)
      (error (lr-track--log 'ask-exit err)))))

(defun lr-track--ask-exit-on-minibuffer ()
  "`minibuffer-setup-hook': a minibuffer closes any one-key map of ours."
  (when (functionp lr-track--ask-exit-fn)
    (condition-case err (funcall lr-track--ask-exit-fn)
      (error (lr-track--log 'ask-exit err)))))

(defun lr-track--ask-away-prompt (shown now)
  "`a' after y: name the latest away, then take one digit for it.
Only the one-key map of `lr-track-ask' reaches this.  SHOWN and NOW are what
the digit is checked against."
  (let* ((ctx (lr-track--ask-context now))
         (away (lr-track--ask-labelable-away ctx)))
    (if (not away)
        (lr-track--ask-say "No away of 15 min or more to label now. Nothing written.")
      (let ((map (make-sparse-keymap)))
        (lr-track--ask-bind-digits
         map (lambda (n) (lr-track--ask-answer (cons 'away n) shown now))
         (lr-track--ask-keys ctx))
        (setq lr-track--ask-shown shown
              lr-track--ask-where (lr-track--ask-where-now)
              lr-track--ask-echo-shown (format "The away %s was:  %s"
                                               (lr-track--ask-away-text away)
                                               (lr-track--ask-stream-list ctx)))
        (lr-track--ask-say lr-track--ask-echo-shown)
        ;; the next key's read says whether this echo is still the message
        (setq lr-track--ask-echo-seen nil)
        (setq lr-track--ask-map map
              lr-track--ask-exit-fn
              (set-transient-map map nil #'lr-track--ask-on-exit nil
                                 lr-track-ask-timeout))))))

(defun lr-track-ask ()
  "Time: Now?  Show in the echo area what each key would write; the next key
answers (y in the f agenda, SPC d j anywhere).
Digits: what you do now.  y: the default the echo names.  a then a digit:
what the latest away was.  u: undo the write the echo names, offered only
for one under 10 min old (SPC d u reaches older ones).  Any other key, focus
loss, a minibuffer or 15 s idle closes it, writing nothing.  A key answers
only while the echo is still the message on screen, and only in the window,
buffer and evil state it was shown in: otherwise it runs as it would, and
the map closes.  Refused while typing (insert, replace or emacs state), in
a keyboard macro, with a minibuffer open or unfocused."
  (interactive)
  (let ((why (lr-track--ask-refusal)))
    (if why
        (lr-track--ask-say (concat "Not now: " why "."))
      (let* ((ctx (lr-track--ask-context))
             (state (lr-track--ask-state ctx))
             (echo (lr-track--ask-echo ctx)))
        (if (eq state 'nostreams)
            (lr-track--ask-say echo)
          (let* ((now (plist-get ctx :now))
                 (shown (cons state (lr-track--ask-signatures ctx)))
                 (map (lr-track--ask-keymap ctx shown now)))
            (setq lr-track--ask-shown shown
                  lr-track--ask-where (lr-track--ask-where-now)
                  lr-track--ask-echo-shown echo)
            (lr-track--ask-say echo)
            ;; the next key's read says whether this echo is still the message
            (setq lr-track--ask-echo-seen nil)
            (setq lr-track--ask-map map
                  lr-track--ask-exit-fn
                  (set-transient-map map nil #'lr-track--ask-on-exit nil
                                     lr-track-ask-timeout))))))))

;;;; the f agenda: one key, the header line

(defun lr-track--agenda-bind-keys ()
  "Bind y and ghain to `lr-track-ask', teh and noon to what j and k run here.
Through evil's minor-mode keys, in motion and normal state, which beat the
agenda's own maps; nothing else is bound."
  (when (fboundp 'evil-define-minor-mode-key)
    (let ((j (key-binding "j"))
          (k (key-binding "k")))
      (evil-define-minor-mode-key '(motion normal) 'lr-track-agenda-mode
        "y" #'lr-track-ask
        (vector lr-track--ar-ghain) #'lr-track-ask)
      (when (and j (symbolp j) (commandp j))
        (evil-define-minor-mode-key '(motion normal) 'lr-track-agenda-mode
          (vector lr-track--ar-teh) j))
      (when (and k (symbolp k) (commandp k))
        (evil-define-minor-mode-key '(motion normal) 'lr-track-agenda-mode
          (vector lr-track--ar-noon) k)))))

(define-minor-mode lr-track-agenda-mode
  "The f agenda's Now? key: y (and ghain) runs `lr-track-ask'.
Teh and noon run what j and k do there.  Every other key keeps its meaning.
Only ever on in the buffer named by `lr-track--agenda-buffer'."
  :lighter nil
  (if (and lr-track-agenda-mode
           (not (equal (buffer-name) lr-track--agenda-buffer)))
      (setq lr-track-agenda-mode nil)
    (when lr-track-agenda-mode (lr-track--agenda-bind-keys))
    (when (and (fboundp 'evil-normalize-keymaps) (bound-and-true-p evil-local-mode))
      (evil-normalize-keymaps))))

(defun lr-track--agenda-install (buffer)
  "Turn on `lr-track-agenda-mode' and the Now? header line in BUFFER.
The header text is refreshed too: the tick skips that until org is loaded,
so the agenda's first build must not show a stale one.  Contained: an error
installs nothing and is logged, never raised."
  (when (buffer-live-p buffer)
    (condition-case err
        (with-current-buffer buffer
          (unless (bound-and-true-p lr-track-agenda-mode)
            (lr-track-agenda-mode 1))
          (setq-local header-line-format lr-track--header-line-form)
          (lr-track--ask-refresh-quietly))
      (error (lr-track--log 'agenda-install err)))))

(defun lr-track--agenda-finalize ()
  "`org-agenda-finalize-hook': the f agenda gets its key and header."
  (when (equal (buffer-name) lr-track--agenda-buffer)
    (lr-track--agenda-install (current-buffer))))

(defun lr-track--agenda-post-command ()
  "`post-command-hook': after a sticky f agenda rebuild, which skips the
finalize hook and kills local variables, put the key and header back."
  (when (memq this-command lr-track--agenda-sticky-commands)
    (lr-track--agenda-install (get-buffer lr-track--agenda-buffer))))

;;;; SPC under the Arabic layout

(defun lr-track-ask-install-arabic-leader ()
  "Arabic PC aliases of the Time keys in `doom-leader-map'.
SPC yeh teh is SPC d j, SPC yeh DIGIT is SPC d N (both Arabic digit sets),
SPC yeh ain is SPC d u, and SPC khah sheen is whatever SPC o a is now."
  (when (and (boundp 'doom-leader-map) (keymapp doom-leader-map))
    (let* ((yeh lr-track--ar-yeh)
           (agenda (lookup-key doom-leader-map "oa"))
           (binds (append
                   (list (cons (vector yeh lr-track--ar-teh) #'lr-track-ask)
                         (cons (vector yeh lr-track--ar-ain) #'lr-track-undo))
                   (cl-loop for n from 1 to 9
                            append (mapcar (lambda (zero)
                                             (cons (vector yeh (+ zero n))
                                                   (intern (format "lr-track-now-%d" n))))
                                           lr-track--ar-digit-zeros))
                   (and agenda (not (numberp agenda))
                        (list (cons (vector lr-track--ar-khah lr-track--ar-sheen)
                                    agenda))))))
      (dolist (b binds)
        (condition-case err (define-key doom-leader-map (car b) (cdr b))
          (error (lr-track--log 'arabic-leader err)))))))

;;;; installed at load (deploy is by `load', I13; each is idempotent)

(add-hook 'org-agenda-finalize-hook #'lr-track--agenda-finalize)
(add-hook 'post-command-hook #'lr-track--agenda-post-command)
(add-hook 'minibuffer-setup-hook #'lr-track--ask-exit-on-minibuffer)
(add-hook 'echo-area-clear-hook #'lr-track--ask-note-echo)
(add-function :after after-focus-change-function #'lr-track--ask-focus-change)
(add-hook 'org-clock-in-hook #'lr-track--ask-refresh-quietly 90)
(add-hook 'org-clock-out-hook #'lr-track--ask-on-clock-out)
(add-hook 'org-clock-out-hook #'lr-track--ask-refresh-quietly 90)
(add-hook 'org-clock-cancel-hook #'lr-track--ask-refresh-quietly 90)
(if (bound-and-true-p doom-init-time)
    (lr-track-ask-install-arabic-leader)
  (add-hook 'doom-after-init-hook #'lr-track-ask-install-arabic-leader))

(provide 'lr-track-ask)
;;; lr-track-ask.el ends here
