;;; lr-track-ask.el --- the Time questions, the keys and every write -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Deterministic by rule: it pops a question only because HE did something
;; (opened his f agenda, started Emacs, stopped a clock, pressed SPC d j), and
;; every time it writes is NOW, a time he typed, or one of his own boundaries
;; (his last stop, a clock's start, where a clock's line ended when Emacs
;; quit).  Nothing about this laptop is read.
;;
;;   STREAMS    time.org (`lr-track-time-file'), created only by SPC d M: ten
;;              plain headings 0 to 9, 0 being off, each with a TRACK_KEY and a
;;              TRACK_MAX limit.
;;   QUESTIONS  `lr-track-pop' asks, one at a time, what the state calls for:
;;              a clock that ran when Emacs quit, a clock past its limit, one
;;              running overnight or 3 h unconfirmed, nothing running 10 min
;;              since his last stop, and "what now?" right after his stop.  One
;;              key answers; RET skips; any other key ends the questions and
;;              runs as usual; C-g, 120 s or losing focus drops them.
;;   KEYS       SPC d 0-9 (that stream from now; SPC u first: from a typed
;;              time), SPC d x stop, SPC d j ask, SPC d s status, SPC d u undo,
;;              SPC d M setup, y in the f agenda.
;;   WRITES     `lr-track--execute-plan' clocks in backdated, ends clocks and
;;              logs closed lines, each with a `- lr TAG:' note under its CLOCK
;;              line, saves silently and pushes ONE undo record that matches by
;;              exact text (`lr-track-undo').

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
(declare-function org-find-olp "org" (path &optional this-buffer))
(declare-function org-time-string-to-time "org" (s))
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


(defconst lr-track--stream-seeds
  '((0 "off" nil) (1 "avey" "10:00") (2 "study" "10:00") (3 "build" "10:00")
    (4 "writing" "10:00") (5 "reading" "8:00") (6 "practice" "4:00")
    (7 "life" "4:00") (8 "leisure" "8:00") (9 "sleep" "16:00"))
  "The streams SPC d M creates, as (KEY NAME MAX).  0 is off: time you did not
track, logged so it is never asked about again; it has no limit.")

(defvar-local lr-track--setup-file nil
  "The file the *Time setup* preview creates.")

(defun lr-track--time-file-text ()
  "The text SPC d M writes: the preamble and one heading per seed."
  (concat
   "#+title: Time\n"
   "#+startup: overview\n"
   "# Managed by lr-track. Streams are plain headings (no TODO keyword), so this\n"
   "# file stays out of the agenda. 0 off is time you did not track. TRACK_MAX is\n"
   "# a stream's limit: a clock running longer asks when it ended.\n"
   "\n"
   (mapconcat
    (lambda (s)
      (concat (format "* %s\n:PROPERTIES:\n:TRACK_KEY:   %d\n" (nth 1 s) (nth 0 s))
              (if (nth 2 s) (format ":TRACK_MAX:   %s\n" (nth 2 s)) "")
              ":END:\n"))
    lr-track--stream-seeds "\n")))

(defun lr-track--setup-text (file)
  "The preview of creating FILE with the seeds."
  (concat
   "Time setup   nothing is written until RET; q cancels\n"
   (format "  create %s with %d streams (no TODO keywords, so it stays out of the agenda):\n"
           (abbreviate-file-name file) (length lr-track--stream-seeds))
   "    "
   (mapconcat (lambda (s) (format "%d %s" (nth 0 s) (nth 1 s)))
              lr-track--stream-seeds "   ")
   "\n"
   "  0 off is time you did not track.  Limits: avey, study, build, writing 10h;\n"
   "  reading, leisure 8h; practice, life 4h; sleep 16h (edit TRACK_MAX in the file)\n"
   "  your old buckets in life.org stay where they are\n"
   "  RET create   q cancel\n"))

(defcustom lr-track-time-file "~/roam/main/time.org"
  "Org file of the streams: top-level headings carrying a numeric TRACK_KEY.
Only `lr-track-setup' creates it.  Answers add CLOCK lines and notes under its
streams; nothing else here ever writes it."
  :type 'file :group 'lr-track)

(defconst lr-track--note-max 70
  "Longest note line, `- lr TAG: ' included.")

(defconst lr-track--undo-max 10
  "Undo records kept; the oldest beyond this can no longer be undone.")

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

(defvar lr-track--streams-cache nil
  "(FILE MODTIME TICK STREAMS): the streams last read, by file modification
time and the visiting buffer's `buffer-chars-modified-tick'.")

(defvar lr-track--header-cache nil
  "The header text, written by `lr-track-ask-refresh-header' only.")

(defvar lr-track--undo-stack nil
  "Undo records, newest first, at most `lr-track--undo-max'.")

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

(defun lr-track--time-file ()
  "`lr-track-time-file', expanded."
  (expand-file-name lr-track-time-file))

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
Keys 0 to 9 are streams (0 is off), since only those have a key to answer with,
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
                  (if (or (not (<= 0 key 9))
                          (seq-find (lambda (x) (eql (plist-get x :key) key))
                                    streams))
                      (push (cons key pos) skipped)
                    (let* ((m (copy-marker pos)))
                      (push (list :key key
                                  :name (save-excursion
                                          (goto-char pos)
                                          (substring-no-properties
                                           (org-get-heading t t t t)))
                                  :max (unless (zerop key) (lr-track--heading-limit m))
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
Each is (:key N :name S :max SECS :marker M), one per top-level heading
with a TRACK_KEY of 0 to 9; max is its TRACK_MAX in seconds (none for 0).
The file is visited with `find-file-noselect', never displayed, and only
when it exists.  Cached until its modification time or
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
  (format "%s has no streams (top-level headings with TRACK_KEY 0 to 9): add them there."
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
      ;; the record starts now: no question about time before the setup
      (lr-track--set-last-stop (lr-track--ask-minute (float-time)) t)
      (lr-track--ask-say (format "Created %s with %d streams. The agenda now asks what you did when something is open."
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
         (pause nil)
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
            (lr-track--set-last-stop to)
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
    (:resume-olp (let ((p (cdr op)))
                   (lr-track--op-resume-olp (plist-get p :file) (plist-get p :olp)
                                            (plist-get p :name) (plist-get p :from)
                                            (plist-get p :tag) (plist-get p :text))))
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
           (start (plist-get a :start)))
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
              (lr-track--set-last-stop (plist-get rec :last-end-before) t))
            (lr-track--ask-refresh-quietly)
            (if err
                (progn (lr-track--log 'undo err)
                       (lr-track--ask-say (format "Undo stopped: %s."
                                                  (error-message-string err))))
              (lr-track--ask-say (format "Undone: %s." (plist-get rec :label))))))))))

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
  "The f agenda's Time key: y (and ghain) runs `lr-track-ask'.
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

(defun lr-track-ask-install-arabic-leader ()
  "Arabic PC aliases of the Time keys in `doom-leader-map'.
SPC yeh teh is SPC d j, SPC yeh DIGIT is SPC d N (both Arabic digit sets, 0 to
9), SPC yeh ain is SPC d u, and SPC khah sheen is whatever SPC o a is now."
  (when (and (boundp 'doom-leader-map) (keymapp doom-leader-map))
    (let* ((yeh lr-track--ar-yeh)
           (agenda (lookup-key doom-leader-map "oa"))
           (binds (append
                   (list (cons (vector yeh lr-track--ar-teh) #'lr-track-ask)
                         (cons (vector yeh lr-track--ar-ain) #'lr-track-undo))
                   (cl-loop for n from 0 to 9
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

(defun lr-track--header-cached ()
  "The header text as last refreshed, ready for `header-line-format'.
A percent sign is doubled, so a task name never reads as a mode-line
construct."
  (let ((h (or lr-track--header-cache "Time")))
    (if (string-search "%" h) (string-replace "%" "%%" h) h)))

(defun lr-track-ask-refresh-header ()
  "Recompute the header from live state; redraw only when its text changed."
  (let ((h (lr-track--header (lr-track--pop-context 'header))))
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


;;;; the questions (deterministic)
;;
;; Every question is asked because HE did something: opened the agenda, started
;; Emacs, stopped a clock, or pressed SPC d j.  Every time written is NOW (to the
;; minute), a time he typed, or one of his own boundaries: his last stop, a
;; clock's start, where a clock's line ended when Emacs quit.  Nothing about this
;; laptop is read; focus is used only to DROP a question nobody is looking at.

(defconst lr-track-gap-seconds 600.0
  "Nothing running this long since his last stop pops \"what was it?\".")

(defconst lr-track-stale-seconds 10800.0
  "A clock running this long since its start or his last yes pops \"still?\".")

(defconst lr-track-day-start-hour 5
  "The day starts at 05:00: a clock from before today's 05:00 is overnight.")

(defconst lr-track-pop-timeout 120
  "Seconds a question waits for a key before it is dropped, nothing written.")

(defconst lr-track--pop-max-questions 6
  "At most this many questions in one go.")

(defconst lr-track--his-clock-out-commands
  '(org-clock-out org-agenda-clock-out +org/clock-out)
  "His own clock-out commands: after one, \"what now?\" pops.")

(defconst lr-track--ar-seen #x633 "Arabic seen, the s key.")

(defvar lr-track--pop-active nil
  "Non-nil while a question is up, so a question never starts inside one.")

(defvar lr-track--pop-startup-armed nil
  "Non-nil once this session's startup question is scheduled.")

;;;; helpers

(defun lr-track--frame-focused-p ()
  "Non-nil when some frame has focus (`unknown' counts as focused).
Used only to drop or hold back a question, never for anything written."
  (and (seq-some (lambda (f) (memq (frame-focus-state f) '(t unknown))) (frame-list))
       t))

(defun lr-track--day-start (now)
  "Float time of the latest 05:00 (`lr-track-day-start-hour') at or before NOW."
  (let* ((d (decode-time (seconds-to-time now)))
         (at (lambda (day)
               (float-time (encode-time
                            (list 0 0 lr-track-day-start-hour day
                                  (decoded-time-month d) (decoded-time-year d)
                                  nil -1 nil)))))
         (today (funcall at (decoded-time-day d))))
    (if (<= today now) today (funcall at (1- (decoded-time-day d))))))

(defun lr-track--pop-when (time now)
  "TIME as HH:MM, with its weekday when it falls before NOW's day."
  (if (>= time (lr-track--day-start now))
      (lr-track--ts-hm time)
    (format-time-string "%a %H:%M" (seconds-to-time time))))

(defun lr-track--heading-limit (m)
  "Seconds of the TRACK_MAX on or above the heading at marker M, else 10 h."
  (or (ignore-errors
        (with-current-buffer (marker-buffer m)
          (save-excursion
            (save-restriction
              (widen)
              (goto-char m)
              (lr-track--parse-max (org-entry-get (point) "TRACK_MAX" t))))))
      lr-track--clock-default-max))

(defun lr-track--last-stop-from-time-file (now)
  "The latest CLOCK end in time.org at or before NOW, or nil."
  (let ((file (lr-track--time-file)) (best nil))
    (when (file-exists-p file)
      (with-current-buffer (lr-track--time-buffer file)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (while (re-search-forward
                    "^[ \t]*CLOCK: \\[[^]\n]*\\]--\\(\\[[^]\n]*\\]\\)" nil t)
              (let ((e (ignore-errors
                         (float-time (org-time-string-to-time (match-string 1))))))
                (when (and e (<= e now) (or (null best) (> e best)))
                  (setq best e))))))))
    best))

(defun lr-track--last-stop (&optional now)
  "His last stop: `lr-track--last-clock-end', else the latest end in time.org."
  (or lr-track--last-clock-end
      (let ((f (lr-track--last-stop-from-time-file (or now (float-time)))))
        (when f (lr-track--set-last-stop f))
        f)))

;;;; context and situations (PURE given the context)

(defun lr-track--pop-clock ()
  "The running clock as (:task :marker :stream-key :start :limit), or nil.
The off stream (key 0) has no limit."
  (when (and (lr-track--clocking-p)
             (not (lr-track--clock-stood-in-p))
             (markerp org-clock-hd-marker)
             (buffer-live-p (marker-buffer org-clock-hd-marker))
             (boundp 'org-clock-start-time) org-clock-start-time)
    (lr-track--sync-running-start)
    (let* ((m org-clock-hd-marker)
           (stream (ignore-errors (lr-track--stream-of-marker m)))
           (key (plist-get stream :key)))
      (list :task (or (lr-track--clock-task) "the task")
            :marker m
            :stream-key key
            :start (float-time org-clock-start-time)
            :limit (unless (eql key 0) (lr-track--heading-limit m))))))

(defun lr-track--pop-state ()
  "The state.eld plist, read once."
  (or lr-track--state (setq lr-track--state (lr-track--read-state))))

(defun lr-track--pop-context (&optional trigger now)
  "Everything a question is computed from, read once, at NOW."
  (let* ((now (or now (float-time)))
         (state (lr-track--pop-state))
         (clock (lr-track--pop-clock))
         (running (plist-get state :running)))
    (list :now now
          :trigger trigger
          :streams (ignore-errors (lr-track--streams))
          :clock clock
          :last-stop (lr-track--last-stop now)
          :confirmed (plist-get state :confirmed)
          :restarted (and (not clock) running
                          (numberp (plist-get running :at)) running))))

(defun lr-track--pop-situations (ctx)
  "The questions CTX calls for, in the order they are asked.
restarted, then one of limit / overnight / stale for a running clock, then
stopped (only right after his clock-out) or gap."
  (let* ((now (plist-get ctx :now))
         (clock (plist-get ctx :clock))
         (last (plist-get ctx :last-stop))
         (confirmed (plist-get ctx :confirmed))
         (restarted (plist-get ctx :restarted))
         (out nil))
    (when (plist-get ctx :streams)
      (when restarted (push (list :kind 'restarted) out))
      (when clock
        (let* ((start (plist-get clock :start))
               (limit (plist-get clock :limit))
               (day (lr-track--day-start now))
               (since (max start (or confirmed start))))
          (cond
           ((and limit (> (- now start) limit)
                 (not (and confirmed (>= confirmed (+ start limit)))))
            (push (list :kind 'limit) out))
           ((and (< start day) (not (and confirmed (>= confirmed day))))
            (push (list :kind 'overnight) out))
           ((>= (- now since) lr-track-stale-seconds)
            (push (list :kind 'stale :since since) out)))))
      (when (and (not clock) (not restarted) last)
        (cond ((eq (plist-get ctx :trigger) 'stopped)
               (push (list :kind 'stopped) out))
              ((>= (- now last) lr-track-gap-seconds)
               (push (list :kind 'gap) out)))))
    (nreverse out)))

;;;; plans (PURE given the context)

(defun lr-track--pop-note (tag fmt &rest args)
  "A note body for TAG: FMT and ARGS, ASCII, cut so the line fits."
  (lr-track--note-text tag (apply #'format fmt args)))

(defun lr-track--plan-start-from (ctx n from tag why)
  "Stream N running from FROM; a running clock ends at FROM first."
  (let* ((stream (seq-find (lambda (s) (eql (plist-get s :key) n))
                           (plist-get ctx :streams)))
         (clock (plist-get ctx :clock))
         (now (plist-get ctx :now)))
    (cond
     ((null stream) (list :refuse (format "No stream %d in time.org. Nothing written." n)))
     ((and clock (eql (plist-get clock :stream-key) n))
      (list :refuse (format "%s is already running (since %s). Nothing written."
                            (plist-get clock :task)
                            (lr-track--pop-when (plist-get clock :start) now))))
     ((and clock (<= from (plist-get clock :start)))
      (list :refuse (format "%s is before %s started (%s). Nothing written."
                            (lr-track--ts-hm from) (plist-get clock :task)
                            (lr-track--ts-hm (plist-get clock :start)))))
     (t
      (list :ops (append
                  (and clock (list (list :end-at from)))
                  (list (list :start n :from from :tag tag
                              :text (lr-track--pop-note tag "from %s, %s"
                                                        (lr-track--ts-hm from) why))))
            :message (format "%s from %s, running%s.  SPC d u undoes."
                             (plist-get stream :name) (lr-track--pop-when from now)
                             (if clock
                                 (format "; %s ended at %s" (plist-get clock :task)
                                         (lr-track--ts-hm from))
                               "")))))))

(defun lr-track--plan-stop-at (ctx at)
  "End the running clock at AT."
  (let ((clock (plist-get ctx :clock)))
    (cond
     ((null clock) (list :refuse "Nothing is running. Nothing written."))
     ((<= at (plist-get clock :start))
      (list :refuse (format "%s is not after %s started (%s). Nothing written."
                            (lr-track--ts-hm at) (plist-get clock :task)
                            (lr-track--ts-hm (plist-get clock :start)))))
     (t (list :ops (list (list :end-at at))
              :message (format "%s ended at %s.  SPC d u undoes."
                               (plist-get clock :task)
                               (lr-track--pop-when at (plist-get ctx :now))))))))

(defun lr-track--plan-log (ctx n from to tag)
  "A closed line FROM to TO under stream N."
  (let ((stream (seq-find (lambda (s) (eql (plist-get s :key) n))
                          (plist-get ctx :streams))))
    (if (null stream)
        (list :refuse (format "No stream %d in time.org. Nothing written." n))
      (list :ops (list (list :log n :from from :to to :tag tag
                             :text (lr-track--pop-note tag "%s to %s"
                                                       (lr-track--ts-hm from)
                                                       (lr-track--ts-hm to))))
            :message (format "%s %s to %s written.  SPC d u undoes."
                             (plist-get stream :name) (lr-track--ts-hm from)
                             (lr-track--ts-hm to))))))

(defun lr-track--plan-resume (ctx running)
  "RUNNING (from state.eld) again, from where its line ended when Emacs quit."
  (let ((at (plist-get running :at)))
    (list :ops (list (list :resume-olp :file (plist-get running :file)
                           :olp (plist-get running :olp)
                           :name (plist-get running :task) :from at :tag "resumed"
                           :text (lr-track--pop-note "resumed" "from %s, Emacs quit then"
                                                     (lr-track--ts-hm at))))
          :message (format "%s again from %s, running.  SPC d u undoes."
                           (plist-get running :task)
                           (lr-track--pop-when at (plist-get ctx :now))))))

;;;; the resume of a clock Emacs quit with

(defun lr-track--ask-in-start-minute-p (at start)
  "Non-nil when AT reads as START's minute or earlier, as a CLOCK stamp does.
A line from START ending there is 0:00 (org removes it), so it is cancelled,
never stretched to a minute he did not earn.  Floats both."
  (< at (+ (lr-track--ask-minute start) 60.0)))

(defun lr-track--op-resume-olp (file olp name from tag text)
  "Run (:resume-olp ...): clock into the heading at OLP in FILE, its line
running since FROM, with the note under it.  Return (ACTION . BUFFER)."
  (let ((m (or (and file olp (file-exists-p file)
                    (ignore-errors (org-find-olp (cons file olp))))
               (error "Could not find %s in %s" name (or file "its file")))))
    (prog1 (lr-track--ask-clock-in name m from (lr-track--note-line tag text))
      (set-marker m nil))))

;;;; reading his answer

(defun lr-track--pop-latin (ev)
  "EV with Arabic digits and the Arabic letters on y and s read as Latin."
  (cond
   ((not (integerp ev)) ev)
   ((<= #x660 ev #x669) (+ ?0 (- ev #x660)))
   ((<= #x6f0 ev #x6f9) (+ ?0 (- ev #x6f0)))
   ((= ev lr-track--ar-ghain) ?y)
   ((= ev lr-track--ar-seen) ?s)
   (t ev)))

(defun lr-track--pop-latin-digits (s)
  "S with Arabic digits as Latin digits."
  (apply #'string (mapcar #'lr-track--pop-latin (string-to-list s))))

(defun lr-track--pop-read-key (prompt choices)
  "Show PROMPT; return the character from CHOICES he pressed, or `later' for
RET.  Any other key ends the questions and runs as it always does (nothing is
swallowed).  C-g, 120 s without a key, or Emacs losing focus signal `quit'."
  (let* ((ev (with-timeout (lr-track-pop-timeout 'lr-track-pop-drop)
               (read-key (propertize prompt 'face 'minibuffer-prompt))))
         (ch (lr-track--pop-latin ev)))
    (cond
     ((memq ev '(lr-track-pop-drop)) (signal 'quit nil))
     ((eql ch ?\C-g) (signal 'quit nil))
     ((memq ch '(?\r ?\n return)) 'later)
     ((memq ch choices) ch)
     ;; back at the FRONT: it was pressed before anything still queued
     (t (push ev unread-command-events)
        (throw 'lr-track-pop-done 'other)))))

(defun lr-track--pop-read-time (prompt parse)
  "Ask PROMPT in the minibuffer and return the float PARSE makes of the answer
\(floored to the minute), or `default' for an empty answer.  Re-asks on an
answer PARSE rejects.  C-g, 120 s, or Emacs losing focus signal `quit'."
  (let ((note nil))
    (catch 'got
      (while t
        (let* ((raw (with-timeout (lr-track-pop-timeout (signal 'quit nil))
                      (read-string (concat prompt (if note (format "[%s] " note) "")))))
               (s (lr-track--pop-latin-digits (string-trim raw))))
          (if (string-empty-p s)
              (throw 'got 'default)
            (let ((tm (funcall parse s)))
              (if (numberp tm)
                  (throw 'got (lr-track--ask-minute tm))
                (setq note (format "not a time in range: %s" s))))))))))

(defun lr-track--pop-parse-ago (anchor now)
  "A parser for \"since when\": a clock time after ANCHOR, or a duration back
from NOW, landing in (ANCHOR, NOW]."
  (lambda (s)
    (let ((s (downcase s)))
      (cond
       ((member s '("now" "n")) now)
       ((lr-track--parse-clock s)
        (let ((hm (lr-track--parse-clock s)))
          (lr-track--clock-on-day (car hm) (cdr hm) anchor now)))
       (t (let ((mins (lr-track--duration-minutes s)))
            (and mins (> mins 0)
                 (let ((tm (- now (* 60.0 mins)))) (and (> tm anchor) tm)))))))))

(defun lr-track--pop-parse-after (anchor now)
  "A parser for \"until when\": a clock time after ANCHOR, or how long it
lasted from ANCHOR, landing in (ANCHOR, NOW]."
  (lambda (s)
    (let ((tm (lr-track--parse-when s anchor now)))
      (and (numberp tm) (> tm anchor) (<= tm now) tm))))

(defun lr-track--pop-focus-change (&rest _)
  "`after-focus-change-function': drop a question nobody is looking at.
Nothing is written for it; the same trigger asks again next time."
  (when (and lr-track--pop-active (not (lr-track--frame-focused-p)))
    (if (> (minibuffer-depth) 0)
        (run-at-time 0 nil (lambda ()
                             (when (and lr-track--pop-active (> (minibuffer-depth) 0)
                                        (not (lr-track--frame-focused-p)))
                               (abort-recursive-edit))))
      (setq unread-command-events
            (append unread-command-events (list 'lr-track-pop-drop))))))

;;;; one question each

(defun lr-track--pop-streams-text (ctx)
  "\"0 off  1 avey  ...\" from CTX's streams."
  (mapconcat (lambda (s) (format "%d %s" (plist-get s :key)
                                 (lr-track--ask-cut (plist-get s :name) 10)))
             (plist-get ctx :streams) "  "))

(defun lr-track--pop-digits (ctx)
  "The digit characters of CTX's streams."
  (mapcar (lambda (s) (+ ?0 (plist-get s :key))) (plist-get ctx :streams)))

(defun lr-track--pop-confirm (ctx)
  "His yes: the running clock is still right, as of now (state only)."
  (lr-track--state-put :confirmed (plist-get ctx :now))
  (lr-track--ask-say (format "OK: %s still running." (plist-get (plist-get ctx :clock) :task))))

(defun lr-track--pop-ask-restarted (ctx head)
  "The clock that ran when Emacs quit: resume it, another, or off."
  (let* ((r (plist-get ctx :restarted))
         (at (plist-get r :at))
         (k (lr-track--pop-read-key
             (format "%s%s was running when Emacs quit at %s.\ny: %s again from %s   or from %s:  %s   RET later "
                     head (plist-get r :task) (lr-track--pop-when at (plist-get ctx :now))
                     (plist-get r :task) (lr-track--ts-hm at) (lr-track--ts-hm at)
                     (lr-track--pop-streams-text ctx))
             (cons ?y (lr-track--pop-digits ctx)))))
    (unless (eq k 'later)
      (let ((plan (if (eql k ?y)
                      (lr-track--plan-resume ctx r)
                    (lr-track--plan-start-from ctx (- k ?0) at "restarted"
                                               "Emacs quit then"))))
        (lr-track--execute-plan plan)
        (unless (plist-get plan :refuse) (lr-track--state-put :running (lr-track--running-info)))))
    k))

(defun lr-track--pop-ask-limit (ctx head)
  "A clock past its limit: when did it end?  RET keeps it running."
  (let* ((c (plist-get ctx :clock))
         (now (plist-get ctx :now))
         (start (plist-get c :start))
         (at (lr-track--pop-read-time
              (format "%s%s has run %s since %s, past its %s limit.\nWhen did it end? (a time like 23:30, or how long it ran like 2h; RET: still running) "
                      head (plist-get c :task) (lr-track--ask-dur (- now start))
                      (lr-track--pop-when start now) (lr-track--ask-dur (plist-get c :limit)))
              (lr-track--pop-parse-after start now))))
    (if (eq at 'default)
        (lr-track--pop-confirm ctx)
      (lr-track--execute-plan (lr-track--plan-stop-at ctx at)))
    at))

(defun lr-track--pop-ask-still (ctx sit head)
  "Overnight or 3 h unconfirmed: still the same?  y, another, or stopped."
  (let* ((c (plist-get ctx :clock))
         (now (plist-get ctx :now))
         (start (plist-get c :start))
         (k (lr-track--pop-read-key
             (format "%sStill %s since %s%s?\ny: yes   0: stopped   or switched to:  %s   RET later "
                     head (plist-get c :task) (lr-track--pop-when start now)
                     (if (eq (plist-get sit :kind) 'stale)
                         (format " (%s)" (lr-track--ask-dur (- now start)))
                       "")
                     (mapconcat (lambda (s) (format "%d %s" (plist-get s :key)
                                                    (lr-track--ask-cut (plist-get s :name) 10)))
                                (seq-remove (lambda (s) (eql (plist-get s :key) 0))
                                            (plist-get ctx :streams))
                                "  "))
             (cons ?y (lr-track--pop-digits ctx)))))
    (cond
     ((eq k 'later) nil)
     ((or (eql k ?y) (eql (- k ?0) (plist-get c :stream-key))) (lr-track--pop-confirm ctx))
     (t (let ((at (lr-track--pop-read-time
                   (format (if (eql k ?0)
                               "%s stopped when? (a time like 15:00, or how long ago like 30m; RET: now) "
                             "Since when? (a time like 15:00, or how long ago like 30m; RET: now) ")
                           (plist-get c :task))
                   (lr-track--pop-parse-ago start now))))
          (when (eq at 'default) (setq at (lr-track--ask-minute now)))
          (lr-track--execute-plan
           (if (eql k ?0)
               (lr-track--plan-stop-at ctx at)
             (lr-track--plan-start-from ctx (- k ?0) at "since" "typed"))))))
    k))

(defun lr-track--pop-split (ctx)
  "Walk the gap from his last stop with typed times: each part is a closed
line; the last part, ending now, keeps running."
  (let ((from (plist-get ctx :last-stop))
        (done nil))
    (while (not done)
      (let* ((ctx (lr-track--pop-context 'split))
             (now (plist-get ctx :now))
             (k (lr-track--pop-read-key
                 (format "From %s, what first?  %s   RET stop here " (lr-track--pop-when from now)
                         (lr-track--pop-streams-text ctx))
                 (lr-track--pop-digits ctx))))
        (if (eq k 'later)
            (setq done t)
          (let ((to (lr-track--pop-read-time
                     (format "Until when? (a time like 17:30, or how long it lasted like 40m; RET: still on it now) ")
                     (lr-track--pop-parse-after from now))))
            (if (or (eq to 'default) (>= to (lr-track--ask-minute now)))
                (progn
                  (lr-track--execute-plan
                   (lr-track--plan-start-from ctx (- k ?0) from "split" "the last part, still running"))
                  (setq done t))
              (lr-track--execute-plan (lr-track--plan-log ctx (- k ?0) from to "split"))
              (setq from to))))))))

(defun lr-track--pop-ask-gap (ctx head)
  "Nothing running since his last stop: what was it?"
  (let* ((now (plist-get ctx :now))
         (last (plist-get ctx :last-stop))
         (k (lr-track--pop-read-key
             (format "%sNothing logged since %s (%s). What was it, still running?\n%s   s: split it   RET later "
                     head (lr-track--pop-when last now) (lr-track--ask-dur (- now last))
                     (lr-track--pop-streams-text ctx))
             (cons ?s (lr-track--pop-digits ctx)))))
    (cond ((eq k 'later) nil)
          ((eql k ?s) (lr-track--pop-split ctx))
          (t (lr-track--execute-plan
              (lr-track--plan-start-from ctx (- k ?0) (lr-track--ask-minute last) "gap"
                                         (format "answered at %s" (lr-track--ts-hm now))))))
    k))

(defun lr-track--pop-ask-stopped (ctx head)
  "Right after his clock-out: what now?"
  (let* ((now (plist-get ctx :now))
         (last (plist-get ctx :last-stop))
         (k (lr-track--pop-read-key
             (format "%sStopped at %s. What now?  %s   RET later "
                     head (lr-track--pop-when last now) (lr-track--pop-streams-text ctx))
             (lr-track--pop-digits ctx))))
    (unless (eq k 'later)
      (lr-track--execute-plan
       (lr-track--plan-start-from ctx (- k ?0) (lr-track--ask-minute last) "now"
                                  "after your stop")))
    k))

;;;; the questions in one go

(defun lr-track--pop-refusal ()
  "Why no question may start now, or nil."
  (cond (lr-track--pop-active "a question is up")
        ((or executing-kbd-macro defining-kbd-macro) "a keyboard macro runs")
        ((active-minibuffer-window) "the minibuffer is in use")
        ((not (lr-track--frame-focused-p)) "Emacs has no focus")))

(defun lr-track-pop (trigger)
  "Ask, one at a time, every question the current state calls for.
TRIGGER is `agenda', `startup', `stopped' or `ask'.  Each answer is written
before the next question.  RET skips one question; any key that is not a
choice, C-g, 120 s without a key, or Emacs losing focus ends them all.
Returns the number of questions asked."
  (let ((asked 0))
    (unless (lr-track--pop-refusal)
      (let ((lr-track--pop-active t)
            (skipped nil)
            (head ""))
        (condition-case nil
            (catch 'lr-track-pop-done
              (while (< asked lr-track--pop-max-questions)
                (let* ((ctx (lr-track--pop-context trigger))
                       (sit (seq-find (lambda (s) (not (memq (plist-get s :kind) skipped)))
                                      (lr-track--pop-situations ctx))))
                  (unless sit (throw 'lr-track-pop-done nil))
                  (cl-incf asked)
                  (let ((r (pcase (plist-get sit :kind)
                             ('restarted (lr-track--pop-ask-restarted ctx head))
                             ('limit (lr-track--pop-ask-limit ctx head))
                             ((or 'overnight 'stale) (lr-track--pop-ask-still ctx sit head))
                             ('gap (lr-track--pop-ask-gap ctx head))
                             ('stopped (lr-track--pop-ask-stopped ctx head)))))
                    (when (or (eq r 'later) (null r))
                      (push (plist-get sit :kind) skipped))
                    ;; "what now?" is asked once; afterwards the gap rule applies
                    (when (eq trigger 'stopped) (setq trigger 'agenda))
                    (let ((m (current-message)))
                      (setq head (if (and m (not (eq r 'later))) (concat m "\n") "")))))))
          (quit (lr-track--ask-say "Time: question dropped, nothing written for it.")))))
    asked))

;;;; header and status (facts only)

(defun lr-track--header (ctx)
  "The Time line for CTX: what is running, or since when nothing is."
  (let* ((now (plist-get ctx :now))
         (clock (plist-get ctx :clock))
         (last (plist-get ctx :last-stop))
         (body
          (cond
           ((null (plist-get ctx :streams))
            (if (file-exists-p (lr-track--time-file))
                (lr-track--nostreams-file-text (lr-track--time-file))
              "no streams yet: SPC d M sets them up"))
           (clock
            (let* ((start (plist-get clock :start))
                   (limit (plist-get clock :limit))
                   (task (lr-track--ask-name (lr-track--ask-cut (plist-get clock :task) 30))))
              (if (and limit (> (- now start) limit))
                  (format "%s running %s since %s, past its %s limit" task
                          (lr-track--ask-dur (- now start)) (lr-track--pop-when start now)
                          (lr-track--ask-dur limit))
                (format "%s running since %s (%s)" task (lr-track--pop-when start now)
                        (lr-track--ask-dur (- now start))))))
           (last (format "nothing running since %s (%s, your last stop)"
                         (lr-track--pop-when last now) (lr-track--ask-dur (- now last))))
           (t "nothing running"))))
    (concat "Time  " body "  |  SPC d j: ask")))

(defun lr-track-status ()
  "SPC d s: what is running, or since when nothing is, in the echo area."
  (interactive)
  (lr-track--ask-say (lr-track--header (lr-track--pop-context 'ask))))

;;;; commands

(defun lr-track-ask ()
  "SPC d j (and y in the f agenda): ask every question that is due now.
With none due, show the Time line."
  (interactive)
  (let ((why (lr-track--pop-refusal)))
    (cond
     (why (lr-track--ask-say (format "Time: not now (%s)." why)))
     ((null (lr-track--streams))
      (lr-track--ask-say (lr-track--header (lr-track--pop-context 'ask))))
     ((zerop (lr-track-pop 'ask))
      (lr-track--ask-say (lr-track--header (lr-track--pop-context 'ask)))))))

(defun lr-track--declare (n ask)
  "Stream N from now, or from a typed time when ASK; a running clock ends then."
  (let* ((ctx (lr-track--pop-context 'ask))
         (now (lr-track--ask-minute (plist-get ctx :now)))
         (clock (plist-get ctx :clock)))
    (if (null (plist-get ctx :streams))
        (lr-track--ask-say (lr-track--header ctx))
      (let ((at (if (not ask) now
                  (let ((anchor (or (and clock (plist-get clock :start))
                                    (plist-get ctx :last-stop)
                                    (- now 86400.0))))
                    (let ((v (lr-track--pop-read-time
                              "Since when? (a time like 15:00, or how long ago like 30m; RET: now) "
                              (lr-track--pop-parse-ago anchor (plist-get ctx :now)))))
                      (if (eq v 'default) now v))))))
        (lr-track--execute-plan
         (lr-track--plan-start-from ctx n at (if ask "since" "now")
                                    (if ask "typed" "said then")))))))

(defmacro lr-track--define-now-commands ()
  "Define `lr-track-now-0' to `lr-track-now-9'."
  `(progn
     ,@(mapcar
        (lambda (n)
          `(defun ,(intern (format "lr-track-now-%d" n)) (&optional ask)
             ,(format "Stream %d from now (SPC d %d); a running clock ends now.
With a prefix argument (SPC u SPC d %d), from a time you type." n n n)
             (interactive "P")
             (lr-track--declare ,n ask)))
        (number-sequence 0 9))))

(lr-track--define-now-commands)

(defun lr-track-stop (&optional ask)
  "SPC d x: stop the running clock now, or at a typed time with a prefix
argument (SPC u SPC d x); then ask what now."
  (interactive "P")
  (let* ((ctx (lr-track--pop-context 'ask))
         (clock (plist-get ctx :clock))
         (now (lr-track--ask-minute (plist-get ctx :now))))
    (if (null clock)
        (lr-track--ask-say "Nothing is running.")
      (let ((at (if (not ask) now
                  (let ((v (lr-track--pop-read-time
                            (format "%s stopped when? (a time like 15:00, or how long ago like 30m; RET: now) "
                                    (plist-get clock :task))
                            (lr-track--pop-parse-ago (plist-get clock :start)
                                                     (plist-get ctx :now)))))
                    (if (eq v 'default) now v)))))
        (let ((plan (lr-track--plan-stop-at ctx at)))
          (lr-track--execute-plan plan)
          (unless (plist-get plan :refuse)
            (lr-track-pop 'stopped)))))))

;;;; triggers

(defun lr-track--pop-after-agenda (&rest _)
  "After his f agenda shows: the Time line, then the questions due."
  (condition-case err
      (progn
        (lr-track--agenda-install (get-buffer lr-track--agenda-buffer))
        (lr-track-pop 'agenda))
    (error (lr-track--log 'pop-agenda err))))

(defun lr-track--pop-stopped-once ()
  "One-shot `post-command-hook': \"what now?\" after his clock-out command."
  (remove-hook 'post-command-hook #'lr-track--pop-stopped-once)
  (condition-case err (lr-track-pop 'stopped)
    (error (lr-track--log 'pop-stopped err))))

(defun lr-track--pop-on-clock-out ()
  "`org-clock-out-hook': after HIS clock-out (O, SPC c o), ask what now."
  (when (and (not lr-track--internal)
             (not lr-track--pop-active)
             (memq this-command lr-track--his-clock-out-commands))
    (add-hook 'post-command-hook #'lr-track--pop-stopped-once)))

(defun lr-track--pop-startup ()
  "The startup question, once, when someone is looking."
  (if (and (lr-track--frame-focused-p) (not (active-minibuffer-window)))
      (condition-case err (lr-track-pop 'startup)
        (error (lr-track--log 'pop-startup err)))
    (add-function :after after-focus-change-function #'lr-track--pop-startup-on-focus)))

(defun lr-track--pop-startup-on-focus (&rest _)
  "The first focus after a startup nobody saw: ask then, once."
  (when (lr-track--frame-focused-p)
    (remove-function after-focus-change-function #'lr-track--pop-startup-on-focus)
    (run-at-time 0.5 nil #'lr-track--pop-startup)))

(defun lr-track--schedule-startup-pop ()
  "`doom-after-init-hook': the startup question, 4 s after init, once."
  (unless lr-track--pop-startup-armed
    (setq lr-track--pop-startup-armed t)
    (run-with-timer 4 nil #'lr-track--pop-startup)))

;;;; installed at load (deploy is by `load'; each is idempotent)

(add-hook 'org-agenda-finalize-hook #'lr-track--agenda-finalize)
(add-hook 'post-command-hook #'lr-track--agenda-post-command)
(add-hook 'org-clock-in-hook #'lr-track--ask-refresh-quietly 90)
(add-hook 'org-clock-out-hook #'lr-track--ask-refresh-quietly 90)
(add-hook 'org-clock-cancel-hook #'lr-track--ask-refresh-quietly 90)
(add-hook 'org-clock-out-hook #'lr-track--pop-on-clock-out 95)
(add-function :after after-focus-change-function #'lr-track--pop-focus-change)
(dolist (f lr-track--agenda-sticky-commands)
  (advice-add f :after #'lr-track--pop-after-agenda))
(add-hook 'doom-after-init-hook #'lr-track--schedule-startup-pop 90)
(if (bound-and-true-p doom-init-time)
    (lr-track-ask-install-arabic-leader)
  (add-hook 'doom-after-init-hook #'lr-track-ask-install-arabic-leader))

;; Leftovers of the presence-based version, removed at load.  A `load' over a
;; running session keeps what the old file installed; on a fresh start each of
;; these is a no-op.
(remove-hook 'minibuffer-setup-hook 'lr-track--ask-exit-on-minibuffer)
(remove-hook 'echo-area-clear-hook 'lr-track--ask-note-echo)
(remove-hook 'org-clock-out-hook 'lr-track--ask-on-clock-out)
(remove-function after-focus-change-function 'lr-track--ask-focus-change)

(provide 'lr-track-ask)
;;; lr-track-ask.el ends here
