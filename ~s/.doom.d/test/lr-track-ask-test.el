;;; lr-track-ask-test.el --- tests for the Now? question, its plans and keys -*- lexical-binding: t; -*-

;;; Commentary:
;; Stage 1 of lr-track v3, contract sections 5 to 10.  The f agenda's header
;; line asks "Now?" only when something is due; `y' shows in the echo area
;; exactly what each key would write; the next key writes that and nothing else.
;;
;; What these tests pin:
;;   - streams come from time.org, which only SPC d M creates, and only on RET;
;;   - the ask state, the answer start, the header and the echo are PURE
;;     functions of a context plist, so every case builds its context by hand
;;     on a fixed day (Sat 3 Oct 2026) and never reads the wall clock;
;;   - a plan is computed when the echo is shown and again at the answer key;
;;     a difference refuses instead of writing something never shown (I18);
;;   - every write is undoable by exact text, back to the original bytes;
;;   - nothing reads, displays, selects or captures keys outside the three
;;     commands allowed to (a source scan and a replayed day).
;;
;; The one-key map runs through the REAL `set-transient-map', with the command
;; loop simulated by `lr-track-ask-test--press': the key's read runs
;; `echo-area-clear-hook' while the echo area still shows its message, the key
;; is looked up with the map active, then `pre-command-hook' runs (that is where
;; a transient map with no KEEP-PRED exits and calls its ON-EXIT), then the
;; command.  That is the order Emacs uses, so a key handler that only works
;; while ON-EXIT has not run yet fails here exactly as it would live.
;;
;; Writes go to scratch org files in temp directories, never ~/roam.  Run from
;; the repo root, with stdin closed so a stray prompt errors instead of waiting:
;;   emacs -Q --batch -L modules -l test/lr-track-ask-test.el \
;;     -f ert-run-tests-batch-and-exit < /dev/null

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(when (boundp 'native-comp-jit-compilation)
  (setq native-comp-jit-compilation nil))

;; Match the live Emacs: its org and evil, not the ones bundled with Emacs.
(let ((b "/Users/l/.emacs.d/.local/straight/build-31.0.91/"))
  (dolist (p '("org" "evil"))
    (when (file-directory-p (concat b p)) (push (concat b p) load-path))))

(defvar lr-track-ask-test--tmp
  (file-name-as-directory (make-temp-file "lr-track-ask-test" t))
  "Scratch root for this run.")

;; Keep org's caches inside the scratch root.  Set before org loads, so its
;; defcustoms find them already bound.
(defvar org-persist-directory
  (expand-file-name "org-persist/" lr-track-ask-test--tmp))
(defvar org-element-cache-persistent nil)

(require 'org)
(require 'org-clock)
(require 'org-agenda)
(require 'evil nil t)

;; `emacs -Q' has no Doom.  Stub what the modules touch at load time.
(defmacro add-hook! (&rest _) nil)
(defmacro after! (&rest body) `(progn ,@body))
(defmacro defadvice! (&rest _) nil)
(unless (fboundp 'map!) (defmacro map! (&rest _) nil))
(defvar doom-data-dir (expand-file-name "etc/" lr-track-ask-test--tmp))
(defvar doom-cache-dir (expand-file-name "cache/" lr-track-ask-test--tmp))
(make-directory doom-data-dir t)
(make-directory doom-cache-dir t)
(defvar doom-leader-map (make-sparse-keymap)
  "Stub of Doom's leader map; the leader test binds a fresh one.")

(defconst lr-track-ask-test--root
  (locate-dominating-file (or load-file-name buffer-file-name) "modules")
  "The .doom.d checkout this file was loaded from.")

(add-to-list 'load-path (expand-file-name "modules" lr-track-ask-test--root))

;; In the RED step lr-track-presence and lr-track-ask do not exist yet.  A
;; missing or broken module must make each test fail on its own (void-function,
;; void-variable), never stop this file from loading.
(dolist (feature '(lr-track-presence lr-track lr-track-ask))
  (condition-case err
      (require feature)
    (error (message "lr-track-ask-test: %s did not load: %S" feature err))))

(setq org-clock-persist nil
      make-backup-files nil
      create-lockfiles nil
      org-clock-out-remove-zero-time-clocks nil)

;; Module variables the tests let-bind.  Declared here so the bindings are
;; dynamic even before the modules define them.
(defvar lr-track-time-file)
(defvar lr-track--presence)
(defvar lr-track--last-sample)
(defvar lr-track--clock-pause)
(defvar lr-track--last-clock-end)
(defvar lr-track--ask-shown)
(defvar lr-track--ask-exit-fn)
(defvar lr-track--ask-echo-shown)
(defvar lr-track--ask-echo-seen)
(defvar lr-track--last-tick)
(defvar lr-track--interval)
(defvar lr-track--undo-stack)
(defvar lr-track--covered-spans)
(defvar lr-track--presence-stepped)
(defvar lr-track--blind-since)
(defvar lr-track--streams-cache)
(defvar lr-track--internal)
(defvar lr-track--focused-p)
(defvar lr-track--unfocused-since)
(defvar lr-track--phase-failures)
(defvar lr-track-agenda-mode)
(defvar evil-state)

;; A prompt in batch reads stdin: with a terminal attached it would wait
;; forever.  Turn every such read into a loud error instead.  The replay test
;; replaces these with recorders for its own duration.
(dolist (fn '(yes-or-no-p y-or-n-p read-char-exclusive read-char read-char-choice
              read-string read-from-minibuffer completing-read read-key))
  (advice-add fn :override
              (let ((name fn))
                (lambda (&rest _)
                  (error "lr-track-ask-test: unexpected prompt (%s) in batch" name)))
              '((name . lr-track-ask-test--no-prompt))))


;;;; time, without the wall clock

(defun lr-track-ask-test--at (h m &optional s)
  "Float time of H:M:S, local time, on Sat 3 Oct 2026.  Never the wall clock."
  (float-time (encode-time (list (or s 0) m h 3 10 2026 nil -1 nil))))

(defun lr-track-ask-test--minute (f)
  "F floored to the minute, the resolution of a CLOCK stamp."
  (* 60.0 (floor f 60)))

(defun lr-track-ask-test--stamp (f)
  "The inactive org stamp for float time F, as a CLOCK line writes it."
  (format-time-string (org-time-stamp-format t t) (seconds-to-time f)))

(defconst lr-track-ask-test--arrows-re
  (regexp-opt (list (string ?- ?>) (string ?< ?-) (string ?= ?>)))
  "Matches an ASCII arrow; built from characters so this file holds none.")

(defun lr-track-ask-test--ascii-p (s)
  "Non-nil when S is a string of ASCII characters only."
  (and (stringp s) (cl-every (lambda (c) (< c 128)) s)))


;;;; streams and contexts, built by hand

(defconst lr-track-ask-test--seeds
  '((1 "avey" machine "10:00") (2 "study" machine "10:00")
    (3 "build" machine "10:00") (4 "writing" machine "10:00")
    (5 "reading" either "8:00") (6 "practice" away "4:00")
    (7 "life" away "4:00") (8 "leisure" either "8:00")
    (9 "sleep" away "16:00"))
  "Contract section 5 seeds: (KEY NAME PLACE MAX).")

(defconst lr-track-ask-test--stream-list
  "1 avey  2 study  3 build  4 writing  5 reading  6 practice  7 life  8 leisure  9 sleep"
  "The stream list as the echo names it.")

(defun lr-track-ask-test--max-seconds (hmm)
  "Seconds in an \"H:MM\" string HMM."
  (let ((p (split-string hmm ":")))
    (float (* 60 (+ (* 60 (string-to-number (car p)))
                    (string-to-number (cadr p)))))))

(defun lr-track-ask-test--streams ()
  "The seeds as stream plists, the shape `lr-track--streams' returns."
  (mapcar (lambda (s)
            (list :key (nth 0 s) :name (nth 1 s) :place (nth 2 s)
                  :max (lr-track-ask-test--max-seconds (nth 3 s)) :marker nil))
          lr-track-ask-test--seeds))

(defconst lr-track-ask-test--time-preamble
  (concat "#+title: Time\n"
          "#+startup: overview\n"
          "# Managed by lr-track. Streams are plain headings (no TODO keyword), so this file stays out\n"
          "# of the agenda. Edit the properties freely.\n"
          "\n")
  "The head of time.org, contract section 5.")

(defun lr-track-ask-test--time-text (&optional extras)
  "The initial time.org text of contract section 5.
EXTRAS is an alist (NAME . TEXT); TEXT goes right after that stream's
property drawer (an existing LOGBOOK, say)."
  (concat lr-track-ask-test--time-preamble
          (mapconcat
           (lambda (s)
             (concat (format (concat "* %s\n:PROPERTIES:\n:TRACK_KEY:   %d\n"
                                     ":TRACK_PLACE: %s\n:TRACK_MAX:   %s\n:END:\n")
                             (nth 1 s) (nth 0 s) (nth 2 s) (nth 3 s))
                     (or (cdr (assoc (nth 1 s) extras)) "")))
           lr-track-ask-test--seeds "\n")))

(defconst lr-track-ask-test--old-logbook
  ":LOGBOOK:\nCLOCK: [2026-10-01 Thu 09:00]--[2026-10-01 Thu 10:00] =>  1:00\n:END:\n"
  "A LOGBOOK that was there before any tracker write.")

(defconst lr-track-ask-test--indented-logbook
  "  :LOGBOOK:\n  CLOCK: [2026-10-01 Thu 09:00]--[2026-10-01 Thu 10:00] =>  1:00\n  :END:\n"
  "An indented LOGBOOK, as life.org has hundreds of.")

(cl-defun lr-track-ask-test--presence
    (&key (mode 'here) first-time return last-input prev-time
          away-from away-kind last-away breaks)
  "A presence state plist (contract section 3), built by hand."
  (list :mode mode :last-input last-input :prev-time prev-time :wake nil
        :first-time first-time :away-from away-from :away-kind away-kind
        :saw-lock nil :return return :last-away last-away
        :breaks breaks :segments nil))

(defun lr-track-ask-test--away (fh fm th tm kind)
  "A completed away from FH:FM to TH:TM of KIND, on the fixed day."
  (list :from (lr-track-ask-test--at fh fm) :to (lr-track-ask-test--at th tm)
        :kind kind))

(cl-defun lr-track-ask-test--clock
    (&key (task "avey") (key 1) start end paused-at why (place 'machine))
  "A context `:clock' plist (contract section 6)."
  (list :task task :marker nil :stream-key key :start start :end end
        :paused-at paused-at :why why :place place))

(cl-defun lr-track-ask-test--ctx
    (&key now presence (streams (lr-track-ask-test--streams)) clock last-end)
  "A context plist (contract section 6)."
  (list :now now :presence presence :streams streams :clock clock
        :last-end last-end :quiet nil))

(defun lr-track-ask-test--with (plist &rest kvs)
  "A copy of PLIST with KVS put into it."
  (let ((p (copy-sequence plist)))
    (while kvs (setq p (plist-put p (pop kvs) (pop kvs))))
    p))

;; The day, by state.  Every fixture returns fresh lists.

(defun lr-track-ask-test--open-back (&optional kind)
  "Nothing running; back at 16:05 from a 25m away of KIND; now 16:40."
  (let ((at #'lr-track-ask-test--at))
    (lr-track-ask-test--ctx
     :now (funcall at 16 40)
     :presence (lr-track-ask-test--presence
                :first-time (funcall at 8 0) :return (funcall at 16 5)
                :last-input (funcall at 16 39) :prev-time (funcall at 16 39 30)
                :last-away (lr-track-ask-test--away 15 40 16 5 (or kind 'locked))))))

(defun lr-track-ask-test--open-back-no-away ()
  "Nothing running; here since Emacs first saw input at 16:00; now 16:40."
  (let ((at #'lr-track-ask-test--at))
    (lr-track-ask-test--ctx
     :now (funcall at 16 40)
     :presence (lr-track-ask-test--presence
                :first-time (funcall at 16 0) :last-input (funcall at 16 39)
                :prev-time (funcall at 16 39 30)))))

(defun lr-track-ask-test--open-back-later ()
  "The open-back world a little later: away again 16:10 to 16:30; now 16:41."
  (let ((at #'lr-track-ask-test--at))
    (lr-track-ask-test--ctx
     :now (funcall at 16 41)
     :presence (lr-track-ask-test--presence
                :first-time (funcall at 8 0) :return (funcall at 16 30)
                :last-input (funcall at 16 40) :prev-time (funcall at 16 40 30)
                :last-away (lr-track-ask-test--away 16 10 16 30 'locked)))))

(defun lr-track-ask-test--paused-back ()
  "avey 14:10 paused at 15:40 (no input); back 16:05; now 16:40."
  (let ((at #'lr-track-ask-test--at))
    (lr-track-ask-test--ctx
     :now (funcall at 16 40)
     :presence (lr-track-ask-test--presence
                :first-time (funcall at 8 0) :return (funcall at 16 5)
                :last-input (funcall at 16 39) :prev-time (funcall at 16 39 30)
                :last-away (lr-track-ask-test--away 15 40 16 5 'idle))
     :clock (lr-track-ask-test--clock
             :start (funcall at 14 10) :end (funcall at 15 40)
             :paused-at (funcall at 15 40) :why 'idle))))

(defun lr-track-ask-test--live (&optional with-away)
  "avey live since 16:05, its line at 17:17; now 17:20.
WITH-AWAY adds the 15:40 to 16:05 locked away as the latest one."
  (let ((at #'lr-track-ask-test--at))
    (lr-track-ask-test--ctx
     :now (funcall at 17 20)
     :presence (if with-away
                   (lr-track-ask-test--presence
                    :first-time (funcall at 8 0) :return (funcall at 16 5)
                    :last-input (funcall at 17 19) :prev-time (funcall at 17 19 30)
                    :last-away (lr-track-ask-test--away 15 40 16 5 'locked))
                 (lr-track-ask-test--presence
                  :first-time (funcall at 8 0) :last-input (funcall at 17 19)
                  :prev-time (funcall at 17 19 30)))
     :clock (lr-track-ask-test--clock :start (funcall at 16 5)
                                      :end (funcall at 17 17)))))

(defun lr-track-ask-test--calm-paused ()
  "avey paused at 15:40 and he is still away; now 15:58."
  (let ((at #'lr-track-ask-test--at))
    (lr-track-ask-test--ctx
     :now (funcall at 15 58)
     :presence (lr-track-ask-test--presence
                :mode 'away :first-time (funcall at 8 0) :return (funcall at 9 0)
                :last-input (funcall at 15 40) :prev-time (funcall at 15 57 30)
                :away-from (funcall at 15 40) :away-kind 'idle
                :last-away (lr-track-ask-test--away 8 30 9 0 'locked))
     :clock (lr-track-ask-test--clock
             :start (funcall at 14 10) :end (funcall at 15 40)
             :paused-at (funcall at 15 40) :why 'idle))))

(defun lr-track-ask-test--calm-no-clock ()
  "Nothing running, away since 17:50, last return 16:05 (over 90m); now 18:10.
The answer start is NOW here: the return is too old and there is no break."
  (let ((at #'lr-track-ask-test--at))
    (lr-track-ask-test--ctx
     :now (funcall at 18 10)
     :presence (lr-track-ask-test--presence
                :mode 'away :first-time (funcall at 8 0) :return (funcall at 16 5)
                :last-input (funcall at 17 50) :prev-time (funcall at 18 9 30)
                :away-from (funcall at 17 50) :away-kind 'idle
                :last-away (lr-track-ask-test--away 15 40 16 5 'locked)))))

(defun lr-track-ask-test--nostreams ()
  "The open-back world before SPC d M: no streams."
  (lr-track-ask-test--with (lr-track-ask-test--open-back) :streams nil))

(defun lr-track-ask-test--all-fixtures ()
  "Every state fixture, as (LABEL . CTX)."
  (list (cons "nostreams" (lr-track-ask-test--nostreams))
        (cons "live" (lr-track-ask-test--live))
        (cons "live, latest away" (lr-track-ask-test--live t))
        (cons "paused-back" (lr-track-ask-test--paused-back))
        (cons "open-back" (lr-track-ask-test--open-back))
        (cons "open-back, no input" (lr-track-ask-test--open-back 'idle))
        (cons "open-back, asleep" (lr-track-ask-test--open-back 'asleep))
        (cons "open-back, no away" (lr-track-ask-test--open-back-no-away))
        (cons "calm, paused" (lr-track-ask-test--calm-paused))
        (cons "calm, no clock" (lr-track-ask-test--calm-no-clock))))

(defun lr-track-ask-test--header (ctx)
  "The header for CTX, without text properties."
  (substring-no-properties (lr-track--header ctx)))

(defun lr-track-ask-test--echo-lines (ctx)
  "The echo for CTX, as a list of lines without text properties."
  (split-string (substring-no-properties (lr-track--ask-echo ctx)) "\n"))


;;;; plans

(defun lr-track-ask-test--ops (ctx key)
  "The OPS of the plan for KEY in CTX."
  (plist-get (lr-track--answer-plan ctx key) :ops))

(defun lr-track-ask-test--opget (op prop)
  "PROP of plan operation OP.
`(:start N :from F ...)', `(:log N ...)' and `(:end-at F)' read as plists;
`(:resume :from F ...)' reads from its cdr."
  (plist-get (if (eq (car op) :resume) (cdr op) op) prop))


;;;; recording

(defmacro lr-track-ask-test--recording-messages (&rest body)
  "Run BODY with `message' recorded in `msgs', newest first.
Each entry is (TEXT . MESSAGE-LOG-MAX) as it was at the call."
  (declare (indent 0) (debug t))
  `(let ((msgs nil))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (let ((s (and fmt (apply #'format-message fmt args))))
                    (push (cons s message-log-max) msgs)
                    s))))
       ,@body)))

(defun lr-track-ask-test--msg-matching (msgs regexp)
  "The newest text in MSGS matching REGEXP, or nil."
  (seq-some (lambda (m) (and (stringp (car m)) (string-match-p regexp (car m))
                             (car m)))
            msgs))

(defun lr-track-ask-test--shown-p (msgs text)
  "Non-nil when TEXT was messaged (properties ignored)."
  (seq-some (lambda (m) (and (stringp (car m))
                             (equal text (substring-no-properties (car m)))))
            msgs))

(defun lr-track-ask-test--ours-p (f)
  "Non-nil when F is a symbol of lr-track's."
  (and f (symbolp f) (string-prefix-p "lr-track" (symbol-name f))))

(defun lr-track-ask-test--fn-is (f sym)
  "Non-nil when F is the function SYM (the symbol or its definition)."
  (or (eq f sym) (and (fboundp sym) (eq f (symbol-function sym)))))


;;;; the one-key world

(defconst lr-track-ask-test--digit-keys
  (append (number-sequence ?1 ?9) (number-sequence #x661 #x669)
          (number-sequence #x6f1 #x6f9))
  "Every digit key: ASCII, Arabic-Indic and Extended Arabic-Indic.")

(defun lr-track-ask-test--sorted (keys)
  "KEYS without duplicates, in a stable order, for set comparison."
  (sort (delete-dups (copy-sequence keys))
        (lambda (a b) (string< (format "%S" a) (format "%S" b)))))

(defun lr-track-ask-test--map-keys (map)
  "Every event MAP binds to a non-nil definition (character ranges expanded)."
  (let (keys)
    (map-keymap (lambda (ev def)
                  (when def
                    (if (consp ev)
                        (cl-loop for c from (car ev) to (cdr ev) do (push c keys))
                      (push ev keys))))
                map)
    (lr-track-ask-test--sorted keys)))

(defun lr-track-ask-test--echo-digits (echo)
  "The digits ECHO names for the main one-key map, ascending.
Read from what it really says: a range like 1-9 or a list like 6 7 9 before
a colon, and the `  N name' entries of a stream list.  The digits after `a
then' belong to the second map and do not count."
  (let ((text (replace-regexp-in-string "a then [0-9 -]+:" "" echo))
        (digits nil)
        (pos 0))
    (while (string-match "\\([1-9]\\)-\\([1-9]\\):" text pos)
      (setq digits (append digits
                           (number-sequence (string-to-number (match-string 1 text))
                                            (string-to-number (match-string 2 text))))
            pos (match-end 0)))
    (setq pos 0)
    (while (string-match "\\(?:\\`\\|\n\\|    \\)\\([1-9]\\(?: [1-9]\\)*\\):" text pos)
      ;; `split-string' moves the match data: read the end first
      (let ((list (match-string 1 text)))
        (setq pos (match-end 0)
              digits (append digits (mapcar #'string-to-number
                                            (split-string list))))))
    (setq pos 0)
    (while (string-match "  \\([1-9]\\) [a-z]" text pos)
      (push (string-to-number (match-string 1 text)) digits)
      (setq pos (match-end 0)))
    (sort (delete-dups digits) #'<)))

(defun lr-track-ask-test--echo-keys (echo)
  "The keys ECHO names, with their Arabic twins (contract section 8)."
  (let ((keys nil)
        (start "\\(?:\\`\\|\n\\|    \\)"))
    (dolist (d (lr-track-ask-test--echo-digits echo))
      (setq keys (append keys (list (+ ?0 d) (+ #x660 d) (+ #x6f0 d)))))
    (when (string-match-p (concat start "y: ") echo)
      (setq keys (append keys (list ?y #x63a))))
    (when (string-match-p (concat start "a then ") echo)
      (setq keys (append keys (list ?a #x634))))
    (when (string-match-p (concat start "u: undo") echo)
      (setq keys (append keys (list ?u #x639))))
    (lr-track-ask-test--sorted keys)))

(defmacro lr-track-ask-test--with-ask-world (ctx-form &rest body)
  "Run BODY as if `lr-track-ask' were pressed in the context CTX-FORM.
`lr-track--ask-context' returns `ctx'; setq it to change the world between
the echo and the key.  Frame focus is `focused'.  The REAL `set-transient-map'
runs, each call recorded in `installs' as (ARGS . EXIT-FUNCTION), newest
first.  `msgs' records messages, `plans' the plans handed to the executor
(which writes nothing here), `undos' counts `lr-track-undo', and `timers'
holds `run-at-time' requests as (SECS FUNCTION . ARGS).  `fire-soon' runs
those due within half a second.  BODY runs in a temp buffer shown in the
selected window; any map still up is closed afterwards."
  (declare (indent 1) (debug t))
  `(let* ((ctx ,ctx-form)
          (focused t)
          (installs nil)
          (msgs nil)
          (plans nil)
          (undos 0)
          (timers nil)
          (real-stm (symbol-function 'set-transient-map))
          (lr-track--ask-exit-fn nil)
          (lr-track--ask-shown nil)
          (lr-track--focused-p t)
          (lr-track--unfocused-since nil)
          (pre-command-hook nil)
          (post-command-hook nil)
          (overriding-terminal-local-map nil))
     (cl-letf (((symbol-function 'lr-track--ask-context) (lambda (&rest _) ctx))
               ((symbol-function 'frame-focus-state) (lambda (&rest _) focused))
               ((symbol-function 'run-at-time)
                (lambda (secs _repeat fn &rest args)
                  (push (cons secs (cons fn args)) timers)
                  (timer-create)))
               ((symbol-function 'set-transient-map)
                (lambda (&rest args)
                  (let ((exit (apply real-stm args)))
                    (push (cons args exit) installs)
                    exit)))
               ((symbol-function 'message)
                (lambda (fmt &rest args)
                  (let ((s (and fmt (apply #'format-message fmt args))))
                    (push (cons s message-log-max) msgs)
                    s)))
               ((symbol-function 'lr-track--execute-plan)
                (lambda (plan &rest _) (push plan plans) nil))
               ((symbol-function 'lr-track-undo)
                (lambda () (interactive) (setq undos (1+ undos)))))
       (cl-flet ((fire-soon ()
                   (let ((due (seq-filter
                               (lambda (tm) (or (null (car tm))
                                                (and (numberp (car tm))
                                                     (<= (car tm) 0.5))))
                               (reverse timers))))
                     (setq timers (seq-difference timers due #'eq))
                     (dolist (tm due) (apply (cadr tm) (cddr tm))))))
         (ignore #'fire-soon)
         (with-temp-buffer
           ;; shown in the selected window, as the agenda is when he
           ;; presses y: a key of the map answers only there
           (set-window-buffer (selected-window) (current-buffer))
           (unwind-protect
               (progn ,@body)
             (when (functionp lr-track--ask-exit-fn)
               (ignore-errors (funcall lr-track--ask-exit-fn)))
             (setq overriding-terminal-local-map nil)))))))

(defun lr-track-ask-test--ask ()
  "Run `lr-track-ask' as the command loop would."
  (let ((this-command 'lr-track-ask)
        (real-this-command 'lr-track-ask))
    (call-interactively #'lr-track-ask)))

(defvar lr-track-ask-test--screen 'ours
  "What the echo area shows when `lr-track-ask-test--press' types a key:
`ours' (the echo of the one-key map that is up, as when nothing replaced
it), another message as a string, or nil for an empty echo area.")

(defun lr-track-ask-test--read-clears-echo ()
  "What the read of a key does to the echo area, as `read_char' does it: run
`echo-area-clear-hook' while it still shows `lr-track-ask-test--screen', and
not at all when it is empty.  In batch the echo area is never on screen."
  (let ((shown (if (eq lr-track-ask-test--screen 'ours)
                   (bound-and-true-p lr-track--ask-echo-shown)
                 lr-track-ask-test--screen)))
    (when shown
      (cl-letf (((symbol-function 'current-message) (lambda () shown)))
        (run-hooks 'echo-area-clear-hook)))))

(defun lr-track-ask-test--lookup (map key)
  "KEY's binding in MAP as the key's read finds it: after its read
\(`lr-track-ask-test--read-clears-echo'), so the filter sees the echo."
  (lr-track-ask-test--read-clears-echo)
  (lookup-key map key))

(defun lr-track-ask-test--press (key)
  "Type KEY (a character) the way the command loop does.
The key's read runs `echo-area-clear-hook' while the echo area still shows
its message (`lr-track-ask-test--read-clears-echo').  The binding is then
looked up with every active map, the transient one included; then
`pre-command-hook' runs, which is where a transient map with no KEEP-PRED
exits and calls its ON-EXIT; then the command runs.  Return the command."
  (lr-track-ask-test--read-clears-echo)
  (let* ((keys (vector key))
         (cmd (key-binding keys t)))
    (let ((this-command cmd)
          (real-this-command cmd)
          (last-command-event key)
          (last-input-event key)
          (current-prefix-arg nil))
      (run-hooks 'pre-command-hook)
      (when (commandp cmd) (call-interactively cmd nil keys))
      (run-hooks 'post-command-hook))
    cmd))


;;;; scratch org files

(defun lr-track-ask-test--cleanup (dir)
  "Cancel any clock, kill every buffer visiting DIR, delete DIR."
  (when (org-clocking-p)
    (let ((org-clock-auto-clock-resolution nil))
      (ignore-errors (org-clock-cancel))))
  (when (get-buffer "*Time setup*") (kill-buffer "*Time setup*"))
  (let ((root (file-truename dir)))
    (dolist (b (buffer-list))
      (let ((f (buffer-file-name b)))
        (when (and f (string-prefix-p root (file-truename f)))
          (with-current-buffer b
            (set-buffer-modified-p nil)
            (remove-hook 'kill-buffer-hook #'org-check-running-clock t))
          (kill-buffer b)))))
  (ignore-errors (delete-directory dir t)))

(defmacro lr-track-ask-test--with-time-file (text &rest body)
  "Run BODY with `lr-track-time-file' at a fresh time.org holding TEXT.
TEXT nil leaves the file absent.  Binds `dir' and `file', and fresh tracker
state (undo stack, pause, last clock end).  Afterwards cancels any clock,
kills every buffer visiting the temp dir and deletes it."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "lr-track-ask" t)))
          (file (expand-file-name "time.org" dir))
          (lr-track-time-file file)
          (lr-track--undo-stack nil)
          (lr-track--clock-pause nil)
          (lr-track--last-clock-end nil)
          (lr-track--covered-spans nil))
     (let ((text ,text)) (when text (with-temp-file file (insert text))))
     (unwind-protect
         (progn ,@body)
       (lr-track-ask-test--cleanup dir))))

(defun lr-track-ask-test--visit (file)
  "The buffer visiting FILE, visiting it if needed."
  (or (find-buffer-visiting file) (find-file-noselect file)))

(defun lr-track-ask-test--file-text (file)
  "FILE's contents on disk."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun lr-track-ask-test--buffer-text (file)
  "The text of the buffer visiting FILE."
  (with-current-buffer (lr-track-ask-test--visit file)
    (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max)))))

(defun lr-track-ask-test--heading-marker (file name)
  "A marker at the heading NAME (any level) in FILE."
  (with-current-buffer (lr-track-ask-test--visit file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (re-search-forward (format "^\\*+ %s[ \t]*$" (regexp-quote name)))
     (copy-marker (line-beginning-position)))))

(defun lr-track-ask-test--entry-lines (file name)
  "The lines under the top-level heading NAME in FILE, up to the next one."
  (with-current-buffer (lr-track-ask-test--visit file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (re-search-forward (format "^\\* %s[ \t]*$" (regexp-quote name)))
     (let ((beg (line-beginning-position 2))
           (end (if (re-search-forward "^\\* " nil t)
                    (line-beginning-position)
                  (point-max))))
       (split-string (buffer-substring-no-properties beg end) "\n" t)))))

(defun lr-track-ask-test--clock-lines (lines)
  "The CLOCK lines among LINES."
  (seq-filter (lambda (l) (string-match-p "\\`[ \t]*CLOCK: " l)) lines))

(defun lr-track-ask-test--clock-and-note (lines clock-re)
  "Find the CLOCK line in LINES matching CLOCK-RE after its indentation.
Return (INDENT LINE NEXT-LINE), or nil when there is none."
  (cl-loop for (line next) on lines
           when (string-match (concat "\\`\\([ \t]*\\)" clock-re) line)
           return (list (match-string 1 line) line next)))

(defun lr-track-ask-test--running-re (start)
  "A CLOCK line starting at START, open or carrying a live end."
  (concat "CLOCK: " (regexp-quote (lr-track-ask-test--stamp start))
          "\\(?:--.*\\)?\\'"))

(defun lr-track-ask-test--closed-re (start end dur)
  "A closed CLOCK line START to END with duration string DUR."
  (concat "CLOCK: " (regexp-quote (lr-track-ask-test--stamp start))
          "--" (regexp-quote (lr-track-ask-test--stamp end))
          " => +" (regexp-quote dur) "\\'"))

(defun lr-track-ask-test--clock-in (marker start)
  "Clock into the heading at MARKER, its line starting at START (float)."
  (let ((org-clock-auto-clock-resolution nil)
        (org-clock-into-drawer t)
        (org-clock-in-switch-to-state nil))
    (org-with-point-at marker
      (org-clock-in nil (seconds-to-time start)))))

(defun lr-track-ask-test--clocked-heading ()
  "The heading text org is clocking, or nil."
  (and (org-clocking-p)
       (org-with-point-at org-clock-hd-marker (org-get-heading t t t t))))


;;;; agenda installers

(defmacro lr-track-ask-test--with-agenda-installers (&rest body)
  "Run BODY with lr-track's agenda installers on their hooks.
They should be on `org-agenda-finalize-hook' and `post-command-hook' once
lr-track-ask is loaded.  If they are only added by `lr-track--install-seams',
install the seams for BODY and remove them afterwards."
  (declare (indent 0) (debug t))
  `(let ((seamed nil))
     (unless (seq-some #'lr-track-ask-test--ours-p
                       (default-value 'org-agenda-finalize-hook))
       (when (fboundp 'lr-track--install-seams)
         (lr-track--install-seams)
         (setq seamed t)))
     (unwind-protect
         (progn
           (unless (seq-some #'lr-track-ask-test--ours-p
                             (default-value 'org-agenda-finalize-hook))
             (ert-fail "no lr-track function on org-agenda-finalize-hook"))
           (unless (seq-some #'lr-track-ask-test--ours-p
                             (default-value 'post-command-hook))
             (ert-fail "no lr-track function on post-command-hook"))
           ,@body)
       (when seamed (lr-track--remove-seams)))))

(defconst lr-track-ask-test--header-form '(:eval (lr-track--header-cached))
  "The header line form the installers set (contract section 8).")


;;;; 5. streams and time.org

(ert-deftest lr-track-streams-nil-without-file ()
  "Before SPC d M there is no time.org: no streams, and asking must not create
the file or leave a buffer visiting it."
  (lr-track-ask-test--with-time-file nil
    (should-not (lr-track--streams))
    (should-not (lr-track--stream-by-key 1))
    (should-not (file-exists-p file))
    (should-not (find-buffer-visiting file))))

(ert-deftest lr-track-setup-text-has-9-streams-no-todo ()
  "The seeds, the exact initial text, the preview, and RET writing it."
  (should (equal lr-track-ask-test--seeds lr-track--stream-seeds))
  (let ((text (lr-track--time-file-text)))
    ;; exact, up to trailing whitespace at the very end of the file
    (should (equal (string-trim-right (lr-track-ask-test--time-text))
                   (string-trim-right text)))
    (should (string-suffix-p "\n" text))
    (should (lr-track-ask-test--ascii-p text))
    ;; 9 top-level streams, none with a TODO keyword (so out of the agenda)
    (with-temp-buffer
      (insert text)
      (org-mode)
      (let ((n 0))
        (org-map-entries
         (lambda ()
           (setq n (1+ n))
           (should (= 1 (org-current-level)))
           (should-not (org-get-todo-state))))
        (should (= 9 n)))))
  ;; the preview never writes on its own; RET does
  (lr-track-ask-test--with-time-file nil
    (lr-track-ask-test--recording-messages
      (lr-track-setup)
      (should-not (file-exists-p file))
      (let ((buf (get-buffer "*Time setup*")))
        (should buf)
        (with-current-buffer buf
          (should (derived-mode-p 'special-mode))
          (let ((lines (mapcar (lambda (l)
                                 (replace-regexp-in-string
                                  "[ \t]+" " " (string-trim l)))
                               (split-string (buffer-string) "\n"))))
            (dolist (want '("Time setup nothing is written until RET; q cancels"
                            "1 avey (machine) 2 study (machine) 3 build (machine) 4 writing (machine) 5 reading (either)"
                            "6 practice (away) 7 life (away) 8 leisure (either) 9 sleep (away)"
                            "your 20 old buckets in life.org stay where they are until a later stage moves them"
                            "RET create q cancel"))
              (should (member want lines)))
            (should (seq-some
                     (lambda (l)
                       (string-match-p
                        (concat "\\`create .*time\\.org with 9 streams "
                                "(no TODO keywords, so it stays out of the agenda):\\'")
                        l))
                     lines)))
          (should-not (file-exists-p file))
          (call-interactively (key-binding (kbd "RET")))))
      (should (file-exists-p file))
      (should (equal (lr-track--time-file-text) (lr-track-ask-test--file-text file)))
      (should (lr-track-ask-test--msg-matching
               msgs "\\`Created .*time\\.org with 9 streams\\. y in your agenda now asks Now\\?\\'"))
      ;; and the streams are there, sorted, with their places
      (let ((streams (lr-track--streams)))
        (should (equal '(1 2 3 4 5 6 7 8 9)
                       (mapcar (lambda (s) (plist-get s :key)) streams)))
        (should (equal (mapcar #'cadr lr-track-ask-test--seeds)
                       (mapcar (lambda (s) (plist-get s :name)) streams)))
        (should (equal (mapcar #'caddr lr-track-ask-test--seeds)
                       (mapcar (lambda (s) (plist-get s :place)) streams))))))
  ;; q cancels and writes nothing
  (lr-track-ask-test--with-time-file nil
    (lr-track-setup)
    (with-current-buffer "*Time setup*"
      (call-interactively (key-binding "q")))
    (should-not (file-exists-p file))))

(ert-deftest lr-track-setup-refuses-if-present ()
  "An existing time.org is his: SPC d M refuses with an echo and writes
nothing, both when the file is there up front and when it appears while
the preview is open."
  (cl-flet ((setup-and-ret (msgs-cell)
              (condition-case err
                  (lr-track-setup)
                (user-error (push (cons (error-message-string err) nil)
                                  (car msgs-cell))))
              (let ((buf (get-buffer "*Time setup*")))
                (when buf
                  (with-current-buffer buf
                    (let ((cmd (key-binding (kbd "RET"))))
                      (when (commandp cmd)
                        (condition-case err
                            (call-interactively cmd)
                          (user-error (push (cons (error-message-string err) nil)
                                            (car msgs-cell)))))))))))
    (lr-track-ask-test--with-time-file "* mine\n"
      (lr-track-ask-test--recording-messages
        (let ((cell (list nil)))
          (setup-and-ret cell)
          (setq msgs (append (car cell) msgs)))
        (should msgs)
        (should (equal "* mine\n" (lr-track-ask-test--file-text file))))))
  ;; created behind the open preview: RET still refuses
  (lr-track-ask-test--with-time-file nil
    (lr-track-ask-test--recording-messages
      (lr-track-setup)
      (should (get-buffer "*Time setup*"))
      (with-temp-file file (insert "* mine\n"))
      (setq msgs nil)
      (with-current-buffer "*Time setup*"
        (condition-case err
            (call-interactively (key-binding (kbd "RET")))
          (user-error (push (cons (error-message-string err) nil) msgs))))
      (should msgs)
      (should (equal "* mine\n" (lr-track-ask-test--file-text file))))))

(ert-deftest lr-track-streams-read-and-sorted ()
  "Only top-level headings with a numeric TRACK_KEY are streams, sorted by
key, with place symbols and maxima in seconds.  The file is visited, never
displayed."
  (lr-track-ask-test--with-time-file
      (concat "#+title: Time\n\n"
              "* build\n:PROPERTIES:\n:TRACK_KEY:   3\n:TRACK_PLACE: machine\n"
              ":TRACK_MAX:   10:00\n:END:\n"
              "** sub of build\n:PROPERTIES:\n:TRACK_KEY:   8\n:END:\n\n"
              "* notes\nno key here\n\n"
              "* avey\n:PROPERTIES:\n:TRACK_KEY:   1\n:TRACK_PLACE: machine\n"
              ":TRACK_MAX:   10:00\n:END:\n\n"
              "* weird\n:PROPERTIES:\n:TRACK_KEY:   x\n:END:\n\n"
              "* life\n:PROPERTIES:\n:TRACK_KEY:   7\n:TRACK_PLACE: away\n"
              ":TRACK_MAX:   4:00\n:END:\n")
    (let ((streams (lr-track--streams)))
      (should (equal '(1 3 7) (mapcar (lambda (s) (plist-get s :key)) streams)))
      (should (equal '("avey" "build" "life")
                     (mapcar (lambda (s) (plist-get s :name)) streams)))
      (should (equal '(machine machine away)
                     (mapcar (lambda (s) (plist-get s :place)) streams)))
      (should (equal '(36000.0 36000.0 14400.0)
                     (mapcar (lambda (s) (float (plist-get s :max))) streams)))
      (dolist (s streams)
        (let ((m (plist-get s :marker)))
          (should (markerp m))
          (should (equal (plist-get s :name)
                         (org-with-point-at m (org-get-heading t t t t))))
          (should-not (get-buffer-window (marker-buffer m) t))))
      (should (equal "life" (plist-get (lr-track--stream-by-key 7) :name)))
      (should-not (lr-track--stream-by-key 2))
      (should-not (lr-track--stream-by-key 8)))))

(ert-deftest lr-track-streams-cache-follows-file-modtime ()
  "The streams are cached by modification time, so an edit saved to disk is
picked up, and picking it up never asks anything (a header refresh runs on
a tick, where a revert prompt would be a prompt from a timer)."
  (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
    (should (= 9 (length (lr-track--streams))))
    (should (equal "reading" (plist-get (lr-track--stream-by-key 5) :name)))
    (with-temp-file file
      (insert (replace-regexp-in-string "^\\* reading$" "* books"
                                        (lr-track-ask-test--time-text))))
    (set-file-times file (time-add (current-time) 10))
    (should (= 9 (length (lr-track--streams))))
    (should (equal "books" (plist-get (lr-track--stream-by-key 5) :name)))))

(ert-deftest lr-track-stream-of-marker ()
  "A marker anywhere in a stream's subtree belongs to that stream; anything
else belongs to none."
  (lr-track-ask-test--with-time-file
      (lr-track-ask-test--time-text
       '(("build" . "** a task under build\nsome text\n")))
    (let ((other (expand-file-name "other.org" dir)))
      (with-temp-file other (insert "* build\n* life\n"))
      (should (= 3 (plist-get (lr-track--stream-of-marker
                               (lr-track-ask-test--heading-marker
                                file "a task under build"))
                              :key)))
      (let ((m (with-current-buffer (lr-track-ask-test--visit file)
                 (org-with-wide-buffer
                  (goto-char (point-min))
                  (re-search-forward "^\\* life$")
                  (re-search-forward "^:TRACK_PLACE:")
                  (point-marker)))))
        (should (= 7 (plist-get (lr-track--stream-of-marker m) :key))))
      (should-not (lr-track--stream-of-marker
                   (with-current-buffer (lr-track-ask-test--visit file)
                     (copy-marker (point-min)))))
      (should-not (lr-track--stream-of-marker
                   (lr-track-ask-test--heading-marker other "life")))
      (should-not (lr-track--stream-of-marker (make-marker))))))


;;;; 6. ask state

(ert-deftest lr-track-state-nostreams ()
  (should (eq 'nostreams (lr-track--ask-state (lr-track-ask-test--nostreams))))
  (should (eq 'nostreams (lr-track--ask-state
                          (lr-track-ask-test--with (lr-track-ask-test--live)
                                                   :streams nil)))))

(ert-deftest lr-track-state-live ()
  "A clock that is not paused is live, whatever presence says."
  (let ((ctx (lr-track-ask-test--live)))
    (should (eq 'live (lr-track--ask-state ctx)))
    (should (eq 'live (lr-track--ask-state (lr-track-ask-test--live t))))
    (should (eq 'live (lr-track--ask-state
                       (lr-track-ask-test--with
                        ctx :presence
                        (lr-track-ask-test--presence
                         :mode 'away :first-time (lr-track-ask-test--at 8 0)
                         :away-from (lr-track-ask-test--at 17 10)
                         :away-kind 'idle)))))
    (should (eq 'live (lr-track--ask-state
                       (lr-track-ask-test--with
                        ctx :presence (lr-track-ask-test--presence :mode 'unknown)))))))

(ert-deftest lr-track-state-paused-back ()
  "Paused at P, back at R > P, and here 2 min or more since R."
  (let* ((ctx (lr-track-ask-test--paused-back))
         (r (lr-track-ask-test--at 16 5)))
    (should (eq 'paused-back (lr-track--ask-state ctx)))
    (should (eq 'paused-back (lr-track--ask-state
                              (lr-track-ask-test--with ctx :now (+ r 120)))))
    (should (eq 'calm (lr-track--ask-state
                       (lr-track-ask-test--with ctx :now (+ r 119)))))))

(ert-deftest lr-track-state-paused-not-back-is-calm ()
  "A paused clock is not a question until he is back from the away it
paused in."
  (let ((ctx (lr-track-ask-test--paused-back)))
    ;; still away
    (should (eq 'calm (lr-track--ask-state (lr-track-ask-test--calm-paused))))
    ;; back only a minute
    (should (eq 'calm (lr-track--ask-state
                       (lr-track-ask-test--with
                        ctx :now (+ (lr-track-ask-test--at 16 5) 60)))))
    ;; the clock paused AFTER his return (at its maximum, say)
    (should (eq 'calm (lr-track--ask-state
                       (lr-track-ask-test--with
                        ctx :presence
                        (lr-track-ask-test--presence
                         :first-time (lr-track-ask-test--at 8 0)
                         :return (lr-track-ask-test--at 14 0)
                         :last-input (lr-track-ask-test--at 16 39)
                         :last-away (lr-track-ask-test--away 13 30 14 0 'locked))))))
    ;; no away has ended this session
    (should (eq 'calm (lr-track--ask-state
                       (lr-track-ask-test--with
                        ctx :presence
                        (lr-track-ask-test--presence
                         :first-time (lr-track-ask-test--at 8 0)
                         :last-input (lr-track-ask-test--at 16 39))))))))

(ert-deftest lr-track-state-open-back-after-10m ()
  "Nothing running and here 10 min or more since the return (or since Emacs
first saw him, when no away has ended yet)."
  (let* ((ctx (lr-track-ask-test--open-back))
         (r (lr-track-ask-test--at 16 5)))
    (should (eq 'open-back (lr-track--ask-state ctx)))
    (should (eq 'open-back (lr-track--ask-state
                            (lr-track-ask-test--with ctx :now (+ r 600)))))
    (should (eq 'calm (lr-track--ask-state
                       (lr-track-ask-test--with ctx :now (+ r 599)))))
    (let ((fresh (lr-track-ask-test--open-back-no-away))
          (first (lr-track-ask-test--at 16 0)))
      (should (eq 'open-back (lr-track--ask-state fresh)))
      (should (eq 'calm (lr-track--ask-state
                         (lr-track-ask-test--with fresh :now (+ first 599))))))
    ;; away with nothing running: calm, there is nobody to ask
    (should (eq 'calm (lr-track--ask-state (lr-track-ask-test--calm-no-clock))))))

(ert-deftest lr-track-state-open-back-respects-last-end ()
  "A clock that ended after his return restarts the 10 min."
  (let ((ctx (lr-track-ask-test--open-back)))
    (should (eq 'calm (lr-track--ask-state
                       (lr-track-ask-test--with
                        ctx :last-end (lr-track-ask-test--at 16 35)))))
    (should (eq 'open-back (lr-track--ask-state
                            (lr-track-ask-test--with
                             ctx :last-end (lr-track-ask-test--at 16 30)))))
    (should (eq 'open-back (lr-track--ask-state
                            (lr-track-ask-test--with
                             ctx :last-end (lr-track-ask-test--at 15 0)))))))


;;;; 6. the answer start

(ert-deftest lr-track-start-is-return-within-90m ()
  (should (= (lr-track-ask-test--at 16 5)
             (lr-track--answer-start (lr-track-ask-test--open-back))))
  ;; floored to the minute
  (should (= (lr-track-ask-test--at 16 5)
             (lr-track--answer-start
              (lr-track-ask-test--with
               (lr-track-ask-test--open-back)
               :presence (lr-track-ask-test--presence
                          :first-time (lr-track-ask-test--at 8 0)
                          :return (lr-track-ask-test--at 16 5 40)
                          :last-away (lr-track-ask-test--away 15 40 16 5 'locked))))))
  ;; exactly 90 min ago still counts
  (should (= (lr-track-ask-test--at 16 5)
             (lr-track--answer-start
              (lr-track-ask-test--with (lr-track-ask-test--open-back)
                                       :now (lr-track-ask-test--at 17 35)))))
  ;; no away yet: here since Emacs first saw him
  (should (= (lr-track-ask-test--at 16 0)
             (lr-track--answer-start (lr-track-ask-test--open-back-no-away)))))

(ert-deftest lr-track-start-cap-90-to-earliest-break-end ()
  "Return over 90 min ago: the earliest break end inside the last 90 min."
  (let ((ctx (lr-track-ask-test--ctx
              :now (lr-track-ask-test--at 18 10)
              :presence (lr-track-ask-test--presence
                         :first-time (lr-track-ask-test--at 8 0)
                         :return (lr-track-ask-test--at 15 0)
                         :breaks (list (list :from (lr-track-ask-test--at 17 56)
                                             :to (lr-track-ask-test--at 18 2))
                                       (list :from (lr-track-ask-test--at 17 0)
                                             :to (lr-track-ask-test--at 17 6 30))
                                       (list :from (lr-track-ask-test--at 16 20)
                                             :to (lr-track-ask-test--at 16 30)))))))
    (should (= (lr-track-ask-test--at 17 6) (lr-track--answer-start ctx)))))

(ert-deftest lr-track-start-now-when-no-break ()
  (let ((ctx (lr-track-ask-test--ctx
              :now (lr-track-ask-test--at 18 10 42)
              :presence (lr-track-ask-test--presence
                         :first-time (lr-track-ask-test--at 8 0)
                         :return (lr-track-ask-test--at 15 0)))))
    (should (= (lr-track-ask-test--at 18 10) (lr-track--answer-start ctx)))
    ;; a break that ended before the 90 min window does not count
    (should (= (lr-track-ask-test--at 18 10)
               (lr-track--answer-start
                (lr-track-ask-test--with
                 ctx :presence
                 (lr-track-ask-test--presence
                  :first-time (lr-track-ask-test--at 8 0)
                  :return (lr-track-ask-test--at 15 0)
                  :breaks (list (list :from (lr-track-ask-test--at 16 0)
                                      :to (lr-track-ask-test--at 16 10))))))))
    (should (= (lr-track-ask-test--at 18 10)
               (lr-track--answer-start (lr-track-ask-test--calm-no-clock))))))

(ert-deftest lr-track-start-is-now-while-presence-is-away ()
  "He presses y, so he is back, but presence has not stepped his return yet
\(it lags up to a tick).  Its here-since and breaks are from the block BEFORE
this away, so a start from them would claim the away: the start is now."
  (let ((at #'lr-track-ask-test--at))
    ;; nothing running, away since 17:50, the previous block from 17:00
    (should (= (funcall at 18 10)
               (lr-track--answer-start
                (lr-track-ask-test--ctx
                 :now (funcall at 18 10 42)
                 :presence (lr-track-ask-test--presence
                            :mode 'away :first-time (funcall at 8 0)
                            :return (funcall at 17 0)
                            :last-input (funcall at 17 50)
                            :away-from (funcall at 17 50) :away-kind 'idle
                            :breaks (list (list :from (funcall at 17 20)
                                                :to (funcall at 17 30))))))))
    ;; paused at the away's start: not from the pause either, from now
    (let ((ctx (lr-track-ask-test--ctx
                :now (funcall at 15 58)
                :presence (lr-track-ask-test--presence
                           :mode 'away :first-time (funcall at 8 0)
                           :return (funcall at 15 0)
                           :last-input (funcall at 15 40)
                           :away-from (funcall at 15 40) :away-kind 'idle)
                :clock (lr-track-ask-test--clock
                        :start (funcall at 14 10) :end (funcall at 15 40)
                        :paused-at (funcall at 15 40) :why 'idle))))
      (should (eq 'calm (lr-track--ask-state ctx)))
      (should (= (funcall at 15 58) (lr-track--answer-start ctx)))
      (let ((ops (lr-track-ask-test--ops ctx 2)))
        (should (equal (mapcar #'car ops) '(:end-at :start)))
        (should (= (nth 1 (nth 0 ops)) (funcall at 15 40)))
        (should (= (lr-track-ask-test--opget (nth 1 ops) :from)
                   (funcall at 15 58)))))))

(ert-deftest lr-track-start-never-before-last-end-or-pause ()
  "Never claim time already on another line or before the paused clock's
pause."
  (let ((ctx (lr-track-ask-test--open-back)))
    (should (= (lr-track-ask-test--at 16 20)
               (lr-track--answer-start
                (lr-track-ask-test--with ctx :last-end (lr-track-ask-test--at 16 20)))))
    ;; the clock paused after his return
    (let ((paused (lr-track-ask-test--with
                   ctx :clock (lr-track-ask-test--clock
                               :start (lr-track-ask-test--at 14 10)
                               :end (lr-track-ask-test--at 16 25)
                               :paused-at (lr-track-ask-test--at 16 25)
                               :why 'max))))
      (should (= (lr-track-ask-test--at 16 25) (lr-track--answer-start paused)))
      (should (= (lr-track-ask-test--at 16 25)
                 (lr-track--answer-start
                  (lr-track-ask-test--with paused
                                           :last-end (lr-track-ask-test--at 16 20))))))
    ;; the capped start is bounded the same way
    (should (= (lr-track-ask-test--at 17 30)
               (lr-track--answer-start
                (lr-track-ask-test--ctx
                 :now (lr-track-ask-test--at 18 10)
                 :last-end (lr-track-ask-test--at 17 30)
                 :presence (lr-track-ask-test--presence
                            :first-time (lr-track-ask-test--at 8 0)
                            :return (lr-track-ask-test--at 15 0)
                            :breaks (list (list :from (lr-track-ask-test--at 17 0)
                                                :to (lr-track-ask-test--at 17 6))))))))))


;;;; 6. header and echo texts

(ert-deftest lr-track-header-golden-each-state ()
  (should (equal "Time  no streams yet: SPC d M sets them up"
                 (lr-track-ask-test--header (lr-track-ask-test--nostreams))))
  ;; D is the line's own length, start to its end stamp (not to now)
  (should (equal "Time  avey 1h12m since 16:05  |  y: switch"
                 (lr-track-ask-test--header (lr-track-ask-test--live))))
  (should (equal (concat "Time  Now?  avey paused 15:40 (your last input), back 16:05"
                         " after 25m away  |  y y: avey again from 16:05"
                         "   y then 1-9: another")
                 (lr-track-ask-test--header (lr-track-ask-test--paused-back))))
  (should (equal (concat "Time  Now?  back 16:05 after 25m away (locked), nothing running"
                         "  |  y then 1-9: what you do now, from 16:05   y a: the away")
                 (lr-track-ask-test--header (lr-track-ask-test--open-back))))
  (should (equal (concat "Time  Now?  back 16:05 after 25m away (no input), nothing running"
                         "  |  y then 1-9: what you do now, from 16:05   y a: the away")
                 (lr-track-ask-test--header (lr-track-ask-test--open-back 'idle))))
  (should (equal (concat "Time  Now?  back 16:05 after 25m away (Mac asleep), nothing running"
                         "  |  y then 1-9: what you do now, from 16:05   y a: the away")
                 (lr-track-ask-test--header (lr-track-ask-test--open-back 'asleep))))
  (should (equal (concat "Time  Now?  here since 16:00, nothing running"
                         "  |  y then 1-9: what you do now, from 16:00")
                 (lr-track-ask-test--header (lr-track-ask-test--open-back-no-away))))
  (should (equal "Time  avey paused 15:40 (your last input)  |  y: switch"
                 (lr-track-ask-test--header (lr-track-ask-test--calm-paused))))
  (should (equal "Time  nothing running  |  y: start something"
                 (lr-track-ask-test--header (lr-track-ask-test--calm-no-clock)))))

(ert-deftest lr-track-header-ascii-no-arrows-essential-in-90 ()
  "One ASCII line, no arrows, and everything before the key hints within the
first 90 columns (the frame is 186 wide; the hints may run past 90)."
  (let ((night (lr-track-ask-test--with
                (lr-track-ask-test--open-back)
                :presence (lr-track-ask-test--presence
                           :first-time (lr-track-ask-test--at 1 0)
                           :return (lr-track-ask-test--at 16 21)
                           :last-away (lr-track-ask-test--away 3 4 16 21 'asleep))))
        (task (lr-track-ask-test--with
               (lr-track-ask-test--paused-back)
               :clock (lr-track-ask-test--clock
                       :task "Write report" :key nil
                       :start (lr-track-ask-test--at 14 10)
                       :end (lr-track-ask-test--at 15 40)
                       :paused-at (lr-track-ask-test--at 15 40) :why 'idle))))
    (dolist (case (append (lr-track-ask-test--all-fixtures)
                          (list (cons "after the night" night)
                                (cons "an org task paused" task))))
      (let* ((h (lr-track-ask-test--header (cdr case)))
             (essential (if (string-match "  |  " h) (substring h 0 (match-beginning 0)) h)))
        (should (equal (list (car case) t) (list (car case) (lr-track-ask-test--ascii-p h))))
        (should-not (string-match-p lr-track-ask-test--arrows-re h))
        (should-not (string-match-p "\n" h))
        (should (string-prefix-p "Time  " h))
        (should (equal (list (car case) t)
                       (list (car case) (<= (string-width essential) 90))))))))

(ert-deftest lr-track-header-refresh-updates-cache-and-redraws-on-change ()
  "The header is read from a cache; a refresh recomputes it and redraws all
windows only when the text changed."
  (let ((ctx (lr-track-ask-test--open-back))
        (redraws nil))
    (cl-letf (((symbol-function 'lr-track--ask-context) (lambda (&rest _) ctx))
              ((symbol-function 'force-mode-line-update)
               (lambda (&optional all) (push all redraws))))
      (lr-track-ask-refresh-header)
      (should (equal (lr-track-ask-test--header ctx)
                     (substring-no-properties (lr-track--header-cached))))
      (setq redraws nil)
      (lr-track-ask-refresh-header)
      (should-not redraws)
      (setq ctx (lr-track-ask-test--calm-no-clock))
      (lr-track-ask-refresh-header)
      (should (equal '(t) (mapcar (lambda (a) (and a t)) redraws)))
      (should (equal (lr-track-ask-test--header ctx)
                     (substring-no-properties (lr-track--header-cached)))))))

(defconst lr-track-ask-test--away-clause
  "6 7 9: from 16:40 (now, for when you step away)"
  "What the echo says of the away-place digits at 16:40 (spec v3 2.2): they
start now, while the other digits start at the answer start.")

(ert-deftest lr-track-echo-golden-each-state ()
  (should (equal (list (concat "Now, from 16:05 (your return, 35m ago):  "
                               lr-track-ask-test--stream-list)
                       (concat lr-track-ask-test--away-clause
                               "    a then 1-9: the away 15:40 to 16:05 (25m, locked)"
                               " was that    other keys: not now"))
                 (lr-track-ask-test--echo-lines (lr-track-ask-test--open-back))))
  ;; no away to label: no `a' part
  (let ((lines (lr-track-ask-test--echo-lines (lr-track-ask-test--open-back-no-away))))
    (should (= 2 (length lines)))
    (should (string-prefix-p "Now, from 16:00" (car lines)))
    (should (string-suffix-p (concat ":  " lr-track-ask-test--stream-list) (car lines)))
    (should (equal (concat lr-track-ask-test--away-clause
                           "    other keys: not now")
                   (cadr lines))))
  ;; nothing running and not back: the start is now, and no away is
  ;; offered while presence says away (round 3.3: the one he means is not
  ;; stepped yet, and the latest completed one is older)
  (let ((lines (lr-track-ask-test--echo-lines (lr-track-ask-test--calm-no-clock))))
    (should (<= (length lines) 2))
    (should (string-prefix-p "Now, from 18:10" (car lines)))
    (should (string-suffix-p (concat ":  " lr-track-ask-test--stream-list) (car lines)))
    (should (equal "other keys: not now" (car (last lines)))))
  (should (equal (list (concat "y: avey again from 16:05 (its 14:10 to 15:40 line stays)"
                               "    1-9: another stream from 16:05, avey stays ended at 15:40")
                       (concat lr-track-ask-test--away-clause
                               "    a then 1-9: the away 15:40 to 16:05 (25m, no input)"
                               " was that    other keys: not now"))
                 (lr-track-ask-test--echo-lines (lr-track-ask-test--paused-back))))
  (should (equal (list (concat "1-9: switch now (avey 16:05 to 17:20, the new one from 17:20)"
                               "    other keys: not now"))
                 (lr-track-ask-test--echo-lines (lr-track-ask-test--live))))
  (let ((lines (lr-track-ask-test--echo-lines (lr-track-ask-test--live t))))
    (should (= 2 (length lines)))
    (should (string-prefix-p "1-9: switch now (avey 16:05 to 17:20, the new one from 17:20)"
                             (car lines)))
    (should (string-prefix-p "a then 1-9: the away 15:40 to 16:05 (25m, locked) was that"
                             (cadr lines))))
  (should (equal '("No streams yet: SPC d M sets them up.")
                 (lr-track-ask-test--echo-lines (lr-track-ask-test--nostreams))))
  ;; every echo: ASCII, at most two lines, no arrows
  (dolist (case (lr-track-ask-test--all-fixtures))
    (unless (equal (car case) "calm, paused")
      (let ((echo (substring-no-properties (lr-track--ask-echo (cdr case)))))
        (should (equal (list (car case) t)
                       (list (car case) (lr-track-ask-test--ascii-p echo))))
        (should (<= (length (split-string echo "\n")) 2))
        (dolist (line (split-string echo "\n"))
          (should (equal (list (car case) t)
                         (list (car case) (<= (string-width line) 186)))))
        (should-not (string-match-p lr-track-ask-test--arrows-re echo))))))

(ert-deftest lr-track-echo-keys-equal-map-keys ()
  "Every key the echo names is bound in the one-key map, and nothing else is."
  (dolist (case (list (cons "open-back" (lr-track-ask-test--open-back))
                      (cons "open-back, no away" (lr-track-ask-test--open-back-no-away))
                      (cons "paused-back" (lr-track-ask-test--paused-back))
                      (cons "live" (lr-track-ask-test--live))
                      (cons "live, latest away" (lr-track-ask-test--live t))
                      (cons "calm, no clock" (lr-track-ask-test--calm-no-clock))))
    (lr-track-ask-test--with-ask-world (cdr case)
      (lr-track-ask-test--ask)
      (should (equal (list (car case) 1) (list (car case) (length installs))))
      (let ((map (car (car (car installs))))
            (echo (substring-no-properties (lr-track--ask-echo ctx))))
        (should (equal (list (car case) (lr-track-ask-test--echo-keys echo))
                       (list (car case) (lr-track-ask-test--map-keys map))))))))


;;;; 7. plans

(ert-deftest lr-track-plan-open-back-digit-from-start ()
  "A machine or either stream runs from the answer start, tagged now."
  (dolist (case (list (list (lr-track-ask-test--open-back) 2 (lr-track-ask-test--at 16 5))
                      (list (lr-track-ask-test--open-back) 5 (lr-track-ask-test--at 16 5))
                      (list (lr-track-ask-test--open-back-no-away) 1
                            (lr-track-ask-test--at 16 0))
                      (list (lr-track-ask-test--calm-no-clock) 2
                            (lr-track-ask-test--at 18 10))))
    (let* ((plan (lr-track--answer-plan (nth 0 case) (nth 1 case)))
           (ops (plist-get plan :ops))
           (op (car ops)))
      (should-not (plist-get plan :refuse))
      (should (stringp (plist-get plan :message)))
      (should (= 1 (length ops)))
      (should (eq :start (car op)))
      (should (eql (nth 1 case) (cadr op)))
      (should (= (nth 2 case) (lr-track-ask-test--opget op :from)))
      (should (equal "now" (lr-track-ask-test--opget op :tag)))
      (should (stringp (lr-track-ask-test--opget op :text))))))

(ert-deftest lr-track-plan-open-back-away-place-digit-from-now ()
  "An away-place stream means `I am leaving for this': it runs from now
(floored to the minute), tagged declared, never backdated to the return."
  (let ((ctx (lr-track-ask-test--with (lr-track-ask-test--open-back)
                                      :now (lr-track-ask-test--at 16 40 25))))
    (dolist (n '(6 7 9))
      (let* ((ops (lr-track-ask-test--ops ctx n))
             (op (car ops)))
        (should (= 1 (length ops)))
        (should (eq :start (car op)))
        (should (eql n (cadr op)))
        (should (= (lr-track-ask-test--at 16 40) (lr-track-ask-test--opget op :from)))
        (should (equal "declared" (lr-track-ask-test--opget op :tag)))))))

(ert-deftest lr-track-plan-paused-back-default-resumes-from-start ()
  "y y: the same task again from his return; the old line stays as it is."
  (let* ((ops (lr-track-ask-test--ops (lr-track-ask-test--paused-back) 'default))
         (op (car ops)))
    (should (= 1 (length ops)))
    (should (eq :resume (car op)))
    (should (= (lr-track-ask-test--at 16 5) (lr-track-ask-test--opget op :from)))
    (should (equal "continued" (lr-track-ask-test--opget op :tag)))
    (should (stringp (lr-track-ask-test--opget op :text))))
  ;; there is no default anywhere else
  (dolist (ctx (list (lr-track-ask-test--open-back) (lr-track-ask-test--live)
                     (lr-track-ask-test--calm-no-clock) (lr-track-ask-test--nostreams)))
    (let ((plan (lr-track--answer-plan ctx 'default)))
      (should (stringp (plist-get plan :refuse)))
      (should-not (plist-get plan :ops)))))

(ert-deftest lr-track-plan-paused-back-other-digit-keeps-end-at-pause ()
  "Another stream: the paused task ends where it paused, the new one runs
from the answer start."
  (let ((ops (lr-track-ask-test--ops (lr-track-ask-test--paused-back) 2)))
    (should (= 2 (length ops)))
    (should (equal (list :end-at (lr-track-ask-test--at 15 40)) (nth 0 ops)))
    (let ((op (nth 1 ops)))
      (should (eq :start (car op)))
      (should (eql 2 (cadr op)))
      (should (= (lr-track-ask-test--at 16 5) (lr-track-ask-test--opget op :from)))
      (should (equal "now" (lr-track-ask-test--opget op :tag))))))

(ert-deftest lr-track-plan-live-digit-switches-now ()
  "A switch while live: the running task ends now and the new one starts
now, both floored to the minute."
  (let ((ctx (lr-track-ask-test--with (lr-track-ask-test--live)
                                      :now (lr-track-ask-test--at 17 20 40))))
    (dolist (n '(2 7))
      (let ((ops (lr-track-ask-test--ops ctx n)))
        (should (= 2 (length ops)))
        (should (equal (list :end-at (lr-track-ask-test--at 17 20)) (nth 0 ops)))
        (let ((op (nth 1 ops)))
          (should (eq :start (car op)))
          (should (eql n (cadr op)))
          (should (= (lr-track-ask-test--at 17 20) (lr-track-ask-test--opget op :from)))
          (should (equal "declared" (lr-track-ask-test--opget op :tag))))))))

(ert-deftest lr-track-plan-live-same-digit-refuses ()
  (let ((plan (lr-track--answer-plan (lr-track-ask-test--live) 1)))
    (should (equal "avey is already running (since 16:05). Nothing written."
                   (plist-get plan :refuse)))
    (should-not (plist-get plan :ops))))

(ert-deftest lr-track-plan-away-label-latest-away ()
  "a then N, in any state: a closed line for the latest away, under N."
  (let ((ops (lr-track-ask-test--ops (lr-track-ask-test--open-back) '(away . 7))))
    (should (equal (list (list :log 7
                               :from (lr-track-ask-test--at 15 40)
                               :to (lr-track-ask-test--at 16 5)
                               :tag "away" :text "locked 25m"))
                   ops)))
  (dolist (case (list (cons (lr-track-ask-test--paused-back) 1)
                      (cons (lr-track-ask-test--live t) 3)))
    (let* ((ops (lr-track-ask-test--ops (car case) (cons 'away (cdr case))))
           (op (car ops)))
      (should (= 1 (length ops)))
      (should (eq :log (car op)))
      (should (eql (cdr case) (cadr op)))
      (should (= (lr-track-ask-test--at 15 40) (lr-track-ask-test--opget op :from)))
      (should (= (lr-track-ask-test--at 16 5) (lr-track-ask-test--opget op :to)))
      (should (equal "away" (lr-track-ask-test--opget op :tag))))))

(ert-deftest lr-track-plan-away-label-refuses-without-away ()
  "No completed away, or one under 15 min: nothing to label.  Nor while
presence still says away (round 3.3): the away he means is not stepped yet,
so the latest completed one is an older one."
  (dolist (ctx (list (lr-track-ask-test--open-back-no-away)
                     (lr-track-ask-test--nostreams)
                     (lr-track-ask-test--calm-no-clock)
                     (lr-track-ask-test--with
                      (lr-track-ask-test--open-back)
                      :presence (lr-track-ask-test--presence
                                 :first-time (lr-track-ask-test--at 8 0)
                                 :return (lr-track-ask-test--at 16 5)
                                 :last-away (lr-track-ask-test--away 15 51 16 5 'idle)))))
    (let ((plan (lr-track--answer-plan ctx '(away . 7))))
      (should (stringp (plist-get plan :refuse)))
      (should-not (plist-get plan :ops))))
  ;; exactly 15 min is an away
  (should (lr-track-ask-test--ops
           (lr-track-ask-test--with
            (lr-track-ask-test--open-back)
            :presence (lr-track-ask-test--presence
                       :first-time (lr-track-ask-test--at 8 0)
                       :return (lr-track-ask-test--at 16 5)
                       :last-away (lr-track-ask-test--away 15 50 16 5 'idle)))
           '(away . 7))))

(ert-deftest lr-track-plan-undo-unknown-stream-and-nostreams ()
  "u plans the undo; a digit with no such stream and any digit before setup
refuse."
  (should (equal '((:undo)) (lr-track-ask-test--ops (lr-track-ask-test--open-back) 'undo)))
  (let ((missing-4 (lr-track-ask-test--with
                    (lr-track-ask-test--open-back)
                    :streams (seq-remove (lambda (s) (= 4 (plist-get s :key)))
                                         (lr-track-ask-test--streams)))))
    (should (stringp (plist-get (lr-track--answer-plan missing-4 4) :refuse))))
  (should (stringp (plist-get (lr-track--answer-plan (lr-track-ask-test--nostreams) 2)
                              :refuse))))

(ert-deftest lr-track-plan-signature-equal-for-same-writes ()
  "Same writes, same signature (the message is not a write); different
writes, different signatures."
  (let* ((ctx (lr-track-ask-test--open-back))
         (a (lr-track--answer-plan ctx 2)))
    (should (equal (lr-track--plan-signature a)
                   (lr-track--plan-signature (lr-track--answer-plan (copy-tree ctx) 2))))
    (should (equal (lr-track--plan-signature a)
                   (lr-track--plan-signature
                    (list :ops (plist-get a :ops) :message "some other words"))))
    (should-not (equal (lr-track--plan-signature a)
                       (lr-track--plan-signature (lr-track--answer-plan ctx 3))))
    (should-not (equal (lr-track--plan-signature a)
                       (lr-track--plan-signature
                        (lr-track--answer-plan (lr-track-ask-test--open-back-later) 2))))))

(ert-deftest lr-track-plan-revalidated-at-key-refuses-when-changed ()
  "The echo offered study from 16:05.  Before he pressed 2 he left and came
back at 16:30, so 2 would now write study from 16:30: a write never shown.
The key refuses and writes nothing (I18).  Unchanged, the same key executes
exactly the plan the echo was built from."
  (lr-track-ask-test--with-ask-world (lr-track-ask-test--open-back)
    (lr-track-ask-test--ask)
    (should installs)
    (setq ctx (lr-track-ask-test--open-back-later))
    (lr-track-ask-test--press ?2)
    (should-not plans)
    (should (lr-track-ask-test--msg-matching msgs "\\`Changed since shown: .*y again\\.\\'")))
  (lr-track-ask-test--with-ask-world (lr-track-ask-test--open-back)
    (lr-track-ask-test--ask)
    (lr-track-ask-test--press ?2)
    (should (= 1 (length plans)))
    (should (equal (lr-track-ask-test--ops ctx 2) (plist-get (car plans) :ops))))
  ;; a clock started elsewhere between the echo and the key
  (lr-track-ask-test--with-ask-world (lr-track-ask-test--open-back)
    (lr-track-ask-test--ask)
    (setq ctx (lr-track-ask-test--with
               ctx :clock (lr-track-ask-test--clock
                           :start (lr-track-ask-test--at 16 38)
                           :end (lr-track-ask-test--at 16 39))))
    (lr-track-ask-test--press ?2)
    (should-not plans)
    (should (lr-track-ask-test--msg-matching msgs "\\`Changed since shown: .*y again\\.\\'"))))

(ert-deftest lr-track-note-ascii-max-70 ()
  "Every note a plan would write is ASCII and at most 70 characters with its
`- lr TAG: ' prefix, even for long or non-ASCII task names."
  (let* ((long "Prepare the quarterly architecture review deck for the platform team offsite")
         (arabic (string #x645 #x631 #x627 #x62c #x639 #x629))
         (task (lambda (ctx name)
                 (lr-track-ask-test--with
                  ctx :clock (lr-track-ask-test--with (plist-get ctx :clock)
                                                      :task name :stream-key nil))))
         (ctxs (list (lr-track-ask-test--open-back)
                     (lr-track-ask-test--open-back 'asleep)
                     (lr-track-ask-test--open-back-no-away)
                     (lr-track-ask-test--paused-back)
                     (lr-track-ask-test--live)
                     (lr-track-ask-test--live t)
                     (lr-track-ask-test--calm-no-clock)
                     (funcall task (lr-track-ask-test--paused-back) long)
                     (funcall task (lr-track-ask-test--paused-back) arabic)
                     (funcall task (lr-track-ask-test--live t) long)
                     (funcall task (lr-track-ask-test--live t) arabic)))
         (keys (append (number-sequence 1 9) '(default)
                       (mapcar (lambda (n) (cons 'away n)) (number-sequence 1 9))))
         (checked 0))
    (dolist (ctx ctxs)
      (dolist (key keys)
        (dolist (op (lr-track-ask-test--ops ctx key))
          (let ((tag (lr-track-ask-test--opget op :tag)))
            (when tag
              (let ((note (format "- lr %s: %s" tag (lr-track-ask-test--opget op :text))))
                (setq checked (1+ checked))
                (should (equal (list note t) (list note (<= (length note) 70))))
                (should (equal (list note t)
                               (list note (lr-track-ask-test--ascii-p note))))))))))
    (should (> checked 50))))


;;;; 7. execution

(ert-deftest lr-track-execute-start-writes-backdated-running-line-and-note ()
  "A start op: a RUNNING clock on the stream, its line backdated to FROM, the
note directly below it at its indentation, saved through the neutralised
save, one undo record, and the message shown without logging it."
  (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
    (let* ((from (- (lr-track-ask-test--minute (float-time)) (* 35 60.0)))
           (message-text "study from 16:05, running (time.org).  SPC d u undoes.")
           (plan (list :ops (list (list :start 2 :from from :tag "now"
                                        :text "from 16:05, your return"))
                       :message message-text))
           (saved nil)
           (real-save (symbol-function 'lr-track--save-buffer)))
      (lr-track-ask-test--recording-messages
        (cl-letf (((symbol-function 'lr-track--save-buffer)
                   (lambda (b) (push b saved) (funcall real-save b))))
          (lr-track--execute-plan plan))
        (should (org-clocking-p))
        (should (equal "study" (lr-track-ask-test--clocked-heading)))
        (should (= from (float-time org-clock-start-time)))
        (let ((hit (lr-track-ask-test--clock-and-note
                    (lr-track-ask-test--entry-lines file "study")
                    (lr-track-ask-test--running-re from))))
          (should hit)
          (should (equal (concat (nth 0 hit) "- lr now: from 16:05, your return")
                         (nth 2 hit))))
        (let ((buf (find-buffer-visiting file)))
          (should (memq buf saved))
          (should-not (buffer-modified-p buf)))
        (should (string-match-p "^- lr now: from 16:05, your return$"
                                (lr-track-ask-test--file-text file)))
        (should (= 1 (length lr-track--undo-stack)))
        (should (seq-some (lambda (m) (and (equal message-text (car m)) (null (cdr m))))
                          msgs))))))

(ert-deftest lr-track-execute-log-writes-closed-line-and-note ()
  "A log op: a closed line under the stream, the note below it at its
indentation (also inside an indented LOGBOOK), saved, one undo record, and
no clock started."
  (dolist (extra (list nil lr-track-ask-test--indented-logbook))
    (lr-track-ask-test--with-time-file
        (lr-track-ask-test--time-text (and extra (list (cons "life" extra))))
      (let ((a (lr-track-ask-test--at 15 40))
            (b (lr-track-ask-test--at 16 5)))
        (lr-track--execute-plan
         (list :ops (list (list :log 7 :from a :to b :tag "away" :text "locked 25m"))
               :message "life 15:40 to 16:05 (0:25) written (time.org)."))
        (should-not (org-clocking-p))
        (let* ((lines (lr-track-ask-test--entry-lines file "life"))
               (hit (lr-track-ask-test--clock-and-note
                     lines (lr-track-ask-test--closed-re a b "0:25"))))
          (should hit)
          (should (equal (concat (nth 0 hit) "- lr away: locked 25m") (nth 2 hit)))
          (should (= (if extra 2 1) (length (lr-track-ask-test--clock-lines lines)))))
        (should-not (buffer-modified-p (find-buffer-visiting file)))
        (should (string-match-p "^[ \t]*- lr away: locked 25m$"
                                (lr-track-ask-test--file-text file)))
        (should (= 1 (length lr-track--undo-stack)))))))

(ert-deftest lr-track-execute-resume-ends-at-pause-then-backdates ()
  "y y on a paused task: org still clocks it, so it is clocked out where it
paused (not now), then the same heading runs again from the start with the
continued note.  Nothing else is written."
  (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
    (let* ((task-file (expand-file-name "t.org" dir))
           (base (lr-track-ask-test--minute (float-time)))
           (t0 (- base (* 180 60.0)))
           (pause (+ t0 (* 90 60.0)))
           (from (- base (* 35 60.0))))
      (with-temp-file task-file (insert "* TODO Write report\n"))
      (lr-track-ask-test--clock-in
       (lr-track-ask-test--heading-marker task-file "TODO Write report") t0)
      (setq lr-track--clock-pause (list :start t0 :paused-at pause :why 'idle))
      (should (= pause (lr-track--paused-at)))
      (lr-track--execute-plan
       (list :ops (list (list :resume :from from :tag "continued"
                              :text "from 16:05, your return"))
             :message "Write report again from 16:05, running."))
      (should (equal "Write report" (lr-track-ask-test--clocked-heading)))
      (should (= from (float-time org-clock-start-time)))
      (let ((lines (lr-track-ask-test--entry-lines task-file "TODO Write report")))
        (should (lr-track-ask-test--clock-and-note
                 lines (lr-track-ask-test--closed-re t0 pause "1:30")))
        (let ((hit (lr-track-ask-test--clock-and-note
                    lines (lr-track-ask-test--running-re from))))
          (should hit)
          (should (equal (concat (nth 0 hit) "- lr continued: from 16:05, your return")
                         (nth 2 hit))))
        (should (= 2 (length (lr-track-ask-test--clock-lines lines)))))
      (should-not (buffer-modified-p (find-buffer-visiting task-file)))
      (should (= 1 (length lr-track--undo-stack))))))


;;;; 7. undo

(ert-deftest lr-track-undo-closed-line-byte-identity ()
  "Write a closed line, undo: the file is byte for byte what it was, with no
LOGBOOK, with one, and with an indented one."
  (should (commandp 'lr-track-undo))
  (dolist (extra (list nil lr-track-ask-test--old-logbook
                       lr-track-ask-test--indented-logbook))
    (lr-track-ask-test--with-time-file
        (lr-track-ask-test--time-text (and extra (list (cons "life" extra))))
      (let ((orig (lr-track-ask-test--file-text file)))
        (lr-track--execute-plan
         (list :ops (list (list :log 7 :from (lr-track-ask-test--at 15 40)
                                :to (lr-track-ask-test--at 16 5)
                                :tag "away" :text "locked 25m"))
               :message "life written"))
        (should-not (equal orig (lr-track-ask-test--file-text file)))
        (lr-track-ask-test--recording-messages
          (lr-track-undo)
          (should (lr-track-ask-test--msg-matching msgs "\\`Undone: ")))
        (should (equal orig (lr-track-ask-test--buffer-text file)))
        (should (equal orig (lr-track-ask-test--file-text file)))
        (should-not lr-track--undo-stack)))))

(ert-deftest lr-track-undo-start-cancels-running-line ()
  "Start a stream, undo: the clock is cancelled and the file is byte for byte
what it was, on a heading with no LOGBOOK and on one with a LOGBOOK."
  (dolist (extra (list nil lr-track-ask-test--old-logbook))
    (lr-track-ask-test--with-time-file
        (lr-track-ask-test--time-text (and extra (list (cons "study" extra))))
      (let ((orig (lr-track-ask-test--file-text file))
            (from (- (lr-track-ask-test--minute (float-time)) (* 35 60.0))))
        (lr-track--execute-plan
         (list :ops (list (list :start 2 :from from :tag "now"
                                :text "from 16:05, your return"))
               :message "study from 16:05"))
        (should (equal "study" (lr-track-ask-test--clocked-heading)))
        (lr-track-undo)
        (should-not (org-clocking-p))
        (should (equal orig (lr-track-ask-test--buffer-text file)))
        (should (equal orig (lr-track-ask-test--file-text file)))))))

(ert-deftest lr-track-undo-end-at-restores-running ()
  "Undo a switch: the new line goes, and the task that was ended runs again
from its original start, its line open again."
  (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
    (let* ((base (lr-track-ask-test--minute (float-time)))
           (t0 (- base 3600.0)))
      (lr-track-ask-test--clock-in (lr-track-ask-test--heading-marker file "avey") t0)
      (lr-track--execute-plan
       (list :ops (list (list :end-at base)
                        (list :start 2 :from base :tag "declared"
                              :text "switched from avey"))
             :message "study from now; avey ended"))
      (should (equal "study" (lr-track-ask-test--clocked-heading)))
      (should (lr-track-ask-test--clock-and-note
               (lr-track-ask-test--entry-lines file "avey")
               (lr-track-ask-test--closed-re t0 base "1:00")))
      (should (= 1 (length lr-track--undo-stack)))
      (lr-track-undo)
      (should (equal "avey" (lr-track-ask-test--clocked-heading)))
      (should (= t0 (float-time org-clock-start-time)))
      (let ((avey (lr-track-ask-test--clock-lines
                   (lr-track-ask-test--entry-lines file "avey"))))
        (should (= 1 (length avey)))
        (should (string-match-p (concat "\\`[ \t]*CLOCK: "
                                        (regexp-quote (lr-track-ask-test--stamp t0))
                                        "\\'")
                                (car avey))))
      (should-not (lr-track-ask-test--clock-lines
                   (lr-track-ask-test--entry-lines file "study")))
      (should-not (string-match-p "- lr declared"
                                  (lr-track-ask-test--buffer-text file))))))

(ert-deftest lr-track-undo-refused-when-changed ()
  "Undo matches by exact text: if he edited what we wrote, undo refuses with
an echo and changes nothing."
  (dolist (edit (list (cons "^- lr away: locked 25m$" "- lr away: locked 25m, tea")
                      (cons " =>  0:25$" " =>  0:26")))
    (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
      (lr-track--execute-plan
       (list :ops (list (list :log 7 :from (lr-track-ask-test--at 15 40)
                              :to (lr-track-ask-test--at 16 5)
                              :tag "away" :text "locked 25m"))
             :message "life written"))
      (with-current-buffer (find-buffer-visiting file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (re-search-forward (car edit))
         (replace-match (cdr edit) t t)))
      (let ((before (lr-track-ask-test--buffer-text file)))
        (lr-track-ask-test--recording-messages
          (lr-track-undo)
          (should (seq-some #'car msgs)))
        (should (equal before (lr-track-ask-test--buffer-text file))))))
  ;; nothing to undo: an echo, no error
  (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
    (lr-track-ask-test--recording-messages
      (lr-track-undo)
      (should (seq-some #'car msgs)))))

(ert-deftest lr-track-undo-stack-keeps-at-most-10 ()
  "The stack holds the last 10 writes; the 11th oldest can no longer be
undone and an undo with nothing left changes nothing."
  (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
    (let ((base (lr-track-ask-test--at 8 0)))
      (dotimes (i 11)
        (lr-track--execute-plan
         (list :ops (list (list :log 7 :from (+ base (* i 1200.0))
                                :to (+ base (* i 1200.0) 600.0)
                                :tag "away" :text (format "locked 10m %d" i)))
               :message "life written")))
      (should (= 10 (length lr-track--undo-stack)))
      (dotimes (_ 10) (lr-track-undo))
      (let ((lines (lr-track-ask-test--entry-lines file "life")))
        (should (= 1 (length (lr-track-ask-test--clock-lines lines))))
        (should (member "- lr away: locked 10m 0" (mapcar #'string-trim lines))))
      (let ((before (lr-track-ask-test--buffer-text file)))
        (lr-track-ask-test--recording-messages
          (lr-track-undo)
          (should (seq-some #'car msgs)))
        (should (equal before (lr-track-ask-test--buffer-text file)))))))


;;;; 6 and 8. the live context and the commands

(ert-deftest lr-track-ask-context-built-from-live-state ()
  "The context is the only door from live state into the pure functions, so
it must carry exactly the contract shape."
  (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
    ;; the shape alone: his key's own sample (`lr-track--settle-at-key',
    ;; pinned by its own tests) would step these hand-made states
    (cl-letf (((symbol-function 'lr-track--settle-at-key) #'ignore))
      (let* ((p (lr-track-ask-test--presence :first-time 1000.0 :return 2000.0))
             (lr-track--presence p)
             (lr-track--last-clock-end 1500.0)
             (ctx (lr-track--ask-context 5000.0)))
        (should (equal 5000.0 (plist-get ctx :now)))
        (should (eq lr-track--presence (plist-get ctx :presence)))
        (should (equal p (plist-get ctx :presence)))
        (should (equal '(1 2 3 4 5 6 7 8 9)
                       (mapcar (lambda (s) (plist-get s :key)) (plist-get ctx :streams))))
        (should (plist-member ctx :clock))
        (should-not (plist-get ctx :clock))
        (should (equal 1500.0 (plist-get ctx :last-end)))
        (should (plist-member ctx :quiet))
        (should-not (plist-get ctx :quiet))
        (should (numberp (plist-get (lr-track--ask-context) :now)))
        ;; a running clock on a stream
        (let* ((base (lr-track-ask-test--minute (float-time)))
               (t0 (- base 3600.0)))
          (lr-track-ask-test--clock-in (lr-track-ask-test--heading-marker file "avey") t0)
          (let ((c (plist-get (lr-track--ask-context base) :clock)))
            (should (equal "avey" (plist-get c :task)))
            (should (eql 1 (plist-get c :stream-key)))
            (should (= t0 (plist-get c :start)))
            (should-not (plist-get c :end))
            (should-not (plist-get c :paused-at))
            (should (eq 'machine (plist-get c :place)))
            (should (markerp (plist-get c :marker)))
            (should (equal "avey" (org-with-point-at (plist-get c :marker)
                                    (org-get-heading t t t t)))))
          ;; paused
          (setq lr-track--clock-pause (list :start t0 :paused-at (+ t0 1800.0) :why 'idle))
          (let ((c (plist-get (lr-track--ask-context base) :clock)))
            (should (= (+ t0 1800.0) (plist-get c :paused-at)))
            (should (eq 'idle (plist-get c :why))))
          ;; ending it records the end, for the next answer's start
          (let ((lr-track--internal t))
            (org-clock-out nil t (seconds-to-time (+ t0 1800.0))))
          (should (= (+ t0 1800.0) lr-track--last-clock-end))
          (should-not (plist-get (lr-track--ask-context base) :clock)))))))

(ert-deftest lr-track-now-N-commands-exist-and-switch ()
  "SPC d N: `I am doing N now'.  From now, never backdated; a running other
stream ends now; the same stream refuses; no streams echoes the setup hint."
  (dotimes (i 9)
    (let ((cmd (intern (format "lr-track-now-%d" (1+ i)))))
      (should (equal (list cmd t) (list cmd (commandp cmd))))))
  (cl-flet ((run (cmd) (let ((this-command cmd) (real-this-command cmd))
                         (call-interactively cmd))))
    ;; no streams yet
    (lr-track-ask-test--with-time-file nil
      (lr-track-ask-test--recording-messages
        (run 'lr-track-now-2)
        (should (lr-track-ask-test--msg-matching msgs "SPC d M")))
      (should-not (file-exists-p file))
      (should-not (org-clocking-p)))
    ;; nothing running: study from now, declared
    (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
      (let ((lr-track--presence (lr-track-ask-test--presence
                                 :first-time (- (float-time) 7200.0)))
            (before (lr-track-ask-test--minute (float-time))))
        (run 'lr-track-now-2)
        (let ((after (float-time)))
          (should (equal "study" (lr-track-ask-test--clocked-heading)))
          (should (<= before (float-time org-clock-start-time) after))
          (should (string-match-p "^[ \t]*- lr declared: "
                                  (lr-track-ask-test--buffer-text file))))))
    ;; avey running: it ends now, writing starts now; then writing again refuses
    (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
      (let* ((lr-track--presence (lr-track-ask-test--presence
                                  :first-time (- (float-time) 7200.0)))
             (base (lr-track-ask-test--minute (float-time)))
             (t0 (- base 3600.0)))
        (lr-track-ask-test--clock-in (lr-track-ask-test--heading-marker file "avey") t0)
        (run 'lr-track-now-4)
        (let ((after (lr-track-ask-test--minute (float-time))))
          (should (equal "writing" (lr-track-ask-test--clocked-heading)))
          (should (<= base (float-time org-clock-start-time) (+ after 60.0)))
          (let ((avey (lr-track-ask-test--clock-lines
                       (lr-track-ask-test--entry-lines file "avey"))))
            (should (= 1 (length avey)))
            (should (string-match "--\\[\\([^]]+\\)\\]" (car avey)))
            (let ((end (float-time (org-time-string-to-time (match-string 1 (car avey))))))
              (should (<= base end after)))))
        (let ((text (lr-track-ask-test--buffer-text file)))
          (lr-track-ask-test--recording-messages
            (run 'lr-track-now-4)
            (should (lr-track-ask-test--msg-matching
                     msgs (concat "\\`writing is already running (since [0-9][0-9]:[0-9][0-9])"
                                  "\\. Nothing written\\.\\'"))))
          (should (equal text (lr-track-ask-test--buffer-text file)))
          (should (equal "writing" (lr-track-ask-test--clocked-heading))))))))


;;;; 8. the one-key map

(ert-deftest lr-track-ask-refused-in-insert-macro-minibuffer-unfocused ()
  "Never a key capture while he is typing, in a macro, in a minibuffer, or
with no frame focused: an echo, and nothing installed."
  (lr-track-ask-test--with-ask-world (lr-track-ask-test--open-back)
    (cl-flet ((refused (label)
                (should (equal (list label nil) (list label installs)))
                (should (equal (list label nil) (list label lr-track--ask-exit-fn)))
                (should (equal (list label nil) (list label lr-track--ask-shown)))
                (should (equal (list label t)
                               (list label (and (lr-track-ask-test--msg-matching
                                                 msgs "\\`Not now:")
                                                t))))
                (setq msgs nil)))
      (setq-local evil-state 'insert)
      (lr-track-ask-test--ask)
      (refused "insert state")
      (setq-local evil-state 'replace)
      (lr-track-ask-test--ask)
      (refused "replace state")
      (setq-local evil-state 'normal)
      (let ((executing-kbd-macro "y")) (lr-track-ask-test--ask))
      (refused "executing a macro")
      (let ((defining-kbd-macro t)) (lr-track-ask-test--ask))
      (refused "defining a macro")
      (cl-letf (((symbol-function 'active-minibuffer-window)
                 (lambda (&rest _) (selected-window))))
        (lr-track-ask-test--ask))
      (refused "a minibuffer is active")
      (cl-letf (((symbol-function 'minibufferp) (lambda (&rest _) t)))
        (lr-track-ask-test--ask))
      (refused "in the minibuffer")
      (setq focused nil)
      (lr-track-ask-test--ask)
      (refused "no frame focused")
      ;; the control: with none of those it asks
      (setq focused t)
      (lr-track-ask-test--ask)
      (should installs)
      (should lr-track--ask-exit-fn))))

(ert-deftest lr-track-ask-installs-map-with-exactly-echoed-keys ()
  "One transient map: no KEEP-PRED, `lr-track--ask-on-exit', no message of
its own, a 15 s timeout; the exit function kept; the echo shown; what was
shown remembered for the key; exactly the echoed keys; any other key exits
and runs normally."
  (should (commandp 'lr-track-ask))
  (lr-track-ask-test--with-ask-world (lr-track-ask-test--paused-back)
    (lr-track-ask-test--ask)
    (should (= 1 (length installs)))
    (let* ((inst (car installs))
           (args (car inst))
           (map (nth 0 args))
           (echo (substring-no-properties (lr-track--ask-echo ctx))))
      (should (keymapp map))
      (should (null (nth 1 args)))
      (should (lr-track-ask-test--fn-is (nth 2 args) 'lr-track--ask-on-exit))
      (should (null (nth 3 args)))
      (should (and (numberp (nth 4 args)) (= 15 (nth 4 args))))
      (should (eq (cdr inst) lr-track--ask-exit-fn))
      (should (lr-track-ask-test--shown-p msgs echo))
      (should (eq (lr-track--ask-state ctx) (car lr-track--ask-shown)))
      (dolist (k (append (number-sequence 1 9) '(default)))
        (let ((plan (lr-track--answer-plan ctx k)))
          (when (plist-get plan :ops)
            (should (equal (cons k (lr-track--plan-signature plan))
                           (cons k (cdr (assoc k (cdr lr-track--ask-shown)))))))))
      (should (equal (lr-track-ask-test--echo-keys echo)
                     (lr-track-ask-test--map-keys map)))
      ;; any other key: the map exits, the key runs as usual
      (lr-track-ask-test--press ?x)
      (should (equal "x" (buffer-string)))
      (should-not (memq map overriding-terminal-local-map))
      (should-not lr-track--ask-exit-fn)
      (should-not lr-track--ask-shown)
      (should-not plans)))
  ;; before setup: the hint, and no key is taken
  (lr-track-ask-test--with-ask-world (lr-track-ask-test--nostreams)
    (lr-track-ask-test--ask)
    (should-not installs)
    (should-not lr-track--ask-exit-fn)
    (should (lr-track-ask-test--shown-p msgs "No streams yet: SPC d M sets them up."))))

(ert-deftest lr-track-ask-exit-on-focus-loss-and-minibuffer-setup ()
  "The map never outlives his attention: focus loss and a minibuffer both
close it, the away map included, and closing clears what was shown."
  (lr-track-ask-test--with-ask-world (lr-track-ask-test--paused-back)
    ;; focus loss
    (lr-track-ask-test--ask)
    (let ((map (car (car (car installs)))))
      (should lr-track--ask-exit-fn)
      (setq focused nil)
      (lr-track--focus-change)
      (fire-soon)
      (should-not lr-track--ask-exit-fn)
      (should-not lr-track--ask-shown)
      (should-not (memq map overriding-terminal-local-map)))
    (setq focused t)
    (lr-track--focus-change)
    ;; a minibuffer opens
    (lr-track-ask-test--ask)
    (let ((map (car (car (car installs)))))
      (should lr-track--ask-exit-fn)
      (with-temp-buffer
        (run-hook-wrapped 'minibuffer-setup-hook
                          (lambda (f) (ignore-errors (funcall f)) nil)))
      (should-not lr-track--ask-exit-fn)
      (should-not lr-track--ask-shown)
      (should-not (memq map overriding-terminal-local-map)))
    ;; the away map, after a
    (lr-track-ask-test--ask)
    (lr-track-ask-test--press ?a)
    (let ((map (car (car (car installs)))))
      (should lr-track--ask-exit-fn)
      (setq focused nil)
      (lr-track--focus-change)
      (fire-soon)
      (should-not lr-track--ask-exit-fn)
      (should-not (memq map overriding-terminal-local-map)))
    (should-not plans)
    ;; and focus loss with nothing up is harmless
    (setq focused t)
    (lr-track--focus-change)
    (setq focused nil)
    (lr-track--focus-change)
    (fire-soon)
    (should-not lr-track--ask-exit-fn)))

(ert-deftest lr-track-arabic-digits-ghain-sheen-ain-in-map ()
  "Under the Arabic layout the same physical keys answer: Arabic-Indic and
Extended digits are the same digit, ghain is y, sheen is a, ain is u."
  (dolist (n '(2 5 8))
    (dolist (key (list (+ ?0 n) (+ #x660 n) (+ #x6f0 n)))
      (lr-track-ask-test--with-ask-world (lr-track-ask-test--paused-back)
        (lr-track-ask-test--ask)
        (should (lr-track-ask-test--lookup (car (car (car installs))) (vector key)))
        (lr-track-ask-test--press key)
        (should (equal (list key (lr-track-ask-test--ops ctx n))
                       (list key (plist-get (car plans) :ops)))))))
  ;; ghain: the default the echo names
  (lr-track-ask-test--with-ask-world (lr-track-ask-test--paused-back)
    (lr-track-ask-test--ask)
    (lr-track-ask-test--press #x63a)
    (should (equal (lr-track-ask-test--ops ctx 'default) (plist-get (car plans) :ops))))
  ;; ain: undo, offered while a recent write is named
  (let ((lr-track--undo-stack (list (list :label "avey from 16:05 removed"
                                          :at (lr-track-ask-test--at 16 38)))))
    (lr-track-ask-test--with-ask-world
        (lr-track-ask-test--with (lr-track-ask-test--paused-back)
                                 :undo "avey from 16:05 removed")
      (lr-track-ask-test--ask)
      (lr-track-ask-test--press #x639)
      (should (or (= 1 undos) (equal '((:undo)) (plist-get (car plans) :ops))))))
  ;; sheen: the away map, then an Arabic digit labels the away
  (lr-track-ask-test--with-ask-world (lr-track-ask-test--paused-back)
    (lr-track-ask-test--ask)
    (lr-track-ask-test--press #x634)
    (should (= 2 (length installs)))
    (let* ((inst (car installs))
           (args (car inst)))
      (should (equal (lr-track-ask-test--sorted lr-track-ask-test--digit-keys)
                     (lr-track-ask-test--map-keys (nth 0 args))))
      (should (null (nth 1 args)))
      (should (and (numberp (nth 4 args)) (= 15 (nth 4 args))))
      (should (eq (cdr inst) lr-track--ask-exit-fn)))
    (should (lr-track-ask-test--shown-p
             msgs (concat "The away 15:40 to 16:05 (25m, no input) was:  "
                          lr-track-ask-test--stream-list)))
    (should-not plans)
    (lr-track-ask-test--press #x667)
    (should (equal (lr-track-ask-test--ops ctx '(away . 7)) (plist-get (car plans) :ops)))
    (should-not lr-track--ask-exit-fn)))


;;;; 9. the Arabic leader

(defun lr-track-ask-test--agenda-command ()
  "Stands in for whatever SPC o a runs."
  (interactive))

(defun lr-track-ask-test--context-command ()
  "Stands in for an lr-context binding under SPC d."
  (interactive))

(ert-deftest lr-track-arabic-leader-lookup ()
  "SPC yeh teh is SPC d j, SPC yeh DIGIT is SPC d N in both Arabic digit
sets, SPC yeh ain is SPC d u, SPC khah sheen is whatever SPC o a was.  The
Latin bindings it reads stay as they were, and installing twice is fine."
  (let ((doom-leader-map (make-sparse-keymap)))
    (define-key doom-leader-map "oa" #'lr-track-ask-test--agenda-command)
    (define-key doom-leader-map "dd" #'lr-track-ask-test--context-command)
    (dotimes (_ 2)
      (lr-track-ask-install-arabic-leader)
      (should (eq 'lr-track-ask (lookup-key doom-leader-map (vector #x64a #x62a))))
      (dotimes (i 9)
        (let ((n (1+ i))
              (cmd (intern (format "lr-track-now-%d" (1+ i)))))
          (should (eq cmd (lookup-key doom-leader-map (vector #x64a (+ #x660 n)))))
          (should (eq cmd (lookup-key doom-leader-map (vector #x64a (+ #x6f0 n)))))))
      (should (eq 'lr-track-undo (lookup-key doom-leader-map (vector #x64a #x639))))
      (should (eq 'lr-track-ask-test--agenda-command
                  (lookup-key doom-leader-map (vector #x62e #x634))))
      (should (eq 'lr-track-ask-test--agenda-command (lookup-key doom-leader-map "oa")))
      (should (eq 'lr-track-ask-test--context-command (lookup-key doom-leader-map "dd"))))))


;;;; 8. the f agenda

(ert-deftest lr-track-agenda-mode-binds-only-y-and-ghain ()
  "In the f agenda, y and ghain ask; teh and noon are j and k; every other
key keeps its meaning, in motion and normal state (1 stays a count, Y still
yanks a line)."
  (skip-unless (featurep 'evil))
  (let ((buf (get-buffer-create "*Org Agenda(f)*")))
    (unwind-protect
        (with-current-buffer buf
          (org-agenda-mode)
          (evil-local-mode 1)
          (let* ((keys (append (number-sequence 32 126)
                               (list #x63a #x62a #x646 #x634 #x639)
                               (number-sequence #x661 #x669)
                               (number-sequence #x6f1 #x6f9)))
                 (snap (lambda ()
                         (mapcar (lambda (k) (cons k (key-binding (vector k)))) keys)))
                 (normal-before (progn (evil-normal-state) (funcall snap)))
                 (motion-before (progn (evil-motion-state) (funcall snap)))
                 (j (key-binding "j"))
                 (k (key-binding "k")))
            (lr-track-agenda-mode 1)
            (dolist (state '(motion normal))
              (if (eq state 'motion) (evil-motion-state) (evil-normal-state))
              (should (equal (list state 'lr-track-ask)
                             (list state (key-binding "y"))))
              (should (equal (list state 'lr-track-ask)
                             (list state (key-binding (vector #x63a)))))
              (dolist (kv (if (eq state 'motion) motion-before normal-before))
                (unless (memq (car kv) (list ?y #x63a #x62a #x646))
                  (should (equal (list state (car kv) (cdr kv))
                                 (list state (car kv)
                                       (key-binding (vector (car kv))))))))
              (should-not (eq 'lr-track-ask (key-binding "1"))))
            (evil-motion-state)
            (should (eq j (key-binding (vector #x62a))))
            (should (eq k (key-binding (vector #x646))))
            (lr-track-agenda-mode -1)
            (should (equal (cdr (assq ?y motion-before)) (key-binding "y")))))
      (with-current-buffer buf (ignore-errors (evil-local-mode -1)))
      (kill-buffer buf))))

(ert-deftest lr-track-header-installed-on-finalize-and-sticky-path ()
  "The header and the mode are installed in the f agenda on finalize, again
after a sticky rebuild (which kills local variables and skips finalize),
never in another agenda, and an error installs nothing and never escapes."
  (lr-track-ask-test--with-agenda-installers
    (let ((f (get-buffer-create "*Org Agenda(f)*"))
          (l (get-buffer-create "*Org Agenda(l)*")))
      (unwind-protect
          (progn
            (with-current-buffer f
              (org-agenda-mode)
              (run-hooks 'org-agenda-finalize-hook)
              (should (bound-and-true-p lr-track-agenda-mode))
              (should (local-variable-p 'header-line-format))
              (should (equal lr-track-ask-test--header-form header-line-format)))
            (with-current-buffer l
              (org-agenda-mode)
              (run-hooks 'org-agenda-finalize-hook)
              (should-not (bound-and-true-p lr-track-agenda-mode))
              (should-not (equal lr-track-ask-test--header-form header-line-format)))
            ;; the sticky path: the rebuild wipes locals, finalize never runs
            (with-current-buffer f
              (org-agenda-mode)
              (should-not (bound-and-true-p lr-track-agenda-mode))
              (should-not (equal lr-track-ask-test--header-form header-line-format))
              (let ((this-command 'next-line)) (run-hooks 'post-command-hook))
              (should-not (equal lr-track-ask-test--header-form header-line-format))
              (dolist (cmd '(salih/org-agenda-no-full-f salih/toggle-agenda-late))
                (org-agenda-mode)
                (let ((this-command cmd)) (run-hooks 'post-command-hook))
                (should (equal (list cmd t)
                               (list cmd (and (bound-and-true-p lr-track-agenda-mode) t))))
                (should (equal (list cmd lr-track-ask-test--header-form)
                               (list cmd header-line-format)))))
            (with-current-buffer l
              (let ((this-command 'salih/org-agenda-no-full-f))
                (run-hooks 'post-command-hook))
              (should-not (bound-and-true-p lr-track-agenda-mode)))
            ;; an error inside the installer: nothing installed, logged, quiet
            (with-current-buffer f
              (org-agenda-mode)
              (let ((log-before (with-current-buffer (get-buffer-create " *lr-track-log*")
                                  (buffer-size))))
                (cl-letf (((symbol-function 'lr-track-agenda-mode)
                           (lambda (&rest _) (error "Boom"))))
                  (should-not (condition-case nil
                                  (progn (run-hooks 'org-agenda-finalize-hook) nil)
                                (error t)))
                  (should-not (condition-case nil
                                  (let ((this-command 'salih/org-agenda-no-full-f))
                                    (run-hooks 'post-command-hook)
                                    nil)
                                (error t))))
                (should-not (equal lr-track-ask-test--header-form header-line-format))
                (should (> (with-current-buffer " *lr-track-log*" (buffer-size))
                           log-before)))))
        (kill-buffer f)
        (kill-buffer l)))))

(ert-deftest lr-track-agenda-install-refreshes-header ()
  "Installing the header also refreshes its text, on finalize and on the
sticky path: the tick skips refreshing until org loads, so the agenda's
first build must not show a stale header.  Other agendas refresh nothing."
  (lr-track-ask-test--with-agenda-installers
    (let ((f (get-buffer-create "*Org Agenda(f)*"))
          (l (get-buffer-create "*Org Agenda(l)*"))
          (calls 0))
      (unwind-protect
          (cl-letf (((symbol-function 'lr-track-ask-refresh-header)
                     (lambda (&rest _) (cl-incf calls) "Time")))
            (with-current-buffer l
              (org-agenda-mode)
              (run-hooks 'org-agenda-finalize-hook))
            (should (= calls 0))
            (with-current-buffer f
              (org-agenda-mode)
              (run-hooks 'org-agenda-finalize-hook))
            (should (= calls 1))
            (with-current-buffer f
              (org-agenda-mode)
              (let ((this-command 'salih/org-agenda-no-full-f))
                (run-hooks 'post-command-hook)))
            (should (= calls 2)))
        (kill-buffer f)
        (kill-buffer l)))))


;;;; 10. never linger

(defconst lr-track-ask-test--prompt-fns
  '(read-string read-char read-char-choice read-key read-from-minibuffer
    completing-read y-or-n-p yes-or-no-p display-buffer pop-to-buffer
    switch-to-buffer select-window set-transient-map
    read-event read-key-sequence read-char-exclusive read-number
    read-multiple-choice read-answer read-passwd read-buffer read-file-name)
  "Functions that read, display, select or capture keys: contract section 10's
list, plus the other reads, which are banned in the same places.")

(defconst lr-track-ask-test--prompt-owners
  '(lr-track-ask lr-track--ask-away-prompt lr-track-setup)
  "The only bodies those functions may appear in.")

(defun lr-track-ask-test--read-forms (file)
  "Every top-level form in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun lr-track-ask-test--scan (form owner)
  "Prompt functions mentioned in FORM outside the allowed command bodies.
Return a list of (DEFINITION . FUNCTION), DEFINITION being the enclosing
named definition (OWNER at the top)."
  (let (hits)
    (cl-labels ((walk (x owner)
                  (cond
                   ((and (consp x)
                         (memq (car x) '(defun cl-defun defmacro cl-defmacro defsubst))
                         (consp (cdr x)) (symbolp (cadr x)))
                    (unless (memq (cadr x) lr-track-ask-test--prompt-owners)
                      (walk (cddr x) (cadr x))))
                   ((consp x)
                    (while (consp x) (walk (car x) owner) (setq x (cdr x)))
                    (when x (walk x owner)))
                   ((vectorp x)
                    (mapc (lambda (e) (walk e owner)) x))
                   ((and (symbolp x) (memq x lr-track-ask-test--prompt-fns))
                    (push (cons owner x) hits)))))
      (walk form owner))
    (nreverse hits)))

(ert-deftest lr-track-source-scan-no-read-or-transient-outside-commands ()
  "No read, display, window selection or key capture anywhere in
lr-track-ask.el or lr-track-presence.el except inside `lr-track-ask', the
away map installer and `lr-track-setup'."
  ;; the scanner itself is not vacuous
  (should (equal '((foo . read-string))
                 (lr-track-ask-test--scan '(defun foo () (read-string "x")) nil)))
  (should (equal '((nil . set-transient-map))
                 (lr-track-ask-test--scan
                  '(add-hook 'h (lambda () (funcall #'set-transient-map m))) nil)))
  (should-not (lr-track-ask-test--scan
               '(defun lr-track-ask () (set-transient-map m nil #'x nil 15)) nil))
  (let ((offenders nil))
    (dolist (rel '("modules/lr-track-ask.el" "modules/lr-track-presence.el"))
      (let ((path (expand-file-name rel lr-track-ask-test--root)))
        (should (equal (list rel t) (list rel (file-readable-p path))))
        (dolist (form (lr-track-ask-test--read-forms path))
          (dolist (hit (lr-track-ask-test--scan form nil))
            (push (list rel (car hit) (cdr hit)) offenders)))))
    (should-not offenders)))

(defmacro lr-track-ask-test--recording-prompts (&rest body)
  "Run BODY with every function in `lr-track-ask-test--prompt-fns' replaced
by a recorder that does nothing.  A call is recorded in `calls' as
\(FUNCTION . THIS-COMMAND) when `this-command' is not one of lr-track's."
  (declare (indent 0) (debug t))
  `(let ((calls nil))
     (cl-letf ,(mapcar (lambda (fn)
                         `((symbol-function ',fn)
                           (lambda (&rest _)
                             (unless (lr-track-ask-test--ours-p this-command)
                               (push (cons ',fn this-command) calls))
                             nil)))
                       lr-track-ask-test--prompt-fns)
       ,@body)))

(defun lr-track-ask-test--day-sample (i t0)
  "Probe sample I of the scripted day starting at T0, one a minute.
0..19 here; 20..26 a 7 min break; 27..49 here; 50..70 no input for 21 min
\(away from 64); 71..90 here; 91..112 locked for 22 min (away from about
105); the Mac sleeps after 112 and wakes 30 s later; 113..199 here."
  (let ((time (+ t0 (* 60.0 i)))
        (wake (- t0 3600.0))
        (idle 2.0)
        (locked nil))
    (cond
     ((<= 20 i 26) (setq idle (* 60.0 (- i 19))))
     ((<= 50 i 70) (setq idle (* 60.0 (- i 49))))
     ((<= 91 i 112) (setq idle (* 60.0 (- i 90)) locked t))
     ((>= i 113) (setq wake (+ t0 (* 60.0 112) 30.0))))
    (list :time time :idle idle :locked locked :wake wake)))

(ert-deftest lr-track-replay-day-zero-prompt-calls ()
  "A scripted day with no command of ours in it: 200 presence samples through
the presence, live-clock and header phases, focus flips (one away from Emacs
for 10 min), agenda builds, the sticky agenda path, three clocks in and out
on a scratch file (two of them ended with O while paused), timers fired as
they come due, and kill-emacs.  Nothing may read, display, select a window
or capture a key."
  (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
    (lr-track-ask-test--with-agenda-installers
      (let* ((task-file (expand-file-name "t.org" dir))
             (t0 (- (lr-track-ask-test--minute (float-time)) (* 200 60.0)))
             (now t0)
             (focused t)
             (timers nil)
             (real-float-time (symbol-function 'float-time))
             (fbuf (get-buffer-create "*Org Agenda(f)*"))
             (task nil)
             (record-timer (lambda (_secs _repeat fn &rest args)
                             (push (cons fn args) timers)
                             (timer-create)))
             (lr-track--presence (lr-track--presence-init))
             (lr-track--last-sample nil)
             (lr-track--ask-exit-fn nil)
             (lr-track--ask-shown nil)
             (lr-track--focused-p t)
             (lr-track--unfocused-since nil)
             (lr-track--phase-failures nil)
             (this-command nil)
             (overriding-terminal-local-map nil))
        (with-temp-file task-file (insert "* TODO Write report\n"))
        (setq task (lr-track-ask-test--heading-marker task-file "TODO Write report"))
        (with-current-buffer fbuf (org-agenda-mode))
        (unwind-protect
            (lr-track-ask-test--recording-prompts
              (cl-letf (((symbol-function 'float-time)
                         (lambda (&optional time)
                           (if time (funcall real-float-time time) now)))
                        ((symbol-function 'frame-focus-state)
                         (lambda (&rest _) focused))
                        ((symbol-function 'run-at-time) record-timer)
                        ((symbol-function 'run-with-timer) record-timer)
                        ((symbol-function 'run-with-idle-timer) record-timer))
                (dotimes (i 200)
                  (setq now (+ t0 (* 60.0 i)))
                  (setq lr-track--last-sample (lr-track-ask-test--day-sample i t0))
                  (dolist (phase '(presence live-clock header))
                    (lr-track--run-phase phase))
                  (pcase i
                    ((or 3 45 120)
                     (with-current-buffer fbuf
                       (org-agenda-mode)
                       (run-hooks 'org-agenda-finalize-hook)))
                    ((or 4 46 121)
                     (with-current-buffer fbuf
                       (let ((this-command 'salih/org-agenda-no-full-f))
                         (run-hooks 'post-command-hook))))
                    ((or 10 85 160)
                     (lr-track-ask-test--clock-in task now))
                    ((or 80 150 199)
                     (when (org-clocking-p)
                       (let ((this-command 'org-agenda-clock-out))
                         (org-clock-out))))
                    ((or 15 30 60 100)
                     (setq focused nil)
                     (lr-track--focus-change))
                    ((or 16 31 61 110)
                     (setq focused t)
                     (lr-track--focus-change)))
                  (let ((due (reverse timers)))
                    (setq timers nil)
                    (dolist (tm due)
                      (condition-case nil (apply (car tm) (cdr tm)) (error nil)))))
                (lr-track--on-kill-emacs))
              (should-not calls)
              (should-not overriding-terminal-local-map)
              (should-not lr-track--ask-exit-fn)
              (should-not (org-clocking-p))
              ;; the day really was replayed
              (should (eq 'here (plist-get lr-track--presence :mode)))
              (should (plist-get lr-track--presence :last-away))
              (dolist (phase '(presence live-clock header))
                (should (equal (cons phase 0)
                               (cons phase (alist-get phase lr-track--phase-failures)))))
              (should (string-prefix-p "Time  " (lr-track--header-cached))))
          (when (buffer-live-p fbuf) (kill-buffer fbuf)))))))


;;;; regressions from the round 1 review

(defun lr-track-ask-test--life-back ()
  "life (an away place) declared 15:39, away 15:40 to 16:05, its line held
at 16:05 since he came back; now 16:12."
  (let ((at #'lr-track-ask-test--at))
    (lr-track-ask-test--ctx
     :now (funcall at 16 12)
     :presence (lr-track-ask-test--presence
                :first-time (funcall at 8 0) :return (funcall at 16 5)
                :last-input (funcall at 16 11) :prev-time (funcall at 16 11 30)
                :last-away (lr-track-ask-test--away 15 40 16 5 'locked))
     :clock (lr-track-ask-test--clock :task "life" :key 7 :place 'away
                                      :start (funcall at 15 39)
                                      :end (funcall at 16 5)))))

(defun lr-track-ask-test--sleep-paused-at-return ()
  "sleep (an away place) from 04:29, paused at 13:30 by his activity at the
Mac, where he came back after the night; now 13:50."
  (let ((at #'lr-track-ask-test--at))
    (lr-track-ask-test--ctx
     :now (funcall at 13 50)
     :presence (lr-track-ask-test--presence
                :first-time (funcall at 1 0) :return (funcall at 13 30)
                :last-input (funcall at 13 49) :prev-time (funcall at 13 49 30)
                :last-away (lr-track-ask-test--away 4 30 13 30 'asleep))
     :clock (lr-track-ask-test--clock :task "sleep" :key 9 :place 'away
                                      :start (funcall at 4 29)
                                      :end (funcall at 13 30)
                                      :paused-at (funcall at 13 30)
                                      :why 'activity))))

(ert-deftest lr-track-regression-answer-steps-pending-wake-sample ()
  "He answers right after a wake, before the tick that would step the sample
the probe already delivered.  The context steps it first: the answer starts
at the wake, never at the return before the sleep."
  (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
    (let* ((at #'lr-track-ask-test--at)
           (lr-track--presence
            (lr-track-ask-test--presence
             :first-time (funcall at 8 0) :return (funcall at 16 49)
             :last-input (funcall at 17 19) :prev-time (funcall at 17 19 10)
             :last-away (lr-track-ask-test--away 16 0 16 49 'idle)))
           (lr-track--presence-stepped nil)
           (lr-track--blind-since nil)
           (lr-track--last-sample (list :time (funcall at 18 0 1) :idle 1.0
                                        :locked nil :wake (funcall at 18 0)))
           (ctx (lr-track--ask-context (funcall at 18 0 10))))
      (should (= (funcall at 18 0)
                 (plist-get (plist-get ctx :presence) :return)))
      (should (= (funcall at 18 0) (lr-track--answer-start ctx)))
      (should (= (funcall at 18 0)
                 (lr-track-ask-test--opget
                  (car (lr-track-ask-test--ops ctx 1)) :from)))
      ;; and only once
      (should (eq lr-track--presence-stepped lr-track--last-sample)))))

(ert-deftest lr-track-regression-away-covered-is-not-offered ()
  "An away a line already books is not offered for a label: the running
away-place clock earned it, or a line that ended this session has it.  A
label there would count the time twice."
  ;; the running practice clock covers its own away
  (let* ((at #'lr-track-ask-test--at)
         (ctx (lr-track-ask-test--ctx
               :now (funcall at 17 1)
               :presence (lr-track-ask-test--presence
                          :first-time (funcall at 8 0) :return (funcall at 16 59)
                          :last-input (funcall at 17 0) :prev-time (funcall at 17 0 30)
                          :last-away (lr-track-ask-test--away 16 20 16 59 'idle))
               :clock (lr-track-ask-test--clock :task "practice" :key 6 :place 'away
                                                :start (funcall at 16 20)
                                                :end (funcall at 16 59)))))
    (should-not (lr-track--ask-labelable-away ctx))
    (should (plist-get (lr-track--answer-plan ctx '(away . 6)) :refuse))
    (should-not (string-match-p "a then" (lr-track--ask-echo ctx))))
  ;; the life clock he is back from covers the lunch
  (let ((ctx (lr-track-ask-test--life-back)))
    (should-not (lr-track--ask-labelable-away ctx))
    (should (plist-get (lr-track--answer-plan ctx '(away . 2)) :refuse)))
  ;; a line that ended this session covers it too
  (let ((ctx (lr-track-ask-test--with
              (lr-track-ask-test--open-back)
              :covered (list (cons (lr-track-ask-test--at 15 39)
                                   (lr-track-ask-test--at 16 5))))))
    (should-not (lr-track--ask-labelable-away ctx))
    (should-not (string-match-p "y a: the away"
                                (lr-track-ask-test--header ctx))))
  ;; a machine line that ends where the away begins does not
  (let ((ctx (lr-track-ask-test--paused-back)))
    (should (lr-track--ask-labelable-away ctx))))

(ert-deftest lr-track-regression-away-labelled-once ()
  "An away he labelled is not offered again, and its label refuses a second
time; undoing the label offers it again."
  (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
    (let* ((at #'lr-track-ask-test--at)
           (lr-track--presence
            (lr-track-ask-test--presence
             :first-time (funcall at 8 0) :return (funcall at 16 5)
             :last-input (funcall at 16 39) :prev-time (funcall at 16 39 30)
             :last-away (lr-track-ask-test--away 15 40 16 5 'locked)))
           (lr-track--last-sample nil)
           (now (funcall at 16 40)))
      (should (lr-track--ask-labelable-away (lr-track--ask-context now)))
      (lr-track--execute-plan
       (lr-track--answer-plan (lr-track--ask-context now) '(away . 7)))
      (should (= 1 (length (lr-track-ask-test--clock-lines
                            (lr-track-ask-test--entry-lines file "life")))))
      (let ((ctx (lr-track--ask-context now)))
        (should-not (lr-track--ask-labelable-away ctx))
        (should (plist-get (lr-track--answer-plan ctx '(away . 7)) :refuse))
        (should (plist-get (lr-track--answer-plan ctx '(away . 6)) :refuse))
        (should-not (string-match-p "a then" (lr-track--ask-echo ctx)))
        (should-not (string-match-p "y a:" (lr-track-ask-test--header ctx))))
      (lr-track-undo)
      (should (lr-track--ask-labelable-away (lr-track--ask-context now))))))

(ert-deftest lr-track-regression-answer-never-resumes-open-line ()
  "Doom sets `org-clock-in-resume': an answer's clock-in must still write a
new line, never take over an open line left in the entry (a crash, an n to
recovery), and undo gives back the original bytes."
  (lr-track-ask-test--with-time-file
      (lr-track-ask-test--time-text
       (list (cons "avey" ":LOGBOOK:\nCLOCK: [2026-10-02 Fri 09:00]\n:END:\n")))
    (let ((org-clock-in-resume t)
          (orig (lr-track-ask-test--file-text file))
          (now (+ (lr-track-ask-test--minute (float-time)) 10.0)))
      (lr-track--execute-plan (lr-track--now-plan (lr-track--ask-context now) 1))
      (should (equal "avey" (lr-track-ask-test--clocked-heading)))
      (should (= (lr-track-ask-test--minute now) (float-time org-clock-start-time)))
      (let ((clocks (lr-track-ask-test--clock-lines
                     (lr-track-ask-test--entry-lines file "avey"))))
        (should (= 2 (length clocks)))
        (should (member "CLOCK: [2026-10-02 Fri 09:00]" clocks)))
      (lr-track-undo)
      (should-not (org-clocking-p))
      (should (equal orig (lr-track-ask-test--buffer-text file)))
      (should (equal orig (lr-track-ask-test--file-text file))))))

(ert-deftest lr-track-regression-end-at-cancel-takes-its-note ()
  "SPC d 1, then SPC d 2 within the same minute: avey's line is cancelled
(no zero-length line), and its `- lr' note goes with it.  Undo puts both
back, byte for byte."
  (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
    (let* ((now (+ (lr-track-ask-test--minute (float-time)) 10.0))
           (orig (lr-track-ask-test--file-text file))
           after-1)
      (lr-track--execute-plan (lr-track--now-plan (lr-track--ask-context now) 1))
      (setq after-1 (lr-track-ask-test--file-text file))
      (should-not (equal orig after-1))
      (lr-track--execute-plan
       (lr-track--now-plan (lr-track--ask-context (+ now 30.0)) 2))
      (should (equal "study" (lr-track-ask-test--clocked-heading)))
      (let ((avey (lr-track-ask-test--entry-lines file "avey")))
        (should-not (seq-some (lambda (l) (string-match-p "- lr \\|CLOCK:\\|:LOGBOOK:" l))
                              avey)))
      (lr-track-undo)
      (should (equal "avey" (lr-track-ask-test--clocked-heading)))
      (should (equal after-1 (lr-track-ask-test--buffer-text file)))
      (should (equal after-1 (lr-track-ask-test--file-text file)))
      (lr-track-undo)
      (should (equal orig (lr-track-ask-test--file-text file))))))

(ert-deftest lr-track-regression-echo-cleared-on-exit ()
  "After the 15 s timeout or focus loss the echo must not go on naming keys
that now run their own commands: the exit clears it, when it still shows."
  (lr-track-ask-test--with-ask-world (lr-track-ask-test--open-back)
    (lr-track-ask-test--ask)
    (let ((echo (substring-no-properties (lr-track--ask-echo ctx))))
      (cl-letf (((symbol-function 'current-message) (lambda () echo)))
        (setq msgs nil)
        (funcall lr-track--ask-exit-fn))
      (should (assoc nil msgs))
      (should-not lr-track--ask-exit-fn)))
  ;; another message took the echo area meanwhile: it stays
  (lr-track-ask-test--with-ask-world (lr-track-ask-test--open-back)
    (lr-track-ask-test--ask)
    (cl-letf (((symbol-function 'current-message) (lambda () "Saved file")))
      (setq msgs nil)
      (funcall lr-track--ask-exit-fn))
    (should-not (assoc nil msgs))))

(ert-deftest lr-track-regression-map-binds-only-stream-digits ()
  "The one-key map takes exactly the digits that have a stream: with three
streams 4 to 9 stay free (the echo says 1-3), and time.org keys outside 1 to
9, or a key given twice, never make an unanswerable entry in the echo."
  (let ((three (lr-track-ask-test--with
                (lr-track-ask-test--paused-back)
                :streams (seq-take (lr-track-ask-test--streams) 3))))
    (lr-track-ask-test--with-ask-world three
      (lr-track-ask-test--ask)
      (let ((map (car (car (car installs))))
            (echo (substring-no-properties (lr-track--ask-echo ctx))))
        (should (string-match-p "1-3: another stream" echo))
        (should-not (lr-track-ask-test--lookup map "4"))
        (should-not (lr-track-ask-test--lookup map (vector #x664)))
        (should (lr-track-ask-test--lookup map "3"))
        (should (equal (lr-track-ask-test--echo-keys echo)
                       (lr-track-ask-test--map-keys map))))))
  (lr-track-ask-test--with-time-file
      (concat lr-track-ask-test--time-preamble
              "* zero\n:PROPERTIES:\n:TRACK_KEY:   0\n:END:\n"
              "* two\n:PROPERTIES:\n:TRACK_KEY:   2\n:END:\n"
              "* again\n:PROPERTIES:\n:TRACK_KEY:   2\n:END:\n"
              "* ten\n:PROPERTIES:\n:TRACK_KEY:   10\n:END:\n")
    (should (equal '((2 . "two"))
                   (mapcar (lambda (s) (cons (plist-get s :key) (plist-get s :name)))
                           (lr-track--streams))))
    (let ((ctx (lr-track-ask-test--with (lr-track-ask-test--open-back-no-away)
                                        :streams (lr-track--streams))))
      (should (equal '(2) (lr-track-ask-test--echo-digits (lr-track--ask-echo ctx)))))))

(ert-deftest lr-track-regression-focus-loss-closes-map-with-mode-off ()
  "With `lr-track-mode' off (SPC d t), y still works, so focus loss must
still close the map: lr-track-ask installs its own focus hook at load."
  (should (advice-function-member-p #'lr-track--ask-focus-change
                                    after-focus-change-function))
  (lr-track-ask-test--with-ask-world (lr-track-ask-test--open-back)
    (let ((lr-track-mode nil))
      (lr-track-ask-test--ask)
      (let ((map (car (car (car installs)))))
        (should lr-track--ask-exit-fn)
        (setq focused nil)
        (lr-track--ask-focus-change)
        (should-not lr-track--ask-exit-fn)
        (should-not (memq map overriding-terminal-local-map))))))

(ert-deftest lr-track-regression-activity-pause-asks-now ()
  "An away-place clock pauses at his return when he stays at the Mac: that
is his being back, so the header asks Now? at once.  An away stream does not
continue (spec 3.2): no y y, y refuses, the digits end it at the pause."
  (let* ((ctx (lr-track-ask-test--sleep-paused-at-return))
         (at #'lr-track-ask-test--at)
         (h (lr-track-ask-test--header ctx))
         (lines (lr-track-ask-test--echo-lines ctx)))
    (should (eq 'paused-back (lr-track--ask-state ctx)))
    (should (string-prefix-p "Time  Now?  sleep paused 13:30 (your return), back 13:30" h))
    (should-not (string-match-p "y y" h))
    (should (string-prefix-p "1-9: another stream from 13:30, sleep stays ended at 13:30"
                             (car lines)))
    (should (plist-get (lr-track--answer-plan ctx 'default) :refuse))
    (should (equal (list (list :end-at (funcall at 13 30)) 1 (funcall at 13 30))
                   (let ((ops (lr-track-ask-test--ops ctx 1)))
                     (list (car ops) (nth 1 (cadr ops))
                           (lr-track-ask-test--opget (cadr ops) :from)))))
    (lr-track-ask-test--with-ask-world ctx
      (lr-track-ask-test--ask)
      (should-not (lookup-key (car (car (car installs))) "y")))))

(ert-deftest lr-track-regression-switch-from-away-place-ends-at-return ()
  "Back at the Mac from lunch (life, an away place, its line held at his
return), he switches: life ends at his return, not now, so the minutes at
the Mac are not life's.  y and a digit starts the new stream there; SPC d N
starts it now."
  (let* ((ctx (lr-track-ask-test--life-back))
         (at #'lr-track-ask-test--at)
         (ops (lr-track-ask-test--ops ctx 1))
         (lines (lr-track-ask-test--echo-lines ctx)))
    (should (eq 'live (lr-track--ask-state ctx)))
    (should (equal (list :end-at (funcall at 16 5)) (car ops)))
    (should (= (funcall at 16 5) (lr-track-ask-test--opget (cadr ops) :from)))
    (should (equal "1-9: switch now (life 15:39 to 16:05, the new one from 16:05)"
                   (car lines)))
    (should (string-prefix-p "6 9: from 16:12 (now, for when you step away)"
                             (cadr lines)))
    ;; an away-place digit is from now
    (should (= (funcall at 16 12)
               (lr-track-ask-test--opget (cadr (lr-track-ask-test--ops ctx 6)) :from)))
    (let ((now-ops (plist-get (lr-track--now-plan ctx 1) :ops)))
      (should (equal (list :end-at (funcall at 16 5)) (car now-ops)))
      (should (= (funcall at 16 12) (lr-track-ask-test--opget (cadr now-ops) :from)))))
  ;; a machine clock still switches at now
  (should (equal (list :end-at (lr-track-ask-test--at 17 20))
                 (car (lr-track-ask-test--ops (lr-track-ask-test--live) 2)))))

(ert-deftest lr-track-regression-echo-names-away-place-start ()
  "Each key writes what the echo shows (I18): away-place digits start now,
and the echo says so when the others start earlier.  In paused-back the
task's own digit is one of the 1-9 the echo names, so it does what they
do: the line stays ended at the pause, the stream runs from the start."
  (let* ((ctx (lr-track-ask-test--open-back))
         (at #'lr-track-ask-test--at)
         (echo (lr-track--ask-echo ctx)))
    (should (string-match-p "\n6 7 9: from 16:40 (now, for when you step away)" echo))
    (dolist (n '(6 7 9))
      (should (= (funcall at 16 40)
                 (lr-track-ask-test--opget (car (lr-track-ask-test--ops ctx n)) :from))))
    (should (= (funcall at 16 5)
               (lr-track-ask-test--opget (car (lr-track-ask-test--ops ctx 2)) :from))))
  ;; nothing to say when every digit starts now
  (should-not (string-match-p "for when you step away"
                              (lr-track--ask-echo (lr-track-ask-test--calm-no-clock))))
  (let* ((ctx (lr-track-ask-test--paused-back))
         (at #'lr-track-ask-test--at)
         (ops (lr-track-ask-test--ops ctx 1)))
    (should (equal (list :end-at (funcall at 15 40)) (car ops)))
    (should (eq :start (car (cadr ops))))
    (should (= (funcall at 16 5) (lr-track-ask-test--opget (cadr ops) :from)))))

(ert-deftest lr-track-regression-setup-preview-golden-layout ()
  "The SPC d M preview is the contract's block, column for column."
  (should (equal (concat
                  "Time setup   nothing is written until RET; q cancels\n"
                  "  create ~/roam/main/time.org with 9 streams (no TODO keywords,"
                  " so it stays out of the agenda):\n"
                  "    1 avey (machine)   2 study (machine)   3 build (machine)"
                  "   4 writing (machine)   5 reading (either)\n"
                  "    6 practice (away)  7 life (away)       8 leisure (either)"
                  "  9 sleep (away)\n"
                  "  your 20 old buckets in life.org stay where they are until a"
                  " later stage moves them\n"
                  "  RET create   q cancel\n")
                 (let ((abbreviated-home-dir "\\`/Users/l\\(/\\|\\'\\)"))
                   (lr-track--setup-text "/Users/l/roam/main/time.org")))))

(ert-deftest lr-track-regression-stream-moved-unsaved ()
  "He moves or deletes a stream heading in time.org without saving: the keys
follow the headings, and a key whose heading is no longer where it was read
writes nothing."
  (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
    (should (lr-track--streams))
    (with-current-buffer (lr-track-ask-test--visit file)
      (goto-char (point-min))
      (re-search-forward "^\\* avey")
      (org-move-subtree-down))
    (should (equal "avey" (org-with-point-at (plist-get (lr-track--stream-by-key 1) :marker)
                            (org-get-heading t t t t))))
    (let ((now (+ (lr-track-ask-test--minute (float-time)) 10.0)))
      (lr-track--execute-plan (lr-track--now-plan (lr-track--ask-context now) 1))
      (should (equal "avey" (lr-track-ask-test--clocked-heading)))
      (lr-track-undo))
    ;; a marker that no longer sits on its stream: refused before the
    ;; first write, so the running clock is not ended either
    (let ((now (+ (lr-track-ask-test--minute (float-time)) 10.0)))
      (lr-track--execute-plan (lr-track--now-plan (lr-track--ask-context now) 2))
      (should (equal "study" (lr-track-ask-test--clocked-heading)))
      (let* ((streams (lr-track--streams))
             (before (lr-track-ask-test--buffer-text file)))
        (set-marker (plist-get (car streams) :marker)
                    (plist-get (cadr streams) :marker))
        (lr-track-ask-test--recording-messages
          (lr-track--execute-plan
           (lr-track--now-plan (lr-track--ask-context (+ now 120.0)) 1))
          (should (lr-track-ask-test--msg-matching msgs "time.org changed")))
        (should (equal "study" (lr-track-ask-test--clocked-heading)))
        (should (equal before (lr-track-ask-test--buffer-text file)))))))

(ert-deftest lr-track-regression-time-file-visit-asks-nothing ()
  "The header tick visits time.org when no buffer has it: an unsafe local
variable there must not reach the question about it (a prompt from a timer)."
  (lr-track-ask-test--with-time-file
      ;; split, so this test file holds no local variables block itself
      (concat (lr-track-ask-test--time-text)
              "\n# Local " "Variables:\n# my-time-file-tweak: 42\n# End:\n")
    (let ((asked nil)
          (lr-track--streams-cache nil))
      (cl-letf (((symbol-function 'hack-local-variables-confirm)
                 (lambda (&rest a) (push a asked) nil)))
        (lr-track--run-phase 'header))
      (should (find-buffer-visiting file))
      (should-not asked))))


;;;; regressions from the round 2 review

(ert-deftest lr-track-regression-answer-settles-the-wake-pause ()
  "avey runs; he typed until 17:19 and closed the lid.  At the wake the
probe's sample lands after the tick that spawned it, and 15 s later he
answers (y then 2, or SPC d 2).  The context settles the clock first: avey
is paused where he left, and both answers end it there, not after the
night."
  (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
    (let* ((at #'lr-track-ask-test--at)
           (left (funcall at 17 19 59))
           (w (+ (funcall at 17 20) 28800.0))
           (orig (symbol-function 'float-time))
           (lr-track-autosave-clock nil)
           (lr-track--state nil)
           (lr-track--presence
            (lr-track-ask-test--presence
             :first-time (funcall at 8 0) :last-input left :prev-time left))
           (lr-track--presence-stepped nil)
           (lr-track--blind-since nil)
           (lr-track--last-sample nil))
      (lr-track-ask-test--clock-in
       (lr-track-ask-test--heading-marker file "avey") (funcall at 16 20))
      (should (lr-track--advance-clock-line left))
      (setq lr-track--last-sample
            (list :time (+ w 1.0) :idle 1.0 :locked nil :wake w))
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional tm) (if tm (funcall orig tm) (+ w 15.0)))))
        ;; the header is display only: it never moves or pauses the line
        (let ((text (lr-track-ask-test--buffer-text file)))
          (lr-track-ask-refresh-header)
          (should-not lr-track--clock-pause)
          (should (equal text (lr-track-ask-test--buffer-text file))))
        (let ((ctx (lr-track--ask-context (+ w 15.0))))
          (should (= left (plist-get (plist-get ctx :clock) :paused-at)))
          (should (equal (list :end-at left)
                         (car (lr-track-ask-test--ops ctx 2))))
          (should (equal (list :end-at left)
                         (car (plist-get (lr-track--now-plan ctx 2) :ops))))
          (should-not (string-match-p "switch now" (lr-track--ask-echo ctx))))))))

(ert-deftest lr-track-regression-resume-empty-line-takes-its-note ()
  "SPC d 1 at 17:00, and he leaves before the line earned a minute: the
clock pauses at its own start.  Back, he answers y y.  The empty line goes
with its `- lr' note (no orphan note), the echo and the message say it
goes, and undo leaves no note behind."
  (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
    (let* ((at #'lr-track-ask-test--at)
           (s (funcall at 17 0))
           (lr-track-autosave-clock nil))
      (lr-track--execute-plan
       (list :ops (list (list :start 1 :from s :tag "declared"
                              :text "from 17:00, said with SPC d 1"))
             :message "started"))
      (should (equal "avey" (lr-track-ask-test--clocked-heading)))
      (setq lr-track--clock-pause (list :start s :paused-at s :why 'idle))
      (let ((ctx (lr-track-ask-test--ctx
                  :now (funcall at 17 25)
                  :presence (lr-track-ask-test--presence
                             :first-time (funcall at 8 0) :return (funcall at 17 20)
                             :last-input (funcall at 17 24)
                             :prev-time (funcall at 17 24 30)
                             :last-away (lr-track-ask-test--away 17 0 17 20 'idle))
                  :clock (lr-track-ask-test--clock :start s :paused-at s :why 'idle))))
        (should (eq 'paused-back (lr-track--ask-state ctx)))
        (let ((line1 (car (lr-track-ask-test--echo-lines ctx))))
          (should (string-match-p "y: avey again from 17:20 (its empty 17:00 line goes)"
                                  line1))
          (should (string-match-p "the empty 17:00 line of avey goes" line1))
          (should-not (string-match-p "stays" line1)))
        (should (string-match-p "Its empty 17:00 line is removed"
                                (plist-get (lr-track--answer-plan ctx 'default)
                                           :message)))
        (should (string-match-p "the empty 17:00 line of avey is removed"
                                (plist-get (lr-track--answer-plan ctx 2) :message))))
      (lr-track--execute-plan
       (list :ops (list (list :resume :from (funcall at 17 20) :tag "continued"
                              :text "from 17:20, your return"))
             :message "resumed"))
      (let ((avey (lr-track-ask-test--entry-lines file "avey")))
        (should-not (seq-some (lambda (l) (string-match-p "- lr declared" l)) avey))
        (should (seq-some (lambda (l) (string-match-p "- lr continued" l)) avey))
        (should (= 1 (length (lr-track-ask-test--clock-lines avey)))))
      (should (string-match-p "its empty 17:00 line stays removed"
                              (plist-get (car lr-track--undo-stack) :label)))
      (lr-track-undo)
      (should-not (org-clocking-p))
      (should-not (seq-some (lambda (l) (string-match-p "- lr \\|CLOCK:" l))
                            (lr-track-ask-test--entry-lines file "avey"))))))

(ert-deftest lr-track-regression-undo-key-names-recent-write-only ()
  "u after y undoes only a write the echo names, and only one under 10 min
old.  After a lunch, y then a mistyped u must not delete the morning's
line: u is then neither named nor bound.  SPC d u keeps the whole stack."
  (let* ((at #'lr-track-ask-test--at)
         (ctx (lr-track-ask-test--paused-back))
         (label "avey from 16:05 removed"))
    (let ((lr-track--undo-stack (list (list :label label :at (funcall at 16 35)))))
      (should (equal label (lr-track--ask-undo-label (funcall at 16 40))))
      (should (equal label (plist-get (lr-track--ask-context (funcall at 16 40))
                                      :undo))))
    (let ((lr-track--undo-stack (list (list :label label :at (funcall at 13 2)))))
      (should-not (lr-track--ask-undo-label (funcall at 16 40))))
    ;; nothing recent: no u
    (lr-track-ask-test--with-ask-world ctx
      (lr-track-ask-test--ask)
      (let ((map (car (car (car installs)))))
        (should-not (string-match-p "u: undo" (lr-track--ask-echo ctx)))
        (should-not (lookup-key map "u"))
        (should-not (lookup-key map (vector #x639)))))
    (let ((lr-track--undo-stack (list (list :label label :at (funcall at 16 38)))))
      ;; a recent write: the echo names it and u undoes it
      (lr-track-ask-test--with-ask-world (lr-track-ask-test--with ctx :undo label)
        (lr-track-ask-test--ask)
        (should (string-match-p (regexp-quote (format "u: undo (%s)" label))
                                (lr-track--ask-echo ctx)))
        (lr-track-ask-test--press ?u)
        (should (= 1 undos)))
      ;; the newest write is another one than the echo named: u refuses
      (lr-track-ask-test--with-ask-world
          (lr-track-ask-test--with ctx :undo "study from 16:30 removed")
        (lr-track-ask-test--ask)
        (lr-track-ask-test--press ?u)
        (should (= 0 undos))
        (should (lr-track-ask-test--msg-matching msgs "Changed since shown"))))))

(ert-deftest lr-track-regression-echo-fits-frame ()
  "Every echo line fits his 186-column frame: a long task name (cut at a
word where the paused line names it twice), a long undo label, the away
part and the away-place clause together."
  (let* ((at #'lr-track-ask-test--at)
         (long "Prepare the quarterly review of the tracker and the agenda")
         (label (concat "study from 16:30 removed; " long " runs again from 14:10"))
         (paused (lr-track-ask-test--with
                  (lr-track-ask-test--paused-back)
                  :clock (lr-track-ask-test--clock
                          :task long :key nil :start (funcall at 14 10)
                          :end (funcall at 15 40) :paused-at (funcall at 15 40)
                          :why 'idle)
                  :undo label))
         (cases (list paused
                      (lr-track-ask-test--with
                       (lr-track-ask-test--live t)
                       :clock (lr-track-ask-test--clock
                               :task long :key nil :start (funcall at 16 5)
                               :end (funcall at 17 17))
                       :undo label)
                      (lr-track-ask-test--with
                       (lr-track-ask-test--live)
                       :clock (lr-track-ask-test--clock
                               :task long :key nil :start (funcall at 16 5)
                               :end (funcall at 17 17))
                       :undo label)
                      (lr-track-ask-test--with (lr-track-ask-test--open-back)
                                               :undo label)
                      (lr-track-ask-test--with
                       (lr-track-ask-test--calm-paused)
                       :clock (lr-track-ask-test--clock
                               :task long :key nil :start (funcall at 14 10)
                               :end (funcall at 15 40)
                               :paused-at (funcall at 15 40) :why 'idle)
                       :undo label))))
    (dolist (ctx cases)
      (let ((lines (lr-track-ask-test--echo-lines ctx)))
        (should (<= (length lines) 2))
        (dolist (line lines)
          (should (equal (list line t) (list line (<= (string-width line) 186)))))
        (should (string-match-p "u: undo (study from 16:30" (mapconcat #'identity lines "\n")))))
    (should (string-prefix-p "y: Prepare the quarterly again from 16:05"
                             (car (lr-track-ask-test--echo-lines paused))))))

(ert-deftest lr-track-regression-header-shows-names-verbatim ()
  "The header's own words are ASCII with no arrows; his heading is his text
and shows as he wrote it, an Arabic one too, with a left-to-right mark after
a right-to-left name so the hint after it keeps its place."
  (let* ((at #'lr-track-ask-test--at)
         (arabic (concat "Arabic " (string #x627 #x644 #x639 #x645 #x644)))
         (ctx (lr-track-ask-test--with
               (lr-track-ask-test--live)
               :clock (lr-track-ask-test--clock :task arabic :key nil
                                                :start (funcall at 16 5)
                                                :end (funcall at 17 17))))
         (h (lr-track-ask-test--header ctx))
         (i (string-search arabic h)))
    (should i)
    (should (eq #x200e (aref h (+ i (length arabic)))))
    (let ((rest (replace-regexp-in-string
                 (regexp-quote (concat arabic (string #x200e))) "" h)))
      (should (lr-track-ask-test--ascii-p rest))
      (should-not (string-match-p lr-track-ask-test--arrows-re rest)))
    (should (string-search arabic (lr-track--ask-echo ctx)))
    ;; an ASCII name gets no mark
    (should (lr-track-ask-test--ascii-p
             (lr-track-ask-test--header (lr-track-ask-test--live))))))

(ert-deftest lr-track-regression-unseen-pause-says-so ()
  "A clock paused because no sample saw the time after its line (presence
started over) says that, not that he left."
  (should (equal "Time  avey paused 15:40 (nothing seen after)  |  y: switch"
                 (lr-track-ask-test--header
                  (lr-track-ask-test--with
                   (lr-track-ask-test--calm-paused)
                   :clock (lr-track-ask-test--clock
                           :start (lr-track-ask-test--at 14 10)
                           :end (lr-track-ask-test--at 15 40)
                           :paused-at (lr-track-ask-test--at 15 40)
                           :why 'unknown))))))

(ert-deftest lr-track-regression-nostreams-names-existing-file ()
  "time.org exists but holds no stream (a renamed TRACK_KEY, no headings):
SPC d M refuses to write over it, so no door may send him there.  The header,
the echo, SPC d N and the setup refusal name the file and what to add."
  (lr-track-ask-test--with-time-file
      (replace-regexp-in-string ":TRACK_KEY:" ":KEY:" (lr-track-ask-test--time-text))
    (let ((ctx (lr-track--ask-context (lr-track-ask-test--at 16 40))))
      (should (eq 'nostreams (lr-track--ask-state ctx)))
      (should (equal file (plist-get ctx :time-file)))
      (should (equal "Time  time.org has no streams: add top-level headings with TRACK_KEY 1 to 9"
                     (lr-track-ask-test--header ctx)))
      (dolist (text (list (lr-track--ask-echo ctx)
                          (plist-get (lr-track--now-plan ctx 1) :refuse)
                          (plist-get (lr-track--answer-plan ctx 1) :refuse)))
        (should (string-match-p "TRACK_KEY 1 to 9): add them there" text))
        (should-not (string-match-p "SPC d M" text)))
      (lr-track-ask-test--recording-messages
        (lr-track-setup)
        (should (lr-track-ask-test--msg-matching msgs "TRACK_KEY 1 to 9.*Nothing written"))
        (should-not (get-buffer "*Time setup*")))))
  ;; no file at all: the setup hint, as before
  (lr-track-ask-test--with-time-file nil
    (let ((ctx (lr-track--ask-context (lr-track-ask-test--at 16 40))))
      (should-not (plist-get ctx :time-file))
      (should (equal "Time  no streams yet: SPC d M sets them up"
                     (lr-track-ask-test--header ctx)))
      (should (equal "No streams yet: SPC d M sets them up."
                     (lr-track--ask-echo ctx))))))

;;;; regressions from the round 3 review

(ert-deftest lr-track-regression-undo-switch-restores-line-in-place ()
  "Undo of a switch gives the bytes back even when the running line is not
the first in its LOGBOOK (a labelled away went on top of it).  The ended
line runs again where it stands, with its note under it, instead of a new
line at the top and the note left under another one."
  (lr-track-ask-test--with-time-file
      (lr-track-ask-test--time-text (list (cons "avey" lr-track-ask-test--old-logbook)))
    (let* ((base (lr-track-ask-test--minute (float-time)))
           (t0 (- base 2400.0)))
      (lr-track--execute-plan
       (list :ops (list (list :start 1 :from t0 :tag "declared"
                              :text "from 16:21, said with SPC d 1"))
             :message "avey from now"))
      (lr-track--advance-clock-line (- base 60.0))
      (lr-track--execute-plan
       (list :ops (list (list :log 1 :from (- base 10800.0) :to (- base 9000.0)
                              :tag "away" :text "labelled"))
             :message "the away was avey"))
      (let* ((before (lr-track-ask-test--buffer-text file))
             (lines (lr-track-ask-test--entry-lines file "avey")))
        ;; the labelled line is above the running one
        (should (string-match-p (regexp-quote (lr-track-ask-test--stamp (- base 10800.0)))
                                (car (lr-track-ask-test--clock-lines lines))))
        (lr-track--execute-plan
         (list :ops (list (list :end-at base)
                          (list :start 2 :from base :tag "declared"
                                :text "switched from avey"))
               :message "study from now"))
        (should (equal "study" (lr-track-ask-test--clocked-heading)))
        (lr-track-undo)
        (should (equal "avey" (lr-track-ask-test--clocked-heading)))
        (should (= t0 (float-time org-clock-start-time)))
        (should (equal before (lr-track-ask-test--buffer-text file)))
        (should (equal "- lr declared: from 16:21, said with SPC d 1"
                       (string-trim (car (lr-track--note-below-running-line)))))))))

(defun lr-track-ask-test--drive (buffer events &optional after-ask)
  "Run EVENTS through the real command loop, starting in BUFFER.
F11 runs `lr-track-ask'.  AFTER-ASK, when non-nil, runs once right after it,
from `post-command-hook': what a timer, a server or a frame switch does
before his next key.  In batch the echo area never shows a message, so no
key read runs `echo-area-clear-hook'; the echo is noted as seen right after
`lr-track-ask', as a terminal's next key finds it when nothing replaced it."
  (let* ((map (make-sparse-keymap))
         (emulation-mode-map-alists (cons (list (cons t map))
                                          emulation-mode-map-alists))
         (seen (lambda ()
                 (when (eq this-command 'lr-track-ask)
                   (setq lr-track--ask-echo-seen t))))
         (h nil))
    (define-key map [f11] #'lr-track-ask)
    (define-key map [f12] #'exit-recursive-edit)
    (add-hook 'post-command-hook seen)
    (when after-ask
      (setq h (lambda ()
                (when (eq this-command 'lr-track-ask)
                  (remove-hook 'post-command-hook h)
                  (funcall after-ask))))
      (add-hook 'post-command-hook h 50))
    (switch-to-buffer buffer)
    (unwind-protect
        (progn
          (setq unread-command-events (append (list 'f11) events (list 'f12)))
          (recursive-edit))
      (remove-hook 'post-command-hook seen)
      (when h (remove-hook 'post-command-hook h))
      (setq unread-command-events nil))))

(ert-deftest lr-track-regression-answer-key-only-where-shown ()
  "After y, a key answers only in the window, buffer and evil state the echo
was shown in.  He moves to another frame and types 1 in insert state there,
a timer selects another window, a server shows another buffer where he
types 3 j, or a hook puts the agenda in emacs state: each key runs as he
meant it, nothing is written, and the map is gone.  Focus on another Emacs
frame closes the map at once: Emacs keeps focus, the echo's frame lost it."
  (lr-track-ask-test--with-ask-world (lr-track-ask-test--open-back)
    (lr-track-ask-test--ask)
    (should lr-track--ask-exit-fn)
    (cl-letf (((symbol-function 'lr-track--frame-focused-now-p) (lambda () t)))
      (setq focused nil)
      (lr-track--ask-focus-change))
    (should-not lr-track--ask-exit-fn)
    (should-not lr-track--ask-where))
  (skip-unless (fboundp 'evil-local-mode))
  (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
    (let* ((now (float-time))
           (plans nil)
           (lr-track--presence (lr-track-ask-test--presence
                                :first-time (- now 3600) :last-input (- now 5)
                                :prev-time (- now 2)))
           (lr-track--last-sample nil)
           (lr-track--ask-exit-fn nil)
           (lr-track--ask-shown nil)
           (agenda (get-buffer-create " *lr-track test agenda*"))
           (notes (get-buffer-create " *lr-track test notes*")))
      (cl-flet ((reset (b text state)
                  (with-current-buffer b
                    (erase-buffer) (insert text) (goto-char (point-min))
                    (evil-local-mode 1)
                    (pcase state
                      ('insert (goto-char (point-max)) (evil-insert-state))
                      (_ (evil-normal-state)))))
                (ours-up ()
                  (and lr-track--ask-map
                       (memq lr-track--ask-map overriding-terminal-local-map)
                       t)))
        (cl-letf (((symbol-function 'lr-track--execute-plan)
                   (lambda (plan &rest _) (push plan plans)))
                  ((symbol-function 'frame-focus-state) (lambda (&rest _) t)))
          (unwind-protect
              (progn
                ;; the control: in the agenda's window and state, 1 answers
                (reset agenda "agenda\n" 'normal)
                (lr-track-ask-test--drive agenda (list ?1))
                (should (= 1 (length plans)))
                (setq plans nil)
                ;; another frame takes the keys: insert state in another buffer
                (reset agenda "agenda\n" 'normal)
                (reset notes "list: " 'insert)
                (lr-track-ask-test--drive
                 agenda (list ?1)
                 (lambda () (switch-to-buffer notes) (evil-insert-state)))
                (should (equal "list: 1" (with-current-buffer notes (buffer-string))))
                (should-not plans)
                (should-not (ours-up))
                (should-not lr-track--ask-exit-fn)
                ;; a timer selects another window, in insert state
                (reset agenda "agenda\n" 'normal)
                (reset notes "S1: " 'insert)
                (delete-other-windows)
                (let ((w2 (split-window-below)))
                  (set-window-buffer w2 notes)
                  (lr-track-ask-test--drive
                   agenda (list ?1)
                   (lambda () (select-window w2)
                     (with-current-buffer notes (evil-insert-state)))))
                (delete-other-windows)
                (should (equal "S1: 1" (with-current-buffer notes (buffer-string))))
                (should-not plans)
                (should-not (ours-up))
                ;; a server shows another buffer in the agenda's window: 3 j
                (reset agenda "agenda\n" 'normal)
                (reset notes "a\nb\nc\nd\ne\nf\n" 'normal)
                (lr-track-ask-test--drive
                 agenda (list ?3 ?j)
                 (lambda () (switch-to-buffer notes) (evil-normal-state)))
                (should (= 4 (with-current-buffer notes (line-number-at-pos))))
                (should-not plans)
                (should-not (ours-up))
                ;; a hook puts the agenda in emacs state: 1 is his text
                (reset agenda "agenda" 'normal)
                (lr-track-ask-test--drive
                 agenda (list ?1)
                 (lambda () (with-current-buffer agenda
                              (goto-char (point-max)) (evil-emacs-state))))
                (should-not plans)
                (should-not (ours-up))
                (should (equal "agenda1" (with-current-buffer agenda (buffer-string)))))
            (when (functionp lr-track--ask-exit-fn) (funcall lr-track--ask-exit-fn))
            (delete-other-windows)
            (dolist (b (list agenda notes))
              (with-current-buffer b (set-buffer-modified-p nil))
              (kill-buffer b))))))))

(ert-deftest lr-track-regression-activity-pause-he-stayed ()
  "He starts life (an away stream) at 15:30, about to leave, but stays at
the Mac: 15 min of activity pause it at its start.  His last return, 13:32,
is from before life started, so the header must not name it, nor the night
before it: life paused 15:30 because he stayed."
  (let* ((at #'lr-track-ask-test--at)
         (ctx (lr-track-ask-test--ctx
               :now (funcall at 15 49)
               :presence (lr-track-ask-test--presence
                          :first-time (funcall at 1 0) :return (funcall at 13 32)
                          :last-input (funcall at 15 48 30)
                          :prev-time (funcall at 15 48 40)
                          :last-away (lr-track-ask-test--away 4 31 13 32 'asleep))
               :clock (lr-track-ask-test--clock :task "life" :key 7 :place 'away
                                                :start (funcall at 15 30)
                                                :end nil
                                                :paused-at (funcall at 15 30)
                                                :why 'activity))))
    (should (equal (funcall at 15 30) (lr-track--ask-back-at ctx)))
    (should (eq 'paused-back (lr-track--ask-state ctx)))
    (should (equal "Time  Now?  life paused 15:30 (you stayed at the Mac)  |  y then 1-9: another"
                   (lr-track-ask-test--header ctx)))
    ;; he did leave and come back after it started: the return is named
    (let ((back (lr-track-ask-test--sleep-paused-at-return)))
      (should (string-prefix-p "Time  Now?  sleep paused 13:30 (your return), back 13:30"
                               (lr-track-ask-test--header back))))))

(ert-deftest lr-track-regression-unknown-pause-comes-due ()
  "A clock paused because nothing was seen after its line (presence started
over at 14:05, the line read 13:59) asks Now? once he has been seen 2 min,
like any return: the header says when he was seen again, and y y continues
from there.  Before, it stayed calm all day."
  (let* ((at #'lr-track-ask-test--at)
         (ctx (lr-track-ask-test--ctx
               :now (funcall at 14 30)
               :presence (lr-track-ask-test--presence
                          :first-time (funcall at 14 5)
                          :last-input (funcall at 14 29 30)
                          :prev-time (funcall at 14 29 30))
               :clock (lr-track-ask-test--clock :start (funcall at 13 30)
                                                :end (funcall at 13 59)
                                                :paused-at (funcall at 13 59)
                                                :why 'unknown))))
    (should (eq 'paused-back (lr-track--ask-state ctx)))
    (should (equal (concat "Time  Now?  avey paused 13:59 (nothing seen after), seen again 14:05"
                           "  |  y y: avey again from 14:05   y then 1-9: another")
                   (lr-track-ask-test--header ctx)))
    (should (equal (funcall at 14 5)
                   (lr-track-ask-test--opget
                    (car (lr-track-ask-test--ops ctx 'default)) :from)))
    ;; seen under 2 min: calm still
    (should (eq 'calm (lr-track--ask-state
                       (lr-track-ask-test--with ctx :now (funcall at 14 6)))))))


;;;; regressions from the round 3.2 review

(defmacro lr-track-ask-test--at-now (now &rest body)
  "Run BODY with `(float-time)' answering NOW, a float; conversions still
convert."
  (declare (indent 1) (debug t))
  (let ((orig (make-symbol "orig")) (n (make-symbol "n")))
    `(let ((,orig (symbol-function 'float-time)) (,n ,now))
       (cl-letf (((symbol-function 'float-time)
                  (lambda (&optional tm) (if tm (funcall ,orig tm) ,n))))
         ,@body))))

(defun lr-track-ask-test--feed (at idle &optional locked)
  "Step the sample (AT IDLE LOCKED) into presence, as the tick does: it is
the newest sample (`lr-track--last-sample') and is stepped once.  The wake
is a fixed one: no sleep."
  (let ((sample (list :time (float at) :idle idle :locked locked
                      :wake (lr-track-ask-test--at 6 0))))
    (setq lr-track--last-sample sample)
    (lr-track--step-sample sample)))

(ert-deftest lr-track-regression-same-minute-pause-is-empty ()
  "SPC d 1 at 17:00:05 starts avey at 17:00; he types until 17:00:28 and
leaves, so the clock pauses at 17:00:28, inside the start's minute.  Back,
y y, y then 2 and SPC d 2 each remove that empty line with its note, and
say so, as his own SPC c o does (org drops the 0:00 line).  None writes a
17:00 to 17:01 line he never earned."
  (dolist (how '(resume digit now clock-out))
    (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
      (let* ((at #'lr-track-ask-test--at)
             (s (funcall at 17 0))
             (paused (+ s 28.0))
             (lr-track-autosave-clock nil)
             (org-clock-out-remove-zero-time-clocks t))
        (lr-track--execute-plan
         (list :ops (list (list :start 1 :from s :tag "declared"
                                :text "from 17:00, said with SPC d 1"))
               :message "started"))
        (setq lr-track--clock-pause (list :start s :paused-at paused :why 'idle))
        (let ((ctx (lr-track-ask-test--ctx
                    :now (funcall at 17 25)
                    :presence (lr-track-ask-test--presence
                               :first-time (funcall at 8 0)
                               :return (funcall at 17 20)
                               :last-input (funcall at 17 24)
                               :prev-time (funcall at 17 24 30)
                               :last-away (list :from paused
                                                :to (funcall at 17 20)
                                                :kind 'idle))
                    :clock (lr-track-ask-test--clock :start s :paused-at paused
                                                     :why 'idle))))
          (should (eq 'paused-back (lr-track--ask-state ctx)))
          (let ((line1 (car (lr-track-ask-test--echo-lines ctx))))
            (should (string-match-p "(its empty 17:00 line goes)" line1))
            (should-not (string-match-p "stays" line1)))
          (let ((plan (pcase how
                        ('resume (lr-track--answer-plan ctx 'default))
                        ('digit (lr-track--answer-plan ctx 2))
                        ('now (lr-track--now-plan ctx 2)))))
            (when plan
              (should (string-match-p "empty 17:00 line.* removed"
                                      (plist-get plan :message))))
            (lr-track-ask-test--recording-messages
              (if plan
                  (lr-track--execute-plan plan)
                (with-current-buffer (marker-buffer org-clock-marker)
                  (org-clock-out)))
              (should (lr-track-ask-test--msg-matching
                       msgs (if plan "empty 17:00 line.* removed"
                              "no time recorded"))))))
        (let ((avey (lr-track-ask-test--entry-lines file "avey")))
          (should-not (seq-some (lambda (l) (string-match-p "17:00\\]--" l)) avey))
          (should-not (seq-some (lambda (l) (string-match-p "- lr declared" l))
                                avey))
          (should (equal (list how (if (eq how 'resume) 1 0))
                         (list how (length (lr-track-ask-test--clock-lines avey))))))))))

(ert-deftest lr-track-regression-unseen-stretch-is-not-an-away ()
  "avey runs from 14:00 and he types without a break until 15:31.  From
15:00:40 to 15:25:30 nothing saw him: no tick ran (App Nap), or the probe
failed every tick.  The clock pauses where it was last seen, but nothing
says he left: the header says nothing was seen and when he was seen again,
never his last input or an away, and the stretch is not offered to label."
  (dolist (cause '(gap probe))
    (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
      (let* ((at #'lr-track-ask-test--at)
             (lr-track-autosave-clock nil)
             (lr-track--state nil)
             (lr-track--presence (lr-track--presence-init))
             (lr-track--presence-stepped nil)
             (lr-track--blind-since nil)
             (lr-track--last-sample nil))
        (lr-track--execute-plan
         (list :ops (list (list :start 1 :from (funcall at 14 0) :tag "declared"
                                :text "from 14:00, said with SPC d 1"))
               :message "started"))
        (cl-loop for x from (funcall at 13 59) to (funcall at 15 0 30) by 30
                 do (lr-track-ask-test--feed x 2.0))
        (lr-track-ask-test--at-now (funcall at 15 0 30) (lr-track--tick-live-clock))
        (when (eq cause 'probe)
          (cl-loop for x from (funcall at 15 1) to (funcall at 15 25) by 30
                   do (lr-track--step-sample (list :time (float x) :idle 'unknown
                                                   :locked 'unknown :wake nil))))
        (cl-loop for x from (funcall at 15 25 30) to (funcall at 15 31) by 30
                 do (lr-track-ask-test--feed x 2.0))
        (lr-track-ask-test--at-now (funcall at 15 31)
          (lr-track--tick-live-clock)
          (should (equal (list cause 'unseen)
                         (list cause (plist-get (plist-get lr-track--presence
                                                           :last-away)
                                                :kind))))
          (let* ((ctx (lr-track--ask-context (funcall at 15 31)))
                 (header (lr-track-ask-test--header ctx))
                 (echo (substring-no-properties (lr-track--ask-echo ctx))))
            (should (eq 'paused-back (lr-track--ask-state ctx)))
            (should (equal (list cause t)
                           (list cause
                                 (and (string-match-p
                                       "avey paused 15:00 (nothing seen after), seen again 15:25"
                                       header)
                                      t))))
            (dolist (text (list header echo))
              (dolist (re '("your last input" "no input" "m away" "the away"))
                (should-not (string-match-p re text))))
            (should-not (lr-track--ask-labelable-away ctx))
            (should (plist-get (lr-track--answer-plan ctx '(away . 7)) :refuse))))))
    ;; with nothing running, the open-back header says the same
    (let ((ctx (lr-track-ask-test--open-back 'unseen)))
      (should (equal (concat "Time  Now?  seen again 16:05 after 25m not seen, nothing running"
                             "  |  y then 1-9: what you do now, from 16:05")
                     (lr-track-ask-test--header ctx))))))

(ert-deftest lr-track-regression-time-file-visit-says-nothing ()
  "time.org has auto-save data newer than the file, as after a crash (the
live line is saved every 5 min, auto-save runs every 30 s).  The tick's
header phase visits it first: no warning in the echo area from a timer, and
no `sit-for' after it."
  (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
    (set-file-times file (time-subtract nil 3600))
    (with-temp-file (expand-file-name "#time.org#" dir)
      (insert (lr-track-ask-test--time-text) "\nunsaved\n"))
    (let ((waits nil)
          (lr-track--streams-cache nil))
      (lr-track-ask-test--recording-messages
        (cl-letf (((symbol-function 'sit-for)
                   (lambda (&rest a) (push a waits) t)))
          ;; batch skips `after-find-file's warnings on `noninteractive'
          (let ((noninteractive nil))
            (lr-track--run-phase 'header)))
        (should (find-buffer-visiting file))
        (should-not waits)
        (should-not (lr-track-ask-test--msg-matching msgs "auto save data"))))))

(ert-deftest lr-track-regression-failing-save-keeps-the-pause-for-y ()
  "avey runs; the last tick came 10 min before he closed the lid at 17:19.
He answers 15 s after the wake, before the tick that would step its sample.
The settle advances the line, and that advance's throttled save fails (an
after-save-hook error, as org-roam's DB update can raise).  The step still
records the pause, so y sees avey paused where he left, not a live clock
whose switch books the night."
  (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
    (let* ((at #'lr-track-ask-test--at)
           (left (funcall at 17 19 59))
           (w (+ (funcall at 17 20) 28800.0))
           (lr-track-autosave-clock t)
           (lr-track--state nil)
           (lr-track--presence
            (lr-track-ask-test--presence
             :first-time (funcall at 8 0) :last-input left :prev-time left))
           (lr-track--presence-stepped nil)
           (lr-track--blind-since nil)
           (lr-track--last-sample nil))
      (lr-track-ask-test--clock-in
       (lr-track-ask-test--heading-marker file "avey") (funcall at 16 20))
      (should (lr-track--advance-clock-line (- left 600)))
      (with-current-buffer (lr-track-ask-test--visit file)
        (add-hook 'after-save-hook (lambda () (error "database is locked")) nil t))
      (setq lr-track--last-sample
            (list :time (+ w 1.0) :idle 1.0 :locked nil :wake w))
      (lr-track-ask-test--at-now (+ w 15.0)
        (let ((ctx (lr-track--ask-context (+ w 15.0))))
          (should (equal left (plist-get (plist-get ctx :clock) :paused-at)))
          (should (equal (list :end-at left)
                         (car (lr-track-ask-test--ops ctx 2))))
          (should-not (string-match-p "switch now" (lr-track--ask-echo ctx)))))
      (with-current-buffer (lr-track-ask-test--visit file)
        (kill-local-variable 'after-save-hook)))))

(ert-deftest lr-track-regression-early-answer-holds-the-block ()
  "Lunch, locked, 16:00 to 16:25:05.  At 16:25:30, before any sample saw
him back, he starts study with SPC d 2, or with y then 2.  At 16:27 he is
called away and locks again until 16:50.  The block his line began in is
held when its return is stepped, so the second away is its own (and can be
labelled), not the lunch with his line merged into it."
  (dolist (how '(now digit))
    (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
      (let* ((at #'lr-track-ask-test--at)
             (lr-track-autosave-clock nil)
             (lr-track--state nil)
             (lr-track--presence (lr-track--presence-init))
             (lr-track--presence-stepped nil)
             (lr-track--blind-since nil)
             (lr-track--last-sample nil)
             (lunch (funcall at 16 0)))
        (cl-loop for x from (funcall at 15 0) to lunch by 30
                 do (lr-track-ask-test--feed x 1.0))
        (cl-loop for x from (+ lunch 60) to (funcall at 16 25) by 60
                 do (lr-track-ask-test--feed x (- x lunch) t))
        (should (eq 'away (plist-get lr-track--presence :mode)))
        (lr-track-ask-test--at-now (funcall at 16 25 30)
          (if (eq how 'now)
              (lr-track--now 2)
            (lr-track--execute-plan
             (lr-track--answer-plan (lr-track--ask-context) 2))))
        (should (equal "study" (lr-track-ask-test--clocked-heading)))
        (lr-track-ask-test--feed (funcall at 16 26) 2.0)
        (lr-track-ask-test--feed (funcall at 16 26 30) 2.0)
        (lr-track-ask-test--feed (funcall at 16 27) 1.0)
        (cl-loop for x from (funcall at 16 28) to (funcall at 16 50) by 60
                 do (lr-track-ask-test--feed x (- x (funcall at 16 26 59)) t))
        (lr-track-ask-test--feed (funcall at 16 50 30) 2.0)
        (let ((gone (plist-get lr-track--presence :last-away)))
          (should (equal (list how (funcall at 16 26 59) 'locked)
                         (list how (plist-get gone :from) (plist-get gone :kind)))))))))

;;;; regressions from the round 3.3 review

(ert-deftest lr-track-regression-answer-key-needs-its-echo ()
  "y, then another message replaces the echo (a timer's or a process's), or
the echo area is emptied: the question is no longer on screen, so a digit
he types is his own (a count), not an answer.  It runs its own binding,
nothing is written, and the map is gone.  With the echo still showing, the
digit answers."
  (dolist (screen (list 'ours "Reverting buffer `todo.org'." nil))
    (lr-track-ask-test--with-ask-world (lr-track-ask-test--open-back)
      (lr-track-ask-test--ask)
      (let* ((lr-track-ask-test--screen screen)
             (cmd (lr-track-ask-test--press ?1)))
        (if (eq screen 'ours)
            (should (= 1 (length plans)))
          (should (equal (list screen nil 'self-insert-command)
                         (list screen plans cmd)))
          (should-not lr-track--ask-exit-fn)
          (should-not (memq lr-track--ask-map overriding-terminal-local-map)))))))

(ert-deftest lr-track-regression-answer-steps-its-own-return ()
  "Coffee 14:30 to 14:50 and lunch 16:00 to 16:25, both locked.  He presses
y at 16:25:30, 25 s after unlocking: the last tick (16:25:00) saw the lock
screen, so presence still says away.  His key is input in Emacs, unlocked,
now: the return is stepped first, so the digits start at 16:25 and `a'
names the lunch, never the coffee 1.5 h before.  After a stalled tick (a
sleep, App Nap) only the probe can say what happened: no older away is
offered then either."
  (dolist (stalled '(nil t))
    (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
      (let* ((at #'lr-track-ask-test--at)
             (lr-track-autosave-clock nil)
             (lr-track--state nil)
             (lr-track--presence (lr-track--presence-init))
             (lr-track--presence-stepped nil)
             (lr-track--blind-since nil)
             (lr-track--last-sample nil)
             (lr-track--interval 60.0)
             (lr-track--last-tick (funcall at 16 (if stalled 20 25))))
        (cl-loop for x from (funcall at 13 0) to (funcall at 14 30) by 30
                 do (lr-track-ask-test--feed x 0.0))
        (cl-loop for x from (funcall at 14 31) to (funcall at 14 50) by 60
                 do (lr-track-ask-test--feed x (- x (funcall at 14 30)) t))
        (cl-loop for x from (funcall at 14 50 30) to (funcall at 16 0) by 30
                 do (lr-track-ask-test--feed x 0.0))
        (cl-loop for x from (funcall at 16 1) to (funcall at 16 25) by 60
                 do (lr-track-ask-test--feed x (- x (funcall at 16 0)) t))
        (should (eq 'away (plist-get lr-track--presence :mode)))
        (cl-letf (((symbol-function 'lr-track-emacs-idle-seconds) (lambda () 0.0)))
          (lr-track-ask-test--at-now (funcall at 16 25 30)
            (let* ((ctx (lr-track--ask-context))
                   (away (lr-track--ask-labelable-away ctx))
                   (echo (substring-no-properties (lr-track--ask-echo ctx))))
              (should (equal (funcall at 16 25) (lr-track--answer-start ctx)))
              (if stalled
                  (progn
                    (should (eq 'away (plist-get lr-track--presence :mode)))
                    (should-not away)
                    (should-not (string-match-p "a then" echo)))
                (should (equal (list (funcall at 16 0) (funcall at 16 25) 'locked)
                               (list (plist-get away :from) (plist-get away :to)
                                     (plist-get away :kind))))
                (should (string-match-p
                         "a then 1-9: the away 16:00 to 16:25 (25m, locked) was that"
                         echo))))))))))

(ert-deftest lr-track-regression-label-in-glance-holds-block ()
  "He locks at 01:30 for the night; at 03:00 he unlocks, labels that away
as sleep (a then 9), and locks again at 03:02.  His answer was written in
that 2 min here-block, so it is no glance: the night stays split there, and
in the morning the away 03:02 to 08:00 is offered to label on its own, not
merged into the one he labelled."
  (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
    (let* ((at #'lr-track-ask-test--at)
           (lr-track-autosave-clock nil)
           (lr-track--state nil)
           (lr-track--presence (lr-track--presence-init))
           (lr-track--presence-stepped nil)
           (lr-track--blind-since nil)
           (lr-track--last-sample nil)
           (lr-track--last-tick nil))
      (cl-letf (((symbol-function 'lr-track-emacs-idle-seconds) (lambda () 0.0)))
        (cl-loop for x from (funcall at 0 30) to (funcall at 1 30) by 30
                 do (lr-track-ask-test--feed x 0.0))
        (cl-loop for x from (funcall at 1 31) to (funcall at 3 0) by 60
                 do (lr-track-ask-test--feed x (- x (funcall at 1 30)) t))
        (cl-loop for x from (funcall at 3 0 30) to (funcall at 3 1) by 30
                 do (lr-track-ask-test--feed x 0.0))
        (lr-track-ask-test--at-now (funcall at 3 1 30)
          (let ((ctx (lr-track--ask-context)))
            (should (equal (funcall at 1 30)
                           (plist-get (lr-track--ask-labelable-away ctx) :from)))
            (lr-track--execute-plan (lr-track--answer-plan ctx '(away . 9)))))
        (lr-track-ask-test--feed (funcall at 3 2) 0.0)
        (cl-loop for x from (funcall at 3 3) to (funcall at 8 0) by 60
                 do (lr-track-ask-test--feed x (- x (funcall at 3 2)) t))
        (cl-loop for x from (funcall at 8 0 30) to (funcall at 8 3) by 30
                 do (lr-track-ask-test--feed x 0.0))
        (lr-track-ask-test--at-now (funcall at 8 3 30)
          (let* ((ctx (lr-track--ask-context))
                 (away (lr-track--ask-labelable-away ctx)))
            (should (equal (list (funcall at 3 2) (funcall at 8 0) 'locked)
                           (list (plist-get away :from) (plist-get away :to)
                                 (plist-get away :kind))))))
        (should (lr-track-ask-test--entry-lines file "sleep"))
        (should (seq-some (lambda (l) (string-match-p "01:30\\]--\\[.*03:00\\]" l))
                          (lr-track-ask-test--entry-lines file "sleep")))))))

(ert-deftest lr-track-regression-away-stream-at-max-no-yy ()
  "SPC d 7 (life, an away stream, max 4:00) at 16:00 as he leaves; back at
21:01, it paused at 20:00, its maximum.  An away stream earns while he is
away, so it never continues from his return at the Mac: no y y in the
header, the echo or the keys, and y y's plan refuses.  The digits are the
way on (an away stream's own digit starts now)."
  (let* ((at #'lr-track-ask-test--at)
         (ctx (lr-track-ask-test--ctx
               :now (funcall at 21 4)
               :presence (lr-track-ask-test--presence
                          :first-time (funcall at 8 0) :return (funcall at 21 1)
                          :last-input (funcall at 21 3 30)
                          :prev-time (funcall at 21 3 30)
                          :last-away (list :from (funcall at 16 0 30)
                                           :to (funcall at 21 1) :kind 'locked))
               :clock (lr-track-ask-test--clock
                       :task "life" :key 7 :place 'away
                       :start (funcall at 16 0) :end (funcall at 20 0)
                       :paused-at (funcall at 20 0) :why 'max))))
    (should (eq 'paused-back (lr-track--ask-state ctx)))
    (should (equal (concat "Time  Now?  life paused 20:00 (its maximum), back 21:01"
                           " after 5h1m away  |  y then 1-9: another")
                   (lr-track-ask-test--header ctx)))
    (let ((echo (substring-no-properties (lr-track--ask-echo ctx))))
      (should-not (memq ?y (lr-track-ask-test--echo-keys echo)))
      (should (string-prefix-p "1-9: another stream from 21:01, life stays ended at 20:00"
                               echo)))
    (should (plist-get (lr-track--answer-plan ctx 'default) :refuse))
    (lr-track-ask-test--with-ask-world ctx
      (lr-track-ask-test--ask)
      (should-not (lr-track-ask-test--lookup (car (car (car installs))) "y")))))

(ert-deftest lr-track-regression-unseen-return-says-seen-again ()
  "He typed all along; the probe failed 14:00 to 14:25, so presence saw him
again at 14:24 after a stretch no sample saw.  The header says seen again,
and so must the echo and the note his answer writes, digit or y y: never
`your return', which says he left."
  (let* ((at #'lr-track-ask-test--at)
         (p (lr-track-ask-test--presence
             :first-time (funcall at 8 0) :return (funcall at 14 24 59)
             :last-input (funcall at 14 35 30) :prev-time (funcall at 14 35 30)
             :last-away (list :from (funcall at 13 59 30) :to (funcall at 14 24 59)
                              :kind 'unseen)))
         (open (lr-track-ask-test--ctx :now (funcall at 14 36) :presence p))
         (paused (lr-track-ask-test--ctx
                  :now (funcall at 14 36) :presence p
                  :clock (lr-track-ask-test--clock
                          :start (funcall at 13 5) :end (funcall at 13 59)
                          :paused-at (funcall at 13 59 30) :why 'unknown))))
    (should (eq 'open-back (lr-track--ask-state open)))
    (should (string-prefix-p "Now, from 14:24 (seen again, 12m ago):  "
                             (car (lr-track-ask-test--echo-lines open))))
    (should (equal "from 14:24, seen again"
                   (lr-track-ask-test--opget (car (lr-track-ask-test--ops open 2))
                                             :text)))
    (should (eq 'paused-back (lr-track--ask-state paused)))
    (should (equal "from 14:24, seen again"
                   (lr-track-ask-test--opget (car (lr-track-ask-test--ops paused 'default))
                                             :text)))
    (dolist (ctx (list open paused))
      (dolist (text (list (lr-track-ask-test--header ctx)
                          (substring-no-properties (lr-track--ask-echo ctx))))
        (should-not (string-match-p "your return" text))))))

(ert-deftest lr-track-regression-paused-end-he-moved-yy ()
  "avey paused at 14:40 (he left the Mac).  Back, he moves the paused line's
end to 15:00 with S-up, the org way.  y y ends that line where he put it,
and runs avey again from his return: never back at 14:40."
  (lr-track-ask-test--with-time-file (lr-track-ask-test--time-text)
    (let* ((at #'lr-track-ask-test--at)
           (lr-track-autosave-clock nil)
           (lr-track--state nil)
           (lr-track--presence (lr-track--presence-init))
           (lr-track--presence-stepped nil)
           (lr-track--blind-since nil)
           (lr-track--last-sample nil)
           (lr-track--last-tick nil)
           (org-time-stamp-rounding-minutes '(0 1))
           (left (funcall at 14 40)))
      (cl-letf (((symbol-function 'lr-track-emacs-idle-seconds) (lambda () 0.0)))
        (lr-track-ask-test--feed (funcall at 13 59) 0.0)
        (lr-track--execute-plan
         (list :ops (list (list :start 1 :from (funcall at 14 0) :tag "declared"
                                :text "from 14:00, said with SPC d 1"))
               :message "started"))
        ;; typing until 14:40, 30 min away, back at 15:10:30
        (cl-loop for x from (funcall at 14 0 30) to (funcall at 15 13) by 30
                 do (lr-track-ask-test--feed
                     x (if (or (<= x left) (> x (funcall at 15 10))) 0.0 (- x left)))
                 (lr-track-ask-test--at-now x (lr-track--tick-live-clock)))
        (should (equal left (lr-track--paused-at)))
        (with-current-buffer (marker-buffer org-clock-marker)
          (save-excursion
            (goto-char org-clock-marker)
            (re-search-forward "--\\[" (line-end-position))
            (re-search-forward "[0-9][0-9]:[0-9][0-9]" (line-end-position))
            (backward-char 1)
            (dotimes (_ 20) (org-shiftup))))
        (lr-track-ask-test--at-now (funcall at 15 14)
          (let ((ctx (lr-track--ask-context)))
            (should (eq 'paused-back (lr-track--ask-state ctx)))
            (should (equal (funcall at 15 0)
                           (plist-get (plist-get ctx :clock) :paused-at)))
            (lr-track--execute-plan (lr-track--answer-plan ctx 'default))))
        (let ((avey (lr-track-ask-test--entry-lines file "avey")))
          (should (seq-some (lambda (l)
                              (string-match-p
                               (regexp-quote
                                (concat (lr-track-ask-test--stamp (funcall at 14 0)) "--"
                                        (lr-track-ask-test--stamp (funcall at 15 0))))
                               l))
                            avey))
          (should (seq-some (lambda (l)
                              (string-match-p
                               (concat "\\`CLOCK: "
                                       (regexp-quote (lr-track-ask-test--stamp
                                                      (funcall at 15 10)))
                                       "\\'")
                               (string-trim l)))
                            avey)))))))

(provide 'lr-track-ask-test)
;;; lr-track-ask-test.el ends here
