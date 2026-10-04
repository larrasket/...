;;; lr-track-test.el --- tests for the deterministic tracker (v4) -*- lexical-binding: t; -*-

;;; Commentary:
;; The questions (situations), their answers (plans and real writes on scratch
;; org files), undo, the readers, the triggers and the keys.  Every write test
;; runs in a temp directory with the straight org build; nothing touches ~/roam.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'cl-lib)

(let ((b "/Users/l/.emacs.d/.local/straight/build-31.0.91/"))
  (dolist (p '("org" "evil"))
    (when (file-directory-p (concat b p)) (push (concat b p) load-path))))
(when (boundp 'native-comp-jit-compilation)
  (setq native-comp-jit-compilation nil))

(defmacro add-hook! (&rest _) nil)
(defmacro after! (&rest body) `(progn ,@body))
(defmacro defadvice! (&rest _) nil)
(defvar lr-track-test--tmp (file-name-as-directory (make-temp-file "lr-track-v4" t)))
(defvar doom-data-dir (expand-file-name "etc/" lr-track-test--tmp))
(defvar doom-cache-dir (expand-file-name "cache/" lr-track-test--tmp))
(make-directory doom-data-dir t)
(make-directory doom-cache-dir t)

(add-to-list 'load-path (expand-file-name "modules"
                                          (locate-dominating-file
                                           (or load-file-name buffer-file-name)
                                           "modules")))
(require 'org)
(require 'org-clock)
(require 'lr-track)
(require 'lr-track-ask)

(setq org-clock-persist nil
      make-backup-files nil
      org-clock-out-remove-zero-time-clocks t
      org-clock-in-resume t)

;;;; fixtures

(defvar doom-leader-map nil)
(defconst lr-track-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar lr-track-test--now nil "When set, `float-time' with no argument returns it.")

(defmacro lr-track-test--world (&rest body)
  "BODY with a fresh time.org (the seeds), an empty state, no clock, and a
fake clock when `lr-track-test--now' is set.  Binds `dir' and `tfile'."
  (declare (indent 0) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "lr-track-w" t)))
          (tfile (expand-file-name "time.org" dir))
          (lr-track-time-file tfile)
          (lr-track--streams-cache nil)
          (lr-track--state nil)
          (lr-track--last-clock-end nil)
          (lr-track--undo-stack nil)
          (real (symbol-function 'float-time))
          (doom-data-dir (file-name-as-directory (expand-file-name "etc" dir))))
     (make-directory doom-data-dir t)
     (with-temp-file tfile (insert (lr-track--time-file-text)))
     (unwind-protect
         (cl-letf (((symbol-function 'float-time)
                    (lambda (&optional tm)
                      (if (or tm (null lr-track-test--now)) (funcall real tm)
                        lr-track-test--now))))
           ,@body)
       (when (org-clocking-p)
         (let ((lr-track--internal t)) (ignore-errors (org-clock-cancel))))
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b) (string-prefix-p dir (buffer-file-name b)))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (ignore-errors (delete-directory dir t)))))

(defun lr-track-test--text (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun lr-track-test--buf-text (file)
  (with-current-buffer (find-buffer-visiting file)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun lr-track-test--t (h m)
  "Float time today at H:M (local), minute exact."
  (let ((d (decode-time)))
    (float-time (encode-time (list 0 m h (decoded-time-day d) (decoded-time-month d)
                                   (decoded-time-year d) nil -1 nil)))))

(defun lr-track-test--ctx (&rest kv)
  "A hand-built context: the seeds as streams unless KV says otherwise."
  (let ((ctx (list :now (lr-track-test--t 14 0) :trigger 'agenda
                   :streams (mapcar (lambda (s) (list :key (nth 0 s) :name (nth 1 s)))
                                    lr-track--stream-seeds)
                   :clock nil :last-stop nil :confirmed nil :restarted nil)))
    (while kv (setq ctx (plist-put ctx (pop kv) (pop kv))))
    ctx))

(defun lr-track-test--kinds (ctx)
  (mapcar (lambda (s) (plist-get s :kind)) (lr-track--pop-situations ctx)))

;;;; situations (pure)

(ert-deftest lr-track-situation-gap-after-10-minutes ()
  (let ((now (lr-track-test--t 14 0)))
    (should (equal '(gap) (lr-track-test--kinds
                           (lr-track-test--ctx :last-stop (- now 600)))))
    (should (null (lr-track-test--kinds
                   (lr-track-test--ctx :last-stop (- now 599)))))
    (should (null (lr-track-test--kinds (lr-track-test--ctx :last-stop nil))))))

(ert-deftest lr-track-situation-stopped-replaces-gap ()
  (let ((now (lr-track-test--t 14 0)))
    (should (equal '(stopped)
                   (lr-track-test--kinds
                    (lr-track-test--ctx :trigger 'stopped :last-stop (- now 3600)))))))

(ert-deftest lr-track-situation-restarted-first-and-no-gap ()
  (let ((now (lr-track-test--t 14 0)))
    (should (equal '(restarted)
                   (lr-track-test--kinds
                    (lr-track-test--ctx :restarted (list :task "avey" :at (- now 600))
                                        :last-stop (- now 7200)))))))

(ert-deftest lr-track-situation-clock-limit-overnight-stale ()
  (let* ((now (lr-track-test--t 14 0))
         (clock (lambda (start &optional limit)
                  (list :task "avey" :start start :limit (or limit 36000.0) :stream-key 1))))
    ;; limit: 11 h > 10 h
    (should (equal '(limit) (lr-track-test--kinds
                             (lr-track-test--ctx :clock (funcall clock (- now 39600))))))
    ;; a yes after the limit was crossed silences it
    (should-not (memq 'limit (lr-track-test--kinds
                              (lr-track-test--ctx :clock (funcall clock (- now 39600))
                                                  :confirmed (- now 60)))))
    ;; overnight: started yesterday 23:00 (before today's 05:00), 15 h, no limit
    (let ((start (- (lr-track-test--t 5 0) 21600)))
      (should (equal '(overnight)
                     (lr-track-test--kinds
                      (lr-track-test--ctx :clock (funcall clock start 99999.0)))))
      ;; a yes today: no longer overnight, and stale only 3 h after the yes
      (should (null (lr-track-test--kinds
                     (lr-track-test--ctx :clock (funcall clock start 99999.0)
                                         :confirmed (- now 3600)))))
      (should (equal '(stale)
                     (lr-track-test--kinds
                      (lr-track-test--ctx :clock (funcall clock start 99999.0)
                                          :confirmed (- now 10800))))))
    ;; stale: exactly 3 h since the start
    (should (equal '(stale) (lr-track-test--kinds
                             (lr-track-test--ctx :clock (funcall clock (- now 10800))))))
    (should (null (lr-track-test--kinds
                   (lr-track-test--ctx :clock (funcall clock (- now 10799))))))))

(ert-deftest lr-track-situation-none-without-streams ()
  (let ((now (lr-track-test--t 14 0)))
    (should (null (lr-track-test--kinds
                   (lr-track-test--ctx :streams nil :last-stop (- now 7200)))))))

(ert-deftest lr-track-day-start-is-0500 ()
  (should (= (lr-track--day-start (lr-track-test--t 5 0)) (lr-track-test--t 5 0)))
  (should (= (lr-track--day-start (lr-track-test--t 14 0)) (lr-track-test--t 5 0)))
  (should (= (lr-track--day-start (lr-track-test--t 4 59))
             (- (lr-track-test--t 5 0) 86400))))

;;;; parsing his typed times, his keys

(ert-deftest lr-track-parse-ago-and-after ()
  (let* ((now (lr-track-test--t 14 0))
         (ago (lr-track--pop-parse-ago (lr-track-test--t 10 0) now))
         (after (lr-track--pop-parse-after (lr-track-test--t 10 0) now)))
    (should (= (funcall ago "13:00") (lr-track-test--t 13 0)))
    (should (= (funcall ago "30m") (lr-track-test--t 13 30)))
    (should (= (funcall ago "now") now))
    (should-not (funcall ago "9:00"))          ; before the anchor
    (should-not (funcall ago "5h"))            ; 09:00, before the anchor
    (should-not (funcall ago "soon"))
    (should (= (funcall after "40m") (lr-track-test--t 10 40)))
    (should (= (funcall after "11:15") (lr-track-test--t 11 15)))
    (should-not (funcall after "15:00"))))     ; after now

(ert-deftest lr-track-parsers-kept ()
  (should (equal (lr-track--parse-clock "3:30pm") '(15 . 30)))
  (should (= (lr-track--duration-minutes "1h30") 90.0))
  (should-not (lr-track--parse-clock "90")))

(ert-deftest lr-track-latin-reads-arabic ()
  (should (eql (lr-track--pop-latin #x662) ?2))
  (should (eql (lr-track--pop-latin #x6f7) ?7))
  (should (eql (lr-track--pop-latin #x63a) ?y))
  (should (eql (lr-track--pop-latin #x633) ?s))
  (should (eql (lr-track--pop-latin ?a) ?a))
  (should (equal (lr-track--pop-latin-digits (string #x661 #x665 ?: #x663 #x660)) "15:30")))

(ert-deftest lr-track-read-key-choices-later-other ()
  (should (eql (ert-simulate-keys (vector ?2)
                 (lr-track--pop-read-key "q " '(?1 ?2)))
               ?2))
  (should (eq (ert-simulate-keys (vector ?\r)
                (lr-track--pop-read-key "q " '(?1 ?2)))
              'later))
  ;; Arabic digit two answers 2
  (should (eql (ert-simulate-keys (vector #x662)
                 (lr-track--pop-read-key "q " '(?1 ?2)))
               ?2))
  ;; any other key ends the questions and is put back to run as usual
  (let ((unread-command-events nil))
    (should (equal (ert-simulate-keys (vector ?j)
                     (list (catch 'lr-track-pop-done
                             (lr-track--pop-read-key "q " '(?1 ?2)))
                           ;; put back first, ahead of what was still queued
                           (car unread-command-events)))
                   (list 'other ?j))))
  ;; the drop event (focus lost) quits, nothing chosen
  (should (eq (condition-case nil
                  (ert-simulate-keys (vector 'lr-track-pop-drop)
                    (lr-track--pop-read-key "q " '(?1)))
                (quit 'quit))
              'quit)))

;;;; header and setup text

(ert-deftest lr-track-time-file-text-has-off-and-limits ()
  (let ((txt (lr-track--time-file-text)))
    (should (string-match-p "^\\* off\n:PROPERTIES:\n:TRACK_KEY:   0\n:END:" txt))
    (should (string-match-p "^\\* avey\n:PROPERTIES:\n:TRACK_KEY:   1\n:TRACK_MAX:   10:00\n:END:" txt))
    (should (string-match-p "^\\* sleep\n:PROPERTIES:\n:TRACK_KEY:   9\n:TRACK_MAX:   16:00" txt))
    (should-not (string-match-p "^\\*+ TODO\\|TRACK_PLACE" txt))))

(ert-deftest lr-track-header-states ()
  (let ((now (lr-track-test--t 14 0)))
    (should (equal (lr-track--header
                    (lr-track-test--ctx :clock (list :task "avey" :start (lr-track-test--t 12 30)
                                                     :limit 36000.0)))
                   "Time  avey running since 12:30 (1h30m)  |  SPC d j: ask"))
    (should (equal (lr-track--header
                    (lr-track-test--ctx :clock (list :task "avey" :start (- now 39600)
                                                     :limit 36000.0)))
                   (format "Time  avey running 11h since %s, past its 10h limit  |  SPC d j: ask"
                           (lr-track--pop-when (- now 39600) now))))
    (should (equal (lr-track--header (lr-track-test--ctx :last-stop (lr-track-test--t 13 0)))
                   "Time  nothing running since 13:00 (1h, your last stop)  |  SPC d j: ask"))
    (should (equal (lr-track--header (lr-track-test--ctx))
                   "Time  nothing running  |  SPC d j: ask"))
    (let ((lr-track-time-file "/nonexistent/time.org"))
      (should (equal (lr-track--header (lr-track-test--ctx :streams nil))
                     "Time  no streams yet: SPC d M sets them up  |  SPC d j: ask")))))

;;;; plans (pure)

(ert-deftest lr-track-plan-start-from-switches-and-refuses ()
  (let* ((now (lr-track-test--t 14 0))
         (clock (list :task "avey" :start (lr-track-test--t 12 0) :stream-key 1 :limit 36000.0))
         (p (lr-track--plan-start-from (lr-track-test--ctx :clock clock) 2
                                       (lr-track-test--t 13 0) "since" "typed")))
    (should (equal (mapcar #'car (plist-get p :ops)) '(:end-at :start)))
    (should (= (nth 1 (car (plist-get p :ops))) (lr-track-test--t 13 0)))
    (should (plist-get (lr-track--plan-start-from (lr-track-test--ctx :clock clock) 1 now "now" "x")
                       :refuse))
    (should (plist-get (lr-track--plan-start-from (lr-track-test--ctx :clock clock) 2
                                                  (lr-track-test--t 11 0) "since" "x")
                       :refuse))
    (should (equal (mapcar #'car (plist-get (lr-track--plan-start-from (lr-track-test--ctx) 3 now "now" "x")
                                            :ops))
                   '(:start)))))

(ert-deftest lr-track-plan-stop-at-refuses-before-start ()
  (let ((clock (list :task "avey" :start (lr-track-test--t 12 0) :stream-key 1)))
    (should (plist-get (lr-track--plan-stop-at (lr-track-test--ctx :clock clock)
                                               (lr-track-test--t 12 0))
                       :refuse))
    (should (equal (plist-get (lr-track--plan-stop-at (lr-track-test--ctx :clock clock)
                                                      (lr-track-test--t 13 0))
                              :ops)
                   (list (list :end-at (lr-track-test--t 13 0)))))))

(ert-deftest lr-track-notes-fit-70 ()
  (let ((n (lr-track--pop-note "restarted" "from %s, %s" "12:00" (make-string 200 ?x))))
    (should (<= (length (lr-track--note-line "restarted" n)) 70))))

;;;; real writes on scratch files

(ert-deftest lr-track-write-gap-digit-runs-from-last-stop-and-undoes ()
  (let ((lr-track-test--now (lr-track-test--t 14 0)))
    (lr-track-test--world
      (lr-track--set-last-stop (lr-track-test--t 12 0))
      (let* ((before (lr-track-test--text tfile))
             (ctx (lr-track--pop-context 'agenda)))
        (should (equal (lr-track-test--kinds ctx) '(gap)))
        (lr-track--execute-plan
         (lr-track--plan-start-from ctx 1 (lr-track-test--t 12 0) "gap" "answered"))
        (should (org-clocking-p))
        (should (equal org-clock-heading "avey"))
        (should (= (float-time org-clock-start-time) (lr-track-test--t 12 0)))
        (should (string-match-p "CLOCK: \\[[^]]* 12:00\\]\n- lr gap: from 12:00, answered"
                                (lr-track-test--buf-text tfile)))
        (lr-track-undo)
        (should-not (org-clocking-p))
        (should (equal before (lr-track-test--buf-text tfile)))
        (should (= lr-track--last-clock-end (lr-track-test--t 12 0)))))))

(ert-deftest lr-track-write-split-closed-then-running ()
  (let ((lr-track-test--now (lr-track-test--t 14 0)))
    (lr-track-test--world
      (lr-track--set-last-stop (lr-track-test--t 12 0))
      (let ((ctx (lr-track--pop-context 'agenda)))
        (lr-track--execute-plan (lr-track--plan-log ctx 7 (lr-track-test--t 12 0)
                                                    (lr-track-test--t 12 40) "split"))
        (should (= lr-track--last-clock-end (lr-track-test--t 12 40)))
        (setq ctx (lr-track--pop-context 'agenda))
        (lr-track--execute-plan (lr-track--plan-start-from ctx 2 (lr-track-test--t 12 40)
                                                           "split" "the last part"))
        (let ((txt (lr-track-test--buf-text tfile)))
          (should (string-match-p "\\* life\n\\(?:.*\n\\)*?CLOCK: \\[[^]]* 12:00\\]--\\[[^]]* 12:40\\] =>  0:40\n- lr split: 12:00 to 12:40" txt))
          (should (string-match-p "\\* study\n\\(?:.*\n\\)*?CLOCK: \\[[^]]* 12:40\\]" txt)))
        (should (= (float-time org-clock-start-time) (lr-track-test--t 12 40)))))))

(ert-deftest lr-track-write-switch-at-typed-time-and-undo-restores-running ()
  (let ((lr-track-test--now (lr-track-test--t 14 0)))
    (lr-track-test--world
      (lr-track--execute-plan (lr-track--plan-start-from (lr-track--pop-context 'ask) 1
                                                         (lr-track-test--t 11 0) "now" "x"))
      (lr-track--tick-live-clock)
      (let ((before (lr-track-test--buf-text tfile)))
        (lr-track--execute-plan (lr-track--plan-start-from (lr-track--pop-context 'ask) 2
                                                           (lr-track-test--t 13 0) "since" "typed"))
        (should (equal org-clock-heading "study"))
        (should (string-match-p "CLOCK: \\[[^]]* 11:00\\]--\\[[^]]* 13:00\\] =>  2:00"
                                (lr-track-test--buf-text tfile)))
        (lr-track-undo)
        (should (equal org-clock-heading "avey"))
        (should (= (float-time org-clock-start-time) (lr-track-test--t 11 0)))
        (should (equal before (lr-track-test--buf-text tfile)))))))

(ert-deftest lr-track-write-limit-typed-end ()
  (let ((lr-track-test--now (lr-track-test--t 14 0)))
    (lr-track-test--world
      (lr-track--execute-plan (lr-track--plan-start-from (lr-track--pop-context 'ask) 1
                                                         (lr-track-test--t 9 0) "now" "x"))
      (lr-track--execute-plan (lr-track--plan-stop-at (lr-track--pop-context 'ask)
                                                      (lr-track-test--t 10 30)))
      (should-not (org-clocking-p))
      (should (string-match-p "CLOCK: \\[[^]]* 09:00\\]--\\[[^]]* 10:30\\] =>  1:30"
                              (lr-track-test--buf-text tfile)))
      (should (= lr-track--last-clock-end (lr-track-test--t 10 30))))))

(ert-deftest lr-track-write-restarted-resume-from-quit ()
  (let ((lr-track-test--now (lr-track-test--t 14 0)))
    (lr-track-test--world
      (let* ((ctx (lr-track--pop-context 'startup))
             (running (list :task "study" :file tfile :olp '("study")
                            :start (lr-track-test--t 10 0) :at (lr-track-test--t 13 30))))
        (lr-track--execute-plan (lr-track--plan-resume ctx running))
        (should (org-clocking-p))
        (should (equal org-clock-heading "study"))
        (should (= (float-time org-clock-start-time) (lr-track-test--t 13 30)))
        (should (string-match-p "- lr resumed: from 13:30, Emacs quit then"
                                (lr-track-test--buf-text tfile)))))))

(ert-deftest lr-track-last-stop-from-clock-out-and-time-file ()
  (let ((lr-track-test--now (lr-track-test--t 14 0)))
    (lr-track-test--world
      (should-not (lr-track--last-stop))
      (with-temp-file tfile
        (insert (lr-track--time-file-text))
        (goto-char (point-min))
        (re-search-forward "^\\* life\n:PROPERTIES:\n\\(?:.*\n\\)*?:END:\n")
        (insert (format "CLOCK: %s--%s =>  0:30\n"
                        (lr-track--ts (seconds-to-time (lr-track-test--t 11 0)))
                        (lr-track--ts (seconds-to-time (lr-track-test--t 11 30))))))
      (setq lr-track--streams-cache nil)
      (should (= (lr-track--last-stop) (lr-track-test--t 11 30)))
      (should (= (plist-get (lr-track--read-state) :last-stop) (lr-track-test--t 11 30))))))

;;;; the session, end to end with simulated keys

(ert-deftest lr-track-pop-session-gap-answer ()
  (let ((lr-track-test--now (lr-track-test--t 14 0)))
    (lr-track-test--world
      (lr-track--set-last-stop (lr-track-test--t 12 0))
      (cl-letf (((symbol-function 'lr-track--frame-focused-p) (lambda () t)))
        (should (= 1 (ert-simulate-keys (vector ?4)
                       (let ((executing-kbd-macro nil)) (lr-track-pop 'agenda))))))
      (should (equal org-clock-heading "writing"))
      (should (= (float-time org-clock-start-time) (lr-track-test--t 12 0))))))

(ert-deftest lr-track-pop-session-later-writes-nothing ()
  (let ((lr-track-test--now (lr-track-test--t 14 0)))
    (lr-track-test--world
      (lr-track--set-last-stop (lr-track-test--t 12 0))
      (let ((before (lr-track-test--text tfile)))
        (cl-letf (((symbol-function 'lr-track--frame-focused-p) (lambda () t)))
          (should (= 1 (ert-simulate-keys (vector ?\r)
                         (let ((executing-kbd-macro nil)) (lr-track-pop 'agenda))))))
        (should-not (org-clocking-p))
        (should (equal before (lr-track-test--text tfile)))))))

(ert-deftest lr-track-pop-refused-when-unsafe ()
  (let ((lr-track-test--now (lr-track-test--t 14 0)))
    (lr-track-test--world
      (lr-track--set-last-stop (lr-track-test--t 12 0))
      (cl-letf (((symbol-function 'lr-track--frame-focused-p) (lambda () nil)))
        (should (= 0 (lr-track-pop 'agenda))))
      (cl-letf (((symbol-function 'lr-track--frame-focused-p) (lambda () t)))
        (let ((lr-track--pop-active t)) (should (= 0 (lr-track-pop 'agenda))))
        (let ((executing-kbd-macro "x")) (should (= 0 (lr-track-pop 'agenda))))))))

;;;; triggers and keys

(ert-deftest lr-track-trigger-stopped-only-for-his-clock-out ()
  (let ((post-command-hook nil))
    (let ((this-command 'org-agenda-clock-out) (lr-track--internal nil))
      (lr-track--pop-on-clock-out))
    (should (memq 'lr-track--pop-stopped-once post-command-hook))
    (setq post-command-hook nil)
    (let ((this-command 'org-agenda-clock-out) (lr-track--internal t))
      (lr-track--pop-on-clock-out))
    (should-not post-command-hook)
    (let ((this-command 'org-clock-in) (lr-track--internal nil))
      (lr-track--pop-on-clock-out))
    (should-not post-command-hook)))

(ert-deftest lr-track-trigger-agenda-advice-installed ()
  (dolist (f lr-track--agenda-sticky-commands)
    (should (advice-member-p #'lr-track--pop-after-agenda f))))

(ert-deftest lr-track-now-commands-0-to-9-and-stop ()
  (dotimes (n 10)
    (should (commandp (intern (format "lr-track-now-%d" n)))))
  (should (commandp 'lr-track-stop))
  (should (commandp 'lr-track-ask))
  (should (commandp 'lr-track-status)))

(ert-deftest lr-track-arabic-leader-0-to-9 ()
  (let ((doom-leader-map (make-sparse-keymap)))
    (define-key doom-leader-map "oa" #'ignore)
    (lr-track-ask-install-arabic-leader)
    (should (eq (lookup-key doom-leader-map (vector #x64a #x62a)) 'lr-track-ask))
    (should (eq (lookup-key doom-leader-map (vector #x64a #x660)) 'lr-track-now-0))
    (should (eq (lookup-key doom-leader-map (vector #x64a #x6f9)) 'lr-track-now-9))
    (should (eq (lookup-key doom-leader-map (vector #x62e #x634)) 'ignore))))

(ert-deftest lr-track-config-bindings ()
  (let ((txt (with-temp-buffer
               (insert-file-contents (expand-file-name
                                      "config.el"
                                      (locate-dominating-file lr-track-test--dir "config.el")))
               (buffer-string))))
    (dolist (k '("d 0" "d 1" "d 9" "d x" "d s" "d u" "d M"))
      (should (string-match-p (format "\"%s\" #'lr-track-" k) txt)))
    (should (string-match-p "\"j\" #'lr-track-ask" txt))
    (should-not (string-match-p "\"d t\" #'lr-track-mode" txt))))

;;;; nothing about this laptop

(ert-deftest lr-track-no-laptop-sensing-in-sources ()
  (dolist (f '("lr-track.el" "lr-track-ask.el"))
    (let ((txt (with-temp-buffer
                 (insert-file-contents (locate-library f))
                 (buffer-string))))
      (dolist (bad '("ioreg" "IOConsole" "waketime" "HIDIdle" "current-idle-time"
                     "lr-track--presence"))
        (should-not (string-match-p (regexp-quote bad) txt)))
      ;; focus is read in exactly one function, which only drops questions
      (should (<= (cl-count-if #'identity
                               (let ((start 0) (hits nil))
                                 (while (string-match "frame-focus-state" txt start)
                                   (push t hits) (setq start (match-end 0)))
                                 hits))
                  1)))))

(provide 'lr-track-test)
;;; lr-track-test.el ends here
