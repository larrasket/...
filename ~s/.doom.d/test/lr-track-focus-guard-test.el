;;; lr-track-focus-guard-test.el --- check-ins never open into the void -*- lexical-binding: t; -*-

;;; Commentary:
;; Interim guard (2026-10-03).  The evidence audit found check-ins opening during
;; sub-second Emacs pass-throughs (AeroSpace workspace hops) and then sitting in a
;; minibuffer nobody was looking at, for 34 minutes and probably 17 hours, ready
;; to swallow the next keystrokes.  These tests pin the guard:
;;   - a return only counts once focus has held for a moment;
;;   - a pass-through never resets when you actually left;
;;   - leaving Emacs while a check-in is open drops it (the C-g path).

;;; Code:

(require 'ert)
(require 'cl-lib)

(when (boundp 'native-comp-jit-compilation)
  (setq native-comp-jit-compilation nil))

(defmacro add-hook! (&rest _) nil)
(defmacro after! (&rest body) `(progn ,@body))
(defmacro defadvice! (&rest _) nil)
(defvar lr-track-focus-test--tmp
  (file-name-as-directory (make-temp-file "lr-track-focus-test" t)))
(defvar doom-data-dir (expand-file-name "etc/" lr-track-focus-test--tmp))
(defvar doom-cache-dir (expand-file-name "cache/" lr-track-focus-test--tmp))
(make-directory doom-data-dir t)
(make-directory doom-cache-dir t)

(add-to-list 'load-path (expand-file-name "modules"
                                          (locate-dominating-file
                                           (or load-file-name buffer-file-name)
                                           "modules")))
(require 'lr-track)

(defmacro lr-track-focus-test--with-world (&rest body)
  "Run BODY with a fake clock, fake focus, captured timers and a recorded trigger.
Binds `focused', `now', `timers' and `triggered' for BODY to manipulate."
  (declare (indent 0))
  `(let ((focused t) (now 1000.0) (timers nil) (triggered nil)
         (lr-track--focused-p t)
         (lr-track--unfocused-since nil)
         (lr-track--checkin-since nil)
         (lr-track--last-checkin 0.0)
         (lr-track--resolving nil)
         (lr-track-checkin-on-return t)
         (lr-track-checkin-after-seconds 180))
     (cl-letf (((symbol-function 'float-time) (lambda (&rest _) now))
               ((symbol-function 'frame-focus-state) (lambda (&rest _) focused))
               ((symbol-function 'run-at-time)
                (lambda (_secs _rep fn &rest args) (push (cons fn args) timers) nil))
               ((symbol-function 'lr-track--trigger-checkin)
                (lambda (ctx) (push (cons ctx lr-track--checkin-since) triggered))))
       (cl-flet ((fire-timers ()
                   (let ((ts (reverse timers)))
                     (setq timers nil)
                     (dolist (tm ts) (apply (car tm) (cdr tm))))))
         ,@body))))

(ert-deftest lr-track-focus-guard-pass-through-never-prompts ()
  "Leave for an hour, flick through Emacs for 0.7 s, leave again: no check-in."
  (lr-track-focus-test--with-world
    (setq focused nil) (lr-track--focus-change)          ; leave at 1000
    (setq now 4600.0 focused t) (lr-track--focus-change) ; back for a blink
    (setq now 4600.7 focused nil) (lr-track--focus-change) ; gone again
    (setq now 4601.5) (fire-timers)                      ; confirm timer fires
    (should-not triggered)))

(ert-deftest lr-track-focus-guard-pass-through-keeps-the-real-departure ()
  "After a pass-through, the next real return asks about the WHOLE gap."
  (lr-track-focus-test--with-world
    (setq focused nil) (lr-track--focus-change)            ; left at 1000
    (setq now 4600.0 focused t) (lr-track--focus-change)   ; blink
    (setq now 4600.7 focused nil) (lr-track--focus-change)
    (setq now 4601.5) (fire-timers)
    (setq now 8200.0 focused t) (lr-track--focus-change)   ; real return
    (setq now 8201.5) (fire-timers)
    (should (equal triggered '((return . 1000.0))))
    (should-not lr-track--unfocused-since)))

(ert-deftest lr-track-focus-guard-stable-return-prompts-once ()
  (lr-track-focus-test--with-world
    (setq focused nil) (lr-track--focus-change)
    (setq now 2000.0 focused t) (lr-track--focus-change)
    (setq now 2001.5) (fire-timers)
    (should (equal triggered '((return . 1000.0))))))

(ert-deftest lr-track-focus-guard-short-absence-does-not-prompt ()
  "Under `lr-track-checkin-after-seconds' nothing is asked, and the departure clears."
  (lr-track-focus-test--with-world
    (setq focused nil) (lr-track--focus-change)
    (setq now 1060.0 focused t) (lr-track--focus-change)
    (setq now 1061.5) (fire-timers)
    (should-not triggered)
    (should-not lr-track--unfocused-since)))

(ert-deftest lr-track-focus-guard-leaving-drops-an-open-check-in ()
  "Losing focus while a check-in is open schedules the C-g path."
  (lr-track-focus-test--with-world
    (let ((aborted nil))
      (cl-letf (((symbol-function 'minibuffer-depth) (lambda () 1))
                ((symbol-function 'abort-recursive-edit) (lambda () (setq aborted t))))
        (setq lr-track--resolving t)
        (setq focused nil) (lr-track--focus-change)
        (fire-timers)
        (should aborted)))))

(ert-deftest lr-track-focus-guard-leaving-leaves-other-minibuffers-alone ()
  "A minibuffer that is NOT a check-in (M-x, find-file) is never touched."
  (lr-track-focus-test--with-world
    (let ((aborted nil))
      (cl-letf (((symbol-function 'minibuffer-depth) (lambda () 1))
                ((symbol-function 'abort-recursive-edit) (lambda () (setq aborted t))))
        (setq lr-track--resolving nil)
        (setq focused nil) (lr-track--focus-change)
        (fire-timers)
        (should-not aborted)))))

(ert-deftest lr-track-focus-guard-idle-open-skips-when-unseen-and-rearms ()
  "The check-in's own idle timer fires while Emacs is unfocused too.  If nobody
is looking it must not open, and the next real return must re-ask the whole gap."
  (lr-track-focus-test--with-world
    (let ((opened nil))
      (cl-letf (((symbol-function 'lr-track-checkin) (lambda (&rest _) (setq opened t)))
                ((symbol-function 'lr-track--safe-to-prompt-p) (lambda () t)))
        (setq lr-track--checkin-since 1000.0
              lr-track--unfocused-since 2000.5   ; set when the blink ended
              lr-track--last-checkin 2000.0
              focused nil)
        (lr-track--open-checkin-if-seen 'return)
        (should-not opened)
        (should (= lr-track--unfocused-since 1000.0))  ; rewound to the real departure
        (should (= lr-track--last-checkin 0.0))         ; cooldown reset
        (setq focused t)
        (lr-track--open-checkin-if-seen 'return)
        (should opened)))))

(provide 'lr-track-focus-guard-test)
;;; lr-track-focus-guard-test.el ends here
