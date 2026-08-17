;;; lr-context.el --- Org-native context switching -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; One command (`lr-context-switch', SPC d d) to move between named working
;; modes -- work / reading / blog / study.  A *context* bundles the state you
;; carry between modes:
;;
;;   - a Doom workspace + its window layout + point         (rendering)
;;   - the running task clock                                (stopped on leave,
;;                                                            offered on enter)
;;   - a notification profile (focus / normal)              (exposed for nudges)
;;   - where you were and *why* you stopped                 (fact about work)
;;   - how long you spent in the context                    (fact about work)
;;
;; WHERE THE STATE LIVES.  Everything that is a *fact about your work* is plain
;; org in `lr-context-file' (default ~/roam/main/contexts.org): one heading per
;; context, the definition + last-state as properties, the time you spent as
;; native CLOCK lines in each context's LOGBOOK (so `org-clock-report' and a
;; clocktable answer "where did my week go, per context" with no custom code),
;; and a human-readable `* Switch Log' of every transition.  You can read,
;; org-ql, agenda, and hand-edit all of it.
;;
;; THE ONE MACHINE-LOCAL THING.  The opaque window-layout blob (pixel geometry,
;; scroll positions) is a re-computable *rendering*, not a fact, and cannot be
;; meaningfully org-queried -- it is cached under `lr-context-cache-directory'
;; (in ~/.emacs.d/.local, never in ~/roam).  Its human-readable shadow -- the
;; list of files you were looking at -- IS written to org (the LAST_FILES
;; property), so "what was I doing in this context" is answerable from org
;; alone even if the blob is lost.  If you would rather force even the blob into
;; org, that is a one-function change; say so.
;;
;; SURVIVES RESTART.  Context definitions, time, reasons, and file lists are in
;; org; the layout blob is on local disk.  After an Emacs restart, switching
;; back into a context reopens its files and restores its layout on demand.

;;; Code:

(require 'org)
(require 'org-clock)
(require 'org-id)
(require 'cl-lib)
(require 'seq)

(defgroup lr-context nil
  "Org-native context switching."
  :group 'org
  :prefix "lr-context-")

(defcustom lr-context-file "~/roam/main/contexts.org"
  "Org file holding context definitions, per-context time, and the switch log."
  :type 'file)

(defcustom lr-context-cache-directory
  (expand-file-name "lr-context/"
                    (or (bound-and-true-p doom-data-dir) user-emacs-directory))
  "Directory for the machine-local window-layout blobs (one .eld per context).
Nothing here is a fact about your work -- only re-computable renderings."
  :type 'directory)

(defcustom lr-context-resume-clock 'ask
  "What to do with a context's remembered task clock when you enter it.
`ask' prompts y/n, t resumes silently, nil never resumes."
  :type '(choice (const :tag "Ask" ask) (const :tag "Always" t) (const :tag "Never" nil)))

(defcustom lr-context-clock-out-on-leave t
  "When non-nil, stop the running task clock when you leave a context.
The task is remembered (LAST_TASK) so you can resume it on return."
  :type 'boolean)

(defcustom lr-context-switch-hook nil
  "Hook run after a successful switch, with the new context id as its argument."
  :type 'hook)

(defvar lr-context-current nil
  "Id (string) of the context currently active, or nil.
Mirrors the org heading carrying `:ACTIVE: t'; rehydrated on load.")

(defvar lr-context-notify-profile nil
  "Notification profile of the current context (a symbol like `focus'/`normal').
Exposed for a suppression/nudge layer to read; this module only records it.")

(defvar lr-context--reason-history nil)
(defvar lr-context--entered-at nil
  "In-memory real time the current context was entered (fast path for durations).")

(defconst lr-context--reason-choices
  '("done for now" "switching tasks" "interrupted" "break" "meeting" "blocked")
  "Presets offered by the reason-for-stopping prompt; free text is allowed.")

;;;; paths / io

(defun lr-context--file ()
  "Absolute path of the contexts org file."
  (expand-file-name lr-context-file))

(defun lr-context--cache-dir ()
  (expand-file-name lr-context-cache-directory))

(defun lr-context--cache-file (id)
  (expand-file-name (concat id ".eld") (lr-context--cache-dir)))

(defun lr-context--write-blob (id blob)
  "Persist BLOB (the layout plist) for context ID under the cache dir."
  (make-directory (lr-context--cache-dir) t)
  (with-temp-file (lr-context--cache-file id)
    (let ((print-level nil) (print-length nil) (print-circle t))
      (prin1 blob (current-buffer)))))

(defun lr-context--read-blob (id)
  (let ((f (lr-context--cache-file id)))
    (when (file-exists-p f)
      (with-temp-buffer
        (insert-file-contents f)
        (ignore-errors (read (current-buffer)))))))

;;;; contexts org file

(defconst lr-context--seed "\
:PROPERTIES:
:ID:       %s
:END:
#+title: Contexts
#+filetags: :context:
#+STARTUP: overview showstars indent

# Managed by lr-context.el, but it is all just plain org -- read/edit freely.
#   - one heading per context; :CONTEXT_ID:/:WORKSPACE:/:NOTIFY_PROFILE: define it
#   - LAST_* properties record where you were and why you last left
#   - CLOCK lines in each LOGBOOK accrue time-in-context (try C-c C-x C-r here)
#   - the Switch Log below is every transition, newest first

* Work avey.ai
:PROPERTIES:
:CONTEXT_ID:     work
:WORKSPACE:      work
:NOTIFY_PROFILE: focus
:END:

* Reading
:PROPERTIES:
:CONTEXT_ID:     reading
:WORKSPACE:      reading
:NOTIFY_PROFILE: focus
:END:

* Blog
:PROPERTIES:
:CONTEXT_ID:     blog
:WORKSPACE:      blog
:NOTIFY_PROFILE: normal
:END:

* Study
:PROPERTIES:
:CONTEXT_ID:     study
:WORKSPACE:      study
:NOTIFY_PROFILE: focus
:END:

* Switch Log
"
  "Initial contents for a fresh contexts file; %s is filled with a fresh ID.")

(defun lr-context--ensure-file ()
  "Create the contexts file with seed contexts if it does not exist.
Return the file path."
  (let ((file (lr-context--file)))
    (unless (file-exists-p file)
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert (format lr-context--seed (org-id-uuid)))))
    file))

(defun lr-context--buffer ()
  "Return the (live) buffer visiting the contexts file, creating the file first."
  (find-file-noselect (lr-context--ensure-file)))

(defun lr-context--save-buffer (buffer)
  "Save BUFFER, neutralising this config's save-time org rewriters.
`before-save-hook' runs three buffer-local rewriters (toc-org,
vulpea-project-update-tag, org-roam-link-replace-all); a bare `let'-bind of the
hook cannot suppress a buffer-local entry (only overriding the symbol-function
can), so temporarily stub whichever are defined, then restore."
  (with-current-buffer buffer
    (when (buffer-modified-p)
      (let* ((names (seq-filter #'fboundp
                                '(toc-org-insert-toc
                                  vulpea-project-update-tag
                                  org-roam-link-replace-all)))
             (saved (mapcar (lambda (n) (cons n (symbol-function n))) names)))
        (unwind-protect
            (progn (dolist (n names) (fset n #'ignore))
                   (let ((org-element-use-cache nil))
                     (save-buffer)))
          (dolist (p saved) (fset (car p) (cdr p))))))))

(defmacro lr-context--with-file (&rest body)
  "Run BODY in the contexts buffer, widened, then save it.
Point is not preserved for the caller; BODY should navigate explicitly."
  (declare (indent 0) (debug t))
  `(let ((buf (lr-context--buffer)))
     (with-current-buffer buf
       (org-with-wide-buffer
        (prog1 (progn ,@body)
          (lr-context--save-buffer buf))))))

;;;; heading navigation

(defun lr-context--goto (id)
  "Move point to the heading whose CONTEXT_ID is ID.  Return point or nil.
Caller must already be in the contexts buffer."
  (let ((pos (org-find-property "CONTEXT_ID" id)))
    (when pos (goto-char pos) pos)))

(defun lr-context--all ()
  "Return a list of (ID . TITLE) for every defined context, in file order."
  (with-current-buffer (lr-context--buffer)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let (out)
       (while (re-search-forward "^\\*+[ \t]" nil t)
         (let ((id (org-entry-get (point) "CONTEXT_ID")))
           (when id
             (push (cons id (org-get-heading t t t t)) out))))
       (nreverse out)))))

(defun lr-context--active-from-org ()
  "Return the CONTEXT_ID of the heading marked `:ACTIVE: t', or nil."
  (with-current-buffer (lr-context--buffer)
    (org-with-wide-buffer
     (goto-char (point-min))
     (catch 'hit
       (while (re-search-forward "^\\*+[ \t]" nil t)
         (when (and (org-entry-get (point) "CONTEXT_ID")
                    (equal "t" (org-entry-get (point) "ACTIVE")))
           (throw 'hit (org-entry-get (point) "CONTEXT_ID"))))
       nil))))

(defun lr-context--prop (id key)
  (with-current-buffer (lr-context--buffer)
    (org-with-wide-buffer
     (when (lr-context--goto id) (org-entry-get (point) key)))))

;;;; org mutations

(defun lr-context--ts (time)
  "Format TIME as an inactive org timestamp string, e.g. [2026-08-06 Thu 22:30]."
  (format-time-string (org-time-stamp-format t t) time))

(defun lr-context--log-clock (id start end)
  "Append a CLOCK line for START..END into context ID's LOGBOOK.
Both are real time values.  Caller is inside `lr-context--with-file'."
  (when (and start end (time-less-p start end))
    (when (lr-context--goto id)
      (let* ((mins (/ (float-time (time-subtract end start)) 60.0))
             (line (format "CLOCK: %s--%s =>  %s"
                           (lr-context--ts start) (lr-context--ts end)
                           (org-duration-from-minutes mins)))
             (org-log-into-drawer "LOGBOOK"))
        (goto-char (lr-context--goto id))
        (goto-char (org-log-beginning t))
        (insert line "\n")))))

(defun lr-context--switch-log (from to reason mins)
  "Prepend a human-readable line to the `* Switch Log' heading.
Caller is inside `lr-context--with-file'."
  (goto-char (point-min))
  (unless (re-search-forward "^\\*+[ \t]+Switch Log[ \t]*$" nil t)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "* Switch Log\n"))
  (forward-line 1)
  ;; skip any drawer/planning right under the heading; insert as first list item
  (let ((line (format "- %s  %s to %s  (%s%s)\n"
                      (lr-context--ts (current-time))
                      (or from "-") to
                      (if (numberp mins) (org-duration-from-minutes mins) "-")
                      (if (and reason (> (length reason) 0))
                          (format ", %s" reason) ""))))
    (insert line)))

;;;; layout

(defun lr-context--capture-layout ()
  "Return a plist snapshot of the selected frame's window layout, or nil."
  (let ((frame (selected-frame)))
    (unless (or (not (frame-live-p frame))
                (window-minibuffer-p (selected-window)))
      (condition-case err
          (let* ((state (window-state-get (frame-root-window frame) t))
                 (files (delete-dups
                         (delq nil (mapcar (lambda (w) (buffer-file-name (window-buffer w)))
                                           (window-list frame 'no-minibuffer))))))
            (list :window-state state :files files))
        (error (message "lr-context: layout capture skipped: %S" err) nil)))))

(defun lr-context--restore-layout (blob)
  "Reopen files in BLOB and restore its window layout in the selected frame."
  (when blob
    (dolist (f (plist-get blob :files))
      (when (and f (file-exists-p f))
        (ignore-errors (find-file-noselect f))))
    (when-let* ((state (plist-get blob :window-state)))
      (condition-case err
          (window-state-put state (frame-root-window (selected-frame)) 'safe)
        (error (message "lr-context: layout restore skipped: %S" err))))))

(defun lr-context--layout-files-string (blob)
  "Return BLOB's file list as a compact, human-readable, org-safe string."
  (let ((files (and blob (plist-get blob :files))))
    (if files
        (mapconcat (lambda (f) (abbreviate-file-name f)) files " | ")
      "")))

;;;; workspace

(defun lr-context--switch-workspace (name)
  "Switch the current frame to Doom workspace NAME, creating it if needed."
  (when (and name (bound-and-true-p persp-mode) (fboundp '+workspace-switch))
    (unless (and (fboundp '+workspace-current-name)
                 (equal name (+workspace-current-name)))
      (condition-case err
          (+workspace-switch name t)     ; t => auto-create
        (error (message "lr-context: workspace switch failed: %S" err))))))

;;;; clocks

(defun lr-context--clock-task-ref ()
  "Return a stable reference to the currently-clocked task, or nil.
Never mints an org id (see the config's id-minting discipline)."
  (when (and (org-clocking-p) (markerp org-clock-marker)
             (buffer-live-p (marker-buffer org-clock-marker)))
    (org-with-point-at org-clock-marker
      (let ((id (org-id-get)))
        (if id (concat "id:" id)
          (format "file:%s::*%s"
                  (abbreviate-file-name (or (buffer-file-name) ""))
                  (org-get-heading t t t t)))))))

(defun lr-context--clock-out-task ()
  "Stop the running task clock, returning a reference to it (or nil)."
  (when (and lr-context-clock-out-on-leave (org-clocking-p))
    (let ((ref (lr-context--clock-task-ref)))
      (condition-case err
          (org-clock-out)
        (error (message "lr-context: clock-out failed: %S" err)))
      ref)))

(defun lr-context--resolve-ref (ref)
  "Resolve a LAST_TASK REF string to a marker, or nil.  Does not create ids."
  (cond
   ((null ref) nil)
   ((string-prefix-p "id:" ref)
    (org-id-find (substring ref 3) 'marker))
   ((string-prefix-p "file:" ref)
    (save-match-data
      (when (string-match "\\`file:\\(.*?\\)::\\*\\(.*\\)\\'" ref)
        (let ((file (expand-file-name (match-string 1 ref)))
              (heading (match-string 2 ref)))
          (when (file-exists-p file)
            (with-current-buffer (find-file-noselect file)
              (org-with-wide-buffer
               (goto-char (point-min))
               (when (re-search-forward
                      (format org-complex-heading-regexp-format
                              (regexp-quote heading))
                      nil t)
                 (copy-marker (line-beginning-position))))))))))))

(defun lr-context--maybe-resume-clock (ref)
  "Per `lr-context-resume-clock', resume the clock on REF's task."
  (when (and ref lr-context-resume-clock (not (org-clocking-p)))
    (when-let* ((marker (lr-context--resolve-ref ref)))
      (when (or (eq lr-context-resume-clock t)
                (y-or-n-p (format "Resume clock on %s? "
                                  (org-with-point-at marker (org-get-heading t t t t)))))
        (condition-case err
            (org-with-point-at marker (org-clock-in))
          (error (message "lr-context: resume-clock failed: %S" err)))))))

;;;; the switch

(defun lr-context--read-reason (from to)
  (completing-read
   (format "Leaving %s to %s, why? " (or from "somewhere") to)
   lr-context--reason-choices nil nil nil 'lr-context--reason-history "switch"))

(defun lr-context--leave (id reason to)
  "Save outgoing context ID's state (layout, time, task, reason)."
  (let* ((now (current-time))
         (entered (or lr-context--entered-at
                      (let ((s (lr-context--prop id "LAST_ENTERED")))
                        (and s (org-time-string-to-time s)))))
         (blob (lr-context--capture-layout))
         (task-ref (lr-context--clock-out-task))
         (mins (and entered (/ (float-time (time-subtract now entered)) 60.0))))
    (when blob (lr-context--write-blob id blob))
    (lr-context--with-file
      (when (lr-context--goto id)
        (org-entry-put (point) "LAST_LEFT" (lr-context--ts now))
        (org-entry-put (point) "LAST_REASON" (or reason ""))
        (org-entry-put (point) "LAST_FILES" (lr-context--layout-files-string blob))
        (when task-ref (org-entry-put (point) "LAST_TASK" task-ref))
        (org-entry-delete (point) "ACTIVE"))
      (when entered (lr-context--log-clock id entered now))
      (lr-context--switch-log id to reason mins))))

(defun lr-context--enter (id)
  "Restore incoming context ID: workspace, layout, profile, properties, clock."
  (let ((profile (lr-context--prop id "NOTIFY_PROFILE"))
        (workspace (or (lr-context--prop id "WORKSPACE") id))
        (task-ref (lr-context--prop id "LAST_TASK"))
        (now (current-time)))
    (lr-context--switch-workspace workspace)
    (lr-context--restore-layout (lr-context--read-blob id))
    (setq lr-context-current id
          lr-context--entered-at now
          lr-context-notify-profile (and profile (intern profile)))
    (lr-context--with-file
      (when (lr-context--goto id)
        (org-entry-put (point) "LAST_ENTERED" (lr-context--ts now))
        (org-entry-put (point) "ACTIVE" "t")))
    (lr-context--maybe-resume-clock task-ref)
    (run-hook-with-args 'lr-context-switch-hook id)))

;;;###autoload
(defun lr-context-switch (id)
  "Switch to context ID, saving the current context's state first.
Interactively, pick from the defined contexts."
  (interactive
   (list (let ((all (lr-context--all)))
           (unless all (user-error "No contexts defined; use `lr-context-define'"))
           (completing-read
            "Switch to context: "
            (mapcar (lambda (c) (car c)) all) nil t))))
  (let ((from (or lr-context-current (lr-context--active-from-org))))
    (cond
     ((equal from id)
      (message "Already in context %s" id))
     (t
      (when from
        (lr-context--leave from (lr-context--read-reason from id) id))
      (lr-context--enter id)
      (message "now in context %s%s" id
               (if lr-context-notify-profile
                   (format "  (%s)" lr-context-notify-profile) ""))))))

;;;###autoload
(defun lr-context-define (id workspace profile)
  "Define a new context ID (a short slug) mapped to WORKSPACE with PROFILE."
  (interactive
   (list (read-string "New context id (slug): ")
         (read-string "Doom workspace name: " nil nil (lambda () nil))
         (completing-read "Notification profile: " '("focus" "normal") nil nil nil nil "normal")))
  (setq workspace (if (string-empty-p workspace) id workspace))
  (when (member id (mapcar #'car (lr-context--all)))
    (user-error "Context %s already exists" id))
  (lr-context--with-file
    (goto-char (point-min))
    ;; insert new context heading just before the Switch Log (or at eob)
    (if (re-search-forward "^\\*+[ \t]+Switch Log[ \t]*$" nil t)
        (goto-char (line-beginning-position))
      (goto-char (point-max))
      (unless (bolp) (insert "\n")))
    (insert (format "* %s\n:PROPERTIES:\n:CONTEXT_ID:     %s\n:WORKSPACE:      %s\n:NOTIFY_PROFILE: %s\n:END:\n\n"
                    (capitalize id) id workspace profile)))
  (message "Defined context %s, workspace %s (%s)" id workspace profile))

;;;###autoload
(defun lr-context-resume-clock ()
  "Resume the clock on the current context's remembered task."
  (interactive)
  (let ((id (or lr-context-current (lr-context--active-from-org))))
    (unless id (user-error "No active context"))
    (let ((ref (lr-context--prop id "LAST_TASK"))
          (lr-context-resume-clock t))
      (unless ref (user-error "Context %s has no remembered task" id))
      (lr-context--maybe-resume-clock ref))))

;;;###autoload
(defun lr-context-visit ()
  "Open the contexts org file."
  (interactive)
  (find-file (lr-context--ensure-file)))

;;;###autoload
(defun lr-context-list ()
  "Show every context with its active flag, last state, and total time."
  (interactive)
  (let ((rows
         (with-current-buffer (lr-context--buffer)
           (org-with-wide-buffer
            (org-clock-sum)
            (let (out)
              (dolist (c (lr-context--all))
                (let ((id (car c)))
                  (lr-context--goto id)
                  (push (list :id id
                              :active (equal "t" (org-entry-get (point) "ACTIVE"))
                              :mins (or (get-text-property (point) :org-clock-minutes) 0)
                              :left (org-entry-get (point) "LAST_REASON")
                              :when (org-entry-get (point) "LAST_LEFT"))
                        out)))
              (nreverse out))))))
    (with-current-buffer (get-buffer-create "*lr-context*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Contexts, time is total across all history\n\n" 'face 'bold))
        (dolist (r rows)
          (insert (format "%s %-10s  %8s   %s\n"
                          (if (plist-get r :active) ">" " ")
                          (plist-get r :id)
                          (org-duration-from-minutes (plist-get r :mins))
                          (if (plist-get r :when)
                              (format "last left %s%s" (plist-get r :when)
                                      (if (plist-get r :left)
                                          (format " (%s)" (plist-get r :left)) ""))
                            "- never left"))))
        (insert (format "\nTip: open %s and run C-c C-x C-r for a clocktable per day/week.\n"
                        (abbreviate-file-name (lr-context--file)))))
      (goto-char (point-min))
      (view-mode 1)
      (display-buffer (current-buffer)))))

;;;; lifecycle

(defun lr-context--rehydrate ()
  "Set `lr-context-current' from the ACTIVE heading, if the file exists."
  (when (file-exists-p (lr-context--file))
    (ignore-errors
      (setq lr-context-current (lr-context--active-from-org)))))

(with-eval-after-load 'org
  (lr-context--rehydrate))

;; Keybindings live in config.el (autoloaded + bound under SPC d) so the prefix
;; works from startup without loading this module -- see the lr-context block
;; there.  This file stays pure logic.

(provide 'lr-context)
;;; lr-context.el ends here
