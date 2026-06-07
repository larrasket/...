;;; lr-editor.el --- Keybindings, evil, editing prefs -*- lexical-binding: t; -*-

(require 'cl-lib)

;;; --- Evil ---
(setq doom-leader-alt-key "M-m")

;;; --- Jinx (lazy — hook only) ---
(add-hook 'text-mode-hook #'jinx-mode)

;;; --- YAS ---
(after! yasnippet (yas-global-mode 1))

;;; --- Comment utility ---
(defun salih/comment-or-uncomment-region-or-line ()
  "Comment/uncomment the region or current line."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (forward-line)))

;;; --- Jinx + save ---
(defun salih/jinx-correct-or-save ()
  "Run `jinx-correct`. If no misspellings found, save buffer."
  (interactive)
  (condition-case msg
      (jinx-correct)
    (error (when (string-match-p "No misspelled word" (error-message-string msg))
             (save-buffer)))))

;;; --- Bidi toggle ---
(defun salih/bidi-direction-toggle ()
  "Toggle bidirectional paragraph direction and Arabic input method."
  (interactive)
  (setq bidi-display-reordering t)
  (if (equal bidi-paragraph-direction 'right-to-left)
      (progn
        (setq bidi-paragraph-direction 'left-to-right)
        (deactivate-input-method))
    (setq bidi-paragraph-direction 'right-to-left)
    (jinx-mode -1)
    (set-input-method "arabic"))
  (message "Direction: %s, Input method: %s"
           bidi-paragraph-direction
           (or current-input-method "none")))

;;; --- Helper functions ---
(defun salih/tmp-buffer ()
  "Open a new temporary buffer."
  (interactive)
  (let ((bufname (generate-new-buffer-name
                  (format "*scratch-%x*" (random most-positive-fixnum)))))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)
    (message "Opened temporary buffer: %s" bufname)))

(defun salih/insert-current-date ()
  (interactive)
  (if (eq major-mode 'org-mode)
      (insert "- " (format-time-string "[%Y-%m-%d %a %H:%M]") " ")
    (let ((current-prefix-arg '(16)))
      (call-interactively 'org-time-stamp-inactive)
      (insert " "))))

(defun salih/vterm ()
  "Open vterm, cd to current directory if already running."
  (interactive)
  (let ((cwd (file-name-directory (or (buffer-file-name) default-directory)))
        (vterm-buffer (get-buffer "*vterm*")))
    (if vterm-buffer
        (progn
          (switch-to-buffer vterm-buffer)
          (vterm-send-string (concat "cd " cwd))
          (vterm-send-return))
      (+vterm/here t))))

(defun salih/kill-all-org-buffers ()
  "Kill all org-mode buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'org-mode)
        (kill-buffer buffer)))))

;;; --- File utilities ---
(defun salih/open-in-external-app (&optional fname)
  "Open file in external app. In dired, open marked files."
  (interactive)
  (let* ((file-list (if fname (list fname)
                      (if (eq major-mode 'dired-mode)
                          (dired-get-marked-files)
                        (list (buffer-file-name)))))
         (do-it (or (<= (length file-list) 5)
                    (y-or-n-p "Open more than 5 files? "))))
    (when do-it
      (dolist (f file-list)
        (pcase system-type
          ('darwin (shell-command (concat "open " (shell-quote-argument f))))
          ('gnu/linux (start-process "" nil "xdg-open" f))
          (_ (browse-url f)))))))

(defun salih/open-book ()
  "Search for a file in ~/roam/source and open it."
  (interactive)
  (let ((default-directory (concat salih/source-directory "/")))
    (call-interactively 'find-file)))

;;; --- Agenda launchers (defined early so keybindings work before org loads) ---
(defun salih/org-agenda-no-full-f ()
  (interactive)
  (setq salih/vulpea-show-full nil)
  (org-agenda nil "f"))

(defun salih/org-agenda-no-full-l ()
  (interactive)
  (setq salih/vulpea-show-full nil)
  (org-agenda nil "l"))

(defun salih/org-agenda-full-f ()
  (interactive)
  (setq salih/vulpea-show-full t)
  (org-agenda nil "l"))

(defun salih/open-agenda ()
  (interactive)
  (org-agenda-remove-restriction-lock)
  (org-agenda nil "v"))

;;; --- Inbox launcher ---
(defun salih/open-inbox ()
  "Open mu4e inbox, loading mu4e if needed."
  (interactive)
  (require 'mu4e)
  (setq mu4e-search-threads t)
  (mu4e~headers-jump-to-maildir "/icloud/Personal")
  (mu4e-search-change-sorting :date 'descending))

(defun salih/zathura-open ()
  (interactive)
  (start-process "" nil "open" buffer-file-name))

;;; --- Dired sort ---
(defun salih/dired-sort ()
  "Sort dired listing."
  (interactive)
  (let* ((choice (completing-read "Sort by:" '("date" "size" "name" "dir")))
         (arg (pcase choice
                ("name" "-Al --si --time-style long-iso ")
                ("date" "-Al --si --time-style long-iso -t")
                ("size" "-Al --si --time-style long-iso -S")
                ("dir"  "-Al --si --time-style long-iso --group-directories-first"))))
    (dired-sort-other arg)))

;;; --- Shell/term hooks ---
(after! eshell (remove-hook 'eshell-mode-hook 'hide-mode-line-mode))
(after! vterm  (remove-hook 'vterm-mode-hook 'hide-mode-line-mode))

;;; --- Linked frames ---
(setq-default cursor-in-non-selected-windows t)

(defvar salih/linked-frame-sync-delay 0.03
  "Idle delay before linked frames mirror the active linked frame.")

(defvar salih/--linked-frame-next-id 0)
(defvar salih/--linked-frame-sync-timer nil)
(defvar salih/--linked-frame-syncing nil)

(defun salih/--linked-frame-new-group ()
  "Return a fresh linked-frame group name."
  (setq salih/--linked-frame-next-id (1+ salih/--linked-frame-next-id))
  (format "salih-linked-frame-%d" salih/--linked-frame-next-id))

(defun salih/--linked-frame-group (&optional frame)
  "Return FRAME's linked-frame group, if any."
  (frame-parameter (or frame (selected-frame)) 'salih-linked-frame-group))

(defun salih/--linked-frame-ensure-group (&optional frame)
  "Ensure FRAME belongs to a linked-frame group and return that group."
  (let* ((frame (or frame (selected-frame)))
         (group (or (salih/--linked-frame-group frame)
                    (salih/--linked-frame-new-group))))
    (set-frame-parameter frame 'salih-linked-frame-group group)
    ;; Doom uses the `workspace' frame parameter for disposable per-frame
    ;; workspaces.  Linked frames share a workspace, so they must not delete it
    ;; when one frame is closed.
    (set-frame-parameter frame 'workspace nil)
    group))

(defun salih/--linked-frame-p (&optional frame)
  "Return non-nil when FRAME is a linked frame."
  (and (salih/--linked-frame-group frame) t))

(defun salih/--linked-frame-siblings (&optional frame)
  "Return all live frames in FRAME's linked-frame group."
  (let ((group (salih/--linked-frame-group frame)))
    (when group
      (cl-remove-if-not
       (lambda (candidate)
         (and (frame-live-p candidate)
              (equal group (salih/--linked-frame-group candidate))))
       (frame-list)))))

(defun salih/--linked-frame-workspace-name (&optional frame)
  "Return the Doom workspace name visible in FRAME, if workspaces are active."
  (when (and (bound-and-true-p persp-mode)
             (fboundp '+workspace-current-name))
    (with-selected-frame (or frame (selected-frame))
      (+workspace-current-name))))

(defun salih/--linked-frame-state (&optional frame)
  "Capture selected-window state from FRAME for linked-frame mirroring."
  (with-selected-frame (or frame (selected-frame))
    (let ((window (selected-window)))
      (unless (window-minibuffer-p window)
        (list :workspace (salih/--linked-frame-workspace-name)
              :buffer (window-buffer window)
              :window-state (window-state-get (frame-root-window frame))
              :point (window-point window)
              :start (window-start window)
              :hscroll (window-hscroll window)
              :vscroll (window-vscroll window t))))))

(defun salih/--linked-frame-bounded-position (buffer position)
  "Return POSITION clamped to BUFFER's accessible range."
  (with-current-buffer buffer
    (min (max (or position (point-min)) (point-min))
         (point-max))))

(defun salih/--linked-frame-switch-workspace (workspace frame)
  "Switch FRAME to WORKSPACE when Doom workspaces are available."
  (when (and workspace (bound-and-true-p persp-mode))
    (with-selected-frame frame
      (unless (equal workspace (salih/--linked-frame-workspace-name frame))
        (cond ((fboundp '+workspace-switch)
               (+workspace-switch workspace t))
              ((fboundp 'persp-frame-switch)
               (persp-frame-switch workspace frame))))))
  (set-frame-parameter frame 'workspace nil))

(defun salih/--linked-frame-add-buffer-to-workspace (buffer)
  "Add BUFFER to the current perspective when persp-mode is active."
  (when (and (buffer-live-p buffer)
             (bound-and-true-p persp-mode)
             (fboundp 'persp-add-buffer)
             (fboundp 'get-current-persp))
    (condition-case nil
        (persp-add-buffer buffer (get-current-persp) nil nil)
      (error nil))))

(defun salih/--linked-frame-add-state-buffers-to-workspace (state)
  "Add buffers in linked-frame STATE to the current perspective."
  (when-let* ((buffer (plist-get state :buffer)))
    (salih/--linked-frame-add-buffer-to-workspace buffer))
  (when-let* ((window-state (plist-get state :window-state)))
    (dolist (buffer (ignore-errors (window-state-buffers window-state)))
      (when (buffer-live-p buffer)
        (salih/--linked-frame-add-buffer-to-workspace buffer)))))

(defun salih/--linked-frame-apply-state (state frame)
  "Apply linked-frame STATE to FRAME."
  (let ((buffer (plist-get state :buffer)))
    (when (and (frame-live-p frame)
               (buffer-live-p buffer))
      (with-selected-frame frame
        (salih/--linked-frame-switch-workspace (plist-get state :workspace) frame)
        (salih/--linked-frame-add-state-buffers-to-workspace state)
        (when-let* ((window-state (plist-get state :window-state)))
          (window-state-put window-state (frame-root-window frame) 'safe))
        (let ((window (selected-window)))
          (unless (or (window-minibuffer-p window)
                      (window-dedicated-p window))
            (set-window-buffer window buffer)
            (set-window-hscroll window (or (plist-get state :hscroll) 0))
            (set-window-vscroll window (or (plist-get state :vscroll) 0) t)
            (set-window-point
             window
             (salih/--linked-frame-bounded-position
              buffer (plist-get state :point)))
            (set-window-start
             window
             (salih/--linked-frame-bounded-position
              buffer (plist-get state :start))
             t)))))))

(defun salih/--linked-frame-sync-eligible-p (state frame)
  "Return non-nil when FRAME should receive linked-frame STATE.
Linked frames are mirrored into one another only while they share a
workspace.  Buffer text and unsaved edits are already shared by Emacs
whenever two frames display the same buffer, so once a frame has diverged
into a different workspace it is left independent: opening a shared buffer
there must not drag the source workspace or window layout across and make
the frames identical again."
  (equal (plist-get state :workspace)
         (salih/--linked-frame-workspace-name frame)))

(defun salih/--linked-frame-sync (source-frame)
  "Mirror SOURCE-FRAME's selected-window state to linked sibling frames."
  (setq salih/--linked-frame-sync-timer nil)
  (when (and (not salih/--linked-frame-syncing)
             (frame-live-p source-frame)
             (salih/--linked-frame-p source-frame))
    (let ((state (salih/--linked-frame-state source-frame))
          (salih/--linked-frame-syncing t))
      (when state
        (dolist (frame (salih/--linked-frame-siblings source-frame))
          (unless (or (eq frame source-frame)
                      (not (salih/--linked-frame-sync-eligible-p state frame)))
            (salih/--linked-frame-apply-state state frame)))))))

(defun salih/--linked-frame-schedule-sync ()
  "Schedule a sync from the selected frame when it is linked."
  (unless salih/--linked-frame-syncing
    (let ((source-frame (selected-frame)))
      (when (and (salih/--linked-frame-p source-frame)
                 (not (minibufferp (window-buffer (selected-window)))))
        (when (timerp salih/--linked-frame-sync-timer)
          (cancel-timer salih/--linked-frame-sync-timer))
        (setq salih/--linked-frame-sync-timer
              (run-with-idle-timer
               salih/linked-frame-sync-delay
               nil
               #'salih/--linked-frame-sync
               source-frame))))))

(add-hook 'post-command-hook #'salih/--linked-frame-schedule-sync)

(defun salih/make-linked-frame (&optional parameters)
  "Create a new frame linked to the selected frame's workspace and cursor state."
  (interactive)
  (let* ((source-frame (selected-frame))
         (group (salih/--linked-frame-ensure-group source-frame))
         (state (salih/--linked-frame-state source-frame))
         (new-frame (make-frame parameters)))
    (set-frame-parameter new-frame 'salih-linked-frame-group group)
    (set-frame-parameter new-frame 'workspace nil)
    (when state
      (salih/--linked-frame-apply-state state new-frame))
    (when (fboundp 'persp-set-frame-buffer-predicate)
      (persp-set-frame-buffer-predicate new-frame))
    (select-frame-set-input-focus new-frame)
    (salih/--linked-frame-sync source-frame)
    new-frame))

;;; --- Embark ---
(after! embark
  (define-key embark-url-map (kbd "c") 'salih/open-url-in-chrome-cross-platform))

;;; --- Flyspell unbind ---
(after! flyspell
  (define-key flyspell-mode-map (kbd "C-;") nil))

;;; ===================================================================
;;; KEYBINDINGS — All via map! for consistency (Doom convention)
;;; ===================================================================

;;; --- Motion state ---
(map! :m "-"   #'er/expand-region
      :m "H-i" #'evil-jump-backward
      :m "C-o" #'evil-jump-forward)

;;; --- Insert state ---
(map! :i "C-x C-s" #'save-buffer
      :i "M-v"     #'yank
      :i "C-v"     #'yank)

;;; --- Normal/visual/insert ---
(map! :nvi "M-n" #'salih/make-linked-frame
      :nvi "M-s" #'salih/jinx-correct-or-save)

;;; --- Global (M-key) ---
(map! "M-;"      #'salih/comment-or-uncomment-region-or-line
      "M-<left>"  #'shrink-window-horizontally
      "M-<right>" #'enlarge-window-horizontally
      "M-<down>"  #'enlarge-window
      "M-<up>"    #'shrink-window
      "C-M-g"     #'+lookup/definition)

;;; --- Leader: Git ---
(map! :leader
      :prefix "m"
      "m" #'magit-status
      "c" #'magit-checkout
      "l" #'magit-log-buffer-file
      "d" #'magit-file-delete)

;;; --- Leader: Code / Errors ---
(map! :leader
      "c r" #'salih/rename-or-iedit
      "c a" #'lsp-execute-code-action
      "c d" #'salih/show-error-at-point
      ;; Error list & navigation
      "l e" #'salih/list-errors
      "l E" #'salih/list-errors-project
      "l a" #'lsp-execute-code-action)

;; Quick error jumping: ] e / [ e (normal mode, no leader)
(map! :n "] e" #'salih/next-error
      :n "[ e" #'salih/prev-error
      :n "] d" #'flycheck-next-error
      :n "[ d" #'flycheck-previous-error)

;;; --- Leader: Translate (visual) ---
(map! :leader :v "w t" #'gt-translate)

;;; --- Leader: Buffers & Navigation ---
(map! :leader
      "RET"   #'consult-buffer
      "["     #'previous-buffer
      "]"     #'next-buffer
      "0"     #'+workspace/close-window-or-workspace
      "b k"   #'kill-current-buffer
      "e e"   #'eshell
      "f p"   #'projectile-switch-project
      "t t"   #'salih/vterm
      "TAB d" #'+workspace:delete)

;;; --- Leader: Org agenda & calendar ---
(map! :leader
      "o a"   #'salih/org-agenda-no-full-f
      "o l"   #'salih/org-agenda-no-full-l
      "o f"   #'salih/org-agenda-full-f
      "o v"   #'salih/open-agenda
      "o i"   #'salih/open-inbox
      "o c"   #'calendar
      "n z"   #'salih/open-book)

;;; --- Leader: Roam (global) ---
(map! :leader
      :map 'override
      "r f" #'org-roam-node-find
      "r c" #'salih/org-roam-capture-fleet
      "r j" #'salih/org-roam-dailies-capture-today
      "r b" #'salih/org-roam-buffer
      "s r" #'salih/consult-org-roam-search
      "r s" #'salih/consult-org-roam-search)

;;; --- Org-mode leader ---
(map! :after org
      :map org-mode-map
      :leader
      "c i"   #'org-clock-in
      "c o"   #'org-clock-out
      "i i"   #'salih/org-id-get-create-with-custom-id
      "i c"   #'citar-insert-citation
      "i b"   #'orb-insert-link
      "b b"   #'org-noter
      "b k"   #'org-noter-kill-session
      "b o"   #'salih/org-noter-open-in-zathura
      "r i"   #'org-roam-node-insert
      "r I"   #'org-id-get-create 
      "r t"   #'org-roam-tag-add
      "r l f" #'consult-org-roam-forward-links
      "m z"   #'org-add-note
      "C-;"   #'salih/rename-or-iedit
      "o o"   #'salih/org-open-file-link-in-macos)

(map! :after org
      :map org-mode-map
      :leader :i
      "m f" #'org-footnote-action)

(map! :after org
      :map org-mode-map
      "C-c 8" #'org-ctrl-c-star)

;;; --- Org-noter ---
(map! :after org-noter
      :map org-noter-notes-mode-map
      :leader
      "b j" #'org-noter-sync-current-note)

(map! :after org-noter
      :map org-noter-doc-mode-map
      :nvi "C-c C-c" #'org-noter-insert-precise-note)

(map! :after nov
      :map nov-mode-map
      :leader
      "b s" #'nov-consult-search)

(after! nov
  (map! :map nov-mode-map   "l" nil)
  (map! :map nov-button-map "l" nil))

;;; --- PDF ---
(after! pdf-view
  (map! :map pdf-view-mode-map
        :prefix "SPC n"
        "i" #'org-noter-insert-precise-note
        "o" #'salih/zathura-open
        "d" #'pdf-view-themed-minor-mode)
  (map! :map pdf-view-mode-map
        :n "J" #'pdf-view-next-page-command
        :n "K" #'pdf-view-previous-page-command))

(map! :after pdf-occur
      :map pdf-occur-buffer-mode-map
      :n "RET" #'salih/pdf-occure)

(add-hook! 'pdf-view-mode-hook
  (setq-local evil-normal-state-cursor (list nil)
              browse-url-browser-function 'salih/open-url-in-chrome-cross-platform))

;;; --- LSP keybindings ---
(map! :after lsp-mode
      :map lsp-mode-map
      "M-RET" #'lsp-execute-code-action)

(map! :after scala-ts-mode
      :map scala-ts-mode-map
      "M-RET" #'eglot-code-actions)

;;; --- Dired ---
(map! :after dired
      :map dired-mode-map
      :leader
      "o o" #'salih/open-in-external-app)

;;; --- Wordnut ---
(map! :after wordnut
      :map wordnut-mode-map
      :n "q" #'+workspace/close-window-or-workspace)

;;; --- EWW ---
(map! :after eww
      :map eww-mode-map
      "C" #'eww-browse-with-external-browser)

;;; --- SHR unbinds ---
(after! shr
  (map! :map shr-map "u" nil "w" nil))

;;; --- Mu4e ---
(map! :after mu4e
      :map mu4e-headers-mode-map
      :n "r" #'mu4e-headers-mark-for-refile)

(map! :after mu4e
      :map mu4e-view-mode-map
      :n ";"       #'salih/mu4e-go-to-url
      :n "B"       #'salih/mu4e-view-and-copy-html
      :n "C-c C-c" #'salih/mu4e-org-store-and-capture)

(map! :after mu4e
      :map mu4e-view-active-urls-keymap
      "RET"    'mu4e--view-browse-url-from-binding
      [return] 'mu4e--view-browse-url-from-binding)

;;; --- Text mode ---
(map! :map text-mode-map
      :n ";" #'embark-act)

;;; --- Calendar ---
(after! calendar
  (evil-define-key 'normal calendar-mode-map (kbd "RET")
    'salih/org-calendar-goto-agenda))

;;; --- Org-fc ---
(after! org-fc
  (evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-flip-mode
    (kbd "r") 'org-fc-review-flip
    (kbd "n") 'org-fc-review-flip
    (kbd "s") 'org-fc-review-suspend-card
    (kbd "q") 'org-fc-review-quit)
  (evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-rate-mode
    (kbd "a") 'org-fc-review-rate-again
    (kbd "h") 'org-fc-review-rate-hard
    (kbd "g") 'org-fc-review-rate-good
    (kbd "e") 'org-fc-review-rate-easy
    (kbd "s") 'org-fc-review-suspend-card
    (kbd "q") 'org-fc-review-quit))

;;; --- Org-present ---
(after! org-present
  (define-key org-present-mode-keymap (kbd "SPC") nil))

(provide 'lr-editor)
