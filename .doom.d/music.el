;;; ../configs/.doom.d/music.el -*- lexical-binding: t; -*-


(use-package bongo
  :ensure
  :config
  (setq bongo-default-directory "~/music/")
  (setq bongo-prefer-library-buffers nil)
  (setq bongo-insert-whole-directory-trees t)
  (setq bongo-logo nil)
  (setq bongo-display-track-icons nil)
  (setq bongo-display-track-lengths nil)
  (setq bongo-display-header-icons nil)
  (setq bongo-display-playback-mode-indicator t)
  (setq bongo-display-inline-playback-progress t)
  (setq bongo-join-inserted-tracks nil)
  (setq bongo-field-separator (propertize " · " 'face 'shadow))
  (setq bongo-mark-played-tracks t)
  (setq bongo-header-line-mode nil)
  ;; (setq bongo-mode-line-indicator-mode nil)
  (setq bongo-enabled-backends '(vlc mpv))
  (setq bongo-vlc-program-name "cvlc")

;;; Bongo playlist buffer
  (defvar prot/bongo-playlist-delimiter
    "\n******************************\n\n"
    "Delimiter for inserted items in `bongo' playlist buffers.")

  (defun prot/bongo-playlist-section ()
    (bongo-insert-comment-text
     prot/bongo-playlist-delimiter))

  (defun prot/bongo-paylist-section-next ()
    "Move to next `bongo' playlist custom section delimiter."
    (interactive)
    (let ((section "^\\*+$"))
      (if (save-excursion (re-search-forward section nil t))
          (progn
            (goto-char (point-at-eol))
            (re-search-forward section nil t))
        (goto-char (point-max)))))

  (defun prot/bongo-paylist-section-previous ()
    "Move to previous `bongo' playlist custom section delimiter."
    (interactive)
    (let ((section "^\\*+$"))
      (if (save-excursion (re-search-backward section nil t))
          (progn
            (goto-char (point-at-bol))
            (re-search-backward section nil t))
        (goto-char (point-min)))))

  (defun prot/bongo-playlist-mark-section ()
    "Mark `bongo' playlist section, delimited by custom markers.
The marker is `prot/bongo-playlist-delimiter'."
    (interactive)
    (let ((section "^\\*+$"))
      (search-forward-regexp section nil t)
      (push-mark nil t)
      (forward-line -1)
      ;; REVIEW any predicate to replace this `save-excursion'?
      (if (save-excursion (re-search-backward section nil t))
          (progn
            (search-backward-regexp section nil t)
            (forward-line 1))
        (goto-char (point-min)))
      (activate-mark)))

  (defun prot/bongo-playlist-kill-section ()
    "Kill `bongo' playlist-section at point.
This operates on a custom delimited section of the buffer.  See
`prot/bongo-playlist-kill-section'."
    (interactive)
    (prot/bongo-playlist-mark-section)
    (bongo-kill))

  (defun prot/bongo-playlist-play-random ()
    "Play random `bongo' track and determine further conditions."
    (interactive)
    (unless (bongo-playlist-buffer)
      (bongo-playlist-buffer))
    (when (or (bongo-playlist-buffer-p)
              (bongo-library-buffer-p))
      (unless (bongo-playing-p)
        (with-current-buffer (bongo-playlist-buffer)
          (bongo-play-random)
          (bongo-random-playback-mode 1)
          (bongo-recenter)))))

  (defun prot/bongo-playlist-random-toggle ()
    "Toggle `bongo-random-playback-mode' in playlist buffers."
    (interactive)
    (if (eq bongo-next-action 'bongo-play-random-or-stop)
        (bongo-progressive-playback-mode)
      (bongo-random-playback-mode)))

  (defun prot/bongo-playlist-reset ())("list-file'."
    (interactive)
    (let* ((path "~/music/")
           (dotless directory-files-no-dot-files-regexp)
           (playlists (mapcar
                       'abbreviate-file-name
                       (directory-files path nil dotless)))
           (choice (completing-read "Insert playlist: " playlists nil t)))
      (if (bongo-playlist-buffer-p)
          (progn
            (save-excursion
              (goto-char (point-max))
              (bongo-insert-playlist-contents
               (format "%s%s" path choice))
              (prot/bongo-playlist-section))
            (prot/bongo-playlist-play-random))
        (user-error "Not in a `bongo' playlist buffer"))))

;;; Bongo + Dired (bongo library buffer)
  (defmacro prot/bongo-dired-library (name doc val)
    "Create `bongo' library function NAME with DOC and VAL."
    `(defun ,name ()
       ,doc
       (when (string-match-p "\\`~/music/" default-directory)
         (bongo-dired-library-mode ,val))))

  (prot/bongo-dired-library
   prot/bongo-dired-library-enable
   "Set `bongo-dired-library-mode' when accessing ~/music.

Add this to `dired-mode-hook'.  Upon activation, the directory
and all its sub-directories become a valid library buffer for
Bongo, from where we can, among others, add tracks to playlists.
The added benefit is that Dired will continue to behave as
normal, making this a superior alternative to a purpose-specific
library buffer.

Note, though, that this will interfere with `wdired-mode'.  See
`prot/bongo-dired-library-disable'."
   1)

  ;; NOTE `prot/bongo-dired-library-enable' does not get reactivated
  ;; upon exiting `wdired-mode'.
  ;;
  ;; TODO reactivate bongo dired library upon wdired exit
  (prot/bongo-dired-library
   prot/bongo-dired-library-disable
   "Unset `bongo-dired-library-mode' when accessing ~/music.
This should be added `wdired-mode-hook'.  For more, refer to
`prot/bongo-dired-library-enable'."
   -1)

  (defun prot/bongo-dired-insert-files ()
    "Add files in a `dired' buffer to the `bongo' playlist."
    (let ((media (dired-get-marked-files)))
      (with-current-buffer (bongo-playlist-buffer)
        (goto-char (point-max))
        (mapc 'bongo-insert-file media)
        (prot/bongo-playlist-section))
      (with-current-buffer (bongo-library-buffer)
        (dired-next-line 1))))

  (defun prot/bongo-dired-insert ()
    "Add `dired' item at point or marks to `bongo' playlist.

The playlist is created, if necessary, while some other tweaks
are introduced.  See `prot/bongo-dired-insert-files' as well as
`prot/bongo-playlist-play-random'.

Meant to work while inside a `dired' buffer that doubles as a
library buffer (see `prot/bongo-dired-library')."
    (interactive)
    (when (bongo-library-buffer-p)
      (unless (bongo-playlist-buffer-p)
        (bongo-playlist-buffer))
      (prot/bongo-dired-insert-files)
      (prot/bongo-playlist-play-random)))

  (defun prot/bongo-dired-make-playlist-file ()
    "Add `dired' marked items to playlist file using completion.

These files are meant to reference filesystem paths.  They ease
the task of playing media from closely related directory trees,
without having to interfere with the user's directory
structure (e.g. a playlist file 'rock' can include the paths of
~/music/Scorpions and ~/music/Queen).

This works by appending the absolute filesystem path of each item
to the selected playlist file.  If no marks are available, the
item at point will be used instead.

Selecting a non-existent file at the prompt will create a new
entry whose name matches user input.  Depending on the completion
framework, such as with `icomplete-mode', this may require a
forced exit (e.g. \\[exit-minibuffer] to parse the input without
further questions).

Also see `prot/bongo-playlist-insert-playlist-file'."
    (interactive)
    (let* ((dotless directory-files-no-dot-files-regexp)
           (pldir "~/music/playlists")
           (playlists (mapcar
                       'abbreviate-file-name
                       (directory-files pldir nil dotless)))
           (plname (completing-read "Select playlist: " playlists nil nil))
           (plfile (format "%s/%s" pldir plname))
           (media-paths
            (if (derived-mode-p 'dired-mode)
                ;; TODO more efficient way to do ensure newline ending?
                ;;
                ;; The issue is that we need to have a newline at the
                ;; end of the file, so that when we append again we
                ;; start on an empty line.
                (concat
                 (mapconcat #'identity
                            (dired-get-marked-files)
                            "\n")
                 "\n")
              (user-error "Not in a `dired' buffer"))))
      ;; The following `when' just checks for an empty string.  If we
      ;; wanted to make this more robust we should also check for names
      ;; that contain only spaces and/or invalid characters…  This is
      ;; good enough for me.
      (when (string-empty-p plname)
        (user-error "No playlist file has been specified"))
      (unless (file-directory-p pldir)
        (make-directory pldir))
      (unless (and (file-exists-p plfile)
                   (file-readable-p plfile)
                   (not (file-directory-p plfile)))
        (make-empty-file plfile))
      (append-to-file media-paths nil plfile)
      (with-current-buffer (find-file-noselect plfile)
        (delete-duplicate-lines (point-min) (point-max))
        (sort-lines nil (point-min) (point-max))
        (save-buffer)
        (kill-buffer))))

  :hook ((dired-mode-hook . prot/bongo-dired-library-enable)
         (wdired-mode-hook . prot/bongo-dired-library-disable)
         )
  :bind (;; ("<C-XF86AudioPlay>" . bongo-pause/resume)
         ;; ("<C-XF86AudioNext>" . bongo-next)
         ;; ("<c-XF86AudioPrev>" . bongo-previous)
         ;; ("<M-XF86AudioPlay>" . bongo-show)
         ;; ("<S-XF86AudioNext>" . bongo-seek-forward-10)
         ;; ("<S-XF86AudioPrev>" . bongo-seek-backward-10)
         ;; ("<M-XF86AudioPlay>" . bongo-show)
         ;; ("<S-XF86AudioNext>" . bongo-seek-forward-10)
         ;; ("<S-XF86AudioPrev>" . bongo-seek-backward-10)
         ("<C-AudioNext>" . bongo-next)
         ("<C-AudioPrev>" . bongo-previous)
         ("<C-AudioPlay>" . bongo-pause/resume)
         :map bongo-playlist-mode-map
         ("n" . bongo-next-object)
         ("p" . bongo-previous-object)
         ("M-n" . prot/bongo-paylist-section-next)
         ("M-p" . prot/bongo-paylist-section-previous)
         ("M-h" . prot/bongo-playlist-mark-section)
         ("M-d" . prot/bongo-playlist-kill-section)
         ("g" . prot/bongo-playlist-reset)
         ("D" . prot/bongo-playlist-terminate)
         ("r" . prot/bongo-playlist-random-toggle)
         ("R" . bongo-rename-line)
         ("j" . bongo-dired-line)       ; Jump to dir of file at point
         ("J" . dired-jump)             ; Jump to library buffer
         ("i" . prot/bongo-playlist-insert-playlist-file)
         ("I" . bongo-insert-special)
         :map bongo-dired-library-mode-map
         ("<C-return>" . prot/bongo-dired-insert)
         ("C-c SPC" . prot/bongo-dired-insert)
         ("C-c +" . prot/bongo-dired-make-playlist-file))
  ;; :custom
  ;; (bongo-mode-line-indicator-mode 't)
  )

;; (setq bongo-mode-line-indicator-mode 't)





;;;;;;;;;;;;;;;;
;; fuck bongo ;;
;;;;;;;;;;;;;;;;







(emms-all)
(emms-default-players)
(setq emms-source-file-default-directory "~/music/")
(setq emms-browser-covers 'emms-browser-cache-thumbnail-async)





;; ("<C-XF86AudioNext>" . bongo-next)
;; ("<c-XF86AudioPrev>" . bongo-previous)
;; ("<M-XF86AudioPlay>" . bongo-show)
;; ("<S-XF86AudioNext>" . bongo-seek-forward-10)
;; ("<S-XF86AudioPrev>" . bongo-seek-backward-10)
;; ("<M-XF86AudioPlay>" . bongo-show)
;; ("<S-XF86AudioNext>" . bongo-seek-forward-10)
;; ("<S-XF86AudioPrev>" . bongo-seek-backward-10)



(global-set-key (kbd "<XF86AudioPrev>") 'emms-previous)
(global-set-key (kbd "<XF86AudioNext>") 'emms-next)
(global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)

;; if in wayland

;; (global-set-key (kbd "<C-AudioPrev>") 'emms-previous)
;; (global-set-key (kbd "<C-AudioNext>") 'emms-next)
;; (global-set-key (kbd "<C-AudioPlay>") 'emms-pause)
(setq emms-info-functions '(emms-info-tinytag))  ;; When using Tinytag




























(provide 'music)
