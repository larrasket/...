;;; config.el -*- lexical-binding: t; -*-

;;; --- User Info ---
(setq user-full-name    "Salih Muhammed"
      user-mail-address "salih.moahabdelhafez@halan.com")

(defvar user-first-name       "Salih")
(defvar user-stmp-server      "smtp.mail.me.com")
(defvar user-stmp-port        587)
(defvar user-short-username   "lr0")
(defvar user-config-repo-path "~/configs/~s")

;;; --- Paths ---
(defvar salih/blog-content-path "~/blog/content")
(defvar salih/hugo-directory    "~/roam/hugo/")
(defvar salih/source-directory  "~/roam/source")

;;; --- Org directories (set early, used by deferred modules) ---
(setq org-roam-directory (file-truename "~/roam")
      org-directory      (file-truename "~/roam")
      org-id-locations-file "~/roam/.orgids")

;;; --- Org capture files ---
(setq +org-capture-changelog-file "~/blog/content/nice.org"
      +org-capture-journal-file   "~/blog/content/stack.org"
      +org-capture-todo-file      "~/roam/main/life.org")

(defvar salih/org-roam-fleet-file "~/roam/main/lr.org")
(defvar salih/org-vocal-store     "~/roam/media/vocal")

;;; --- State variables ---
(defvar salih/vulpea-show-full nil)
(defvar salih/adding-note?    nil)
(defvar salih/org-roam-dailies-capture-p nil)

;;; --- Font ---
(setq doom-font (font-spec :family "PragmataPro" :size 16)
      doom-variable-pitch-font (font-spec :family "Iosevka Term" :size 16))

;;; --- Theme ---
;;; doom-badger
;;; doom-opera
;;; doom-bluloco-dark
;;; doom-tomorrow-night
;;; doom-wilmersdorf
;;; ef-owl
(setq doom-theme 'doom-badger)

;;; --- Basic settings ---
(setq display-line-numbers-type 'relative
      auto-save-no-message      t
      warning-minimum-level     :error
      evil-respect-visual-line-mode t
      fast-but-imprecise-scrolling t
      auto-window-vscroll nil
      process-adaptive-read-buffering nil)

(setq-default bidi-paragraph-direction 'left-to-right
              frame-title-format       '("%b"))

;;; --- Encryption ---
(setq epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-select-keys    'silent
      epa-file-encrypt-to     user-mail-address)

;;; --- GC / Performance ---
(after! gcmh
  (setq gcmh-high-cons-threshold (* 256 1024 1024)
        gc-cons-threshold        (* 100 1024 1024)
        gc-cons-percentage       0.6))

(setq read-process-output-max (* 4 1024 1024)
      undo-limit           80000000
      undo-strong-limit    120000000
      undo-outer-limit     360000000
      treesit-font-lock-level 3)

;;; --- Safe local variables ---
(setq safe-local-variable-values
      '((org-download-image-dir . "../i")
        (org-download-image-dir . "../../media")
        (salih/rebuild . t)
        (go-test-args . "-tags libsqlite3 -timeout 120s")
        (go-test-args . "-tags libsqlite3 -timeout 90s")))

(put 'org-download-image-dir 'safe-local-variable #'stringp)

;;; --- Keyboard translation (must be early) ---
(defun salih/keyboard-config ()
  (when (display-graphic-p)
    (keyboard-translate ?\C-m ?\H-m)
    (keyboard-translate ?\C-i ?\H-i))
  (define-key key-translation-map (kbd "C-g") (kbd "<escape>")))

(salih/keyboard-config)

;;; --- Popup rules ---
(set-popup-rules! '(("^\\*Project errors\\*" :size 0.25)))

;;; --- Fix: global-git-commit-mode void-variable 'function' bug ---
(remove-hook 'doom-first-file-hook #'global-git-commit-mode)
(with-eval-after-load 'git-commit
  (add-hook 'find-file-hook #'git-commit-setup-check-buffer)
  (add-hook 'after-change-major-mode-hook #'git-commit-setup-font-lock-in-buffer))

;;; --- Suppress org-roam's blocking full DB sync ---
;; org-roam-db-sync opens EVERY roam file via find-file-noselect, which
;; triggers vc-refresh-state → git subprocess per file → Emacs freezes.
;;
;; Sources of unwanted syncs we block:
;;   • org-roam-db-autosync-enable on startup
;;   • citar-org-roam-setup during font-lock (triggered by citar-org-activate)
;;   • +org-roam-try-init-db-a (Doom's lazy-init advice) on first db-query
;;
;; We allow sync only when:
;;   • Called interactively (M-x org-roam-db-sync)
;;   • Our idle timer sets salih/--org-roam-allow-sync to t
;;
;; Per-file incremental updates (org-roam-db-update-file via after-save-hook)
;; are NOT affected — they never go through org-roam-db-sync.
(defvar salih/--org-roam-allow-sync nil)
(defadvice! salih/org-roam-block-eager-sync-a (&rest _)
  :before-while #'org-roam-db-sync
  (or salih/--org-roam-allow-sync
      (called-interactively-p 'any)))

;;; --- Load core modules ---
(require 'lr-macos)
(require 'lr-ui)
(require 'lr-completion)
(require 'lr-editor)
(require 'lr-prog)
(require 'lr-tools)
;; (require 'lr-writing)

;;; --- Defer heavy modules ---
(with-eval-after-load 'org
  (require 'lr-org-core)
  (require 'lr-org-roam)
  (require 'lr-roam-lint)
  (require 'lr-org-noter)
  (require 'lr-academic)
  (require 'lr-roam-lint))

;; (with-eval-after-load 'mu4e
;;   (require 'lr-email))

(with-eval-after-load 'circe
  (require 'lr-irc))

;;; --- Pre-load org-roam in background (idle) and sync DB ---
;; At 3s idle: load org + org-roam so "r" in consult-buffer works.
;; At 5s idle: run the full DB sync (safe — Emacs is idle, no blocking UX).
;;   Uses the allow-flag to bypass the eager-sync block above.
(run-with-idle-timer
 3 nil
 (lambda ()
   (require 'org)
   (require 'org-roam)))

(run-with-idle-timer
 5 nil
 (lambda ()
   (when (featurep 'org-roam)
     (let ((salih/--org-roam-allow-sync t))
       (org-roam-db-sync)))))

;;; --- Pre-warm agenda file buffers in background ---
;; At 8s idle (after org/org-roam are loaded by the 3s timer), populate
;; org-agenda-files from vulpea and pre-parse the buffers.  This way the
;; first `org-agenda` call only needs to render, not do file I/O.
(run-with-idle-timer
 8 nil
 (lambda ()
   (when (and (featurep 'org-roam)
              (fboundp 'vulpea-agenda-files-update))
     (vulpea-agenda-files-update)
     (when org-agenda-files
       (org-agenda-prepare-buffers org-agenda-files)))))

;; (add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e")
;; (mu4e-alert-enable-mode-line-display)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(defun join-paragraph-lines ()
  "Join wrapped lines within each paragraph into a single line.  Paragraphs are
separated by one or more blank lines.  Skips org headings (lines starting with
*) and property drawers (lines starting with :)."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      ;; Skip blank lines between paragraphs
      (while (and (not (eobp)) (looking-at "^[[:space:]]*$"))
        (forward-line 1))
      ;; Join lines within the current paragraph
      (while (and (not (eobp)) (not (looking-at "^[[:space:]]*$")))
        (if (looking-at "^\\*\\|^:")
            ;; It's a heading or property line — skip it entirely
            (forward-line 1)
          ;; It's a regular paragraph line — join with next if next is also regular
          (end-of-line)
          (when (not (eobp))
            (let ((next-line-empty-or-special
                   (save-excursion
                     (forward-line 1)
                     (or (looking-at "^[[:space:]]*$")
                         (looking-at "^\\*")
                         (looking-at "^:")))))
              (unless next-line-empty-or-special
                (delete-char 1)
                (just-one-space))))
          (forward-line 1))))))

(defun remove-org-properties ()
  "Remove all :PROPERTIES: drawers from an org buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]*:PROPERTIES:\n" nil t)
      (let ((start (match-beginning 0)))
        (when (re-search-forward "^[[:space:]]*:END:\n?" nil t)
          (delete-region start (point)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
