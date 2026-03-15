;;; config.el -*- lexical-binding: t; -*-

;;; --- User Info ---
(setq user-full-name    "Salih Muhammed"
      user-mail-address "root@lr0.org")

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
(defvar salih/temp-roam-insert nil)
(defvar salih/org-roam-dailies-capture-p nil)

;;; --- Font ---
(setq doom-font (font-spec :family "Pragmasevka" :size 16)
      doom-variable-pitch-font (font-spec :family "Iosevka Term" :size 16))

;;; --- Theme ---
(setq doom-theme 'ef-elea-dark)

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
;; Doom adds this to doom-first-file-hook but it errors on first file open.
;; Remove from the hook and wire up git-commit manually when the package loads.
(remove-hook 'doom-first-file-hook #'global-git-commit-mode)
(with-eval-after-load 'git-commit
  (add-hook 'find-file-hook #'git-commit-setup-check-buffer)
  (add-hook 'after-change-major-mode-hook #'git-commit-setup-font-lock-in-buffer))

;;; --- Load core modules (needed at or shortly after startup) ---
(require 'lr-macos)
(require 'lr-ui)
(require 'lr-completion)
(require 'lr-editor)
(require 'lr-prog)
(require 'lr-tools)

;;; --- Defer heavy modules (load only when their parent package loads) ---
(with-eval-after-load 'org
  (require 'lr-org-core)
  (require 'lr-org-roam)
  (require 'lr-org-noter)
  (require 'lr-academic))

(with-eval-after-load 'mu4e
  (require 'lr-email))

(with-eval-after-load 'circe
  (require 'lr-irc))

(add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e")
