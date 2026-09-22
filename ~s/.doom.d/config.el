;;; config.el -*- lexical-binding: t; -*-

;; Declare `so-long-target-modes' special EARLY.  Doom's lang/org module
;; lexically `let'-binds it inside `+org-get-agenda-file-buffer' (compiled
;; before so-long loads).  On Emacs 32, if that advice runs before so-long.el's own `defvar', the later defvar hard-errors with "Defining
;; as dynamic an already lexical var so-long-target-modes" - which then
;; cascades into flycheck's org-lint checker.  Marking it special here,
;; before any agenda/first-file activity, makes so-long.el's defvar a
;; harmless re-declaration instead of a conflict.
(defvar so-long-target-modes nil)

(setq user-full-name    "Saleh"
      user-mail-address "root@lr0.org")

(defvar user-first-name       "Saleh")
(defvar user-stmp-server      "smtp.mail.me.com")
(defvar user-stmp-port        587)
(defvar user-short-username   "lr0")
(defvar user-config-repo-path "~/configs/~s")

(defvar salih/blog-content-path "~/blog/content")
(defvar salih/hugo-directory    "~/roam/hugo/")
(defvar salih/source-directory  "~/roam/source")

(setq org-roam-directory (file-truename "~/roam")
      org-directory      (file-truename "~/roam")
      org-id-locations-file "~/roam/.orgids")

;;; Org capture files
(setq +org-capture-changelog-file "~/blog/content/nice.org"
      +org-capture-journal-file   "~/blog/content/stack.org"
      +org-capture-todo-file      "~/roam/main/life.org")

(defvar salih/org-roam-fleet-file "~/roam/main/lr.org")
(defvar salih/org-vocal-store     "~/roam/media/vocal")

;;; State variables
(defvar salih/vulpea-show-full nil)
(defvar salih/org-roam-dailies-capture-p nil)

;;; Font
(setq doom-font (font-spec :family "Pragmasevka" :size 18)
      doom-variable-pitch-font (font-spec :family "Iosevka Term" :size 18))

;;; Theme
;;; doom-badger
;;; doom-opera
;;; doom-bluloco-dark
;;; doom-tomorrow-night
;;; doom-wilmersdorf
;;; ef-owl
;;; doom-tokyo-night
;;; doom-one
;;; ef-maris-dark
;;; ef-dream
;;; modus-vivendi-tritanopia
;;; ef-cherie

;;; Basic settings
(setq display-line-numbers-type 'relative
      auto-save-no-message      t
      warning-minimum-level     :error
      fast-but-imprecise-scrolling t
      auto-window-vscroll nil
      process-adaptive-read-buffering nil)

(setq-default bidi-paragraph-direction 'left-to-right
              frame-title-format       '("%b"))

(setq epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-select-keys    'silent
      epa-file-encrypt-to     user-mail-address)

(after! gcmh
  (setq gcmh-high-cons-threshold (* 256 1024 1024)
        gc-cons-threshold        (* 100 1024 1024)
        gc-cons-percentage       0.6))

(setq read-process-output-max   (* 4 1024 1024)
      undo-limit                80000000
      undo-strong-limit         120000000
      undo-outer-limit          360000000
      treesit-font-lock-level   3)

(setq safe-local-variable-values
      '((org-download-image-dir . "../i")
        (org-download-image-dir . "../../media")
        (salih/rebuild . t)
        (go-test-args . "-tags libsqlite3 -timeout 120s")
        (go-test-args . "-tags libsqlite3 -timeout 90s")))

(put 'org-download-image-dir 'safe-local-variable #'stringp)

(defun salih/keyboard-config ()
  (when (display-graphic-p)
    (keyboard-translate ?\C-m ?\H-m)
    (keyboard-translate ?\C-i ?\H-i))
  (define-key key-translation-map (kbd "C-g") (kbd "<escape>")))

(salih/keyboard-config)

;;; Popup rules
(set-popup-rules! '(("^\\*Project errors\\*" :size 0.25)))

;;; Fix: global-git-commit-mode void-variable 'function' bug
(remove-hook 'doom-first-file-hook #'global-git-commit-mode)
(with-eval-after-load 'git-commit
  (add-hook 'find-file-hook #'git-commit-setup-check-buffer)
  (add-hook 'after-change-major-mode-hook #'git-commit-setup-font-lock-in-buffer))

;;; Suppress org-roam's blocking full DB sync
;; org-roam-db-sync opens EVERY roam file via find-file-noselect, which
;; triggers vc-refresh-state to git subprocess per file to Emacs freezes.
;;
;; Sources of unwanted syncs we block:
;;   - org-roam-db-autosync-enable on startup
;;   - citar-org-roam-setup during font-lock (triggered by citar-org-activate)
;;   - +org-roam-try-init-db-a (Doom's lazy-init advice) on first db-query
;;
;; We allow sync only when:
;;   - Called interactively (M-x org-roam-db-sync)
;;   - Our idle timer sets salih/--org-roam-allow-sync to t
;;
;; Per-file incremental updates (org-roam-db-update-file via after-save-hook)
;; are NOT affected - they never go through org-roam-db-sync.
(defvar salih/--org-roam-allow-sync nil)
(defadvice! salih/org-roam-block-eager-sync-a (&rest _)
  :before-while #'org-roam-db-sync
  (or salih/--org-roam-allow-sync
      (called-interactively-p 'any)))

;;; Load core modules
(require 'lr-macos)
(require 'lr-ui)
(require 'lr-completion)
(require 'lr-editor)
(require 'lr-prog)
(require 'lr-tools)
(require 'lr-elfeed)
(require 'lr-fedi)
(require 'lr-agent)

;;; Context switcher (lr-context)
;; One command to move between working modes (work / reading / blog / study),
;; carrying workspace + window layout + clock + notification profile, with all
;; facts about work in ~/roam/main/contexts.org.  Lazy: the SPC d prefix is
;; live from startup; the module (and org) load on first use via the autoloads,
;; so this adds nothing to boot time.
(dolist (cmd '(lr-context-switch lr-context-define lr-context-list
               lr-context-resume-clock lr-context-visit))
  (autoload cmd "lr-context" nil t))
(map! :leader
      (:prefix ("d" . "context")
       :desc "Switch context"        "d" #'lr-context-switch
       :desc "New context"           "n" #'lr-context-define
       :desc "List contexts / time"  "l" #'lr-context-list
       :desc "Resume context clock"  "r" #'lr-context-resume-clock
       :desc "Open contexts.org"     "o" #'lr-context-visit
       :desc "Coach check-in / clock" "j" #'lr-track-checkin))

;;; Attention tracker + accountability coach (lr-track)
;; Background sensing (HID + Emacs idle + focus) -> engaged/elsewhere/away/slept,
;; a modeline badge, an org activity log (~/roam/main/activity.org), trustworthy
;; clocks (auto clock-out on away/sleep, idle subtracted), and nudges when you're
;; clocked-but-elsewhere.  org-free at load; org loads lazily on first clock.
(require 'lr-track)
;; Plain "d X" sequences: a SECOND (:prefix ("d" . "context") ...) block would
;; recreate the SPC d keymap and wipe the context bindings defined above.
(map! :leader
      :desc "Clock in (search)"    "d i" #'lr-track-clock-in
      :desc "Track status (now)"   "d s" #'lr-track-status
      :desc "Toggle tracking"      "d t" #'lr-track-mode
      :desc "Tracker doctor"       "d k" #'lr-track-doctor)
;; Start the coach a couple seconds after init so the first frame is never blocked.
(add-hook 'doom-after-init-hook
          (lambda () (run-with-timer 2 nil (lambda () (lr-track-mode 1)))))

;;; Defer heavy modules
(with-eval-after-load 'org
  (require 'lr-org-core)
  (require 'lr-org-roam)
  (require 'lr-org-noter)
  (require 'lr-academic))

;; (with-eval-after-load 'mu4e
;;   (require 'lr-email))

(with-eval-after-load 'circe
  (require 'lr-irc))

;; Kept so `salih/open-inbox' can (require 'mu4e) on demand while the :email
;; module is disabled.
(add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e")


(defvar my/theme-cycle nil
  "Shuffled list of themes to cycle through without repetition.")

(defun my/shuffle-list (list)
  "Return a shuffled copy of LIST."
  (let ((vec (vconcat list)))
    (dotimes (i (length vec))
      (let* ((j (+ i (random (- (length vec) i))))
             (tmp (aref vec i)))
        (aset vec i (aref vec j))
        (aset vec j tmp)))
    (append vec nil)))

(defun my/load-random-theme ()
  "Load a random theme without repeating until all themes are used."
  (interactive)
  (unless my/theme-cycle
    (setq my/theme-cycle (my/shuffle-list (custom-available-themes))))
  
  (let ((theme (pop my/theme-cycle)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)
    (message "Loaded theme: %s" theme)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq magit-git-executable "/opt/homebrew/bin/git")




(setq org-extend-today-until 5)

(use-package ghostel-eshell
  :hook (eshell-load . ghostel-eshell-visual-command-mode))

(use-package ghostel-comint
  :hook (after-init . ghostel-comint-global-mode))

(use-package ghostel-compile
  :hook (after-init . ghostel-compile-global-mode))

(use-package evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))


(map! "M-f" #'consult-line)

(setq doom-theme 'kaolin-dark)

(solaire-global-mode +1)

;; Zero fringes.  Doom's `vc-gutter +pretty' runs `(fringe-mode 8)' and the
;; initial GUI frame doesn't retain an early `set-fringe-style', so re-assert 0
;; once init has settled.  (Solaire already remaps the `fringe' face to
;; `solaire-fringe-face' per buffer, so a fringe would still match each buffer's
;; background if one is ever wanted back.)
(set-fringe-style 0)
(add-hook 'doom-after-init-hook (lambda () (fringe-mode 0)))
