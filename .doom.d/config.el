(add-to-list 'load-path "~/.doom.d/")
(require '+roam)
(require '+handy)
(require 'keys)
(require 'epa-file)


(add-to-list 'org-agenda-files "~/roam/journal/agenda/todo.org")
(add-to-list 'org-agenda-files "~/roam/journal/agenda/births.org")


(setq load-prefer-newer t ;; avoid warnings
;; set org files
      +org-capture-journal-file "~/blog/content/stack.org"
      +org-capture-changelog-file "~/blog/content/nice.org"
      +org-capture-todo-file "~/roam/main/life.org"
      org-preview-html-viewer 'xwidget
      org-roam-directory "~/roam"

      highlight-indent-guides-method 'bitmap
      browse-url-browser-function 'xwidget-webkit-browse-url
      inferior-lisp-program "sbcl"
      large-file-warning-threshold nil

      org-crypt-key "ghd@keemail.me"
      epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-select-keys 'silent
      epa-file-encrypt-to "ghd@keemail.me"


      user-full-name "Salih Muhammed"
      user-mail-address "ghd@keemail.me"

      fancy-splash-image "~/.doom.d/pan.png"

;; modern org mode style
      highlight-indent-guides-method 'bitmap
      org-modern-block-name '(("" "" ""))
      org-modern-checkbox nil
      org-modern-keyword '(("" . ""))
      org-modern-list nil
      org-modern-priority nil
      org-modern-star nil
      org-modern-tag nil
      org-modern-timestamp nil
      org-modern-todo nil

      doom-theme 'distinguished)





(add-hook 'org-mode-hook 'highltier)
(add-hook 'prog-mode-hook 'highltier)
(add-hook 'prog-mode-hook 'column-enforce-mode)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'neotree-mode-hook #'hide-mode-line-mode)


(epa-file-enable)
(global-wakatime-mode)
(global-org-modern-mode)
















;;(advice-add #'vertico--display-candidates :around #'my-display)
;;(advice-add #'vertico--resize-window :around #'no-resize-vertico)


;;(defun no-resize-vertico (&rest _))

;; TODO open issues:
;; DONE cursor in minibuffer is still shown
;; DONE vertico--exhibit/consult--refresh-hook should be hooked into, in order to ensure that resize works
;; TODO recursive minibuffers
;; TODO mini-frame resizing
;; TODO find a good name

(defun my-display (_ lines))
  ;;(mini-popup--make-frame 0 (- (frame-pixel-height) 300) (- (frame-pixel-width) 20) 300 (string-join lines))
  ;;(mini-popup--make-frame 0 0 (frame-pixel-width) 300 (string-join lines))
  ;;(mini-popup--make-frame 100 100 (frame-pixel-width) 300 (string-join lines))


(defvar mini-popup--frame-parameters nil)

(setq mini-popup--frame-parameters
  '((no-accept-focus . t)
    (min-width . t)
    (min-height . t)
    (width . 0)
    (height . 0)
    (border-width . 0)
    (child-frame-border-width . 1)
    (left-fringe . 20)
    (right-fringe . 20)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (no-other-frame . t)
    (unsplittable . t)
    (undecorated . t)
    (cursor-type . t)
    (minibuffer . nil)
    (visibility . nil)
    (no-special-glyphs . t)
    (desktop-dont-save . t)))

(defvar mini-popup--frame nil)

(add-hook 'minibuffer-setup-hook
          (lambda ()
            ;;(mini-popup--make-frame 0 0 (frame-pixel-width) 300 "")
            (mini-popup--make-frame 200 200 (- (frame-pixel-width) 400) 300 "")))
            ;;(set-cursor-color "#feffff")


(add-hook 'minibuffer-exit-hook
          (lambda ()
            (delete-frame mini-popup--frame)))
            ;;(set-cursor-color "#010000")


(defface mini-popup-background
  '((((class color) (min-colors 88) (background dark))
     :background "#222")
    (((class color) (min-colors 88) (background light))
     :background "#ffe")
    (t :background "gray"))
  "Face used to for the popup background.")

(defface mini-popup-border
  '((((class color) (min-colors 88) (background dark)) :background "#444")
    (((class color) (min-colors 88) (background light)) :background "#bbb")
    (t :background "gray"))
  "The background color used for the thin border.")

(defvar-local vertico--cf-hide nil)
(defvar-local vertico--cf-cursor nil)

(defun mini-popup--pre-overlay ())
  ;; (when vertico--cf-hide
  ;;   (delete-overlay vertico--cf-hide))
  ;; (when vertico--cf-cursor
  ;;   (delete-overlay vertico--cf-cursor))


(defun mini-popup--move-overlay ()
  (when vertico--cf-hide
    (delete-overlay vertico--cf-hide))
  (when vertico--cf-cursor
    (delete-overlay vertico--cf-cursor))
  (dolist (ov (overlays-in (point-min) (point-max)))
    (unless (overlay-get ov 'vertico--cf-hide)
      (overlay-put ov 'window (frame-root-window mini-popup--frame))))
  (with-selected-window (frame-root-window mini-popup--frame)
    (goto-char (with-selected-window (active-minibuffer-window)
                 (point))))

  (setq vertico--cf-hide (make-overlay (point-min) (point-min) nil t t))
  (overlay-put vertico--cf-hide 'vertico--cf-hide t)
  (overlay-put vertico--cf-hide 'window (selected-window))
  (overlay-put vertico--cf-hide 'priority 99999)
  (overlay-put vertico--cf-hide 'before-string "\n\n\n\n\n")
  (window-resize (selected-window) (- (window-height))))

  ;;(setq vertico--cf-cursor (make-overlay (point) (1+ (point))))
  ;;(overlay-put vertico--cf-cursor 'window (frame-root-window mini-popup--frame))
  ;;(overlay-put vertico--cf-cursor 'face '(:background "black"))


;; Function adapted from posframe.el by tumashu
(defun mini-popup--make-frame (x y width height content)
  "Show child frame at X/Y with WIDTH/HEIGHT and CONTENT."
  (let* ((window-min-height 1)
         (window-min-width 1)
         (x-gtk-resize-child-frames
          (let ((case-fold-search t))
            (and
             ;; XXX Hack to fix resizing on gtk3/gnome taken from posframe.el
             ;; More information:
             ;; * https://github.com/minad/mini-popup/issues/17
             ;; * https://gitlab.gnome.org/GNOME/mutter/-/issues/840
             ;; * https://lists.gnu.org/archive/html/emacs-devel/2020-02/msg00001.html
             (string-match-p "gtk3" system-configuration-features)
             (string-match-p "gnome\\|cinnamon" (or (getenv "XDG_CURRENT_DESKTOP")
                                                    (getenv "DESKTOP_SESSION") ""))
             'resize-mode)))
         (after-make-frame-functions)
         (edge (window-inside-pixel-edges))
         (lh (default-line-height))
         ;; (x (max 0 (min (+ (car edge) x
         ;;                   (- (alist-get 'child-frame-border-width mini-popup--frame-parameters)))
         ;;                (- (frame-pixel-width) width))))
         (yb (+ (cadr edge) y lh))
  	 ;; (y (if (> (+ yb height lh lh) (frame-pixel-height))
  	 ;;        (- yb height lh 1)
         ;;      yb))
         (buffer (get-buffer-create " *Minibuf-1*")))
    (with-current-buffer buffer
      (setq-local mode-line-format nil
                  header-line-format nil
                  frame-title-format ""
                  truncate-lines t
                  ;;cursor-type nil
                  ;;cursor-in-non-selected-windows nil
                  cursor-in-non-selected-windows 'box
                  cursor-type '(bar . 0)
                  show-trailing-whitespace nil
                  display-line-numbers nil
                  left-fringe-width nil
                  right-fringe-width nil
                  left-margin-width 0
                  right-margin-width 0
                  fringes-outside-margins 0)
      (add-hook 'pre-command-hook #'mini-popup--pre-overlay nil 'local)
      (add-hook 'consult--completion-refresh-hook #'mini-popup--move-overlay 99 'local)
      (add-hook 'post-command-hook #'mini-popup--move-overlay 99 'local))
    ;;(set-window-buffer nil (get-buffer-create " *no-minibuf*"))
    ;; (with-current-buffer buffer
    ;;   (setq-local mode-line-format nil
    ;;               header-line-format nil
    ;;               frame-title-format ""
    ;;               truncate-lines t
    ;;               cursor-type nil
    ;;               cursor-in-non-selected-windows nil
    ;;               show-trailing-whitespace nil
    ;;               display-line-numbers nil
    ;;               left-fringe-width nil
    ;;               right-fringe-width nil
    ;;               left-margin-width nil
    ;;               right-margin-width nil
    ;;               fringes-outside-margins 0
    ;;               face-remapping-alist fr)
    ;;   (let ((inhibit-modification-hooks)
    ;;         (inhibit-read-only))
    ;;     (erase-buffer)
    ;;     (insert-buffer-substring-no-properties " *Minibuf-1*")
    ;;     (insert "\n")
    ;;     (insert content)
    ;;     (goto-char (point-min))))

    (unless (and (frame-live-p mini-popup--frame)
                 (eq (frame-parent mini-popup--frame) (window-frame)))
      (when mini-popup--frame (delete-frame mini-popup--frame))
      (setq mini-popup--frame (make-frame
                               `((parent-frame . ,(window-frame))
                                 (line-spacing . ,line-spacing)
                                 ;; Set `internal-border-width' for Emacs 27
                                 (internal-border-width
                                  . ,(alist-get 'child-frame-border-width mini-popup--frame-parameters))
                                 ,@mini-popup--frame-parameters))))
    (set-face-background
     (if (facep 'child-frame-border) 'child-frame-border 'internal-border)
     (face-attribute 'mini-popup-border :background) mini-popup--frame)
    (set-face-background
     'fringe
     (face-attribute 'mini-popup-background :background) mini-popup--frame)
    (set-frame-parameter
     mini-popup--frame 'background-color
     (face-attribute 'mini-popup-background :background))
    (set-window-buffer (frame-root-window mini-popup--frame) buffer)
    ;; XXX Make the frame invisible before moving the popup from above to below
    ;; the line in order to avoid flicker.
    (unless (eq (< (cdr (frame-position mini-popup--frame)) yb) (< y yb))
      (make-frame-invisible mini-popup--frame))
    (set-frame-size mini-popup--frame width height t)
    (set-frame-position mini-popup--frame x y)
    (make-frame-visible mini-popup--frame)))
