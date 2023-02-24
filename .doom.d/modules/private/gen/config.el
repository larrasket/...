;;; mine/gen/config.el -*- lexical-binding: t; -*-

(setq completion-ignore-case t)
(setq-default frame-title-format '("%b")
              bidi-paragraph-direction 'left-to-right)


;; disable emojis
(add-to-list 'doom-emoji-fallback-font-families "Symbola")

(setq c-default-style "linux"
      c-basic-offset 4)
(setq doom-font (font-spec :family "PragmataPro" :size 12)
      treemacs-position 'right
      dired-sidebar-refresh-on-special-commands 't
      display-line-numbers-type 'relative
      all-the-icons-color-icons nil
      neo-theme 'icons
      neo-window-width 35
      doom-modeline-height 20
      doom-modeline-buffer-state-icon nil
      bidi-paragraph-direction 'left-to-right)



(yas-global-mode 1)
(add-hook 'yas-minor-mode(lambda() (yas-activate-extra-mode 'fundamental-mode)))
(add-hook 'neotree-mode-hook(lambda () (solaire-mode -1)))
(add-hook 'dired-mode-hook 'org-download-enable)
(add-hook 'dired-mode-hook(lambda () (solaire-mode -1)))
;; I wrote this code many years ago when I was learning elisp for the first time
;; It's very shitty and I don't remember what it was about, but it made me feel
;; cozy <3

(setq display-buffer-alist
      `(("*compilation*" . ((name . "*compilation*")
                            ,@default-frame-alist))))

(set-popup-rule! "\\*compilation\\*" :side 'bottom :width 2.5)
;; ----------------------------------



;; I wrote this when I was using sage <3
;; (add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)



(with-eval-after-load 'vertigo
  (vertigo-mode 1)
  (setq vertigo-completing-read-function 'ivy-completing-read))

(after! eww
  (push '("\\*eww\\*" . (display-buffer-same-window)) display-buffer-alist))


;; Disable images in eww buffer
(setq-default shr-inhibit-images t
              org-download-image-dir "./org-media")


(after! eshell (remove-hook 'eshell-mode-hook 'hide-mode-line-mode))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side            . bottom)
               (reusable-frames . visible)
               (window-height   . 0.18)))
