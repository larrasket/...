;;; mine/gen/config.el -*- lexical-binding: t; -*-

(setq completion-ignore-case t)
(setq-default frame-title-format '("%b"))


;; disable emojis
(add-to-list 'doom-emoji-fallback-font-families "Symbola")
(setq fancy-splash-image "~/.doom.d/pan.png")
(setq c-default-style "linux"
      c-basic-offset 4)
(setq org-roam-directory "~/roam")
;; (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 12))
;; (setq doom-font (font-spec :family "PragmataPro Mono" :size 12 :weight 'normal :width 'normal))
(setq doom-font (font-spec :family "PragmataPro" :size 12))
(setq browse-url-generic-program "chromium")


(setq treemacs-position 'right)
(setq dired-sidebar-refresh-on-special-commands 't)


(setq user-full-name "Salih Muhammed"
      user-mail-address "salhghd7@gmail.com")

(yas-global-mode 1)
(add-hook 'yas-minor-mode(lambda() (yas-activate-extra-mode 'fundamental-mode)))




;; I wrote this code many years ago when I was learning elisp for the first time
;; It's very shitty and I don't remember what it was about, but it made me feel
;; cozy <3

(setq display-buffer-alist
      `(("*compilation*" . ((name . "*compilation*")
                            ,@default-frame-alist))))

(set-popup-rule! "\\*compilation\\*" :side 'bottom :width 2.5)




(setq display-line-numbers-type 'relative)
(setq all-the-icons-color-icons nil)



;; TODO Fix neotree modeline
;;
(setq neo-theme 'icons
      neo-window-width 35)
(add-hook 'neotree-mode-hook(lambda () (solaire-mode -1)))


;; I wrote this when I was using sage <3
;; (add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)


(add-hook 'dired-mode-hook 'org-download-enable)
(add-hook 'dired-mode-hook(lambda () (solaire-mode -1)))














(setq doom-modeline-height 20
      doom-modeline-buffer-state-icon nil)














(with-eval-after-load 'vertigo
  (vertigo-mode 1)
  (setq vertigo-completing-read-function 'ivy-completing-read))
