;;; mine/gen/config.el -*- lexical-binding: t; -*-




(setq c-default-style "linux"
      c-basic-offset 4)


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
