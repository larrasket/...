(add-to-list 'load-path "~/.doom.d/")
(require '+roam)
(require 'leader)
(require 'keys)
(require '+handy)
;; (setq org-agenda-files (directory-files-recursively "~/roam/journal/agenda" "\\.org$"))

(add-to-list 'org-agenda-files "~/roam/journal/agenda/todo.org")
(add-to-list 'org-agenda-files "~/roam/journal/agenda/birthday.org")
(setq +org-capture-journal-file "~/blog/content/stack.org")
(setq +org-capture-changelog-file "~/blog/content/nice.org")
(setq +org-capture-todo-file "~/roam/main/life.org")
(setq neo-autorefresh 't)
(setq neo-mode-line-type 'default)
;; (after! highlight-indent-guides
;;   (highlight-indent-guides-auto-set-faces))
;;
;; Die, Doc-View-mode! die!
;; (defalias 'doc-view-mode #'doc-view-fallback-mode) ;Or fundamental-mode, ...
(setq load-prefer-newer t)

(custom-set-variables
 '(highlight-indent-guides-method 'bitmap))


(defun highltier ()
  (require 'highlight-indent-guides)
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (highlight-indent-guides-mode))


(add-hook 'org-mode-hook 'highltier)
(add-hook 'prog-mode-hook 'highltier)










(require 'elgantt)
(setq org-visual-indent-color-indent
      (cl-loop for x from 1 to 5
               for color in (elgantt--create-gradient "gray" "black" 5)
               collect `(,x  ,(list :background (elgantt--color-rgb-to-hex color))
                          :foreground (elgantt--color-rgb-to-hex color)
                          :height .1)))
(add-hook 'org-mode-hook 'org-visual-indent-mode)
