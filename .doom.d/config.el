(add-to-list 'load-path "~/.doom.d/")
(require '+roam)
(require 'leader)
(require 'keys)
(require '+handy)
(add-to-list 'org-agenda-files "~/roam/journal/agenda/todo.org")
(add-to-list 'org-agenda-files "~/roam/journal/agenda/birthday.org")
(setq +org-capture-journal-file "~/blog/content/stack.org")
(setq +org-capture-changelog-file "~/blog/content/nice.org")
(setq +org-capture-todo-file "~/roam/main/life.org")
(setq neo-autorefresh 't)
(setq neo-mode-line-type 'default)
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

(define-key evil-normal-state-map (kbd "C-g") 'evil-escape)
(define-key evil-visual-state-map (kbd "C-g") 'evil-escape)
(define-key evil-insert-state-map (kbd "C-g") 'evil-escape)
(define-key evil-replace-state-map (kbd "C-g") 'evil-escape)
(define-key evil-operator-state-map (kbd "C-g") 'evil-escape)

(defun my/evil-escape-and-abort-company ()
  (interactive)
  (company-abort)
  (evil-escape))
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-g") 'my/evil-escape-and-abort-company)
  (define-key company-search-map (kbd "C-g") 'my/evil-escape-and-abort-company))




(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "C-j") 'bibtex-next-field)
  (define-key evil-insert-state-map (kbd "C-k") 'bibtex-previous-entry))

(with-eval-after-load 'vertigo
  (vertigo-mode 1)
  (setq vertigo-completing-read-function 'ivy-completing-read))

