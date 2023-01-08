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





(defun narrow-dwim ()
  "Toggle narrowing."
  (interactive)
  (cond ((region-active-p)
         ;; If region is highlighted, narrow to that
         (call-interactively #'narrow-to-region)
         (deactivate-mark t))
        ((buffer-narrowed-p)
         ;; Otherwise widen if narrowed
         (widen))
        ((derived-mode-p 'org-mode)
         (call-interactively #'org-narrow-to-subtree))
        (t
         (message "Do not know what to narrow to.")
         (call-interactively #'narrow-to-defun))))

;; (setq bibtex-file-path '("/home/ghd/source"))


(setq bibtex-completion-bibliography "~/configs/ref.bib")
(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "C-j") 'bibtex-next-field)
  (define-key evil-insert-state-map (kbd "C-k") 'bibtex-previous-entry))

(with-eval-after-load 'vertigo
  (vertigo-mode 1)
  (setq vertigo-completing-read-function 'ivy-completing-read))




(setq bibtex-completion-bibliography '("~/configs/ref.bib" )
	bibtex-completion-library-path '("~/source/")
	bibtex-completion-notes-path "~/roam/reference/"
	bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
	bibtex-completion-additional-search-fields '(keywords)
	bibtex-completion-display-formats
	'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}")))


(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

(define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)


(use-package citar
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  (citar-bibliography '("~/configs/ref.bib")))

(setq citar-templates
      '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
        (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
        (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
        (note . "Notes on ${author editor}, ${title}")))


(setq citar-symbols
      `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
        (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
        (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
(setq citar-symbol-separator "  ")



(use-package citar-org-roam
  :after citar org-roam
  :no-require
  :config (citar-org-roam-mode))
(setq citar-org-roam-note-title-template "${author} - ${title}\n- tags :: [[roam:Book]]\n")


(add-hook 'helm-minibuffer-set-up-hook
          (lambda ()
            (advice-add #'doom-modeline--active :override (lambda () t))))
(add-hook 'helm-cleanup-hook
          (lambda ()
            (advice-remove #'doom-modeline--active (lambda () t))))


(require 'citar-org-roam)






(use-package! org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref))




(setq orb-preformat-keywords
      '("citekey" "title" "url" "author-or-editor" "keywords" "file")
      orb-process-file-keyword t
      orb-attached-file-extensions '("pdf"))

;; (setq org-roam-capture-templates
;;       '())
(define-key org-roam-bibtex-mode-map (kbd "C-c n a") #'orb-note-actions)



(defun my/org-insert-chess-diagram (notation file)
  "Generate a chess diagram from NOTATION and insert it into the current Org-mode buffer.
Save the diagram to FILE."
  (let ((cmd (format "scidb -n %S -o %S" notation file)))
    (call-process-shell-command cmd nil nil)
    (insert (format "[[file:%s]]" file))))

;; (defun my/org-eval-chess-block ()
;;   "Evaluate the chess block at point and insert the resulting diagram into the current Org-mode buffer."
;;   (interactive)
;;   (let* ((element (org-element-at-point))
;;          (type (org-element-type element))
;;          (value (org-element-property :value element))
;;          (file (make-temp-file "chess-diagram" nil ".png")))
;;     (when (equal type 'src-block)
;;       (let ((language (org-element-property :language element))
;;             (parameters (org-element-property :parameters element)))
;;         (when (equal language "chess")
;;           (my/org-insert-chess-diagram value file)
;;           (delete-file file))))))

;; (defun my/org-confirm-babel-evaluate (lang body)
;;   "Confirm before evaluating a code block."
;;   (if (string= lang "chess")
;;       (if (y-or-n-p (format "Evaluate chess block?\n%s" body))
;;           'yes)
;;     t))


;; (add-to-list 'org-confirm-babel-evaluate 'my/org-confirm-babel-evaluate)




;; (defun org-babel-execute:chess (body params)
;;   "Execute a block of Chess code with org-babel.
;; This function is called by `org-babel-execute-src-block'."
;;   (let* ((output-file (cdr (assq :file params)))
;;          (pgn-file (make-temp-file "chess-notation" nil ".pgn"))
;;          (cmd (format "python ~/configs/elchess.py %s %s" pgn-file output-file)))
;;     (with-temp-buffer
;;       (insert body)
;;       (write-file pgn-file))
;;     (shell-command cmd)
;;     (org-babel-result-to-file output-file)))




(defun org-babel-execute:chess (body params)
  "Execute a block of Chess code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((output-file (cdr (assq :file params)))
         (notation (cdr (assq :notation params)))
         (extension (if (equal notation "fen") ".fen" ".pgn"))
         (notation-file (make-temp-file "chess-notation" nil extension))
         (cmd (format "python ~/configs/elchess.py %s %s %s" notation-file output-file notation)))
    (with-temp-buffer
      (insert body)
      (write-file notation-file))
    (shell-command cmd)
    (org-babel-result-to-file output-file)))

(setq org-babel-default-header-args:chess
      '((:results . "raw")))


(org-babel-do-load-languages
 'org-babel-load-languages
 '((chess . t)))
(add-hook 'csv-mode-hook 'csv-align-mode)



(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))
