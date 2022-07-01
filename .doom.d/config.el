(setq c-default-style "linux"
      c-basic-offset 4)

;; Settings
(setq
 doom-font (font-spec :family "Iosevka Term SS04" :size 13 :weight 'regular)
 doom-big-font (font-spec :family "Iosevka Term SS04" :size 36)
 )
;;(centaur-tabs-mode t)
(custom-theme-set-faces! 'doom-tomorrow-night
  '(default :background "#121212"))


(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "brave-browser-nightly")

(org-babel-do-load-languages 'org-babel-load-languages '((csharp . t)))
;;(add-hook 'dired-mode-hook 'centaur-tabs-local-mode)
;;(add-hook 'dired-sidebar-mode 'centaur-tabs-local-mode)
;;(add-hook 'org-agenda-mode-hook 'centaur-tabs-local-mode)

(setq treemacs-position 'right)
(setq dired-sidebar-refresh-on-special-commands 't)
(setq user-full-name "ghd"
      user-mail-address "ghd@keemail.me")
(setq fill-column 92)


;; (setq org-directory "/home/ghd/leet/")
;; (setq org-agenda-files '("/home/ghd/leet/"))
;; (setq org-journal-dir "~/leet/journal/")





(setq display-line-numbers-type t)

(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))




(with-eval-after-load 'org
  ;; Allow multiple line Org emphasis markup.
  ;; http://emacs.stackexchange.com/a/13828/115
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
  ;; Below is needed to apply the modified `org-emphasis-regexp-components'
  ;; settings from above.
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
            (auto-save-mode)))
;; (add-hook 'pdf-tools-enabled-hook 'pdf-view-themed-minor-mode)













;; bi

(setq bidi-paragraph-direction 'left-to-right)
   (setq-default bidi-paragraph-direction 'left-to-right)
    (defun bidi-direction-toggle ()
      (interactive "")
      (setq bidi-display-reordering t)
      (if (equal bidi-paragraph-direction 'right-to-left)
          (setq bidi-paragraph-direction 'left-to-right)
        (setq bidi-paragraph-direction 'right-to-left)
        )
      (message "%s" bidi-paragraph-direction))












;; hooks

(add-hook 'dired-mode-hook 'org-download-enable)

;; C++




(defun my-cpp-mode-setup ()
  (lsp)
  (company-mode)
  (flycheck-mode)
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4))

(add-hook 'c++-mode-hook 'my-csharp-mode-setup t)
(add-hook 'c-mode-hook 'my-csharp-mode-setup t)



(defun compileandrun()
(interactive)
(save-buffer)
(compile (concat "g++ "  (file-name-nondirectory (buffer-file-name)) " -o "
(file-name-sans-extension   (file-name-nondirectory (buffer-file-name))) " && ./"
(file-name-sans-extension  (file-name-nondirectory (buffer-file-name))) " && rm "
(file-name-sans-extension  (file-name-nondirectory (buffer-file-name)))
) t  ) (other-window t)
(end-of-add-hook 'c++-mode))



(defun sharprun()
(interactive)
(save-buffer)
(compile (concat "dotnet run") t  ) (other-window t)
(end-of-add-hook 'csharp-mode))

(defun compiledep()
(interactive)
(save-buffer)
(compile (concat "g++ "  (file-name-nondirectory (buffer-file-name)) " -g -o "
(file-name-sans-extension  (file-name-nondirectory (buffer-file-name)))
) t  )
(other-window t)
;; (+debugger/start)
(end-of-add-hook 'c++-mode))


(add-hook 'c++-mode-hook
          (lambda () (local-set-key (kbd "<f2>") 'compileandrun)))

(add-hook 'csharp-mode-hook
          (lambda () (local-set-key (kbd "<f2>") 'sharprun)))


(add-hook 'c++-mode-hook
          (lambda () (local-set-key (kbd "M-<f5>") 'compiledep)))


(add-hook 'lsp-mode-hook
          (lambda ()
             (add-hook 'after-save-hook 'lsp-format-buffer nil 'make-it-local)))



(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)
(require 'real-auto-save)
(add-hook 'prog-mode-hook 'real-auto-save-mode)
(setq real-auto-save-interval 5) ;; in seconds


(yas-global-mode 1)
(add-hook 'yas-minor-mode(lambda()
                           (yas-activate-extra-mode 'fundamental-mode)))
(add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)












;; hugo
(setq easy-hugo-basedir "~/blog/")
(setq HUGO_BASE_DIR "~/blog/")
(setq org-hugo-base-dir "~/blog/")
(setq easy-hugo-url "/")
(setq easy-hugo-default-ext ".org")
(setq org-startup-folded t)












;; else

(setq-default fill-column 92)
(require 'dirtree)













;; modline


(setq doom-modeline-height 8)
(setq doom-modeline-bar-width 2)
(setq doom-modeline-icon (display-graphic-p))
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-buffer-state-icon t)
(setq doom-modeline-buffer-modification-icon t)
(setq doom-modeline-unicode-fallback nil)
(setq doom-modeline-minor-modes nil)
(setq doom-modeline-enable-word-count nil)














;; themes

(custom-theme-set-faces! 'doom-homage-black
  '(default :background "#121212"))
(custom-theme-set-faces! 'doom-one
  '(default :background "#121212"))
(custom-theme-set-faces! 'doom-one-light
  '(default :background "#ffffff"))
;; (custom-theme-set-faces! 'kaolin-aurora
;;   '(default :background "#121221"))
;;
;;
;;
;; (setq doom-theme 'kaolin-dark)
(setq doom-theme 'leuven)

;; (setq doom-theme 'doom-badger)














;; sql

(require 'ejc-sql)
(ejc-create-connection
 "q"
 :classpath "[/home/ghd/.m2/repository/postgresql/postgresql/9.3-1102.jdbc41/postgresql-9.3-1102.jdbc41.jar]"
 :password ""
 :user "postgres"
 :port "5432"
 :host "localhost"
 :dbname "postgres"
 :dbtype "postgresql")
(require 'ejc-autocomplete)
(add-hook 'ejc-sql-minor-mode-hook
          (lambda ()
            (auto-complete-mode t)
            (ejc-ac-setup)))
(require 'ejc-company)

(push 'ejc-company-backend company-backends)
(add-hook 'ejc-sql-minor-mode-hook
          (lambda ()
            (company-mode t)))
(setq ejc-complete-on-dot t)


(setq sql-connection-alist
      '((pool-a
         (sql-product 'postgresql)
         (sql-server "localhost")
         (sql-user "postgresql")
         (sql-password "")
         (sql-database "dvdrental ")
         (sql-port 5433))))











;; LaTeX

(defun my-tex ()
  (interactive)
  (save-buffer)
  (TeX-command "LaTeX" 'TeX-master-file -1))
(defun aftaa () (add-hook 'after-save-hook 'my-tex))
(add-hook 'LaTeX-mode-hook #'aftaa)
(defun JH/remove-electric-indent-mode ()
  (electric-indent-local-mode -1))
(setq LaTeX-indent-environment-list '())
(setq LaTeX-indent-level 0)
(setq LaTeX-item-indent 0)
(setq LaTeX-left-right-indent-level 0)
(setq TeX-brace-indent-level 0)
(add-hook 'LaTeX-mode-hook 'JH/remove-electric-indent-mode)
(add-hook 'tex-mode-hook 'JH/remove-electric-indent-mode)
(setq TeX-brace-indent-level 4)
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))
(setq shell-command-switch "-ic")








(use-package exec-path-from-shell
   :if (memq window-system '(mac ns))
   :ensure t
   :config
   (exec-path-from-shell-initialize))











;; Csharp

(defun my-csharp-mode-setup ()
  (lsp)
  (company-mode)
  (flycheck-mode)
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)



(setq org-tree-slide-skip-outline-level 4)
(setq term-default-fg-color "#FFFFFF") (setq term-default-bg-color "#FFFFFF")














;; LSP

(setq lsp-enable-symbol-highlighting t)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-ui-doc-show-with-mouse t)
(setq lsp-lens-enable t)
(setq lsp-headerline-breadcrumb-enable t)
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-sideline-enable t)
(setq lsp-modeline-code-actions-enable t)
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-eldoc-enable-hover t)
(setq lsp-modeline-diagnostics-enable t)
(setq lsp-signature-auto-activate t) ;; you could manually request them via `lsp-signature-activate`
(setq lsp-signature-render-documentation t)
(setq lsp-completion-show-detail t)
(setq lsp-completion-show-kind t)
(setq quickrun-focus-p nil)




















;; Indent
;;(require 'highlight-indent-guides)
;;(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;;
;;

(add-hook 'prog-mode-hook 'highlight-indentation-mode)

(require 'highlight-indentation)
(set-face-background 'highlight-indentation-face "#e3e3d3")

(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

;;(setq highlight-indent-guides-method 'character)
;; ;;(set-face-background 'highlight-indent-guides-odd-face "#3d3d3d")
;; ;;(set-face-background 'highlight-indent-guides-even-face "#3d3d3d")
;; ;;(set-face-foreground 'highlight-indent-guides-character-face "#3d3d3d")

;; (set-face-background 'highlight-indent-guides-odd-face "darkgray")
;; (set-face-background 'highlight-indent-guides-even-face "dimgray")
;; (set-face-foreground 'highlight-indent-guides-character-face "dimgray")










;; IVY

(require 'ivy-posframe)
(setq ivy-posframe-height-alist '((swiper . 20)
                                  (t      . 40)))
(setq ivy-posframe-display-functions-alist
      '((swiper          . ivy-display-function-fallback)
        (complete-symbol . ivy-posframe-display-at-point)
        (counsel-M-x     . ivy-posframe-display-at-frame-top-center)
        (t               . ivy-posframe-display)))
(ivy-posframe-mode 1)
(setq ivy-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))



(setq display-buffer-alist
      `(("*compilation*" . ((name . "*compilation*")
                            ,@default-frame-alist
                            (left . (- 1))
                            (top . 0)))))

(defun open-popup-on-side-or-below (buffer &optional alist)
  (+popup-display-buffer-stacked-side-window-fn
   buffer (append `((side . ,(if (one-window-p)
                                 'right
                               'bottom)))
                  alist)))

(set-popup-rule! "\\*compilation\\*" :side 'bottom :width 2.5)













;; keys

(global-set-key [f3] 'toggle-maximize-buffer)
(global-set-key (kbd "C-<=>") 'text-scale-increase)
(global-set-key (kbd "C-<->") 'text-scale-decrease)
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<up>") 'enlarge-windor)
(global-set-key (kbd "M-n") 'make-frame)
(global-set-key (kbd "M-z") 'flycheck-list-errors) (global-set-key (kbd "M-w") 'eww-search-words)
(global-set-key (kbd "M-f") 'org-footnote-action)


(map! :leader
      :desc "move to jorunal"
      "j" #'org-journal-new-entry)
(map! :leader
      :desc "run vterm"
      "t t" 'vterm)
(map! :leader
      :desc "run mail"
      "m m" 'mu4e)
(map! :leader
      :desc "watch var"
      "o w" 'dap-ui-expressions-add)
(map! :leader
      :desc "open-ajenda"
      "a" #'org-agenda)

(map! :leader
      :desc "insert date"
      "d" #'org-schedule)


(map! :leader
      :desc "show errors"
      "e" #'flycheck-list-errors)

(map! :leader
      :desc "insert date"
      "l" #'TeX-command-master "Latex")



(global-set-key (kbd "<f8>") 'org-tree-slide-mode)
(global-set-key [f6] (lambda () (interactive) (dired-sidebar-toggle-sidebar) (lsp-treemacs-symbols) (evil-window-next) ))
(global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)
(global-set-key (kbd "<f11>") 'org-tree-slide-move-next-tree)
(global-set-key (kbd "<f12>") 'org-tree-slide-move-previous-tree)
(global-set-key (kbd "M-RET") 'lsp-execute-code-action)


;; debug
;;
(global-set-key [f5] '+debugger/start)
(global-set-key (kbd "C-<f5>")'+debugger/quit)
(global-set-key [f11] 'dap-step-in)
(global-set-key [f12] 'lsp-goto-implementation)
(global-set-key [f9] 'dap-breakpoint-toggle)










;; language tool

(setq languagetool-language-tool-jar
      "~/.languagetool/languagetool-commandline.jar")
(setq languagetool-default-language "en-GB")
(global-set-key (kbd "C-c l c") 'languagetool-check)
(global-set-key (kbd "C-q") 'org-agenda-open-link)
(global-set-key (kbd "C-;") 'iedit-mode)
(global-set-key (kbd "C-c l d") 'langrrgetool-clear-buffer)
(global-set-key (kbd "C-c l p") 'languagetool-correct-at-point)
(global-set-key (kbd "C-c l b") 'languagetool-correct-buffer)
(global-set-key (kbd "C-c l l") 'languagetool-set-language)
(global-set-key (kbd "C-c l l") 'languagetool-set-language)
(global-set-key (kbd "C-c x") 'org-latex-preview)
(global-set-key (kbd "M-[")  'centaur-tabs-backward)
(global-set-key (kbd "M-]") 'centaur-tabs-forward)
(global-set-key (kbd "M-t") 'centaur-tabs--create-new-tab)





;;(setq display-line-numbers-type 'relative)
;;(menu-bar--display-line-numbers-mode-relative)




;; Trigger completion immediately.
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
;; (setq company-show-numbers t)


;; (menu-bar-mode)

(dap-mode 1)
(dap-tooltip-mode 1)
(tooltip-mode 1)
(dap-ui-controls-mode 1)
(require 'dap-netcore)
(require 'dap-cpptools)
(require 'dap-lldb)

(setq dap-netcore-download-url "https://github.com/Samsung/netcoredbg/releases/download/2.0.0-895/netcoredbg-linux-amd64.tar.gz")




 (defun the-cpp-mode-setup ()
 (setq dap-lldb-debug-program '("/usr/bin/lldb-vscode"))

 (dap-register-debug-template
    "C++ LLDB dap"
    (list :type "lldb-vscode"
          ;; :cwd nil
          ;; :args nil
          :request "launch"
          :cwd "${workspaceFolder}"
          :program "${fileBasenameNoExtension}"))


(dap-register-debug-template
  "NetCoreDbg::Launch"
  (list :type "coreclr"
        :request "launch"
        :cwd "${workspaceFolder}"
        :mode "launch"
        :program "dlol/Debug/net6.0/${fileBasenameNoExtension}.dll"
        :name "NetCoreDbg::Shibi"))

(dap-register-debug-template
  "cpptools::Run Configuration"
  (list :type "cppdbg"
        :request "launch"
        :name "cpptools::Run Configuration"
        :MIMode "gdb"
        :program "${fileBasenameNoExtension}"
        :cwd "${workspaceFolder}"))


 (defun dap-debug-create-or-edit-json-template ()
     "Edit the C++ debugging configuration or create + edit if none exists yet."
     (interactive)
     (let ((filename (concat (lsp-workspace-root) "/launch.json"))
 	  (default "~/.doom/default-launch.json"))
       (unless (file-exists-p filename)
 	(copy-file default filename))
       (find-file-existing filename)))
   )

(add-hook 'c++-mode-hook 'the-cpp-mode-setup t)
(add-hook 'c-mode-hook 'the-cpp-mode-setup t)
;; (setq dap-auto-configure-features '(sessions locals controls tooltip)) ;


(add-hook 'pdf-view-mode-hook
          (lambda () (local-set-key (kbd "<f2>") #'pdf-annot-add-text-annotation)))
(add-hook 'pdf-view-mode-hook
          (lambda () (local-set-key (kbd "<f3>") #'pdf-annot-add-underline-markup-annotation)))



(setq org-journal-date-format "%A, %d %B %Y")
(setq org-journal-file-format "%Y%m%d.org")
(setq org-journal-enable-agenda-integration t)

(setq org-directory "/mnt/disk/leet")
(setq org-agenda-files '("/mnt/disk/leet"))
(setq org-journal-dir "/mnt/disk/leet")
(add-to-list 'org-agenda-files org-journal-dir)


(use-package! awqat
  :commands (awqat-display-prayer-time-mode
             awqat-times-for-day)
  :config
  (setq calendar-latitude 30.0
        calendar-longitude 31.2
        awqat-mode-line-format " 🕌 ${prayer} (${hours}h${minutes}m) "))
(awqat-display-prayer-time-mode)
(global-wakatime-mode)
