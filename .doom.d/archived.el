
;;; other.el -*- lexical-binding: t; -*-
(defun title-capitalization-string (s)
  (with-temp-buffer
    (erase-buffer)
    (insert s)
    (title-capitalization (point-min)
                          (point-max))
    (buffer-substring-no-properties (point-min)
                                    (point-max))))

(defun title-capitalization-dwim (&optional arg)
  (interactive)
  (cond
   (arg
    (title-capitalization-string arg))
   ((use-region-p)
    (title-capitalization-string
     (buffer-substring-no-properties (region-beginning)
                                     (region-end))))
   (t
    (title-capitalization-string
     (buffer-substring-no-properties (point-a))))))ated: %<%Y-%m-%dT%H%M%S>"
;;                :unnarrowed t))
;; (setq org-roam-dailies-capture-templates
;;       '(("d" "default" entry "%A, %d %B %Y""
;;          :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
;; (setq org-journal-enable-encryption 't)
;; (setq org-journal-encrypt-journal 't)

;; (org-alert-enable)
;; (setq alert-default-style 'notification)
;; (setq org-alert-interval 200
;;         org-alert-notify-cutoff 10
;;         org-alert-notify-after-event-cutoff 10)
;; (setq alert-default-style 'libnotify)
;; (setq mail-user-agent 'mu4e-user-agent)
;; (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
;; 	org-msg-startup "hidestars indent inlineimages"
;; 	org-msg-greeting-fmt "\nHi%s,\n\n"
;; 	;; org-msg-recipient-names '(("jeremy.compostella@gmail.com" . "Jérémy"))
;; 	org-msg-greeting-name-limit 3
;; 	org-msg-default-alternatives '((new		. (text html))
;; 				       (reply-to-html	. (text html))
;; 				       (reply-to-text	. (text)))
;; 	org-msg-convert-citation t
;; 	org-msg-signature "

;;  Regards,
;;  /Salh Jabr/,
;; ")
;; ;; The command used to get your emails (adapt this line, see section 2.3):
;; (setq mu4e-get-mail-command "mbsync --config ~/.emacs.d/.mbsyncrc nameaccount")
;; ;; Further customization:
;; ;; SMTP settings:
;; (setq send-mail-function 'smtpmail-send-it)    ; should not be modified
;; (setq smtpmail-smtp-server "mail.mailo.com") ; host running SMTP server
;; (setq smtpmail-smtp-service 465)               ; SMTP service port number
;; (setq smtpmail-stream-type 'ssl)          ; type of SMTP connections to use


;; (setq mu4e-drafts-folder "/Drafts")
;; (setq mu4e-sent-folder   "/Sent")
;; (setq mu4e-trash-folder  "/Trash")

;;; org-eww.el ends here

;; (global-set-key [M-q] 'org-agenda-open-link)
;; (add-hook 'c++-mode-hook
;;           (lambda () (local-set-key (kbd "<f2>") #'quickrun-shell)))
;; ;; (add-hook 'org-mode-hook
;;           ;; (lambda () (local-set-key (kbd "<c-q>") #'org-agenda-open-link)))
;; (add-hook 'c-mode-hook
;;           (lambda () (local-set-key (kbd "<f2>") #'quickrun-shell)))

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
;; (require 'eaf)
;; (require 'eaf-browser)
;; (require 'eaf-pdf-viewer)
;; (add-hook 'org-journal-mode-hook
;;           (lambda ()
;;             (add-hook 'auto-save-hook 'org-save-all-org-buffers nil nil)
;;             (real-auto-save-mode nil)
;;             (auto-save-mode nil)))

;(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
;(require 'eaf)
;; (add-hook 'org-agenda-mode-hook
;;           (lambda ()
;;             (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
;;             (auto-save-mode)))


;(setq
;send-mail-function 'smtpmail-send-it
;message-send-mail-function 'smtpmail-send-it
;user-mail-address "salehmu@mailru"
;smtpmail-starttls-credentials '(("smtp.mail.ru" "465" nil nil))
;smtpmail-auth-credentials (expand-file-name "~/.authinfo")
;smtpmail-default-smtp-server "smtp.mail.ru"
;smtpmail-smtp-server "smtp.mail.ru"
;smtpmail-smtp-service 465
;smtpmail-debug-info t
;starttls-extra-arguments nil
;starttls-gnutls-program "/usr/bin/gnutls-cli"
;starttls-extra-arguments nil
;starttls-use-gnutls t
;)



;(with-eval-after-load 'ox
 ;;(require 'ox-hugo))
 ;;
 ;;
 ;;
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)

;; (use-package delve
;;   :after (org-roam)
;;   ;; this is necessary if use-package-always-defer is true
;;   :demand t
;;   :bind
;;   ;; the main entry point, offering a list of all stored collections
;;   ;; and of all open Delve buffers:
;;   (("<f12>" . delve))
;;   :config
;;   ;; set meaningful tag names for the dashboard query
;;   (setq delve-dashboard-tags '("Tag1" "Tag2"))
;;   ;; optionally turn on compact view as default
;;   (add-hook #'delve-mode-hook #'delve-compact-view-mode)
;;  ;; turn on delve-minor-mode when Org Roam file is opened:
;;   (delve-global-minor-mode))
;; (use-package! vulpea
;;   :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))



;;;; org



;; (setq org-hide-emphasis-markers t)
;;   (font-lock-add-keywords 'org-mode
;;                           '(("^ *\\([-]\\) "
;;                              (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;;   (use-package org-bullets
;;     :config
;;     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;   (let* ((variable-tuple
;;           (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
;;                 ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;                 ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;                 ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;                 ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;                 (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;          (base-font-color     (face-foreground 'default nil 'default))
;;          (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

;;     (custom-theme-set-faces
;;      'user
;;      `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;      `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;      `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;      `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;      `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))


;;   (custom-theme-set-faces
;;    'user
;;    '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin))))
;;    '(fixed-pitch ((t ( :family "Fira Code Retina" :height 160)))))

;;   (add-hook 'org-mode-hook 'variable-pitch-mode)

;;   (custom-theme-set-faces
;;    'user
;;    '(org-block ((t (:inherit fixed-pitch))))
;;    '(org-code ((t (:inherit (shadow fixed-pitch)))))
;;    '(org-document-info ((t (:foreground "dark orange"))))
;;    '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;;    '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
;;    '(org-link ((t (:foreground "royal blue" :underline t))))
;;    '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;    '(org-property-value ((t (:inherit fixed-pitch))) t)
;;    '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;    '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
;;    '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;;    '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
;;



;; (dap-mode 1)
;; (dap-tooltip-mode 1)
;; (tooltip-mode 1)
;; (dap-ui-controls-mode 1)
;; (require 'dap-netcore)
;; (require 'dap-cpptools)
;; (require 'dap-lldb)

;; (setq dap-netcore-download-url "https://github.com/Samsung/netcoredbg/releases/download/2.0.0-895/netcoredbg-linux-amd64.tar.gz")




;;  (defun the-cpp-mode-setup ()
;;  (setq dap-lldb-debug-program '("/usr/bin/lldb-vscode"))

;;  (dap-register-debug-template
;;     "C++ LLDB dap"
;;     (list :type "lldb-vscode"
;;           ;; :cwd nil
;;           ;; :args nil
;;           :request "launch"
;;           :cwd "${workspaceFolder}"
;;           :program "${fileBasenameNoExtension}"))


;; (dap-register-debug-template
;;   "NetCoreDbg::Launch"
;;   (list :type "coreclr"
;;         :request "launch"
;;         :cwd "${workspaceFolder}"
;;         :mode "launch"
;;         :program "dlol/Debug/net6.0/${fileBasenameNoExtension}.dll"
;;         :name "NetCoreDbg::Shibi"))

;; (dap-register-debug-template
;;   "cpptools::Run Configuration"
;;   (list :type "cppdbg"
;;         :request "launch"
;;         :name "cpptools::Run Configuration"
;;         :MIMode "gdb"
;;         :program "${fileBasenameNoExtension}"
;;         :cwd "${workspaceFolder}"))


;;  (defun dap-debug-create-or-edit-json-template ()
;;      "Edit the C++ debugging configuration or create + edit if none exists yet."
;;      (interactive)
;;      (let ((filename (concat (lsp-workspace-root) "/launch.json"))
;;  	  (default "~/.doom/default-launch.json"))
;;        (unless (file-exists-p filename)
;;  	(copy-file default filename))
;;        (find-file-existing filename)))
;;    )

;; (add-hook 'c++-mode-hook 'the-cpp-mode-setup t)
;; (add-hook 'c-mode-hook 'the-cpp-mode-setup t)
;; (setq dap-auto-configure-features '(sessions locals controls tooltip)) ;


;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             (org-roam-mode 0)
;;             ))

;; (use-package deft
;; :after org
;; ;; :config
;; ;;   (setq deft-extensions '("org")
;; ;;         deft-directory org-roam-directory
;; ;;         deft-recursive t
;; ;;         deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
;; ;;         deft-use-filename-as-title t)
;; :custom
;; (deft-recursive t)
;; (deft-use-filter-string-for-filename t)
;; (deft-default-extension "org")
;; (deft-directory "~/roam/"))



;; Indent
;;
;;


;; (require 'highlight-indentation)
;; (set-face-background 'highlight-indentation-face "#e3e3d3")

;; (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

;; (add-hook 'org-agenda-mode-hook
;;           (lambda ()
;;             (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
;;             (auto-save-mode)))
;; (add-hook 'pdf-tools-enabled-hook 'pdf-view-themed-minor-mode)

;; (defun my-tex ()
;;   (interactive)
;;   (save-buffer)
;;   (TeX-command "LaTeX" 'TeX-master-file -1))
;; (defun aftaa () (add-hook 'after-save-hook 'my-tex))
;; (add-hook 'LaTeX-mode-hook #'aftaa)
;; (defun JH/remove-electric-indent-mode ()
;;   (electric-indent-local-mode -1))
;; (setq LaTeX-indent-environment-list '())
;; (setq LaTeX-indent-level 0)
;; (setq LaTeX-item-indent 0)
;; (setq LaTeX-left-right-indent-level 0)
;; (setq TeX-brace-indent-level 0)
;; (add-hook 'LaTeX-mode-hook 'JH/remove-electric-indent-mode)
;; (add-hook 'tex-mode-hook 'JH/remove-electric-indent-mode)
;; (setq TeX-brace-indent-level 4)
;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell (replace-regexp-in-string
;;                           "[ \t\n]*$"
;;                           ""
;;                           (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq eshell-path-env path-from-shell) ; for eshell users
;;     (setq exec-path (split-string path-from-shell path-separator))))
