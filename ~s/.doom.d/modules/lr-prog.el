;;; lr-prog.el --- LSP, treesitter, formatters, compilation -*- lexical-binding: t; -*-

;;; HACK --- go-mod-ts-mode-maybe shim ---
;; Emacs 30's built-in go-ts-mode lacks this function that Doom registers as an autoload.
;; Define it here early so the autoload is never triggered.
(defun go-mod-ts-mode-maybe ()
  "Use go-mod-ts-mode if tree-sitter gomod grammar is available."
  (if (treesit-language-available-p 'gomod)
      (go-mod-ts-mode)
    (when (fboundp 'go-mod-mode) (go-mod-mode))))

;;; --- LSP ---
(after! lsp-mode
  (setq lsp-enable-symbol-highlighting nil
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-code-actions t
        lsp-modeline-code-actions-enable t
        lsp-eldoc-enable-hover t
        lsp-signature-auto-activate t
        lsp-signature-render-documentation nil
        lsp-headerline-breadcrumb-enable t
        lsp-idle-delay 0.5
        lsp-use-plists t))

;;; --- LSP booster ---
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or (when (equal (following-char) ?#)
        (let ((bytecode (read (current-buffer))))
          (when (byte-code-function-p bytecode)
            (funcall bytecode))))
      (apply old-fn args)))

(advice-add (if (progn (require 'json) (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)
             (not (file-remote-p default-directory))
             lsp-use-plists
             (not (functionp 'json-rpc-connection))
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((from-exec (executable-find (car orig-result))))
            (setcar orig-result from-exec))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

;;; --- Rename/iedit ---
(defun salih/rename-or-iedit ()
  "LSP rename if available, otherwise iedit."
  (interactive)
  (if (and (featurep 'lsp) (bound-and-true-p lsp-mode))
      (call-interactively #'lsp-rename)
    (call-interactively #'iedit-mode)))

(defun salih/list-errors ()
  "LSP diagnostics if available, otherwise flycheck."
  (interactive)
  (if (and (featurep 'lsp) (bound-and-true-p lsp-mode))
      (call-interactively #'consult-lsp-diagnostics)
    (call-interactively #'flycheck-projectile-list-errors)))

;;; --- Flycheck-golangci-lint patch ---
(after! flycheck-golangci-lint
  (defun flycheck-golangci-lint--executable ()
    (or flycheck-golangci-lint-executable "golangci-lint"))

  (defun flycheck-golangci-lint--parse-version ()
    (unless flycheck-golangci-lint--version
      (let* ((output (ignore-errors
                       (with-temp-buffer
                         (call-process (flycheck-golangci-lint--executable) nil t nil "--version")
                         (buffer-string))))
             (rx "version \\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)"))
        (when (and output (string-match rx output))
          (setq flycheck-golangci-lint--version
                (list (string-to-number (match-string 1 output))
                      (string-to-number (match-string 2 output))
                      (string-to-number (match-string 3 output)))))))
    flycheck-golangci-lint--version)

  (defun flycheck-golangci-lint--output-format-flags ()
    (let ((version (flycheck-golangci-lint--parse-version)))
      (if (and version (>= (car version) 2))
          '("--output.checkstyle.path=stdout")
        '("--out-format=checkstyle")))))

;;; --- Scalafmt ---
(defconst scalafmt-default-config
  "version = \"3.2.1\"
style = default
runner.dialect = scala213
docstrings.wrap = \"no\"
maxColumn = 140
rewrite.rules = [AvoidInfix RedundantBraces RedundantParens AsciiSortImports PreferCurlyFors]
indent.main = 2
indent.defnSite = 2
indent.callSite = 2
indent.extendSite = 2
align.preset = more
rewrite.trailingCommas.style = keep
newlines.source = keep
")

(defun scalafmt-find-project-root ()
  (or (locate-dominating-file default-directory ".scalafmt.conf")
      (locate-dominating-file default-directory "build.sbt")
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun scalafmt-ensure-config ()
  (let* ((root (scalafmt-find-project-root))
         (config (expand-file-name ".scalafmt.conf" root)))
    (unless (file-exists-p config)
      (with-temp-file config (insert scalafmt-default-config))
      (message "Created .scalafmt.conf in %s" root))
    config))

(after! apheleia
  (setf (alist-get 'scalafmt apheleia-formatters)
        '("scalafmt" "--stdin" "--config" (eval (scalafmt-ensure-config))))
  (setf (alist-get 'scala-mode apheleia-mode-alist) 'scalafmt)
  (setf (alist-get 'scala-ts-mode apheleia-mode-alist) 'scalafmt))

(add-hook 'scala-mode-hook #'apheleia-mode)
(add-hook 'scala-ts-mode-hook #'apheleia-mode)

;;; --- Metals (Scala LSP) ---
(defun metals-import-build ()
  "Run sbt bloopInstall for Scala projects."
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (async-shell-command "sbt bloopInstall")
    (message "Running sbt bloopInstall...")))

;;; --- Prog mode hooks ---
(add-hook! 'prog-mode-hook
  (setq prettify-symbols-alist '(("lambda" . 923)))
  (jinx-mode -1))

;;; --- Compilation ---
(defun salih/compile-and-run-cpp ()
  "Compile and run C++ file."
  (interactive)
  (save-buffer)
  (let ((base (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    (compile (format "g++ %s -o %s && ./%s && rm %s"
                     (file-name-nondirectory (buffer-file-name))
                     base base base)
             t)))

(defun salih/compile-and-run-c ()
  "Compile and run C file."
  (interactive)
  (save-buffer)
  (let ((base (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    (compile (format "gcc %s -o %s && ./%s && rm %s"
                     (file-name-nondirectory (buffer-file-name))
                     base base base)
             t)))

;;; --- Magit: restart LSP on branch checkout ---
(defun salih/lsp-restart-workspaces-after-checkout ()
  "Restart all active LSP workspaces after a git branch switch.
Ensures language servers pick up files that may have changed on the new branch."
  (when (featurep 'lsp-mode)
    (let (seen-workspaces)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (and (bound-and-true-p lsp-mode)
                     (bound-and-true-p lsp--buffer-workspaces))
            (dolist (ws lsp--buffer-workspaces)
              (unless (memq ws seen-workspaces)
                (push ws seen-workspaces)
                (lsp-workspace-restart ws)))))))))

(after! magit
  (add-hook 'magit-post-checkout-hook #'salih/lsp-restart-workspaces-after-checkout))

(provide 'lr-prog)
