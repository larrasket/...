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
        ;; No inline sideline clutter
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-hover nil
        lsp-modeline-code-actions-enable t
        lsp-modeline-diagnostics-enable t
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
  "Show all errors in a filterable consult buffer (LSP or flycheck)."
  (interactive)
  (if (and (featurep 'lsp-mode) (bound-and-true-p lsp-mode))
      (call-interactively #'consult-lsp-diagnostics)
    (call-interactively #'consult-flycheck)))

(defun salih/list-errors-project ()
  "Show all project-wide diagnostics via consult (LSP) or flycheck-projectile."
  (interactive)
  (if (and (featurep 'lsp-mode) (bound-and-true-p lsp-mode))
      ;; consult-lsp-diagnostics with universal arg = project-wide
      (let ((current-prefix-arg '(4)))
        (call-interactively #'consult-lsp-diagnostics))
    (call-interactively #'flycheck-projectile-list-errors)))

(defun salih/next-error ()
  "Jump to next flycheck error with peek preview."
  (interactive)
  (flycheck-next-error)
  (recenter))

(defun salih/prev-error ()
  "Jump to previous flycheck error with peek preview."
  (interactive)
  (flycheck-previous-error)
  (recenter))

(defun salih/show-error-at-point ()
  "Show full error message at point in a posframe or minibuffer."
  (interactive)
  (if (and (featurep 'lsp-ui-doc) (bound-and-true-p lsp-mode))
      (lsp-ui-doc-show)
    (flycheck-display-error-at-point)))

;;; --- Auto-show error list when compilation/check finishes with errors ---
(after! flycheck
  ;; Quicker feedback
  (setq flycheck-display-errors-delay 0.15
        flycheck-idle-change-delay 0.5)

  (setq flycheck-error-list-mode-line nil))

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
  (add-hook 'magit-post-checkout-hook #'salih/lsp-restart-workspaces-after-checkout)
  (add-hook 'magit-post-checkout-hook #'projectile-invalidate-cache))

;;; --- Fix flycheck's `org-lint' checker on Emacs 32 / new org-mode ---
;; Newer `org-lint` returns reports whose line slot is a propertized
;; string (e.g. #("442" ... org-lint-marker ...)) instead of a plain int.
;; Flycheck's built-in `org-lint' checker passes that string straight to
;; `flycheck-error-new-at', which calls `number-or-marker-p' on it and
;; errors out: "Wrong type argument: number-or-marker-p, #(\"442\" ...)".
;; Coerce the value to an int in the checker's :start function.
;; TODO Disable this when it gets resovled in FlyCheck
(after! flycheck
  (when (flycheck-valid-checker-p 'org-lint)
    ;; Use `put' directly instead of `(setf (flycheck-checker-get ...))':
    ;; the latter relies on flycheck's gv-setter being registered at
    ;; macro-expansion time, which isn't guaranteed when this file is
    ;; byte-compiled before flycheck loads — causing
    ;; "(void-function (setf flycheck-checker-get))".
    (put 'org-lint 'flycheck-start
          (lambda (checker callback)
            (condition-case err
                (let ((errors
                       (delq nil
                             (mapcar
                              (lambda (e)
                                (pcase e
                                  (`(,_n [,line ,_trust ,desc ,_checker])
                                   (flycheck-error-new-at
                                    (if (stringp line)
                                        (string-to-number line)
                                      line)
                                    nil 'info desc
                                    :checker checker))
                                  (_
                                   (flycheck-error-new-at
                                    1 nil 'warning
                                    (format "Unexpected org-lint format: %S" e)
                                    :checker checker))))
                              (org-lint)))))
                  (funcall callback 'finished errors))
              (error (funcall callback 'errored
                              (error-message-string err)))))))

  ;; --- Stop org-lint from running as a LIVE flycheck checker ---------------
  ;; `org-lint' is a whole-buffer linter not designed for continuous use.
  ;; In current org, its `org-lint-invalid-id-link' check calls
  ;; `org-id-update-id-locations', which rebuilds the org-id DB by scanning
  ;; every agenda + archive file with recursive `file-truename' — murderous
  ;; over a 160+ file iCloud roam tree, on EVERY flycheck idle tick.  Remove
  ;; it from the auto-run checker list; `M-x org-lint' still works on demand.
  (setq flycheck-checkers (delq 'org-lint flycheck-checkers)))

(provide 'lr-prog)
