;;; lr-writing.el --- Grammar checking via LanguageTool / ltex-ls -*- lexical-binding: t; -*-
;;
;; ltex-ls (Language Tool Extended) is a grammar checker that speaks LSP.
;; It is NOT the LaTeX language server — the name is misleading.
;; We use the Homebrew-installed ltex-ls (/opt/homebrew/bin/ltex-ls).
;;
;; The server is "registered" by symlinking:
;;   ~/.local/share/doom/profiles/user@default/lsp/ltex-ls/latest
;;     -> /opt/homebrew/Cellar/ltex-ls/16.0.0/libexec
;; plus a dummy ltex-ls-16.0.0.tar.gz so lsp-ltex detects version "16.0.0"
;; without hitting GitHub's API (github-tags package).

;;; --- Tell lsp-ltex where the server lives (before it loads) ---
;; This must be set before lsp-ltex.el is loaded so that
;; lsp-ltex-server-store-path has the correct value when defcustoms evaluate.
(setq lsp-ltex-server-store-path
      (expand-file-name "ltex-ls"
                        (file-name-concat (or (getenv "XDG_DATA_HOME")
                                              "~/.local/share")
                                          "doom/profiles/user@default/lsp")))

;;; --- lsp-ltex settings ---
(after! lsp-ltex
  (setq lsp-ltex-language "en-US"
        lsp-ltex-completion-enabled nil         ; ltex-ls doesn't do completion
        ;; Spell-checking is handled by jinx; only check grammar/style.
        lsp-ltex-disabled-rules
        '(:en-US ["MORFOLOGIK_RULE_EN_US"
                  "WHITESPACE_RULE"
                  "COMMA_PARENTHESIS_WHITESPACE"])
        lsp-ltex-sentence-start-with-uppercase t))

;;; --- Enable in writing modes ---
(defun salih/ltex-enable ()
  "Enable ltex-ls grammar checking in the current buffer.
ltex-ls only provides diagnostics — strip its completion capf so
Corfu doesn't try textDocument/completion (which ltex-ls doesn't support)."
  (when (executable-find "ltex-ls")
    (lsp-deferred)
    ;; Remove LSP completion — ltex-ls is diagnostics-only
    (add-hook 'lsp-after-open-hook #'salih/ltex-remove-completion-capf 0 t)))

(defun salih/ltex-remove-completion-capf ()
  "Strip lsp-completion-at-point from capf in grammar-checking buffers."
  (setq-local completion-at-point-functions
              (remq #'lsp-completion-at-point completion-at-point-functions)))

(add-hook 'org-mode-hook      #'salih/ltex-enable)
(add-hook 'message-mode-hook  #'salih/ltex-enable)
(add-hook 'markdown-mode-hook #'salih/ltex-enable)

;;; --- Add word to personal dictionary ---
(defun salih/ltex-add-word ()
  "Add word at point to lsp-ltex personal dictionary."
  (interactive)
  (if (fboundp 'lsp-ltex-plus-check-word)
      (call-interactively #'lsp-ltex-plus-check-word)
    ;; Fallback: use lsp execute code action
    (lsp-execute-code-action-by-kind "quickfix")))

(map! :after lsp-ltex
      :localleader
      :map (org-mode-map markdown-mode-map message-mode-map)
      "ad" #'salih/ltex-add-word)

(provide 'lr-writing)
