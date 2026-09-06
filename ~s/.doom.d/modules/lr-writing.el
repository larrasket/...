;;; lr-writing.el --- Grammar checking via LanguageTool / ltex-ls -*- lexical-binding: t; -*-
;;
;; ltex-ls (Language Tool Extended) is a grammar checker that speaks LSP.
;; It is NOT the LaTeX language server - the name is misleading.
;; We use the Homebrew-installed ltex-ls (/opt/homebrew/bin/ltex-ls).
;;
;; The server is "registered" by symlinking:
;;   ~/.local/share/doom/profiles/user@default/lsp/ltex-ls/latest
;;     -> /opt/homebrew/Cellar/ltex-ls/16.0.0/libexec
;; plus a dummy ltex-ls-16.0.0.tar.gz so lsp-ltex detects version "16.0.0"
;; without hitting GitHub's API (github-tags package).

;;; Tell lsp-ltex where the server lives (before it loads)
;; These must be set before lsp-ltex.el is loaded so that defcustoms pick
;; up the right values at evaluation time.
(setq lsp-ltex-server-store-path
      (expand-file-name "ltex-ls"
                        (file-name-concat (or (getenv "XDG_DATA_HOME")
                                              "~/.local/share")
                                          "doom/profiles/user@default/lsp")))

;; Remove org-mode from ltex active modes BEFORE lsp-ltex registers its client.
;; lsp-register-client evaluates lsp-ltex-active-modes at load time.
;; org-mode is kept so `salih/ltex-toggle' (SPC t G) can start ltex-ls manually.
;; Auto-start is prevented by NOT adding salih/ltex-enable to org-mode-hook.
(setq lsp-ltex-active-modes nil)

;;; lsp-ltex settings
(after! lsp-ltex
  (setq lsp-ltex-language "en-US"
        lsp-ltex-completion-enabled nil         ; ltex-ls doesn't do completion
        ;; Spell-checking is handled by jinx; only check grammar/style.
        lsp-ltex-disabled-rules
        '(:en-US ["MORFOLOGIK_RULE_EN_US"
                  "WHITESPACE_RULE"
                  "COMMA_PARENTHESIS_WHITESPACE"])
        lsp-ltex-sentence-start-with-uppercase t))

;;; Enable in writing modes (manual)
;; ltex-ls runs over lsp-mode, whose completion-at-point conflicts with corfu,
;; so it is never auto-started.  `salih/ltex-toggle' (SPC t G) turns grammar
;; checking on/off in the current buffer on demand.
(defun salih/ltex-enable ()
  "Enable ltex-ls grammar diagnostics in the current buffer."
  (when (executable-find "ltex-ls")
    (lsp-deferred)))

(defun salih/ltex-toggle ()
  "Toggle ltex-ls grammar checking in the current buffer."
  (interactive)
  (if (and (featurep 'lsp-mode) (bound-and-true-p lsp-mode))
      (progn (lsp-disconnect) (message "ltex-ls disabled"))
    (salih/ltex-enable)
    (message "ltex-ls enabled")))

;; No auto-start hooks.  lsp-mode's completion-at-point conflicts with corfu in
;; these buffers (it was breaking completion) and spins up a JVM per buffer, so
;; grammar checking is started manually with `salih/ltex-toggle' (SPC t G).

;;; Add word to personal dictionary
(defun salih/ltex-add-word ()
  "Add word at point to lsp-ltex personal dictionary."
  (interactive)
  (if (fboundp 'lsp-ltex-plus-check-word)
      (call-interactively #'lsp-ltex-plus-check-word)
    ;; Fallback: use lsp execute code action
    (lsp-execute-code-action-by-kind "quickfix")))

;;; Keybindings
(map! :leader
      "t G" #'salih/ltex-toggle)  ; SPC t G - toggle grammar in any buffer

(map! :after lsp-ltex
      :localleader
      :map (org-mode-map markdown-mode-map message-mode-map)
      "ad" #'salih/ltex-add-word)

(provide 'lr-writing)
