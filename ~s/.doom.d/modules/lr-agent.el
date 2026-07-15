;;; lr-agent.el --- Claude Code in Emacs via agent-shell/ACP -*- lexical-binding: t; -*-
;;
;; agent-shell drives Claude Code from an Emacs comint buffer through the
;; Agent Client Protocol (ACP).  It spawns the Homebrew-installed
;; `claude-agent-acp' bridge, which has a `#!/usr/bin/env node' shebang and in
;; turn talks to Claude Code using the same login/subscription as the `claude'
;; CLI.
;;
;; Emacs.app launches from Finder/Dock, so it does NOT inherit the shell PATH:
;; /opt/homebrew/bin is absent from `exec-path' (this is also why
;; `magit-git-executable' is hardcoded in config.el).  Without a fix the bridge
;; can't be found, and even when found its `env node' shebang would fail.  We
;; prepend Homebrew's bin to both `exec-path' (so Emacs finds the bridge) and
;; the PATH env var (so the spawned bridge finds `node' and `claude').

(let ((brew-bin "/opt/homebrew/bin"))
  (add-to-list 'exec-path brew-bin)
  (let ((path (or (getenv "PATH") "")))
    (unless (member brew-bin (split-string path path-separator))
      (setenv "PATH" (concat brew-bin path-separator path)))))

(use-package agent-shell
  :defer t
  :commands (agent-shell agent-shell-anthropic-start-claude-code)
  :init
  ;; SPC o c → start a Claude Code session in the current project.
  (map! :leader
        (:prefix ("o" . "open")
         :desc "Claude Code (agent-shell)" "c"
         #'agent-shell-anthropic-start-claude-code))
  :config
  ;; Authenticate with the Claude Code login/subscription (same credentials as
  ;; the `claude' CLI), not a separate ANTHROPIC_API_KEY.
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))
  ;; Launch the ACP bridge by absolute path — belt-and-suspenders alongside the
  ;; exec-path fix above, in case exec-path is ever reset.
  (setq agent-shell-anthropic-claude-acp-command
        '("/opt/homebrew/bin/claude-agent-acp")))

(provide 'lr-agent)
;;; lr-agent.el ends here
