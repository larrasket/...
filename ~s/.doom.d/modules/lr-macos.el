;;; lr-macos.el --- macOS-specific settings -*- lexical-binding: t; -*-

(when (eq system-type 'darwin)
  (menu-bar-mode -1)
  (require 'ls-lisp)
  (setq mac-function-modifier  'control
        mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier   'meta
        mac-option-modifier    'none
        frame-title-format     nil
        org-download-screenshot-method "/usr/local/bin/pngpaste %s"
        ls-lisp-dirs-first     t
        ls-lisp-use-insert-directory-program nil
        epg-pinentry-mode      'loopback)

  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(defun salih/mail-current-file ()
  "Open macOS Mail app with a new message and attach the current buffer's file."
  (interactive)
  (unless buffer-file-name (user-error "Buffer is not visiting a file"))
  (when (buffer-modified-p) (save-buffer))
  (let ((script (format
                 "tell application \"Mail\"
  set newMessage to make new outgoing message with properties {subject:\"\", content:\"\", visible:true}
  tell newMessage
    make new attachment with properties {file name:\"%s\"} at after the last paragraph
  end tell
  activate
end tell" buffer-file-name)))
    (shell-command (concat "osascript -e " (shell-quote-argument script)))))

(provide 'lr-macos)
