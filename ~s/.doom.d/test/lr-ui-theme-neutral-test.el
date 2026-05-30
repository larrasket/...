;;; lr-ui-theme-neutral-test.el --- tests for theme-neutral UI config -*- lexical-binding: t; -*-

(require 'ert)

(defconst lr-ui-theme-neutral-test--file
  (or load-file-name buffer-file-name))

(defun lr-ui-theme-neutral-test--module-path ()
  (expand-file-name "../modules/lr-ui.el"
                    (file-name-directory lr-ui-theme-neutral-test--file)))

(ert-deftest lr-ui-does-not-hard-code-hex-colors ()
  (with-temp-buffer
    (insert-file-contents (lr-ui-theme-neutral-test--module-path))
    (should-not (re-search-forward "\"#[0-9A-Fa-f]\\{6\\}\"" nil t))))

(ert-deftest lr-ui-does-not-hard-code-face-colors ()
  (with-temp-buffer
    (insert-file-contents (lr-ui-theme-neutral-test--module-path))
    (should-not (re-search-forward ":[a-z-]*color\\|:foreground\\|:background" nil t))))

(ert-deftest lr-ui-restores-theme-neutral-face-emphasis ()
  (with-temp-buffer
    (insert-file-contents (lr-ui-theme-neutral-test--module-path))
    (should (re-search-forward "(custom-set-faces!" nil t))
    (dolist (snippet '("'(font-lock-keyword-face     :weight bold)"
                        "'(font-lock-builtin-face     :weight bold)"
                        "'(font-lock-comment-face           :slant italic)"
                        "'(font-lock-variable-name-face     :slant italic)"
                        "'(org-level-1 :weight bold)"
                        "'(orderless-match-face-0 :weight bold)"
                        "'(org-document-title :inherit outline-1 :height 1.5 :weight normal)"))
      (goto-char (point-min))
      (should (search-forward snippet nil t)))))

;;; lr-ui-theme-neutral-test.el ends here
