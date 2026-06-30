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
    (should-not (re-search-forward ":[a-z-]*color\\s-+\"" nil t))
    (should-not (re-search-forward ":foreground\\s-+\"" nil t))
    (should-not (re-search-forward ":background\\s-+\"" nil t))))

(ert-deftest lr-ui-neutralizes-ef-maris-dark-line-number-palette ()
  (with-temp-buffer
    (insert-file-contents (lr-ui-theme-neutral-test--module-path))
    (should (search-forward "(fg-line-number-active fg-line-number-inactive)" nil t))
    (should (search-forward "(bg-line-number-active unspecified)" nil t))
    (should (search-forward "(bg-line-number-inactive unspecified)" nil t))))

(ert-deftest lr-ui-removes-ef-maris-dark-current-line-number-effects ()
  (with-temp-buffer
    (insert-file-contents (lr-ui-theme-neutral-test--module-path))
    (should (search-forward "line-number-current-line" nil t))
    (should (search-forward ":inherit line-number" nil t))
    (should (search-forward ":foreground unspecified" nil t))
    (should (search-forward ":background unspecified" nil t))
    (should (search-forward ":weight normal" nil t))))

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
