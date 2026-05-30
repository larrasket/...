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

(ert-deftest lr-ui-does-not-override-theme-faces ()
  (with-temp-buffer
    (insert-file-contents (lr-ui-theme-neutral-test--module-path))
    (should-not (re-search-forward "(custom-set-faces!" nil t))))

;;; lr-ui-theme-neutral-test.el ends here
