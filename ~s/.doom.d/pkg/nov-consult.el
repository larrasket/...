;;; nov-consult.el --- Consult integration for nov.el epub reader -*- lexical-binding: t; -*-

;; Author: AlexLewandowski
;; Created with assistance from Claude (Anthropic)
;; URL: https://gist.github.com/AlexLewandowski/56f491f8d1a7fbc5d91ee8bcd3e03c54
;; Requires: nov.el, consult.el

;; Usage:
;;   M-x nov-consult-search (while in a nov-mode buffer)

(require 'nov)
(require 'consult)

(defvar nov-consult-search-history nil
  "History for nov-consult search.")

(defvar nov-consult--overlay nil
  "Overlay for highlighting matches during preview.")

(defface nov-consult-match-face
  '((t :inherit consult-highlight-match))
  "Face for search matches in nov-consult results.")

(defun nov-consult--truncate-around-match (str pattern n max-length)
  "Return STR truncated to show the Nth occurrence of PATTERN centered.
Full string is kept, but display is truncated to MAX-LENGTH chars around match."
  (let ((case-fold-search t)
        (pos 0)
        (count 0)
        match-start)
    ;; Find the nth match position
    (while (and (< count n) (string-match (regexp-quote pattern) str pos))
      (setq count (1+ count))
      (setq match-start (match-beginning 0))
      (setq pos (match-end 0)))
    (if (and match-start (> (length str) max-length))
        ;; Center around match
        (let* ((half (/ max-length 2))
               (start (max 0 (- match-start half)))
               (end (min (length str) (+ start max-length)))
               (start (max 0 (- end max-length)))) ; adjust if we hit the end
          (concat (if (> start 0) "..." "")
                  (substring str start end)
                  (if (< end (length str)) "..." "")))
      str)))

(defun nov-consult--highlight-nth-match (str pattern n)
  "Highlight only the Nth occurrence of PATTERN in STR."
  (let ((case-fold-search t)
        (result (copy-sequence str))
        (pos 0)
        (count 0))
    (while (and (< count n) (string-match (regexp-quote pattern) result pos))
      (setq count (1+ count))
      (when (= count n)
        (add-face-text-property (match-beginning 0) (match-end 0)
                                'nov-consult-match-face nil result))
      (setq pos (match-end 0)))
    result))

(defun nov-consult--collect-candidates (pattern)
  "Collect search candidates for PATTERN across all epub documents."
  (let ((version nov-epub-version)
        (index 1)
        (case-fold-search t)
        candidates
        metadata)
    (while (< index (1- (length nov-documents)))
      (seq-let (id &rest path) (aref nov-documents index)
        (let ((imagep (seq-find (lambda (item) (string-match-p (car item) path))
                                image-type-file-name-regexps))
              (default-directory (file-name-directory path)))
          (unless imagep
            (with-temp-buffer
              (if (and (version< version "3.0") (eq id nov-toc-id))
                  (insert (nov-ncx-to-html path))
                (insert (nov-slurp path)))
              (goto-char (point-min))
              (when (search-forward pattern nil t)
                (nov-render-html)
                (goto-char (point-min))
                (let ((doc-occurrence 0))
                  (while (search-forward pattern nil t)
                    (setq doc-occurrence (1+ doc-occurrence))
                    (let* ((line-raw (substring-no-properties (thing-at-point 'line)))
                           (line-clean (string-trim
                                        (replace-regexp-in-string "[\n\r\t]+" " " line-raw)))
                           ;; Count occurrence within this line for highlighting
                           (line-start (line-beginning-position))
                           (pos-in-line (- (point) line-start))
                           (line-occurrence
                            (with-temp-buffer
                              (insert line-clean)
                              (goto-char (point-min))
                              (let ((count 0))
                                (while (and (search-forward pattern nil t)
                                            (<= (match-end 0) pos-in-line))
                                  (setq count (1+ count)))
                                (1+ count))))
                           (line-highlighted (nov-consult--highlight-nth-match
                                              line-clean pattern line-occurrence))
                           (line-display (nov-consult--truncate-around-match
                                          line-highlighted pattern line-occurrence 100))
                           (file-label (format "File %d" index))
                           ;; Use display text as key with suffix instead of 'display property
                           (suffix (propertize (format " @%d.%d" index doc-occurrence)
                                               'face 'shadow))
                           (key (concat line-display suffix)))
                      ;; replace 'display property and use key for the display
                      (push (propertize key 'nov-file file-label) candidates)
                      (push (list key index doc-occurrence) metadata)))))))))
      (setq index (1+ index)))
    (cons (nreverse candidates) (nreverse metadata))))

(defun nov-consult--find-nth-match (pattern n)
  "Find the Nth occurrence of PATTERN in current buffer. Return (start . end) or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (count 0))
      (while (and (< count n) (search-forward pattern nil t))
        (setq count (1+ count)))
      (when (= count n)
        (cons (match-beginning 0) (match-end 0))))))

(defun nov-consult--state (metadata pattern)
  "Return a state function for nov-consult preview."
  (let ((original-doc nov-documents-index)
        (original-point (point))
        (original-window-start (window-start)))
    (lambda (action cand)
      (pcase action
        ('preview
         (ignore-errors
           (when (overlayp nov-consult--overlay)
             (delete-overlay nov-consult--overlay)))
         (when-let* ((match (and cand (assoc cand metadata #'string=)))
                     (index (nth 1 match))
                     (doc-occurrence (nth 2 match)))
           (nov-goto-document index)
           ;; Find the nth match in the rendered buffer
           (when-let ((bounds (nov-consult--find-nth-match pattern doc-occurrence)))
             (goto-char (car bounds))
             (setq nov-consult--overlay (make-overlay (car bounds) (cdr bounds)))
             (overlay-put nov-consult--overlay 'face 'isearch)
             (recenter))))
        ('exit
         (ignore-errors
           (when (overlayp nov-consult--overlay)
             (delete-overlay nov-consult--overlay)))
         (when (null cand)
           (nov-goto-document original-doc)
           (goto-char original-point)
           (set-window-start nil original-window-start)))))))

(defun nov-consult--group (cand transform)
  "Group function for nov-consult candidates."
  (if transform
      cand
    (get-text-property 0 'nov-file cand)))

;;;###autoload
(defun nov-consult-search (pattern)
  "Search the current epub for PATTERN using consult."
  (interactive
   (list (read-string "Search epub: " nil 'nov-consult-search-history)))
  (unless (eq major-mode 'nov-mode)
    (user-error "Not in a nov-mode buffer"))
  (when (string-empty-p pattern)
    (user-error "Empty search pattern"))
  (message "Searching epub...")
  (pcase-let ((`(,candidates . ,metadata) (nov-consult--collect-candidates pattern)))
    (if (null candidates)
        (message "No matches found for '%s'" pattern)
      (let ((selected (consult--read
                       candidates
                       :prompt (format "Results for '%s' (%d matches): " pattern (length candidates))
                       :category 'nov-search
                       :sort nil
                       :require-match t
                       :history 'nov-consult-search-history
                       :group #'nov-consult--group
                       :state (nov-consult--state metadata pattern))))
        ;; Clean up overlay
        (when (overlayp nov-consult--overlay)
          (delete-overlay nov-consult--overlay))
        (when-let* ((match (and selected (assoc selected metadata #'string=)))
                    (index (nth 1 match))
                    (doc-occurrence (nth 2 match)))
          (nov-goto-document index)
          (when-let ((bounds (nov-consult--find-nth-match pattern doc-occurrence)))
            (goto-char (car bounds))
            (pulse-momentary-highlight-region (car bounds) (cdr bounds)))
          (recenter))))))

(provide 'nov-consult)
;;; nov-consult.el ends here

