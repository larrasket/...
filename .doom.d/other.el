;;; other.el -*- lexical-binding: t; -*-
(defun title-capitalization-string (s)
  (with-temp-buffer
    (erase-buffer)
    (insert s)
    (title-capitalization (point-min)
                          (point-max))
    (buffer-substring-no-properties (point-min)
                                    (point-max))))

(defun title-capitalization-dwim (&optional arg)
  (interactive)
  (cond
   (arg
    (title-capitalization-string arg))
   ((use-region-p)
    (title-capitalization-string
     (buffer-substring-no-properties (region-beginning)
                                     (region-end))))
   (t
    (title-capitalization-string
     (buffer-substring-no-properties (point-at-bol)
                                     (point-at-eol))))))
