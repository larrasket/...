;;; ../configs/.doom.d/handy.el -*- lexical-binding: t; -*-
;;; contains some handy functions to use at once; not loaded by default


(defun gk-next-theme ()
  "Switch to the next theme in ‘custom-known-themes’.
If exhausted, disable themes.  If run again thereafter, wrap to
the beginning of the list."
  (interactive)
  (let* ((ct (or (car custom-enabled-themes)
                 (car custom-known-themes)))
         (next (cadr (memq ct custom-known-themes))))
    (when (memq next '(user changed))
      (setq next nil))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (if next
        (progn
          (load-theme next t)
          (message "Loaded theme ‘%S’" next))
      (message "All themes disabled"))))
