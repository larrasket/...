;;; private/awqat/config.el -*- lexical-binding: t; -*-
(use-package! awqat
  :commands (awqat-display-prayer-time-mode
             awqat-times-for-day)
  :custom
  (awqat-mode-line-format " ${prayer} (${hours}h${minutes}m) ")
  (awqat-update-interval (* 60 5)))
